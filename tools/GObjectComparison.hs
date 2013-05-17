{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Control.Monad ( when, unless )
import Control.Monad.Trans.RWS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as F
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( mapMaybe )
import Data.Monoid
import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Text ( Text )
import qualified Data.Text as T
import System.Environment ( getArgs )
import Text.Printf
import Text.XML as XML
import Text.XML.Cursor as XML

import Foreign.Inference.Interface

-- import Debug.Trace
-- debug = flip trace

-- | A diagnostic for a single function.  The first String is the
-- function name and the list is a list of diagnostics for the
-- function
data Diag = Diag Text [String]

instance Show Diag where
  show = showDiag

showDiag :: Diag -> String
showDiag (Diag n ds) = printf "%s\n%s" (T.unpack n) ds'
  where
    ds' = unlines $ map (\s -> "  " ++ s) ds

type LogM = RWS Text (Seq Diag) [String]

data FunctionIndex =
  FunctionIndex {
    functionIndexMap :: Map String ForeignFunction,
    functionIndexRCTypes :: Set String
    }

main :: IO ()
main = do
  [ifaceFile, girFile] <- getArgs
  iface <- readLibraryInterface ifaceFile
  girBS <- LBS.readFile girFile
  let girDoc = parseLBS_ def girBS
      girCursor = fromDocument girDoc
      freeFunctions = girCursor $// laxElement "function"
      methods = girCursor $// laxElement "method"
      allFuncs = (tagFree freeFunctions) ++ (tagMethod methods)
      libIndex = indexLibrary iface

  let (_, diags) = evalRWS (mapM_ (examineFunction libIndex) allFuncs) undefined []
  F.mapM_ print diags
  return ()

tagFree :: [a] -> [(Bool, a)]
tagFree = zip (repeat False)

tagMethod :: [a] -> [(Bool, a)]
tagMethod = zip (repeat True)

indexLibrary :: LibraryInterface -> FunctionIndex
indexLibrary iface =
  FunctionIndex {
    functionIndexMap = foldr addFunction mempty (libraryFunctions iface),
    functionIndexRCTypes = S.fromList $ mapMaybe toStructName annotatedTypes
    }
  where
    addFunction ff = M.insert (foreignFunctionName ff) ff
    hasTypeAnnotation = not . null . snd
    annotatedTypes = map fst $ filter hasTypeAnnotation (libraryTypes iface)

lookupFunction :: FunctionIndex -> Text -> Maybe ForeignFunction
lookupFunction (FunctionIndex functionIndex _) name =
  M.lookup (T.unpack name) functionIndex

-- | Start a new environment for the current function.  After the
-- function is analyzed (before this function returns), commit the
-- diagnostics generated (if any) to the log.
--
-- Note that this function clears the State (which is meant to be
-- function-local).
withFunction :: Text -> LogM () -> LogM ()
withFunction n a = put [] >> local (const n) a >> logDiagnostics n

logDiagnostics :: Text -> LogM ()
logDiagnostics n = do
  s <- get
  case null s of
    True -> return ()
    False -> tell $! Seq.singleton (Diag n s)

reportDiagnostic :: String -> LogM ()
reportDiagnostic s = modify (s:)

examineFunction :: FunctionIndex -> (Bool, Cursor) -> LogM ()
examineFunction functionIndex (isMethod, cur) = do
  let [name] = cur $| laxAttribute "identifier"
      ff = lookupFunction functionIndex name
      retCur = cur $/ laxElement "return-value"
      paramCurs = cur $// laxElement "parameter"
      -- If this is a method, drop the first parameter in the inferred
      -- parameter list since it is treated as an implicit "this" and
      -- does not have a node in the GIR

  case ff of
    Nothing -> return ()
    Just ff' -> do
      let params = case isMethod of
            True -> drop 1 (foreignFunctionParameters ff')
            False -> foreignFunctionParameters ff'
      withFunction name $ do
        checkAllocator functionIndex ff' retCur
        mapM_ checkParam (zip params paramCurs)

-- TODO: check escape, array, nullability (allow-none)
checkParam :: (Parameter, Cursor) -> LogM ()
checkParam (p, cur) = do
  let dir = cur $| laxAttribute "direction"
      allowNone = cur $| laxAttribute "allow-none"
      xfer = cur $| laxAttribute "transfer-ownership"

  -- The out param checks are less useful right now because glib
  -- treats all pointers to scalar types as out parameters by default.
  -- This will be more useful for pointers to structs when we extend
  -- the out param analysis to identify those.
  when (dir == ["out"] && not (isOutParam p)) $ do
    reportDiagnostic $ printf "Expected out parameter %s, but analysis disagrees" (parameterName p)
  when (isOutParam p && dir /= ["out"]) $ do
    reportDiagnostic $ printf "Analysis claims that %s is an out param" (parameterName p)


  -- allow-none is only specified as allow-none="1", meaning it is
  -- never present for a not-null parameter.  When we infer PANotNull,
  -- we expect an empty list.
  when (allowNone == ["1"] && isNotNull p) $ do
    reportDiagnostic $ printf "Expected parameter %s to be nonnull, analysis disagrees" (parameterName p)
  when (isNotNull p && allowNone /= []) $ do
    reportDiagnostic $ printf "Analysis claims %s is nonnull" (parameterName p)


  -- The escape annotation has a lot of false positives right now.
  -- Just check for cases where gir says a parameter escapes and
  -- ensure we get that too.
  when (xfer == ["full"] && not (isEscapeParam p)) $ do
    reportDiagnostic $ printf "Analysis claims %s does not escape, gir says it is transfered" (parameterName p)

  return ()

isEscapeParam :: Parameter -> Bool
isEscapeParam = any (==PAEscape) . parameterAnnotations

isNotNull :: Parameter -> Bool
isNotNull = any (==PANotNull) . parameterAnnotations

isOutParam :: Parameter -> Bool
isOutParam = any (==PAOut) . parameterAnnotations

isRefCountedType :: FunctionIndex -> Text -> Bool
isRefCountedType functionIndex typeName =
  S.member (T.unpack typeName) (functionIndexRCTypes functionIndex)

checkAllocator :: FunctionIndex -> ForeignFunction -> [Cursor] -> LogM ()
checkAllocator functionIndex ff [retCur] =
  case isAllocator ff of
    False -> do
      when (xfer == ["full"] && isRefCountedType functionIndex tyname) $ do
        -- This is only a problem if the type is not reference counted
        -- (since then the caller owns a reference)
        reportDiagnostic $ printf "Expected full transfer of ownership in return value to imply %s is an allocator" fname
    True ->
      -- Default is transfer=full, so accept the empty list
      unless (xfer == ["full"] || xfer == []) $ do
        reportDiagnostic $ printf "Expected allocator %s to fully transfer ownership: %s" fname (show xfer)
  where
    xfer = retCur $| laxAttribute "transfer-ownership"
    tyname = case retCur $/ laxElement "type" &| laxAttribute "type" of
      [[tname]] -> tname
      _ -> case retCur $// laxElement "array" &| laxAttribute "type" of
        [[tname]] -> tname
        _ -> error ("What is this " ++ (show ff))
    fname = foreignFunctionName ff
checkAllocator _ ff _ =
  error ("Multiple return values for function " ++ foreignFunctionName ff)

isAllocator :: ForeignFunction -> Bool
isAllocator ff = any isAlloc annots
  where
    annots = foreignFunctionAnnotations ff
    isAlloc (FAAllocator _) = True
    isAlloc _ = False

-- | Extract the name of a struct, stripping off leading underscores.  This is
-- a fast heuristic to get rid of the manual name mangling used with:
--
-- > typedef struct _Foo {} Foo;
--
-- since we don't get to see typedefs in LLVM.
toStructName :: CType -> Maybe String
toStructName (CStruct n _) = Just $! demangleName n
toStructName _ = Nothing

demangleName :: String -> String
demangleName = dropWhile (=='_')
