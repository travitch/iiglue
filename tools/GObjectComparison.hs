{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Control.Monad ( unless, when )
import qualified Data.ByteString.Lazy as LBS
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( mapMaybe )
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Text ( Text )
import qualified Data.Text as T
import System.Environment ( getArgs )
import Text.Printf
import Text.XML as XML
import Text.XML.Cursor as XML

import Foreign.Inference.Interface

import Debug.Trace
debug = flip trace

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
      allFuncs = freeFunctions ++ methods
      libIndex = indexLibrary iface

  mapM_ (examineFunction libIndex) allFuncs

  return ()

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

lookupFunction :: FunctionIndex -> Text -> ForeignFunction
lookupFunction (FunctionIndex functionIndex _) name =
  M.findWithDefault errMsg (T.unpack name) functionIndex
  where
    errMsg = error ("Function not found " ++ show name)

examineFunction :: FunctionIndex -> Cursor -> IO ()
examineFunction functionIndex cur = do
  let [name] = cur $| laxAttribute "identifier"
      ff = lookupFunction functionIndex name
      retCur = cur $/ laxElement "return-value"
  print name
  checkAllocator functionIndex ff retCur

isRefCountedType :: FunctionIndex -> Text -> Bool
isRefCountedType functionIndex typeName =
  S.member (T.unpack typeName) (functionIndexRCTypes functionIndex)

checkAllocator :: FunctionIndex -> ForeignFunction -> [Cursor] -> IO ()
checkAllocator functionIndex ff [retCur] =
  case isAllocator ff of
    False -> do
      when (xfer == ["full"] && isRefCountedType functionIndex tyname) $ do
        -- This is only a problem if the type is not reference counted
        -- (since then the caller owns a reference)
        printf "Expected full transfer of ownership in return value to imply %s is an allocator\n" fname
    True ->
      -- Default is transfer=full, so accept the empty list
      unless (xfer == ["full"] || xfer == []) $ do
        printf "Expected allocator %s to fully transfer ownership: %s\n" fname (show xfer)
  where
    xfer = retCur $| laxAttribute "transfer-ownership"
    [[tyname]] = retCur $/ laxElement "type" &| laxAttribute "type"
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
