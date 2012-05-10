module Main ( main ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( mapMaybe )
import Data.Monoid
import qualified Data.Set as S
import System.Environment ( getArgs )
import System.IO ( stdout )
import Text.XML.Generator

import Foreign.Inference.Interface

main :: IO ()
main = do
  [ifaceFile] <- getArgs
  iface <- readLibraryInterface ifaceFile
  let (classGroups, freeFunctions) = groupByMethods iface
      classFragments = map (uncurry classToFragment) (M.toList classGroups)
      funcFragments = map (functionToFragment "function") freeFunctions

      x = doc defaultDocInfo $
        xelem "repository" $
         xelems (funcFragments ++ classFragments)

  LBS.hPut stdout (xrender x)
  return ()

functionToFragment :: String -> ForeignFunction -> Xml Elem
functionToFragment funcType ff =
  xelem funcType (funcAttrs <#> xelems [returnElem, pelems])
  where
    n = foreignFunctionName ff
    funcAttrs = xattrs [xattr "name" n, xattr "c:identifier" n]
    returnElem = returnElemFragment ff
    paramElems = map paramFragment (foreignFunctionParameters ff)
    pelems = xelem "parameters" $ xelems paramElems

paramFragment :: Parameter -> Xml Elem
paramFragment p =
  xelem "parameter" (attrs <#> typeElem)
  where
    attrs = xattrs [xattr "name" (parameterName p), xattr "transfer-ownership" xfer]
    xfer = case parameterEscapes p of
      False -> "none"
      True -> "full"
    typeElem = typeToElem (parameterType p)

parameterEscapes :: Parameter -> Bool
parameterEscapes = any (==PAEscape) . parameterAnnotations

returnElemFragment :: ForeignFunction -> Xml Elem
returnElemFragment ff =
  xelem "return-value" (xattr "transfer-ownership" transferType <#> typeElem)
  where
    rt = foreignFunctionReturnType ff
    -- Can also model containers...
    transferType = case isAllocator ff of
      True -> "full"
      False -> "none"
    typeElem = typeToElem rt

isAllocator :: ForeignFunction -> Bool
isAllocator ff = any isAlloc annots
  where
    annots = foreignFunctionAnnotations ff
    isAlloc (FAAllocator _) = True
    isAlloc _ = False

-- FIXME make some real names here.  Probably need to pass in the set
-- of ref counted types to identify gobjects vs other ptrs
typeToElem :: CType -> Xml Elem
typeToElem t = xelem "type" (xattrs [xattr "name" "", xattr "c:type" cname])
  where
    cname = case t of
      CVoid -> "void"
      CInt n -> "gint" ++ show n
      CUInt n -> "guint" ++ show n
      CFloat -> "gfloat"
      CDouble -> "gdouble"
      _ -> "other"

classToFragment :: String -> [ForeignFunction] -> Xml Elem
classToFragment typeName methods =
  xelem "class" (attrs <#> members)
  where
    attrs = xattrs [ xattr "name" typeName
                   , xattr "c:type" typeName
                   ]
    members = xelems $ map (functionToFragment "method") methods


-- | Extract the name of a struct, stripping off leading underscores.  This is
-- a fast heuristic to get rid of the manual name mangling used with:
--
-- > typedef struct _Foo {} Foo;
--
-- since we don't get to see typedefs in LLVM.
toStructName :: CType -> Maybe String
toStructName (CStruct n _) = Just $! dropWhile (=='_') n
toStructName _ = Nothing

-- | Decompose a foreign library interface into methods attached to
-- objects, along with the rest of the (free) functions.
--
-- The matching is done on the type of the first argument IFF the
-- first argument is a pointer to a ref counted type more specific
-- than GObject.
--
-- FIXME: Also add constructors to the method list for an object
groupByMethods :: LibraryInterface -> (Map String [ForeignFunction], [ForeignFunction])
groupByMethods iface =
  foldr classifyFunction (mempty, mempty) (libraryFunctions iface)
  where
    hasTypeAnnotation = not . null . snd
    rcStructs = map fst $ filter hasTypeAnnotation (libraryTypes iface)
    rcTypeNames = S.fromList $ mapMaybe toStructName rcStructs

    classifyFunction f (m, freeFuncs) =
      case foreignFunctionParameters f of
        [Parameter (CPointer (CStruct name _)) _ _] ->
          case S.member name rcTypeNames of
            False -> (m, f : freeFuncs)
            True -> (M.insertWith' (++) name [f] m, freeFuncs)
        _ -> (m, f : freeFuncs)