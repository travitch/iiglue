{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Control.Applicative
import Control.Monad ( forM_ )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text.Lazy.IO as T
import Options.Applicative
import Text.Blaze.Html5 ( Html, toHtml )
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Text as T

import Foreign.Inference.Interface

data Opts = Opts { ignoredAnnotations :: [ParamAnnotation]
                 , interfaceFiles :: [FilePath]
                 }
          deriving (Show)

cmdOpts :: Parser Opts
cmdOpts = Opts
  <$> many (option
      ( long "ignore"
      & short 'i'
      & metavar "ANNOTATION"
      & help "Ignore an annotation.  Can be specified multiple times"))
  <*> arguments str ( metavar "FILE" )

main :: IO ()
main = execParser args >>= realMain
  where
    args = info (helper <*> cmdOpts)
      ( fullDesc
      & progDesc "Compute aggregate statistics for FILEs"
      & header "iistats - report interface statistics")

realMain :: Opts -> IO ()
realMain opts = do
  interfaces <- mapM readLibraryInterface (interfaceFiles opts)
  let stats = map (interfaceStats (ignoredAnnotations opts)) interfaces
      h = renderStatsHTML stats
  T.putStrLn (T.renderHtml h)

renderStatsHTML :: [InterfaceStats] -> Html
renderStatsHTML stats = H.docTypeHtml $ do
  H.head $ do
    H.title "Aggregate Stats"
  H.body $ do
    mapM_ renderStatsTable stats
    renderStatsTable (mconcat stats)
    H.p $ do
      stringToHtml "Annotated Percent List = "
      toHtml (show pctList)
  where
    pctList = map annotatedFunctionPercent stats


renderStatsTable :: InterfaceStats -> Html
renderStatsTable stats = do
  H.h1 (toHtml (statsForLibrary stats))
  H.table $ do
    H.tr $ do
      H.td "Total Functions"
      H.td $ toHtml $ show $ statsTotalFunctions stats
    H.tr $ do
      H.td "Functions With Annotations"
      H.td $ toHtml $ show $ length (statsAnnotatedFunctions stats)
    H.tr $ do
      H.td "Total Annotations"
      H.td $ toHtml $ show $ statsTotalAnnotations stats
    H.tr $ do
      H.td "Percent With Annotations"
      H.td $ toHtml $ show $ annotatedFunctionPercent stats
    forM_ (M.toList (statsPerFuncAnnotation stats)) $ \(annot, lst) -> do
      H.tr $ do
        H.td $ toHtml (show annot)
        H.td $ toHtml $ show (length lst)
    forM_ (M.toList (statsPerParamAnnotation stats)) $ \(annot, lst) -> do
      H.tr $ do
        H.td $ toHtml (show annot)
        H.td $ toHtml $ show (length lst)

annotatedFunctionPercent :: InterfaceStats -> Double
annotatedFunctionPercent stats = annotLen / totalFuncs
  where
    annotLen :: Double
    annotLen = fromInteger $ toInteger $ length (statsAnnotatedFunctions stats)
    totalFuncs :: Double
    totalFuncs = fromInteger $ toInteger $ statsTotalFunctions stats

data InterfaceStats =
  InterfaceStats { statsForLibrary :: String
                 , statsTotalFunctions :: Int
                 , statsAnnotatedFunctions :: [ForeignFunction]
                 , statsPerParamAnnotation :: Map ParamAnnotation [Parameter]
                 , statsPerFuncAnnotation :: Map FuncAnnotation [ForeignFunction]
                 }

-- | Combine interface stats in a sane way.  The maps are updated with
-- unions.
instance Monoid InterfaceStats where
  mempty = InterfaceStats mempty 0 mempty mempty mempty
  mappend is1 is2 =
    InterfaceStats { statsForLibrary = "aggregate"
                   , statsTotalFunctions = statsTotalFunctions is1 + statsTotalFunctions is2
                   , statsAnnotatedFunctions = statsAnnotatedFunctions is1 ++ statsAnnotatedFunctions is2
                   , statsPerParamAnnotation = M.unionWith (++) (statsPerParamAnnotation is1) (statsPerParamAnnotation is2)
                   , statsPerFuncAnnotation = M.unionWith (++) (statsPerFuncAnnotation is1) (statsPerFuncAnnotation is2)
                   }

interfaceStats :: [ParamAnnotation] -> LibraryInterface -> InterfaceStats
interfaceStats ignored libIface =
  InterfaceStats { statsForLibrary = libraryName libIface
                 , statsTotalFunctions = length funcs
                 , statsAnnotatedFunctions = filter (funcHasAnnotation ignored) funcs
                 , statsPerParamAnnotation = foldr (collectParamAnnotations ignored) mempty params
                 , statsPerFuncAnnotation = foldr collectFuncAnnotations mempty funcs
                 }
  where
    params = concatMap foreignFunctionParameters funcs
    funcs = libraryFunctions libIface

statsTotalAnnotations :: InterfaceStats -> Int
statsTotalAnnotations is =
  length (concat (M.elems (statsPerParamAnnotation is))) +
    length (concat (M.elems (statsPerFuncAnnotation is)))

collectFuncAnnotations :: ForeignFunction
                          -> Map FuncAnnotation [ForeignFunction]
                          -> Map FuncAnnotation [ForeignFunction]
collectFuncAnnotations ff acc =
  foldr go acc (foreignFunctionAnnotations ff)
  where
    go annot = M.insertWith' (++) annot [ff]

collectParamAnnotations :: [ParamAnnotation]
                           -> Parameter
                           -> Map ParamAnnotation [Parameter]
                           -> Map ParamAnnotation [Parameter]
collectParamAnnotations ignored p acc =
  foldr go acc (parameterAnnotations p)
  where
    go annot m =
      case annot `elem` ignored of
        False -> M.insertWith' (++) annot [p] m
        True -> m

funcHasAnnotation :: [ParamAnnotation] -> ForeignFunction -> Bool
funcHasAnnotation ignored ff =
  not (null fannots) || any hasParamAnnotations params
  where
    fannots = foreignFunctionAnnotations ff
    params = foreignFunctionParameters ff
    hasParamAnnotations = not . null . filter notIgnored . parameterAnnotations
    notIgnored = not . (`elem` ignored)

stringToHtml :: String -> Html
stringToHtml = toHtml