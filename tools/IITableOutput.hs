{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Control.Applicative
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative hiding ( (&) )
import System.FilePath
import Text.LaTeX.Base as Tex

import Foreign.Inference.Interface

data Opts = Opts { interfaceFiles :: [FilePath]
                 }
          deriving (Show)

cmdOpts :: Parser Opts
cmdOpts = Opts <$> arguments str ( metavar "FILE" )

main :: IO ()
main = execParser args >>= realMain
  where
    args = info (helper <*> cmdOpts)
      ( fullDesc
      <> progDesc "Output LaTeX tables for the dissertation"
      <> header "iitables - generate LaTeX tables")

realMain :: Opts -> IO ()
realMain opts = do
  interfaces <- mapM readLibraryInterface (interfaceFiles opts)
  let t = renderPointerStatsTable interfaces
  T.putStrLn (Tex.render t)

renderPointerStatsTable :: [LibraryInterface] -> LaTeX
renderPointerStatsTable ifaces =
  mconcat (map pointerSummaryToRow pointerSummaries) <> {-avgs <>-} totals
  where
    pointerSummaries = map toPointerSummary ifaces
    totals = textbf (texy ("Total" :: Text))
               & summField psNumFuncs pointerSummaries
               & summField psOutFuncs pointerSummaries
               & summField psOutParams pointerSummaries
               & summField psInOutFuncs pointerSummaries
               & summField psInOutParams pointerSummaries
               & summField psArrayFuncs pointerSummaries
               & summField psArrayParams pointerSummaries
               & texy (hmean (map psPercentAnnot pointerSummaries))
               <> lnbk %: ""

hmean :: [Int] -> Int
hmean ns = round $ realN / sum recips
  where
    realN :: Double
    realN = fromIntegral (length ns)
    recips :: [Double]
    recips = map ((1.0/) . fromIntegral) ns

summField :: (a -> Int) -> [a] -> LaTeX
summField f = texy . foldr (+) 0 . map f

pointerSummaryToRow :: PointerSummary -> LaTeX
pointerSummaryToRow ps =
  texy (psLibraryName ps) &
    texy (psNumFuncs ps) &
    texy (psOutFuncs ps) &
    texy (psOutParams ps) &
    texy (psInOutFuncs ps) &
    texy (psInOutParams ps) &
    texy (psArrayFuncs ps) &
    texy (psArrayParams ps) &
    texy (psPercentAnnot ps) <>
    lnbk %: psLibraryName ps

data PointerSummary =
  PointerSummary { psLibraryName :: Text
                 , psNumFuncs :: Int
                 , psOutFuncs :: Int
                 , psOutParams :: Int
                 , psInOutFuncs :: Int
                 , psInOutParams :: Int
                 , psArrayFuncs :: Int
                 , psArrayParams :: Int
                 , psPercentAnnot :: Int
                 }
  deriving (Eq, Ord, Show)

toPointerSummary :: LibraryInterface -> PointerSummary
toPointerSummary i =
  PointerSummary { psLibraryName = T.pack $ dropExtensions $ libraryName i
                 , psNumFuncs = nFuncs
                 , psOutFuncs = countIf (funcHasAnnot (==PAOut)) fs
                 , psOutParams = countIf (paramHasAnnot (==PAOut)) ps
                 , psInOutFuncs = countIf (funcHasAnnot (==PAInOut)) fs
                 , psInOutParams = countIf (paramHasAnnot (==PAInOut)) ps
                 , psArrayFuncs = countIf (funcHasAnnot isArray) fs
                 , psArrayParams = countIf (paramHasAnnot isArray) ps
                                   -- FIXME: This could be updated with not-null
                 , psPercentAnnot = round $ 100.0 * totalAnnotFuncs / fromIntegral nFuncs
                 }
  where
    totalAnnotFuncs :: Double
    totalAnnotFuncs = fromIntegral $ countIf (funcHasAnnot isPointerAnnot) fs
    nFuncs = length fs
    fs = libraryFunctions i
    ps = concatMap foreignFunctionParameters fs

isPointerAnnot :: ParamAnnotation -> Bool
isPointerAnnot (PAArray _) = True
isPointerAnnot PAOut = True
isPointerAnnot PAInOut = True
isPointerAnnot _ = False

isArray :: ParamAnnotation -> Bool
isArray (PAArray _) = True
isArray _ = False

countIf :: (t -> Bool) -> [t] -> Int
countIf p = length . filter p

paramHasAnnot :: (ParamAnnotation -> Bool) -> Parameter -> Bool
paramHasAnnot p = any p . parameterAnnotations

funcHasAnnot :: (ParamAnnotation -> Bool) -> ForeignFunction -> Bool
funcHasAnnot p = or . map (paramHasAnnot p) . foreignFunctionParameters

{-

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
-}
