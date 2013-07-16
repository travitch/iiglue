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

data Opts = Opts { destRoot :: FilePath
                 , interfaceFiles :: [FilePath]
                 }
          deriving (Show)

cmdOpts :: Parser Opts
cmdOpts =  Opts
  <$> strOption
      ( long "root"
      <> short 'r'
      <> metavar "DIR"
      <> help "The root directory in which generated tables will be placed")
  <*> arguments str ( metavar "FILE" )

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
  let mt = renderMemoryStatsTable interfaces
      pt = renderPointerStatsTable interfaces
      ot = renderOutStatsTable interfaces
      at = renderArrayStatsTable interfaces
      nt = renderNullStatsTable interfaces
  T.writeFile (destRoot opts </> "pointers/overall-table.tex") (Tex.render pt)
  T.writeFile (destRoot opts </> "pointers/null-table.tex") (Tex.render nt)
  T.writeFile (destRoot opts </> "pointers/out-table.tex") (Tex.render ot)
  T.writeFile (destRoot opts </> "pointers/array-table.tex") (Tex.render at)
  T.writeFile (destRoot opts </> "memory/big-table.tex") (Tex.render mt)

-- renderPointerStatsTable :: [LibraryInterface] -> LaTeX
-- renderPointerStatsTable ifaces =
--   mconcat (map pointerSummaryToRow pointerSummaries)
--     <> (raw "\\midrule" %: "")
--     <> totals
--   where
--     pointerSummaries = map toPointerSummary ifaces
--     totals = textbf (texy ("Total" :: Text))
--                & summField psNumFuncs pointerSummaries
--                & summField psOutFuncs pointerSummaries
--                & summField psOutParams pointerSummaries
--                & summField psInOutFuncs pointerSummaries
--                & summField psInOutParams pointerSummaries
--                & summField psArrayFuncs pointerSummaries
--                & summField psArrayParams pointerSummaries
--                & texy (hmean (map psPercentAnnot pointerSummaries))
--                <> lnbk %: ""

renderPointerStatsTable :: [LibraryInterface] -> LaTeX
renderPointerStatsTable ifaces =
  mconcat (map pointerSummaryToRow pointerSummaries)
    <> (raw "\\midrule" %: "")
    <> totals
  where
    pointerSummaryToRow :: PointerSummary -> LaTeX
    pointerSummaryToRow ps =
      texy (psLibraryName ps) &
      texy (psNumFuncs ps) &
      texy (psPercentAnnot ps) <>
      lnbk %: psLibraryName ps
    pointerSummaries = map toPointerSummary ifaces
    totals = textbf (texy ("Total" :: Text))
               & summField psNumFuncs pointerSummaries
               & texy (hmean (map psPercentAnnot pointerSummaries))
               <> lnbk %: ""


renderOutStatsTable :: [LibraryInterface] -> LaTeX
renderOutStatsTable ifaces =
  mconcat (map pointerSummaryToRow pointerSummaries)
    <> (raw "\\midrule" %: "")
    <> totals
  where
    pointerSummaryToRow :: PointerSummary -> LaTeX
    pointerSummaryToRow ps =
      texy (psLibraryName ps) &
      texy (psNumFuncs ps) &
      texy (psOutFuncs ps) &
      texy (psOutParams ps) &
      texy (psInOutFuncs ps) &
      texy (psInOutParams ps) <>
      lnbk %: psLibraryName ps
    pointerSummaries = map toPointerSummary ifaces
    totals = textbf (texy ("Total" :: Text))
               & summField psNumFuncs pointerSummaries
               & summField psOutFuncs pointerSummaries
               & summField psOutParams pointerSummaries
               & summField psInOutFuncs pointerSummaries
               & summField psInOutParams pointerSummaries
               <> lnbk %: ""

renderArrayStatsTable :: [LibraryInterface] -> LaTeX
renderArrayStatsTable ifaces =
  mconcat (map pointerSummaryToRow pointerSummaries)
    <> (raw "\\midrule" %: "")
    <> totals
  where
    pointerSummaryToRow :: PointerSummary -> LaTeX
    pointerSummaryToRow ps =
      texy (psLibraryName ps) &
      texy (psNumFuncs ps) &
      texy (psArrayFuncs ps) &
      texy (psArrayParams ps)  <>
      lnbk %: psLibraryName ps
    pointerSummaries = map toPointerSummary ifaces
    totals = textbf (texy ("Total" :: Text))
               & summField psNumFuncs pointerSummaries
               & summField psArrayFuncs pointerSummaries
               & summField psArrayParams pointerSummaries
               <> lnbk %: ""

renderNullStatsTable :: [LibraryInterface] -> LaTeX
renderNullStatsTable ifaces =
  mconcat (map pointerSummaryToRow pointerSummaries)
    <> (raw "\\midrule" %: "")
    <> totals
  where
    pointerSummaryToRow :: PointerSummary -> LaTeX
    pointerSummaryToRow ps =
      texy (psLibraryName ps) &
      texy (psNumFuncs ps) &
      texy (psNullFuncs ps) &
      texy (psNullParams ps) <>
      lnbk %: psLibraryName ps
    pointerSummaries = map toPointerSummary ifaces
    totals = textbf (texy ("Total" :: Text))
               & summField psNumFuncs pointerSummaries
               & summField psNullFuncs pointerSummaries
               & summField psNullParams pointerSummaries
               <> lnbk %: ""


renderMemoryStatsTable :: [LibraryInterface] -> LaTeX
renderMemoryStatsTable ifaces =
  mconcat (map memorySummaryToRow memorySummaries)
    <> (raw "\\midrule" %: "")
    <> totals
  where
    memorySummaries = map toMemorySummary ifaces
    totals = textbf (texy ("Total" :: Text))
      & summField msNumFuncs memorySummaries
      & summField msNumAllocators memorySummaries
      & summField msNumFinalizers memorySummaries
      <> lnbk %: ""

memorySummaryToRow :: MemorySummary -> LaTeX
memorySummaryToRow ms =
  texy (msLibraryName ms)
    & texy (msNumFuncs ms)
    & texy (msNumAllocators ms)
    & texy (msNumFinalizers ms)
    <> lnbk %: ""

-- | Harmonic mean of a list of ints
hmean :: [Int] -> Int
hmean ns = round $ realN / sum recips
  where
    realN :: Double
    realN = fromIntegral (length ns)
    recips :: [Double]
    recips = map ((1.0/) . fromIntegral) ns

summField :: (a -> Int) -> [a] -> LaTeX
summField f = texy . foldr (+) 0 . map f

data MemorySummary =
  MemorySummary { msLibraryName :: Text
                , msNumFuncs :: Int
                , msNumAllocators :: Int
                , msNumFinalizers :: Int
                }
  deriving (Eq, Ord, Show)

toMemorySummary :: LibraryInterface -> MemorySummary
toMemorySummary i =
  MemorySummary { msLibraryName = T.pack $ dropExtensions $ libraryName i
                , msNumFuncs = nFuncs
                , msNumFinalizers = countIf (paramHasAnnot (==PAFinalize)) ps
                , msNumAllocators = countIf (funcIs isAlloc) fs
                }
  where
    nFuncs = length fs
    fs = libraryFunctions i
    ps = concatMap foreignFunctionParameters fs

isAlloc :: FuncAnnotation -> Bool
isAlloc (FAAllocator _) = True
isAlloc _ = False

data PointerSummary =
  PointerSummary { psLibraryName :: Text
                 , psNumFuncs :: Int
                 , psOutFuncs :: Int
                 , psOutParams :: Int
                 , psInOutFuncs :: Int
                 , psInOutParams :: Int
                 , psArrayFuncs :: Int
                 , psArrayParams :: Int
                 , psNullFuncs :: Int
                 , psNullParams :: Int
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
                 , psNullFuncs = countIf (funcHasAnnot (==PANotNull)) fs
                 , psNullParams = countIf (paramHasAnnot (==PANotNull)) ps
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
isPointerAnnot PANotNull = True
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

funcIs :: (FuncAnnotation -> Bool) -> ForeignFunction -> Bool
funcIs p = or . map p . foreignFunctionAnnotations

