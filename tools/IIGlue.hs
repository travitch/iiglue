module Main ( main ) where

import Control.Applicative
import Control.Exception ( tryJust )
import Control.Monad ( guard )
import Data.Lens.Common
import Data.Monoid
import Options.Applicative
import System.Environment ( getEnv )
import System.FilePath
import System.IO.Error ( isDoesNotExistError )

import Codec.Archive

import LLVM.Analysis
import LLVM.Analysis.CallGraph
import LLVM.Analysis.CallGraphSCCTraversal
import LLVM.Parse

import Foreign.Inference.Diagnostics
import Foreign.Inference.Interface
import Foreign.Inference.Report
import Foreign.Inference.Preprocessing
import Foreign.Inference.Analysis.Allocator
import Foreign.Inference.Analysis.Array
import Foreign.Inference.Analysis.Escape
import Foreign.Inference.Analysis.Finalize
import Foreign.Inference.Analysis.Nullable
import Foreign.Inference.Analysis.Output
import Foreign.Inference.Analysis.RefCount
import Foreign.Inference.Analysis.Return
import Foreign.Inference.Analysis.ScalarEffects
import Foreign.Inference.Analysis.IndirectCallResolver
import Foreign.Inference.Analysis.Util.CompositeSummary

-- | The repository location is first chosen based on an environment
-- variable.  The command line argument, if specified, will override
-- it.  If the environment variable is not set, the command line
-- argument must be specified.
data Opts = Opts { inputDependencies :: [String]
                 , repositoryLocation :: FilePath
                 , diagnosticLevel :: Classification
                 , librarySource :: Maybe FilePath
                 , reportDir :: Maybe FilePath
                 , annotationFile :: Maybe FilePath
                 , inputFile :: FilePath
                 }
          deriving (Show)

cmdOpts :: FilePath -> Parser Opts
cmdOpts defaultRepo = Opts
          <$> strOption
              ( long "dependency"
              & short 'd'
              & metavar "DEPENDENCY"
              & multi
              & help "A dependency of the library being analyzed.")
          <*> strOption
              ( long "repository"
              & short 'r'
              & metavar "DIRECTORY"
              & value defaultRepo
              & help "The directory containing dependency summaries.  The summary of the input library will be stored here. (Default: consult environment)")
          <*> option
              ( long "diagnostics"
              & metavar "DIAGNOSTIC"
              & value Warning
              & help "The level of diagnostics to show (Debug, Info, Warning, Error).  Default: Warning" )
          <*> option
              ( long "source"
              & short 's'
              & metavar "FILE"
              & help "The source for the library being analyzed (tarball or zip archive).  If provided, a report will be generated"
              & value Nothing
              & reader (Just . str))
          <*> option
              ( long "reportDir"
              & metavar "DIRECTORY"
              & help "The directory in which the summary report will be produced.  Defaults to the REPOSITORY."
              & value Nothing
              & reader (Just . str))
          <*> option
              ( long "annotations"
              & short 'a'
              & metavar "FILE"
              & help "An optional file containing annotations for the library being analyzed."
              & value Nothing
              & reader (Just . str))
          <*> argument str ( metavar "FILE" )


main :: IO ()
main = do
  mRepLoc <- tryJust (guard . isDoesNotExistError) (getEnv "INFERENCE_REPOSITORY")
  let repLoc = either (error "No dependency repository specified") id mRepLoc
      args = info (helper <*> cmdOpts repLoc)
        ( fullDesc
        & progDesc "Infer interface annotations for FILE (which can be bitcode or llvm assembly)"
        & header "iiglue - A frontend for the FFI Inference engine")

  execParser args >>= realMain

realMain :: Opts -> IO ()
realMain opts = do
  let name = takeBaseName (inputFile opts)
      parseOpts = case librarySource opts of
        Nothing -> defaultParserOptions { metaPositionPrecision = PositionNone }
        Just _ -> defaultParserOptions
  mm <- readBitcode (parseLLVMFile parseOpts) (inputFile opts)
  either error (dump opts name) mm

dump :: Opts -> String -> Module -> IO ()
dump opts name m = do
  let pta = identifyIndirectCallTargets m
      cg = mkCallGraph m pta []
      deps = inputDependencies opts
      repo = repositoryLocation opts
  baseDeps <- loadDependencies [repo] deps
  ds <- case annotationFile opts of
    Nothing -> return baseDeps
    Just af -> do
      annots <- loadAnnotations af
      return $! addLibraryAnnotations baseDeps annots

  -- Have to give a type signature here to fix all of the FuncLike
  -- constraints to our metadata blob.
  let analyses :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
      analyses = [ identifyReturns ds returnSummary
                 , identifyScalarEffects scalarEffectSummary
                 , identifyArrays ds arraySummary
                 , identifyFinalizers ds pta finalizerSummary
                 , identifyEscapes ds escapeSummary
                 , identifyNullable ds nullableSummary returnSummary
                 , identifyAllocators ds pta allocatorSummary escapeSummary finalizerSummary
                 , identifyOutput ds outputSummary allocatorSummary escapeSummary
                 , identifyRefCounting ds refCountSummary finalizerSummary scalarEffectSummary
                 ]
      analysisFunction = callGraphComposeAnalysis analyses
      analysisResult =
        parallelCallGraphSCCTraversal cg analysisFunction mempty

      diags = mconcat $ extractSummary analysisResult (getL diagnosticLens)
      summaries = extractSummary analysisResult ModuleSummary

  case formatDiagnostics (diagnosticLevel opts) diags of
    Nothing -> return ()
    Just diagString -> putStrLn diagString

  -- Persist the module summary
  saveModule repo name deps m summaries ds
  case (reportDir opts, librarySource opts) of
    (Nothing, _) -> return ()
    (Just d, Nothing) -> writeSummary m summaries ds d
    (Just d, Just archive) -> writeDetailedReport m summaries ds d archive

writeSummary :: Module -> [ModuleSummary] -> DependencySummary -> FilePath -> IO ()
writeSummary m summaries ds rDir = do
  let rep = compileSummaryReport m summaries ds
  writeHTMLSummary rep rDir

-- | Called when a source tarball was provided.  This generates and
-- writes the report for the Module in the location specified by the
-- user.
writeDetailedReport :: Module -> [ModuleSummary] -> DependencySummary -> FilePath -> FilePath -> IO ()
writeDetailedReport m summaries ds rDir fp = do
  arc <- readArchive fp
  let rep = compileDetailedReport m arc summaries ds
  writeHTMLReport rep rDir
