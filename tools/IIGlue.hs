module Main ( main ) where

import Control.Applicative
import Control.Exception ( tryJust )
import Control.Lens ( (.~), view )
import Control.Monad ( guard )
import Data.Monoid
import Options.Applicative
import System.Environment ( getEnv )
import System.FilePath
import System.IO.Error ( isDoesNotExistError )

import Codec.Archive

import LLVM.Analysis
import LLVM.Analysis.CallGraph
import LLVM.Analysis.CallGraphSCCTraversal
import LLVM.Analysis.Util.Testing
import LLVM.Parse

import Foreign.Inference.Diagnostics
import Foreign.Inference.Interface
import Foreign.Inference.Report
import Foreign.Inference.Preprocessing
import Foreign.Inference.Analysis.Allocator
import Foreign.Inference.Analysis.Array
import Foreign.Inference.Analysis.ErrorHandling
import Foreign.Inference.Analysis.Escape
import Foreign.Inference.Analysis.Finalize
import Foreign.Inference.Analysis.Nullable
import Foreign.Inference.Analysis.Output
import Foreign.Inference.Analysis.RefCount
import Foreign.Inference.Analysis.Return
import Foreign.Inference.Analysis.SAP
import Foreign.Inference.Analysis.ScalarEffects
import Foreign.Inference.Analysis.Transfer
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
          <$> many (strOption
              ( long "dependency"
              <> short 'd'
              <> metavar "DEPENDENCY"
              <> help "A dependency of the library being analyzed."))
          <*> strOption
              ( long "repository"
              <> short 'r'
              <> metavar "DIRECTORY"
              <> value defaultRepo
              <> help "The directory containing dependency summaries.  The summary of the input library will be stored here. (Default: consult environment)")
          <*> option
              ( long "diagnostics"
              <> metavar "DIAGNOSTIC"
              <> value Warning
              <> help "The level of diagnostics to show (Debug, Info, Warning, Error).  Default: Warning" )
          <*> optional (strOption
              ( long "source"
              <> short 's'
              <> metavar "FILE"
              <> help "The source for the library being analyzed (tarball or zip archive).  If provided, a report will be generated"))
          <*> optional (strOption
              ( long "reportDir"
              <> short 'p'
              <> metavar "DIRECTORY"
              <> help "The directory in which the summary report will be produced.  Defaults to the REPOSITORY."))
          <*> optional (strOption
              ( long "annotations"
              <> short 'a'
              <> metavar "FILE"
              <> help "An optional file containing annotations for the library being analyzed."))
          <*> argument str ( metavar "FILE" )


main :: IO ()
main = do
  mRepLoc <- tryJust (guard . isDoesNotExistError) (getEnv "INFERENCE_REPOSITORY")
  let repLoc = either (error "No dependency repository specified") id mRepLoc
      args = info (helper <*> cmdOpts repLoc)
        ( fullDesc
        <> progDesc "Infer interface annotations for FILE (which can be bitcode or llvm assembly)"
        <> header "iiglue - A frontend for the FFI Inference engine")

  execParser args >>= realMain

realMain :: Opts -> IO ()
realMain opts = do
  let name = takeBaseName (inputFile opts)
      parseOpts = case librarySource opts of
        Nothing -> defaultParserOptions { metaPositionPrecision = PositionNone }
        Just _ -> defaultParserOptions
  mm <- buildModule requiredOptimizations (parseLLVMFile parseOpts) (inputFile opts)
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

  -- FIXME: adapt the parallelCallGraphSCCTraversal to accept a list
  -- of FuncLikes so that we can share one set among the two global
  -- passes and the two SCC traversals

  -- Have to give a type signature here to fix all of the FuncLike
  -- constraints to our metadata blob.
  let funcLikes :: [FunctionMetadata]
      funcLikes = map fromFunction (moduleDefinedFunctions m)
      errRes = identifyErrorHandling funcLikes ds pta
      res0 = (errorHandlingSummary .~ errRes) mempty
      phase1 :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
      phase1 = [ identifyReturns ds returnSummary
               , identifySAPs ds sapSummary
               , identifyScalarEffects scalarEffectSummary
               , identifyArrays ds arraySummary
               , identifyFinalizers ds pta finalizerSummary
               , identifyEscapes ds pta escapeSummary
               , identifyRefCounting ds refCountSummary finalizerSummary scalarEffectSummary
               ]
      phase1Func = callGraphComposeAnalysis phase1
      phase1Res = parallelCallGraphSCCTraversal cg phase1Func res0
      -- The transferRes includes (builds on) the phase1Res.  The
      -- transfer analysis depends on finalizers (and maybe escape)
      transferRes = identifyTransfers funcLikes cg ds pta phase1Res finalizerSummary transferSummary
      -- Phase2 depends on the results of the transfer analysis (and
      -- the error analysis)
      phase2 :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
      phase2 = [ identifyAllocators ds pta allocatorSummary escapeSummary finalizerSummary -- transferSummary
               , identifyOutput ds outputSummary allocatorSummary escapeSummary -- transferSummary
                 -- Nullable will depend on the error analysis result
               , identifyNullable ds nullableSummary returnSummary
               ]
      phase2Func = callGraphComposeAnalysis phase2
      phase2Res = parallelCallGraphSCCTraversal cg phase2Func transferRes
      -- Extract the diagnostics from each analysis and combine them
      diags = mconcat $ extractSummary phase2Res (view diagnosticLens)
      -- Now just take the summaries
      summaries = extractSummary phase2Res ModuleSummary

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
