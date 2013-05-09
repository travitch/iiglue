{-# LANGUAGE PatternGuards #-}
-- | This program trains a SVM to classify functions as either error-reporting
-- or non-error-reporting.  It then saves the model, which is meant to be
-- fed to @iiglue@ for use during the error reporting code identification
-- analysis.
--
-- The dependency and repository arguments are the same as for iiglue, and
-- that data should be shared.  The new input is label information for
-- the input libraries.  This tool supports multiple libraries as input
-- so that they can all contribute to training the same model.  The
-- @labels@ command line argument is the filename of a label map.
--
-- A label map is a @Map String (Set String)@ that will be parsed using
-- 'read'.  The key of the map is the library name (derived from the
-- file name of the bitcode file for that library).  The mapping from
-- filename to key should be equivalent to @dropExtensions . takeBaseName@.
-- libxml2.so.2.7.8.bc becomes libxml2.
--
-- The 'Set' of strings contains the names of functions that are used to
-- report errors in the library.
module Main ( main ) where

import AI.SVM.Simple
import Control.Applicative
import Control.Arrow ( first )
import Control.Exception ( tryJust )
import Control.Monad ( guard )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as S
import Options.Applicative
import System.Environment ( getEnv )
import System.FilePath
import System.IO
import System.IO.Error ( isDoesNotExistError )

import LLVM.Analysis
import LLVM.Analysis.Util.Testing
import LLVM.Parse

import Foreign.Inference.Interface
import Foreign.Inference.Preprocessing
import Foreign.Inference.Analysis.ErrorHandling
import Foreign.Inference.Analysis.IndirectCallResolver
import Foreign.Inference.Analysis.Util.CompositeSummary

-- | The repository location is first chosen based on an environment
-- variable.  The command line argument, if specified, will override
-- it.  If the environment variable is not set, the command line
-- argument must be specified.
data Opts = Opts { inputDependencies :: [String]
                 , repositoryLocation :: FilePath
                 , labelFile :: FilePath
                 , outputFile :: FilePath
                 , inputFiles :: [FilePath]
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
          <*> strOption
              ( long "labels"
              <> short 'l'
              <> metavar "FILE"
              <> help "A file containing labels noting the error functions for input libraries" )
          <*> strOption
              ( long "output"
              <> short 'o'
              <> metavar "FILE"
              <> help "The file in which the generated model will be stored" )
          <*> some (argument str ( metavar "INFILE" ))


main :: IO ()
main = do
  mRepLoc <- tryJust (guard . isDoesNotExistError) (getEnv "INFERENCE_REPOSITORY")
  let repLoc = either (error "No dependency repository specified") id mRepLoc
      args = info (helper <*> cmdOpts repLoc)
        ( fullDesc
        <> progDesc "Train an SVM classifier for error reporting functions"
        <> header "iitrain - A helper to train an SVM for iiglue")

  execParser args >>= realMain

realMain :: Opts -> IO ()
realMain opts = withFile (labelFile opts) ReadMode $ \h -> do
  labelStr <- hGetContents h
  let labels = read labelStr
  dataSets <- mapM (buildTrainingData opts labels) (inputFiles opts)
  -- FIXME: Investigate the cost parameter here, along with the gamma for RBF
  let (msgs, classifier) = trainClassifier (C 1.0) (RBF 1.0) (concat dataSets)
  putStrLn msgs
  save (outputFile opts) classifier

buildTrainingData :: Opts -> Map FilePath (Set String)
                  -> FilePath
                  -> IO [(ErrorFuncClass, FeatureVector)]
buildTrainingData opts allLabels fname = do
  let libName = dropExtensions $ takeBaseName fname
      Just labels = M.lookup libName allLabels
      parseOpts = defaultParserOptions { metaPositionPrecision = PositionNone }
  m <- buildModule [] requiredOptimizations (parseLLVMFile parseOpts) fname
  let pta = identifyIndirectCallTargets m
      deps = inputDependencies opts
      repo = repositoryLocation opts
  ds <- loadDependencies [repo] deps
  let funcLikes :: [FunctionMetadata]
      funcLikes = map fromFunction (moduleDefinedFunctions m)
      trainingData = errorHandlingTrainingData funcLikes ds pta
  return $ fmap (first (valueToLabel labels)) trainingData

valueToLabel :: Set String -> Value -> ErrorFuncClass
valueToLabel labels val
  | Just funcName <- valToFuncName val
  , S.member funcName labels = ErrorReporter
  | otherwise = OtherFunction

valToFuncName :: Value -> Maybe String
valToFuncName val =
  case valueContent' val of
    ExternalFunctionC ef -> return $ identifierAsString (externalFunctionName ef)
    FunctionC f -> return $ identifierAsString (functionName f)
    _ -> fail "Not a function"

{-

dump :: Opts -> String -> Module -> IO ()
dump opts name m = do
  let pta = identifyIndirectCallTargets m
      cg = callGraph m pta []
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
  let funcLikes :: [FunctionMetadata]
      funcLikes = map fromFunction (moduleDefinedFunctions m)
      errRes = identifyErrorHandling funcLikes ds pta
      res0 = (errorHandlingSummary .~ errRes) mempty
      phase1 :: [ComposableAnalysis AnalysisSummary FunctionMetadata]
      phase1 = [ identifyReturns ds returnSummary
               , identifyOutput m ds outputSummary
                 -- Nullable will depend on the error analysis result
               , identifyNullable ds nullableSummary returnSummary
               , identifyScalarEffects scalarEffectSummary
               , identifyArrays ds arraySummary
                 -- Finalizers will depend on nullable so that error
                 -- paths don't interfere with finalizers
               , identifyFinalizers ds pta finalizerSummary
               , identifySAPPTRels ds sapPTRelSummary
               , identifySAPs ds pta sapSummary sapPTRelSummary finalizerSummary
               , identifyEscapes ds pta escapeSummary
               , identifyRefCounting ds refCountSummary finalizerSummary scalarEffectSummary
               , identifyAllocators ds pta allocatorSummary escapeSummary finalizerSummary outputSummary
               ]
      phase1Func = callGraphComposeAnalysis phase1
      phase1Res = parallelCallGraphSCCTraversal cg phase1Func res0
      -- The transferRes includes (builds on) the phase1Res.  The
      -- transfer analysis depends on finalizers and symbolic access paths
      transferRes = identifyTransfers funcLikes cg ds pta phase1Res finalizerSummary sapSummary transferSummary
      -- Extract the diagnostics from each analysis and combine them
      diags = mconcat $ extractSummary transferRes (view diagnosticLens)
      -- Now just take the summaries
      summaries = extractSummary transferRes ModuleSummary

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
  -}
