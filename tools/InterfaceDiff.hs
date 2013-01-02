module Main ( main ) where

import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as BS
import Options.Applicative
import System.Exit

import Foreign.Inference.Interface
import Foreign.Inference.Interface.Diff

data Opts =
  Opts { outputFormat :: OutputFormat
       , oldInput :: FilePath
       , newInput :: FilePath
       }

data OutputFormat = None
                  | Html
                  | Text
                  deriving (Eq, Ord, Show, Read)

cmdOpts :: Parser Opts
cmdOpts = Opts
  <$> option
      ( long "format"
      <> short 'f'
      <> metavar "FORMAT"
      <> help "The output format.  One of None, Html, or Text (default Text)"
      <> value Text)
  <*> argument str ( metavar "OLDIFACE" )
  <*> argument str ( metavar "NEWIFACE" )

main :: IO ()
main = execParser args >>= realMain
  where
    args = info (helper <*> cmdOpts)
      ( fullDesc
      <> progDesc "Display the differences in inferred anntations between OLDIFACE and NEWIFACE"
      <> header "InterfaceDiff - structured diff for library interfaces")

realMain :: Opts -> IO ()
realMain opts = do
  let oldPath = oldInput opts
      newPath = newInput opts

  oldInterface <- readLibraryInterface oldPath
  newInterface <- readLibraryInterface newPath

  let diff = libraryDiff oldInterface newInterface

  case diff == emptyLibraryDiff of
    True -> exitSuccess
    False -> printDiff (outputFormat opts) diff

printDiff :: OutputFormat -> LibraryDiff -> IO ()
printDiff None _ = exitWith (ExitFailure 1)
printDiff Text diff =
  let bs = diffToByteString diff
  in BS.putStrLn bs >> exitWith (ExitFailure 1)
printDiff Html _ = error "HTML output is currently unimplemented"
