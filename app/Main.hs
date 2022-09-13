module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import Tests.Tests

data Args = Args
  { input :: String,
    execute :: Bool
  }
  deriving (Show)

args :: Parser Args
args =
  Args
    <$> strOption
      ( short 'c'
          <> metavar "Source"
          <> help "Source file"
      )
    <*> switch
      ( long "executable"
          <> short 'q'
          <> help "create executable ./out file"
      )

main :: IO ()
main = execParser opts >>= (\(Args {..}) -> runProg input execute)
  where
    opts =
      info
        (args <**> helper)
        ( fullDesc
            <> progDesc "c compiler"
            <> header "c compiler"
        )
