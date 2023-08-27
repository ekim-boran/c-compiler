module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import Tests.Tests (runCompiler)

data Args = Args
  { input :: String,
    output :: String,
    opt :: Bool
  }
  deriving (Show)

args :: Parser Args
args =
  Args
    <$> strOption (short 'c' <> metavar "Source" <> help "Source file")
    <*> strOption (short 'o' <> help "Output executable file if output extension is .ir generates ir else generates assembly")
    <*> switch (short 'o' <> help "Optimize")

main :: IO ()
main = execParser opts >>= (\(Args {..}) -> runCompiler input output opt)
  where
    opts =
      info
        (args <**> helper)
        ( fullDesc
            <> progDesc "c compiler"
            <> header "c compiler"
        )
