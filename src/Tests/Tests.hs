module Tests.Tests where

import Asm.Printer
import AsmGen.Gen (asmGen)
import Control.Exception
import Control.Monad
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString qualified as B
import Data.Foldable (traverse_)
import Data.List
import Data.List.Extra (notNull)
import Data.Map qualified as M
import Ir.Parse
import Ir.Printer qualified
import Ir.Types
import Irgen.Lib
import Language.C
import Opt.Deadcode (deadcode)
import Opt.Gvn (gvn)
import Opt.Loop
import Opt.Mem2Reg (mem2Reg)
import Opt.SimplifyCfg (constantProp, emptyBlocks, mergeBlocks, removeUnreachable, simplifyCfg)
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Text.PrettyPrint (render)
import Text.Read
import Util (Print (write), fromMaybe, runUntilNoChange)

runOptimization :: (FunctionDefinition -> (Bool, FunctionDefinition)) -> TranslationUnit -> TranslationUnit
runOptimization f = go
  where
    go (TranslationUnit decls structs) = if cont then go (TranslationUnit m structs) else TranslationUnit m structs
      where
        (cont, m) = foldr cfg (False, []) decls
        cfg (name, Function s d) (b, xs) =
          let (b', d') = f d
           in (b || b', (name, Function s d') : xs)
        cfg x (b, xs) = (b, x : xs)

optimize f d = let d' = f d in (d /= d', d')

runOpt = runOptimization . optimize

testIrGen1 = runTest $ TestConfig "examples/c/" "examples/ir_/" ".c" ".ir" [] ["_one"] id

testIrGen = runTest $ TestConfig "examples/c/" "examples/ir_/" ".c" ".ir" [] [] id

testSimplify1 = runTest $ TestConfig "examples/ir0/" "examples/ir0_out/" ".ir" ".ir" [] ["minus_constant"] (runOpt simplifyCfg)

testSimplify2 = traverse_ runTest $ go <$> configs
  where
    go (f, file) = TestConfig "examples/simplify_cfg/" "examples/simplify_cfg_out/" ".input.ir" ".output.ir" [] [file] (runOpt f)
    configs =
      [ (constantProp, "const_prop"),
        (removeUnreachable, "reach"),
        (mergeBlocks, "merge"),
        (emptyBlocks, "empty")
      ]

testMem2Reg = runTest $ TestConfig "examples/ir0/" "examples/ir1_out/" ".ir" ".ir" ["minus_constant"] [] (runOpt mem2Reg)

testLoop = runTest $ TestConfig "examples/ir0/" "examples/ir5/" ".ir" ".ir" [] [] f'
  where
    f' = runOpt (licm . deadcode . gvn . mem2Reg . simplifyCfg)

data TestConfig = TestConfig
  { indir :: FilePath,
    outDir :: FilePath,
    inExtension :: String,
    outExtension :: String,
    notExecute :: [String],
    onlyExecute :: [String],
    opt :: TranslationUnit -> TranslationUnit
  }

runGCC input output = do
  (exitCode, out, err) <- readCreateProcessWithExitCode (proc "riscv64-linux-gnu-gcc" ["-O0", "-static", input, "-o", output]) ""
  case exitCode of
    ExitFailure _ -> print err >> error ("GCC failed " ++ input)
    _ -> return ()

runProgram file = do
  (exitCode, out, err) <- readCreateProcessWithExitCode (proc "qemu-riscv64-static" [file]) ""
  let time = fromMaybe (0 :: Int) $ readMaybe out
  case exitCode of
    ExitFailure x -> return (time, x)
    ExitSuccess -> return (time, 0)

produceOutput ir outFile = writeFile outFile $ if outex == ".ir" then render $ write (allOpt ir) else render (write $ asmGen (allOpt ir)) ++ "\n"
  where
    (_, outex) = splitExtension outFile

runCompiler :: FilePath -> FilePath -> Bool -> IO ()
runCompiler inputFile outFile opt = do
  ir <- readSourceFile inputFile
  let ir' = if opt then allOpt ir else ir
  produceOutput ir' outFile

readSourceFile fName = do
  let (_, inex) = splitExtension fName
  contents <- B.readFile fName
  let ir =
        if inex == ".c"
          then case parseC contents (initPos fName) of
            Left e -> error ("c parse error: " ++ fName)
            (Right r) -> case genTranslUnit r of -- trace (show (() <$ r)) $
              Left e -> error $ "translationError error" ++ e
              Right r -> r
          else case parseOnly irParser contents of
            Left _ -> error "cannot parse ir"
            Right r -> r
  return ir

getFiles (TestConfig {..}) = do
  dirFiles <- getDirectoryContents indir
  removeDirectoryRecursive outDir `catch` (\(e :: SomeException) -> pure ())
  createDirectory outDir `catch` (\(e :: SomeException) -> pure ())
  let files =
        sort
          [ (joinPath [indir, fileName ++ inExtension], joinPath [outDir, fileName ++ outExtension])
            | file <- dirFiles,
              file `notElem` [".", ".."],
              let (fileName, extension) = splitExtension file,
              extension == inExtension,
              fileName `notElem` notExecute,
              null onlyExecute || fileName `elem` onlyExecute
          ]
  return files

runTest :: TestConfig -> IO [(String, String)]
runTest t@(TestConfig {..}) = do
  files <- getFiles t
  traverse_ readSource files
  return files
  where
    readSource (fName, outName) = do
      print $ "compiling: " ++ fName
      ir <- readSourceFile fName
      let (_, outex) = splitExtension outName
      writeFile outName $ if outex == ".ir" then render $ write (opt ir) else render (write $ asmGen (opt ir)) ++ "\n"

executeFiles = traverse go
  where
    go (s, d) = do
      let xs = splitPath s
      (tx, out1) <- runGCC s "out" >> runProgram "out"
      (ty, out2) <- runGCC d "out" >> runProgram "out"
      return (s, tx, ty, out1, out2)

allOpt = runOpt (simplifyCfg . licm . deadcode . gvn . mem2Reg)

fill n s = s ++ replicate (n - length s) ' '

printReport xs = do
  print "Filename             |GCC Output|Output|GCC -O0 Execution Time|Execution Time|% Faster"
  forM_ xs $ \(s, tx, ty, out1, out2) -> do
    when (tx /= 0) $ putStrLn $ fill 22 (snd (splitFileName s)) ++ "|" ++ fill 10 (show out1) ++ "|" ++ fill 6 (show out2) ++ "|" ++ fill 22 (show tx) ++ "|" ++ fill 14 (show ty) ++ "|" ++ show (100 * (tx - ty) `div` ty)
    when (tx == 0) $ putStrLn $ fill 22 (snd (splitFileName s)) ++ "|" ++ fill 10 (show out1) ++ "|" ++ fill 6 (show out2) ++ "|" ++ fill 22 "-" ++ "|" ++ fill 14 "-" ++ "|" ++ fill 30 "-"

testAsm :: IO ()
testAsm = do
  files1 <- runTest $ TestConfig "examples/programs/" "examples/programs/asm/" ".c" ".s" [] [] allOpt
  files2 <- runTest $ TestConfig "examples/bench/" "examples/bench/asm/" ".c" ".s" [] [] allOpt
  executeFiles (files1 ++ files2) >>= printReport
