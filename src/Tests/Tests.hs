module Tests.Tests where

import Asm.Printer
import AsmGen.Allocator
import AsmGen.ConstraintSatisfaction
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

data TestConfig = TestConfig
  { indir :: FilePath,
    outDir :: FilePath,
    inExtension :: String,
    outExtension :: String,
    notExecute :: [String],
    onlyExecute :: [String],
    opt :: TranslationUnit -> TranslationUnit
  }

runProg fileName executable = do
  let (x, inex) = splitExtension fileName
  readSource (fileName, x ++ ".ir")
  readSource (fileName, x ++ ".s")
  let outName = "./out"
  _ <- readCreateProcessWithExitCode (proc "riscv64-linux-gnu-gcc" ["-static", x ++ ".s", "-o", outName]) ""
  return ()
  where
    readSource (fName, outName) = do
      contents <- B.readFile fName
      let (_, inex) = splitExtension fName
      let (_, outex) = splitExtension outName
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
      let ir' = allOpt ir
      writeFile outName $ if outex == ".ir" then render $ write $ (ir') else (render $ write $ asmGen ir') ++ "\n"

printMap m = unlines [show k ++ ":" ++ show k' ++ ":" ++ show v' | (k, v) <- M.assocs m, (k', v') <- M.assocs v]

printMap' m = unlines [show k ++ ":" ++ show v | (k, v) <- M.assocs m]

call (TranslationUnit decls _) = traverse_ (go . snd) decls
  where
    go (Function _ x) = do
      let g = (mkGraph $ liveness x)
      putStrLn (printMap' $ g)
      putStrLn (printMap $ liveness x)
      putStrLn (printMap' $ solveCSP g (initialAssignment (allElems (liveness x)) x))
    go _ = return ()

runTest :: TestConfig -> IO [([Char], [Char])]
runTest (TestConfig {..}) = do
  dirFiles <- getDirectoryContents indir
  outFiles <- getDirectoryContents indir
  removeDirectoryRecursive outDir `catch` (\(e :: SomeException) -> pure ())
  createDirectory outDir `catch` (\(e :: SomeException) -> pure ())
  let files =
        sort
          [ (indir ++ fileName ++ inExtension, outDir ++ fileName ++ outExtension)
            | file <- dirFiles,
              file `notElem` [".", ".."],
              let (fileName, extension) = span (/= '.') file,
              extension == inExtension,
              fileName `notElem` notExecute,
              null onlyExecute || fileName `elem` onlyExecute
          ]
  traverse_ (readSource opt) files
  return files
  where
    readSource f (fName, outName) = do
      print fName
      contents <- B.readFile fName
      let (_, inex) = splitExtension fName
      let (_, outex) = splitExtension outName
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
      let ir' = f ir
      writeFile outName $ if outex == ".ir" then render $ write $ (ir') else (render $ write $ asmGen ir') ++ "\n"

executeC file = do
  let fName = "./exe" ++ [last file]
  (exitCode, a, b) <- readCreateProcessWithExitCode (proc "riscv64-linux-gnu-gcc" ["-O1", "-static", file, "-o", fName]) ""
  case exitCode of
    ExitFailure _ -> print b
    _ -> return ()
  (exitCode, stdout, _) <- readCreateProcessWithExitCode (proc "qemu-riscv64-static" [fName]) ""
  let time = fromMaybe (0 :: Int) $ readMaybe stdout
  case exitCode of
    ExitSuccess -> return (time, 0)
    ExitFailure x -> return (time, x)

executeIR file = do
  (exitCode, stdout, err) <- readCreateProcessWithExitCode (proc "./kecc" ["--irrun", file]) ""
  case exitCode of
    ExitSuccess -> return (0, read $ last $ words $ last $ lines stdout)
    ExitFailure x -> error $ "execution error ir" ++ err

execute file =
  let (_, extension) = splitExtension file
   in case extension of
        ".c" -> executeC file
        ".s" -> executeC file
        ".ir" -> executeIR file

executeFiles n xs = replicateM_ n $ traverse_ go xs
  where
    go (s, d) = do
      print s
      (tx, x) <- execute s
      (ty, y) <- execute d
      when (tx /= 0) $ print ("times: " ++ show tx ++ "-" ++ show ty ++ "-" ++ (show $ (100 * (ty - tx)) `div` tx))
      print (show x ++ "-" ++ show y)
      when (x /= y) $ error s

allOpt = runOpt (simplifyCfg . licm . deadcode . gvn . mem2Reg)

testAsm = do
  _ <- runTest $ TestConfig "examples/c/" "examples/c_ir/" ".c" ".ir" [] [] allOpt
  files <- runTest $ TestConfig "examples/c/" "examples/c_asm/" ".c" ".s" [] [] allOpt
  executeFiles 1 files
  _ <- runTest $ TestConfig "examples/bench/" "examples/bench_ir/" ".c" ".ir" [] [] allOpt
  files <- runTest $ TestConfig "examples/bench/" "examples/bench_asm/" ".c" ".s" [] [] allOpt
  executeFiles 1 files

-- >>> testAsm

cfgTests :: [(FunctionDefinition -> (Bool, FunctionDefinition), String, String)]
cfgTests =
  [ (optimize constantProp, "examples/simplify_cfg", "const_prop"),
    (optimize removeUnreachable, "examples/simplify_cfg", "reach"),
    (optimize mergeBlocks, "examples/simplify_cfg", "merge"),
    (optimize emptyBlocks, "examples/simplify_cfg", "empty")
  ]
