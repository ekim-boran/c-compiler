module Tests.Cases where

import Data.Foldable
import Opt.Deadcode
import Opt.Loop
import Opt.Mem2Reg
import Opt.SimplifyCfg
import Tests.Tests
import Opt.Gvn

testIrGen1 = runTest $ TestConfig "examples/c/" "examples/ir_/" ".c" ".ir" [] ["_one"] id

testIrGen = runTest $ TestConfig "examples/c/" "examples/ir_/" ".c" ".ir" [] [] id

-------------------------------------------------------
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

testMem2Reg = runTest $ TestConfig "examples/ir0/" "examples/ir1_out/" ".ir" ".ir" ["minus_constant"] [] (runOpt (mem2Reg))

f' = runOpt (licm . deadcode . gvn . mem2Reg . simplifyCfg)

testLoop = runTest $ TestConfig "examples/ir0/" "examples/ir5/" ".ir" ".ir" [] [] (f')
