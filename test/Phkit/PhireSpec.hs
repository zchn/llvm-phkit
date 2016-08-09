{-# LANGUAGE GADTs #-}

module Phkit.PhireSpec (spec) where

import qualified Control.Applicative as CA
import qualified Data.List as DL
import qualified Data.Set as DS

import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)

import Test.Hspec (
  describe, it, shouldBe, shouldContain, shouldNotContain, Spec)

import Phkit.IO (buildAndExec, withModuleFromPathIO)
import Phkit.Phire
import Phkit.TestUtil

phModuleFromPath :: String -> IO PhModule
phModuleFromPath p = withModuleFromPathIO p $
        \theModule -> return $ testOnlyRunWithEmptyMap $ phModuleFromModule theModule

spec :: Spec
spec =
  describe "Phire construction tests" $ do
    it "converts LGA.Module to Funcs." $ do
      result <- show CA.<$> phModuleFromPath
        "test/testdata/interpret-indirectbr.c"
      result `shouldContain` "phFunctionName"
      result `shouldContain` "phFunctionEntry = L1"
      result `shouldContain` "phFunctionBody"
      result `shouldContain` "NameInsn"
      result `shouldContain` "InsnInsn"
      result `shouldContain` "TermInsn"
      result `shouldContain` "GetElementPtr"
      result `shouldContain` "Br"
      result `shouldNotContain` "PatternMatchFail!"
    it "supports all types in buffer_overflow_1.c." $ do
      result <- show CA.<$> phModuleFromPath
        "test/testdata/buffer_overflow_1.c"
      result `shouldNotContain` "PatternMatchFail!"
    it "supports all types in argbuf_heap_overflow_fptr_rewrite.c." $ do
      result <- show CA.<$> phModuleFromPath
        "test/testdata/argbuf_heap_overflow_fptr_rewrite.c"
      result `shouldNotContain` "PatternMatchFail!"
    it "normalizes phv*." $ do
      result <- show CA.<$> phModuleFromPath
        "test/testdata/phv_normalization.c"
      result `shouldContain` "deja_vu"
      result `shouldContain` "phvL"
      result `shouldNotContain` "phv1987"
    -- it "emits the same PhModule for parallel conversion." $ do
    --   let filename = "test/testdata/phv_normalization.c"
    --   result1 <- show <$> phModuleFromPath filename
    --   result2 <- show <$> phModuleFromPath filename
    --   result1 `shouldContain` "phModule"
    --   linesDiff
    --     (unlines $ tail $ lines result1)
    --     (unlines $ tail $ lines result2) `shouldBe` ""
    it "emits the same PhModule for sequential conversion." $ do
      let filename = "test/testdata/phv_normalization.c"
      phm1 <- phModuleFromPath filename
      let phm2 = testOnlyRunWithEmptyMap $ phModuleFromModule
                 $ phModuleToModule phm1
          result1 = show phm1
          result2 = show phm2
      result1 `shouldContain` "phModule"
      linesDiff
        (unlines $ tail $ lines result1)
        (unlines $ tail $ lines result2) `shouldBe` ""
    it "emits the same LGA.Module for parallel conversion." $ do
      let filename = "test/testdata/phv_normalization.c"
      origMod <- withModuleFromPathIO filename return
      let result1 = show $ phModuleToModule $ testOnlyRunWithEmptyMap
                    $ phModuleFromModule origMod
          result2 = show $ phModuleToModule $ testOnlyRunWithEmptyMap
                    $ phModuleFromModule origMod
      result1 `shouldContain` "Module"
      result1 `shouldBe` result2
    it "emits the same LGA.Module for sequential conversion." $ do
      let filename = "test/testdata/phv_normalization.c"
      origMod <- withModuleFromPathIO filename return
      let phm1 = phModuleToModule $ testOnlyRunWithEmptyMap
                 $ phModuleFromModule origMod
          phm2 =  phModuleToModule $ testOnlyRunWithEmptyMap
                 $ phModuleFromModule phm1
      let result1 = show phm1
      let result2 = show phm2
      result1 `shouldContain` "Module"
      result1 `shouldBe` result2
    it "emits different LGA.Module than original." $ do
      let filename = "test/testdata/phv_normalization.c"
      origMod <- withModuleFromPathIO filename return
      let phm1 = phModuleToModule $ testOnlyRunWithEmptyMap
                 $ phModuleFromModule origMod
      let result0 = show origMod
      let result1 = show phm1
      result0 `shouldContain` "Module"
      result0 `shouldContain` "phv1987"
      result1 `shouldContain` "Module"
      result1 `shouldNotContain` "phv1987"
      -- result0 `shouldBe` result1
      -- (linesDiff
      --   (unlines $ DL.sort $ words result0)
      --   (unlines $ DL.sort $ words result1)) `shouldBe` ""
    it "generates legit llvm IR." $ do
      phM <- phModuleFromPath "test/testdata/buffer_overflow_1.c"
      (exitCode, stdout, stderr) <- buildAndExec (phModuleToModule phM
                                                 ) [] [] [] "test"
      exitCode `shouldBe` ExitSuccess
      stdout `shouldContain`
        -- TODO(zchn): Fix (door's location changes after reboot)
        "location of deja_vu: 0x400"
      stdout `shouldContain`
        -- TODO(zchn): Fix (door's location changes after reboot)
        "location of door: 0x7fffffff"
      stderr `shouldBe` ""
