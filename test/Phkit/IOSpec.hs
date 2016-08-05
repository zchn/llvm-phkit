module Phkit.IOSpec (spec) where

import Control.Monad ((>=>), liftM)
import Control.Monad.Except (runExceptT)
import Data.Either (isRight)
import GHC.IO.Handle ()
import LLVM.General.Context (withContext)
import LLVM.General.Module as LLVMModule (
  File(File), moduleLLVMAssembly, withModuleFromAST,
  withModuleFromBitcode)

import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ()
import System.Posix.Files (fileSize, getFileStatus)
import System.Posix.Types (FileOffset)
import System.Process (readProcessWithExitCode)
import System.IO.Temp ()

import Test.Hspec (
  describe, it, shouldBe, shouldContain, shouldReturn, Spec)

import Phkit.IO (
  buildAndExec, buildModule, withModuleFromPath,
  withModuleFromPathIO)

spec :: Spec
spec = do
  describe "buildAndExec tests" $ do
    it "builds and execute an executable buffer_overflow_1.c" $ do
      (exitCode, stdout, stderr) <-
        withModuleFromPathIO "test/testdata/buffer_overflow_1.c" $
         \theModule -> buildAndExec theModule [] [] [] "test"
      exitCode `shouldBe` ExitSuccess
      stdout `shouldContain`
        -- TODO(zchn): Fix (door's location changes after reboot)
        "location of deja_vu: 0x400"
      stdout `shouldContain`
        -- TODO(zchn): Fix (door's location changes after reboot)
        "location of door: 0x7fffffff"
      stderr `shouldBe` ""
    it "overflows buffer_overflow_1" $
      withModuleFromPathIO "test/testdata/buffer_overflow_1.c"
        (\theModule -> buildAndExec theModule [] [] [] $ replicate 100 'a')
        -- 11 is SIGSEGV
        `shouldReturn` (ExitFailure (-11), "", "")
  describe "buildModule tests" $ do
    it "parses C or bitcode from files in repo" (
      buildModule [] []
        (\fpath ->
           liftM isRight (withContext
           (\c ->
              runExceptT (withModuleFromBitcode c (LLVMModule.File fpath)
                           moduleLLVMAssembly))))
        "test/testdata/bzip2.c"
        `shouldReturn` True)
    it "returns the correct file size" (
      buildModule [] []
        (getFileSize >=> (return . take 3 . show))
        "test/testdata/bzip2.c"
        `shouldReturn` "142") -- "142124" or "142148"
  describe "readProcessWithExitCode tests" $ do
    it "works on echo" (
      readProcessWithExitCode "echo" ["test"] ""
        `shouldReturn`
        (ExitSuccess, "test\n", ""))
    it "works on cat" (
      readProcessWithExitCode "cat" [] "test"
       `shouldReturn`
       (ExitSuccess, "test", ""))
  describe "withModuleFromPath tests" $
    it "parses C or bitcode from files in repo" (
     liftM isRight (withContext
                    (\c -> runExceptT
                      (withModuleFromPath "test/testdata/bzip2.c"
                       (\astModule ->
                          withModuleFromAST c astModule moduleLLVMAssembly))))
       `shouldReturn` True)
  describe "genBitcodeWithTransform tests" $
    it "TODO" (1 `shouldBe` 1)

-- ---- From stack-overflow -----------------------------------------------

-- | Returns the the size of the file at 'path'.
getFileSize :: FilePath -> IO FileOffset
getFileSize path = do
  stat <- getFileStatus path
  return (fileSize stat)
