module Phkit.ExperimentalSpec (spec) where

import Control.Monad (liftM)
import Control.Monad.Except (runExceptT)

import Data.Either (isRight)

import LLVM.General.AST (defaultModule)
import LLVM.General.Context (withContext)

import LLVM.General.Module as LLVMModule (
  moduleLLVMAssembly, withModuleFromAST, withModuleFromLLVMAssembly)

import System.FilePath ()
import System.Posix.Files ()
import System.Posix.Types ()

import Test.Hspec (
  describe, it, shouldBe, shouldReturn, Spec)

import Phkit.IO (withModuleFromPath)
import Phkit.Experimental

sampleTextModule :: String

spec :: Spec
spec = do
  describe "llvm-general tests" $ do
      it "converts pure Module to C Module then Assembly"
        (withContext
         (\c ->
            runExceptT (withModuleFromAST c defaultModule moduleLLVMAssembly))
         `shouldReturn` Right "; ModuleID = '<string>'\n")
      it "parses text LLVM Assembly to pure C Module then back to text"
        (liftM isRight
         (withContext
          (\c ->
              runExceptT (withModuleFromLLVMAssembly c sampleTextModule
                            moduleLLVMAssembly)))
         `shouldReturn` True)
  describe "simpleLlvm tests" $
      it "handles the defaultModule"
      (simpleLlvm defaultModule `shouldBe` "<string>")
  describe "getFunctionNames test" $ do
      it "works with bzip2.c"
        (runExceptT (withModuleFromPath "test/testdata/bzip2.c"
                     (show . take 3 . getFunctionNames))
         `shouldReturn`
         Right
         "[Name \"BZ2_blockSort\",Name \"fallbackSort\",Name \"mainSort\"]")
      it "works with bzip2.bc"
        (runExceptT (withModuleFromPath "test/testdata/bzip2.bc"
                     (show . take 3 . getFunctionNames))
         `shouldReturn`
         Right
         "[Name \"BZ2_blockSort\",Name \"fallbackSort\",Name \"mainSort\"]")
      -- The gcc.c test is too slow
      -- >>
      -- it "works with gcc.c"
      -- (runExceptT (withModuleFromPath "test/testdata/gcc.c"
      --              (show . (take 3) . getFunctionNames))
      --  `shouldReturn`
      --  Right "[Name \"BZ2_blockSort\",Name \"fallbackSort\",Name \"mainSort\"]")
      -- >>
      -- it "works with gzip.c"
      -- (runExceptT (withModuleFromPath "test/testdata/gzip.c"
      --              (show . (take 3) . getFunctionNames))
      --  `shouldReturn`
      --  Right "[Name \"bi_init\",Name \"file_read\",Name \"send_bits\"]")
      -- >>
      -- it "works with oggenc.c"
      -- (runExceptT (withModuleFromPath "test/testdata/oggenc.c"
      --              (show . (take 3) . getFunctionNames))
      --  `shouldReturn`
      --  Right "[Name \"main\",Name \"llvm.memcpy.p0i8.p0i8.i64\",Name \"parse_options\"]")
  describe "listIndirectTransfer tests" $ do
      it "lists no indirect transfers for bzip.c"
        (runExceptT (withModuleFromPath "test/testdata/oggenc.c"
                     (length . listIndirectTransfer))
         `shouldReturn`
         Right 45)
      it "lists indirect transfers for virtual_function_table.cc"
        (runExceptT (withModuleFromPath "test/testdata/virtual_function_table.cc"
                     (length . listIndirectTransfer))
         `shouldReturn`
         Right 2)
      it "lists indirect transfers for interpret-indirectbr.c"
        (runExceptT (withModuleFromPath "test/testdata/interpret-indirectbr.c"
                     (length . listIndirectTransfer))
         `shouldReturn`
         Right 7)

sampleTextModule = unlines [
  "; Declare the string constant as a global constant.",
  "@.str = private unnamed_addr constant [12 x i8] c\"hello world\\00\"",
  "",
  "; External declaration of the puts function",
  "declare i32 @puts(i8* nocapture) nounwind",
  "",
  "; Definition of main function",
  "define i32 @main() {   ; i32()*",
  "  ; Convert [13 x i8]* to i8  *...",
  "  %cast210 = getelementptr [12 x i8]* @.str, i64 0, i64 0",
  "",
  "  ; Call puts function to write out the string to stdout.",
  "  call i32 @puts(i8* %cast210)",
  "  ret i32 0",
  "}",
  "",
  "; Named metadata",
  "!1 = metadata !{i32 42}",
  "!foo = !{!1}"
  ]
