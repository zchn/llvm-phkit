{-# LANGUAGE GADTs #-}

module Phkit.TransformSpec (spec) where

import qualified Compiler.Hoopl as CH
import qualified Control.Monad as CM
import qualified Data.List as DL
import qualified Data.Set as DS
import qualified LLVM.General.AST.Instruction as LGAI

import Test.Hspec (
  describe, it, shouldBe, shouldContain, Spec)

import Phkit.Phire
import Phkit.Analysis
import Phkit.IO
import Phkit.TestUtil
import Phkit.Transform

spec :: Spec
spec =
  describe "Hoopl analysis tests" $ do
    it "does not change between no-op rewrite." $ do
      modu <- withModuleFromPathIO "test/testdata/interpret-indirectbr.c" return
      let newMod = fwdRewriteResultOf modu CH.noFwdRewrite
      origModString <- ioStringOfAstModule modu
      newModString <- ioStringOfAstModule newMod

      newModString `shouldContain`
        "%struct.Interpreter = type { i32 (i8*)*, i8* }"
      newModString `shouldContain`
        "  indirectbr i8* %16, [label %56, label %47, label %38, label %29, label %17]"
      newModString `shouldContain`
        "define internal i32 @interpret_threaded(i8* %opcodes) #1 {"

      -- TODO(zchn): Fix it.
      (sortedLinesDiff
       (normalizeNumbers origModString)
       (normalizeNumbers newModString)) `shouldBe`
        "4,5d3\n" ++
        "< ; <label>:N                                       ; preds = %N\n" ++
        "< ; <label>:N                                       ; preds = %N\n" ++
        "\n1a2\n" ++
        "> ; <label>:N                                       ; preds = " ++
        "%N, %N, %N, %N, %N\n\n\n\n4c4,5\n" ++
        "< ; <label>:N                                      ; preds = " ++
        "%N, %N, %N, %N, %N\n---\n" ++
        "> ; <label>:N                                      ; preds = %N\n" ++
        "> ; <label>:N                                      ; preds = %N\n" ++
        "\n\n\n"
    it "rewrites the alignment of Alloca" $ do
      modu <- withModuleFromPathIO "test/testdata/interpret-indirectbr.c" return
      let newMod = fwdRewriteResultOf modu $ CH.mkFRewrite (
            \phI _ ->
              case phI of
                (InsnInsn (n LGAI.:= a@LGAI.Alloca{LGAI.alignment = align})) ->
                  return $ Just $ phGUnit $
                  InsnInsn (n LGAI.:= a {LGAI.alignment = align * 2})
                other -> return $ Just $ phGUnit other)
      origModString <- ioStringOfAstModule modu
      newModString <- ioStringOfAstModule newMod

      origModString `shouldContain` "alloca"
      newModString `shouldContain` "alloca"
      (sortedLinesDiff
        (normalizeVars origModString)
        (normalizeVars newModString)) `shouldContain`
        "<   %codetable = alloca [5 x i8*], align 16\n" ++
        "<   %result = alloca i32, align 4\n" ++
        "<   %result = alloca i32, align 4\n" ++
        "<   %result = alloca i32, align 4\n" ++
        "---\n" ++
        ">   %codetable = alloca [5 x i8*], align 32\n" ++
        ">   %result = alloca i32, align 8\n" ++
        ">   %result = alloca i32, align 8\n" ++
        ">   %result = alloca i32, align 8\n"
