{-# LANGUAGE GADTs #-}

module Phkit.FlattenTransformSpec (spec) where

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
import Phkit.FlattenTransform

spec :: Spec
spec =
  describe "Flatten transform tests" $ do
    it "performs flattening." $ do
      modu <- withModuleFromPathIO "test/testdata/interpret-indirectbr.c" return
      let newMod = flattenResultOf modu
      origModString <- ioStringOfAstModule modu
      newModString <- ioStringOfAstModule newMod

      newModString `shouldContain`
        "%struct.Interpreter = type { i32 (i8*)*, i8* }"
      newModString `shouldContain`
        "  indirectbr i8* %16, [label %56, label %47, label %38, label %29, label %17]"
      newModString `shouldContain`
        "define internal i32 @interpret_threaded(i8* %opcodes) #1 {"

      -- TODO(zchn): Fix it.
      sortedLinesDiff
       (normalizeNumbers origModString)
       (normalizeNumbers newModString) `shouldBe`
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
