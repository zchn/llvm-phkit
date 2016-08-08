{-# LANGUAGE GADTs #-}

module Phkit.AnalysisSpec (spec) where

import qualified Data.Set as DS

import Test.Hspec (
  describe, it, shouldReturn, Spec)

import Phkit.IO (withModuleFromPathIO)
import Phkit.Analysis

spec :: Spec
spec =
  describe "Hoopl analysis tests" $ do
    it "correctly invokes Hoopl." (
      withModuleFromPathIO "test/testdata/interpret-indirectbr.c" (
          return . DS.toAscList . iSetAnalysisResultOf)
      `shouldReturn` ["AShr","Add","Alloca","BitCast","Br","Call",
                      "GetElementPtr","IndirectBr","Load","Or","Phi",
                      "Ret","Shl","Store","Sub","Switch","ZExt"])
    it "analyzes buffer_overflow_1.cc" (
      withModuleFromPathIO "test/testdata/buffer_overflow_1.c" (
          return . DS.toAscList . iSetAnalysisResultOf)
      `shouldReturn` ["Alloca","Call","GetElementPtr","Ret","Store"])
    it "runs aOnceFAnalysis and terminates" (
      withModuleFromPathIO "test/testdata/interpret-indirectbr.c" (
          \modu -> return $ length $
                   fwdAnalysisResultOf modu $
                   PhFwdAnalysis aOnceLattice phInitWithNone aOnceFTransfer)
      `shouldReturn` 9)
    it "runs aOnceBAnalysis and terminates" (
      withModuleFromPathIO "test/testdata/interpret-indirectbr.c" (
          \modu -> return $ length $
                   bwdAnalysisResultOf modu $
                   PhBwdAnalysis aOnceLattice phInitWithNone aOnceBTransfer)
      `shouldReturn` 9)
