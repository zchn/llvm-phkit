{-# LANGUAGE GADTs #-}

module Phkit.SoftBoundTransform (
  softBoundRewriteResultOf) where

import qualified Compiler.Hoopl as CH
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Word as DW
import qualified LLVM.General.AST as LGA
import qualified LLVM.General.AST.CallingConvention as LGACa
import qualified LLVM.General.AST.Constant as LGAC
import qualified LLVM.General.AST.Global as LGAG
import qualified LLVM.General.AST.Instruction as LGAI
import qualified LLVM.General.AST.Operand as LGAO
import qualified LLVM.General.AST.Type as LGAT

import Phkit.Analysis
import Phkit.Phire
import Phkit.SoftBound.Checker
import Phkit.SoftBound.Lang
import Phkit.SoftBound.Tracker
import Phkit.Transform

softBoundRewriteResultOf :: LGA.Module -> LGA.Module
softBoundRewriteResultOf = softBoundAddCheckResultOf . softBoundAddTrackResultOf
  . softBoundAddHeaderResultOf

softBoundAddHeaderResultOf :: LGA.Module -> LGA.Module
softBoundAddHeaderResultOf modu@LGA.Module { LGA.moduleDefinitions = defs } =
  modu { LGA.moduleDefinitions =
           LGA.GlobalDefinition (LGA.functionDefaults {
                                     LGAG.returnType  = LGAT.VoidType,
                                     LGAG.name = LGA.Name "sbinit" }) :
           LGA.GlobalDefinition (LGA.functionDefaults {
                                     LGAG.returnType  = LGAT.VoidType,
                                     LGAG.name = LGA.Name "sbcopy" }) :
           LGA.GlobalDefinition (LGA.functionDefaults {
                                     LGAG.returnType  = LGAT.VoidType,
                                     LGAG.name = LGA.Name "sbsave" }) :
           LGA.GlobalDefinition (LGA.functionDefaults {
                                     LGAG.returnType  = LGAT.VoidType,
                                     LGAG.name = LGA.Name "sbload" }) :
           LGA.GlobalDefinition (LGA.functionDefaults {
                                     LGAG.returnType  = LGAT.VoidType,
                                     LGAG.name = LGA.Name "sbcheck" }) :
           defs }
