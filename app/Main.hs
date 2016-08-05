{-# LANGUAGE DeriveDataTypeable #-}

import Control.Exception (Exception, throwIO)

import Data.Either (Either(..))

import Data.Typeable (Typeable)

import LLVM.General.AST (Module)

import Phkit.IO (
  genBitcodeWithTransform, genElfWithTransform)

import Phkit.Phire

import System.Environment (
  getArgs)

import Phkit.Experimental

data MainException = InvalidArgument [String]
                   | UnknownTransform String
                   deriving (Typeable, Show)

instance Exception MainException

toyTransform :: Module -> Module
toyTransform = id

toyPhireTransform :: Module -> Module
toyPhireTransform m = finalizeModule $ fmap phModuleToModule $
  phModuleFromModule m

getTransformByName :: String -> Either MainException (Module -> Module)
getTransformByName "toy" = Right toyTransform
getTransformByName "toyPhire" = Right toyPhireTransform
getTransformByName other = Left $ UnknownTransform other

chainTransforms :: [String] -> Either MainException (Module -> Module)
chainTransforms [] = Right id
chainTransforms (h:t) =
  case getTransformByName h of
    Right headTransform ->
      case chainTransforms t of
        Right tailTransform -> Right (tailTransform . headTransform)
        Left e -> Left e
    Left e -> Left e


runMain :: [String] -> IO ()
runMain (inFile:(outFile:transforms)) =
  case chainTransforms transforms of
    Right transform -> do
      genBitcodeWithTransform inFile outFile transform
      genElfWithTransform inFile (concat [outFile, ".out"]) transform
    Left e -> throwIO e
runMain other =
  throwIO $ InvalidArgument other

-- | 'main' runs the main program
main :: IO ()
main = getArgs >>= runMain
