{-# LANGUAGE DeriveDataTypeable #-}

import qualified Compiler.Hoopl as CH
import qualified Control.Applicative as CA
import qualified Control.Exception as CE -- (Exception, throwIO)
import qualified Data.Either as DE -- (Either(..))
import qualified Data.Typeable as DT -- (Typeable)
import qualified LLVM.General.AST as LGA -- (Module)
import qualified System.Environment as SE -- (getArgs)
import Phkit.IO (genBitcodeWithTransform, genElfWithTransform)
import Phkit.Phire
import Phkit.SoftBoundTransform
import Phkit.Experimental

data MainException
  = InvalidArgument [String]
  | UnknownTransform String
  deriving (DT.Typeable, Show)

instance CE.Exception MainException

toyTransform :: LGA.Module -> LGA.Module
toyTransform = id

toyPhireTransform :: LGA.Module -> LGA.Module
toyPhireTransform m =
  finalizeModule $ CH.liftFuel $ phModuleToModule CA.<$> phModuleFromModule m

getTransformByName :: String -> Either MainException (LGA.Module -> LGA.Module)
getTransformByName "toy" = Right toyTransform
getTransformByName "toyPhire" = Right toyPhireTransform
getTransformByName "softbound" = Right softBoundRewriteResultOf
getTransformByName other = Left $ UnknownTransform other

chainTransforms :: [String] -> Either MainException (LGA.Module -> LGA.Module)
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
      genElfWithTransform inFile (outFile ++ ".out") transform
    Left e -> CE.throwIO e
runMain other = CE.throwIO $ InvalidArgument other

-- | 'main' runs the main program
main :: IO ()
main = SE.getArgs >>= runMain
