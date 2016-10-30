{-# LANGUAGE DeriveDataTypeable #-}

module Phkit.IO
  ( buildAndExec
  , buildElf
  , buildModule
  , genBitcodeWithTransform
  , genElfWithTransform
  , ioStringOfAstModule
  , runProcessInEnv
  , withModuleFromPath
  , withModuleFromPathIO
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad ((>=>), liftM, when)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)

import Data.Typeable (Typeable)

import GHC.IO.Handle (hClose, hGetLine, hPutStr)

import LLVM.General.AST (Module)
import LLVM.General.Context (withContext)
import LLVM.General.Module as LLVMModule
       (File(File), moduleAST, moduleLLVMAssembly, withModuleFromAST,
        withModuleFromBitcode, writeBitcodeToFile, writeObjectToFile)
import LLVM.General.Target (withHostTargetMachine)

import System.Environment (getEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ()
import System.FilePath.Posix (takeExtension, takeFileName)
import System.Directory (findExecutable)
import System.IO.Error (catchIOError, tryIOError)
import System.IO.Temp (withSystemTempFile)
import System.Process
       (CreateProcess(..), StdStream(CreatePipe), createProcess, proc,
        readProcessWithExitCode, waitForProcess)

newtype StringException =
  StringException String
  deriving (Typeable, Show)

instance Exception StringException

doOrThrow :: ExceptT String IO a -> (a -> IO b) -> IO b
doOrThrow aOrExcept aHandler = do
  errorOrA <- runExceptT aOrExcept
  case errorOrA of
    Left errStr -> throwIO (StringException errStr)
    Right rightA -> aHandler rightA

ioStringOfAstModule :: Module -> IO String
ioStringOfAstModule modu =
  withContext $
  \ctx -> doOrThrow (withModuleFromAST ctx modu moduleLLVMAssembly) return

-- | Given an input file, compile if necessary, load the bitcode into
-- a Module, call 'moduleHandler', and write the resulting bitcode to
-- the output file.
genBitcodeWithTransform :: FilePath -> FilePath -> (Module -> Module) -> IO ()
genBitcodeWithTransform fileIn fileOut moduleHandler =
  let moduleOrExceptT = withModuleFromPath fileIn moduleHandler
  in doOrThrow
       moduleOrExceptT
       (\oldModule ->
           let newModule = moduleHandler oldModule
           in withContext
                (\context ->
                    doOrThrow
                      (withModuleFromAST
                         context
                         newModule
                         (\cModule ->
                             doOrThrow
                               (writeBitcodeToFile (File fileOut) cModule)
                               return))
                      return))

-- | Given an input file, compile if necessary, load the bitcode into
-- a Module, call 'moduleHandler', and compile the resulting bitcode
-- to executable.
genElfWithTransform :: FilePath -> FilePath -> (Module -> Module) -> IO ()
genElfWithTransform fileIn fileOut moduleHandler =
  let moduleOrExceptT = withModuleFromPath fileIn moduleHandler
  in doOrThrow moduleOrExceptT $
     \oldModule ->
        let newModule = moduleHandler oldModule
        in doOrThrow
             (withHostTargetMachine $
              \targetMachine ->
                 withContext $
                 \context ->
                    doOrThrow
                      (withModuleFromAST context newModule $
                       \cModule ->
                          doOrThrow
                            (writeObjectToFile targetMachine (File fileOut) cModule)
                            return)
                      return)
             return

-- | Given an input file, compile if necessary, load the bitcode into
-- a Module, and call 'moduleHandler' on it. The input file can be C,
-- C++, or LLVM bitcode.
withModuleFromPath :: FilePath -> (Module -> a) -> ExceptT String IO a
withModuleFromPath path moduleHandler =
  ExceptT
    (buildModule
       []
       []
       (\bitcodeFilePath ->
           withContext
             (\c ->
                 runExceptT
                   (liftM
                      moduleHandler
                      (withModuleFromBitcode
                         c
                         (LLVMModule.File bitcodeFilePath)
                         moduleAST))))
       path)

withModuleFromPathIO :: FilePath -> (Module -> IO a) -> IO a
withModuleFromPathIO path moduleHandler =
  buildModule
    []
    []
    (\bitcodeFilePath ->
        withContext
          (\c -> do
             aOrError <-
               runExceptT
                 (withModuleFromBitcode
                    c
                    (LLVMModule.File bitcodeFilePath)
                    (moduleAST >=> moduleHandler))
             case aOrError of
               Right a -> return a
               Left e -> throwIO (StringException e)))
    path

runProcessInEnv :: FilePath
                -> [String]
                -> st
                -> (st -> Maybe String -> (st, Maybe String))
                -> IO st
runProcessInEnv filePath args initState stateTransformer = do
  (Just hin, Just hout, _, _) <-
    createProcess
      (proc filePath args)
      { std_in = CreatePipe
      , std_out = CreatePipe
      }
  invokeTransformer hin hout initState stateTransformer
  where
    invokeTransformer hin hout initS stateTrans = do
      errOrLine <- tryIOError (hGetLine hout)
      let (st, output) =
            case errOrLine of
              Right line -> stateTrans initS (Just line)
              Left _ -> stateTrans initS Nothing
      case output of
        Nothing -> return st
        Just outStr -> do
          hPutStr hin outStr
          invokeTransformer hin hout st stateTrans

-- ---- From llvm-analysis ------------------------------------------------
data BuildException
  = ClangFailed FilePath
                ExitCode
  | NoBuildMethodForInput FilePath
  | NoOptBinaryFound
  | OptFailed FilePath
              ExitCode
  | LlcFailed FilePath
              ExitCode
  deriving (Typeable, Show)

instance Exception BuildException

-- | Optimize the bitcode in the given bytestring using opt with the
-- provided options
optify :: [String] -> FilePath -> FilePath -> IO ()
optify args inp optFile = do
  opt <- findOpt
  let cmd = proc opt ("-o" : optFile : inp : args)
  (_, _, _, p) <- createProcess cmd
  rc <- waitForProcess p
  when (rc /= ExitSuccess) $ throwIO $ OptFailed inp rc

-- | Given an input file, bitcode parsing function, and options to
-- pass to opt, return a Module.  The input file can be C, C++, or
-- LLVM bitcode.
--
-- Note that this function returns an Either value to report some
-- kinds of errors.  It can also raise IOErrors.
buildModule
  :: [String] -- ^ Front-end options (passed to clang) for the module.
  -> [String] -- ^ Optimization options (passed to opt) for the
     -- module.  opt is not run if the list is empty
  -> (FilePath -> IO a) -- ^ A function to turn a bitcode file into a
  -> FilePath -- ^ The input file (either bitcode or C/C++)
  -> IO a
buildModule clangOpts optOpts parseFile inputFilePath = do
  clang <- findClang
  clangxx <- findClangxx
  case takeExtension inputFilePath of
    ".ll" -> simpleBuilder inputFilePath
    ".bc" -> simpleBuilder inputFilePath
    ".c" -> clangBuilder inputFilePath clang
    ".C" -> clangBuilder inputFilePath clangxx
    ".cc" -> clangBuilder inputFilePath clangxx
    ".cxx" -> clangBuilder inputFilePath clangxx
    ".cpp" -> clangBuilder inputFilePath clangxx
    _ -> throwIO $ NoBuildMethodForInput inputFilePath
  where
    simpleBuilder inp
      | null optOpts = parseFile inp
      | otherwise =
        withSystemTempFile ("opt_" ++ takeFileName inp) $
        \optFname _ -> do
          optify optOpts inp optFname
          parseFile optFname
    clangBuilder inp driver =
      withSystemTempFile ("base_" ++ takeFileName inp) $
      \baseFname _ -> do
        let cOpts = clangOpts ++ ["-emit-llvm", "-o", baseFname, "-c", inp]
        (_, _, _, p) <- createProcess $ proc driver cOpts
        rc <- waitForProcess p
        when (rc /= ExitSuccess) $ throwIO $ ClangFailed inputFilePath rc
        if null optOpts
          then parseFile baseFname
          else withSystemTempFile ("opt_" ++ takeFileName inp) $
               \optFname _ -> do
                 optify optOpts baseFname optFname
                 parseFile optFname

buildAndExec
  :: Module
  -> [String]
  -> [String]
  -> [String]
  -> String
  -> IO (ExitCode, String, String)
buildAndExec theModule clangOpts llcOpts args input =
  withSystemTempFile "a.out" $
  \exePath exeHandle -> do
    buildElf theModule clangOpts llcOpts exePath
    hClose exeHandle
    -- to disable ASLR
    readProcessWithExitCode "setarch" (["x86_64", "-R", exePath] ++ args) input

buildElf
  :: Module
  -> [String] -- ^ Front-end options (passed to clang) for the module.
  -> [String] -- ^ llc options
  -> FilePath -- ^ The output file
  -> IO ()
buildElf theModule clangOpts llcOpts outputFilePath =
  withSystemTempFile ("base_" ++ takeFileName outputFilePath ++ ".bc") $
  \baseFilePath _ -> do
    withContext
      (\context ->
          doOrThrow
            (withModuleFromAST
               context
               theModule
               (\cModule ->
                   doOrThrow (writeBitcodeToFile (File baseFilePath) cModule) return))
            return)
    withSystemTempFile ("llc_" ++ takeFileName outputFilePath ++ ".s") $
      \llcFilePath _ -> do
        llc <- findLlc
        let realLlcOpts = llcOpts ++ ["-o", llcFilePath, baseFilePath]
        (_, _, _, llcProc) <- createProcess $ proc llc realLlcOpts
        llcRet <- waitForProcess llcProc
        case llcRet of
          ExitSuccess -> do
            clang <- findClang
            let cOpts = clangOpts ++ ["-o", outputFilePath, llcFilePath]
            (_, _, _, p) <- createProcess $ proc clang cOpts
            rc <- waitForProcess p
            when (rc /= ExitSuccess) $ throwIO $ ClangFailed outputFilePath rc
          _ -> throwIO $ LlcFailed outputFilePath llcRet

-- | Find a suitable @opt@ binary in the user's PATH
--
-- First consult the LLVM_OPT environment variable.  If that is not
-- set, try a few common opt aliases.
findOpt :: IO FilePath
findOpt = do
  let fbin = findBin ["opt-3.5", "opt"]
  catchIOError (getEnv "LLVM_OPT") (const fbin)

findClang :: IO FilePath
findClang = do
  let clangBin = findBin ["clang-3.5", "clang"]
  catchIOError (getEnv "LLVM_CLANG") (const clangBin)

findClangxx :: IO FilePath
findClangxx = do
  let clangxxBin = findBin ["clang++-3.5", "clang++"]
  catchIOError (getEnv "LLVM_CLANGXX") (const clangxxBin)

findLlc :: IO FilePath
findLlc = do
  let llcBin = findBin ["llc-3.5", "llc"]
  catchIOError (getEnv "LLVM_LLC") (const llcBin)

findBin :: [String] -> IO FilePath
findBin [] = throwIO NoOptBinaryFound
findBin (bin:bins) = do
  b <- findExecutable bin
  case b of
    Just e -> return e
    Nothing -> findBin bins
