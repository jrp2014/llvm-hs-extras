{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright: (c) 2021 jrp2014
-- SPDX-License-Identifier: MIT
-- Maintainer: jrp2014 <jrp2014@users.noreply.github.com>
-- Stability: experimental
-- Description: A collection of extra functions for "LLVM" (12 and above)
--
-- See README for more info
module LLVM.Extras
  ( -- * JIT
    withSimpleJIT,
  )
where

import Control.DeepSeq
  ( NFData,
    force,
  )
import Control.Monad (unless)
import Control.Exception ( evaluate )
import qualified Data.ByteString.Char8 as BS
import Foreign.Ptr
  ( FunPtr,
    castPtrToFunPtr,
    wordPtrToPtr,
  )
import LLVM.AST
  ( Module,
    Name (..),
  )
import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified LLVM.CodeModel as CodeModel
import LLVM.Context
  ( withContext,
  )
import LLVM.Linking (loadLibraryPermanently)
import LLVM.Module
  ( moduleLLVMAssembly,
    withModuleFromAST,
  )
import LLVM.OrcJIT
  ( JITSymbol (JITSymbol),
    addDynamicLibrarySearchGeneratorForCurrentProcess,
    addModule,
    createIRCompileLayer,
    createJITDylib,
    createRTDyldObjectLinkingLayer,
    lookupSymbol,
    withClonedThreadSafeModule,
    withExecutionSession,
  )
import LLVM.PassManager
  ( PassSetSpec (optLevel),
    defaultCuratedPassSetSpec,
    runPassManager,
    withPassManager,
  )
import qualified LLVM.Relocation as Reloc
import LLVM.Target (withHostTargetMachine)

-- * JIT (ORC v2)

optimize3 :: PassSetSpec
optimize3 = defaultCuratedPassSetSpec {optLevel = Just 3}

-- | Given an AST ('Module') a 'PassSetSpec' (eg, 'defaultCuratedPassSetSpec)
-- and the name of the function, compile and run it and return the result.
withSimpleJIT ::
  NFData a =>
  Module ->
  PassSetSpec ->
  Name ->
  -- | The signature of the function (eg, @FunPtr (IO Int32) -> IO Int322)
  (FunPtr (IO ft) -> IO ft) ->
  -- | what to do with the resulting function
  (ft -> a) ->
  IO a
withSimpleJIT ast passes (Name name) mkFun apFun = do
  loaded <- loadLibraryPermanently Nothing -- make the symbols from the current process available.
  unless loaded $
    putStrLn "withSimpleJIT: Failed to load symbols from current process"
  withContext $ \context -> do
    withModuleFromAST context ast $ \modul -> do
      putStrLn "Original Module Assembly:"
      asm <- moduleLLVMAssembly modul
      BS.putStrLn asm

      withPassManager passes $ \pm -> do
        optimized <- runPassManager pm modul
        unless optimized $ putStrLn "withSimpleJIT: Failed to optimize module"

        optasm <- moduleLLVMAssembly modul
        BS.putStrLn optasm

        withHostTargetMachine Reloc.PIC CodeModel.Default CodeGenOpt.Default $ \tm ->
          withExecutionSession $ \executionSession -> do
            dylib <- createJITDylib executionSession "SimpleDylib"
            withClonedThreadSafeModule modul $ \tsm -> do
              linkingLayer <- createRTDyldObjectLinkingLayer executionSession
              compileLayer <- createIRCompileLayer executionSession linkingLayer tm
              addDynamicLibrarySearchGeneratorForCurrentProcess compileLayer dylib
              addModule tsm dylib compileLayer
              symbol <- lookupSymbol executionSession compileLayer dylib name
              case symbol of
                Left err -> do
                  print err
                  error "quit"
                Right (JITSymbol fnAddr _) -> do
                  fn <- mkFun . castPtrToFunPtr $ wordPtrToPtr fnAddr
                  evaluate $ force (apFun fn)
