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

import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS
import Foreign.Ptr (FunPtr, castPtrToFunPtr, wordPtrToPtr)
import LLVM.AST
import LLVM.AST (Module)
import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified LLVM.CodeModel as CodeModel
import LLVM.Context (Context)
import LLVM.Linking (loadLibraryPermanently)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)
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

-- | Given a 'Context', an AST ('Module') a 'PassSetSpec'
-- (eg, 'defaultCuratedPassSetSpec) and the name of the function, return
-- a pointer to the resulting JIT-compiled function which can then be run in that 'Context'
-- (perhaps using 'Control.DeepSeq.force' and then 'Control.Exception.evaluate')
withSimpleJIT :: Context -> Module -> PassSetSpec -> Name -> IO (Maybe (FunPtr a))
withSimpleJIT context ast passes (Name name) = do
  loaded <- loadLibraryPermanently Nothing -- make the symbols from the current process available.
  unless loaded $ putStrLn "withSimpleJIT: Failed to load symbols from current process"

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
            --              addDynamicLibrarySearchGeneratorForCurrentProcess compileLayer dylib
            addModule tsm dylib compileLayer
            symbol <- lookupSymbol executionSession compileLayer dylib name
            case symbol of
              Left err -> do
                print err
                return Nothing
              Right (JITSymbol mainFn _) -> return $ Just (castPtrToFunPtr (wordPtrToPtr mainFn))
