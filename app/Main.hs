{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import LLVM.AST
  ( BasicBlock (BasicBlock),
    Definition (GlobalDefinition),
    Module (moduleDefinitions, moduleName),
    Name (Name),
    Named (Do),
    Operand (ConstantOperand),
    Terminator (Ret),
    Type (IntegerType),
    defaultModule,
    functionDefaults,
  )
import qualified LLVM.AST as AST
import LLVM.AST.Constant (Constant (Int))
import LLVM.AST.Global
  ( Global (basicBlocks, name, parameters, returnType),
  )
import LLVM.Extras (withSimpleJIT)
import LLVM.PassManager (defaultCuratedPassSetSpec)

foreign import ccall "dynamic"
  mkMain :: FunPtr (IO Int32) -> IO Int32

int :: Type
int = IntegerType 32

defAdd :: Definition
defAdd =
  GlobalDefinition
    functionDefaults
      { name = Name "add",
        parameters = ([], False),
        returnType = int,
        basicBlocks = [body]
      }
  where
    body =
      BasicBlock
        (Name "entry")
        []
        (Do $ Ret (Just (ConstantOperand (Int 32 42))) [])

module_ :: AST.Module
module_ =
  defaultModule
    { moduleName = "llvm-hs-examples",
      moduleDefinitions = [defAdd]
    }

main :: IO ()
main = do
  result <- withSimpleJIT module_ defaultCuratedPassSetSpec "add" mkMain (* 2)
  print result
