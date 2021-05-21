{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

import LLVM.Extras ( withSimpleJIT )
import Data.Int ( Int32 )
import Foreign.Ptr ( FunPtr )
import LLVM.AST
    ( defaultModule,
      functionDefaults,
      Definition(GlobalDefinition),
      Module(moduleName, moduleDefinitions),
      BasicBlock(BasicBlock),
      Named(Do),
      Terminator(Ret),
      Name(Name),
      Operand(ConstantOperand),
      Type(IntegerType) )
import qualified LLVM.AST as AST
import LLVM.AST.Constant ( Constant(Int) )
import LLVM.AST.Global
    ( Global(name, parameters, returnType, basicBlocks) )
import LLVM.Context ( withContext )
import LLVM.PassManager ( defaultCuratedPassSetSpec )

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
  withContext $ \ context -> do
    Just fn <- withSimpleJIT context module_ defaultCuratedPassSetSpec "add"
    result <- mkMain fn
    print result
