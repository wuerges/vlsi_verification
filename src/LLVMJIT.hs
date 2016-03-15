module LLVMJIT where


 {-
import LLVM.General.PrettyPrint
import LLVM.General.AST
import LLVM.General.Module

compileGraph :: G -> String
compileGraph = show . defineModule
  -}




import Data.Int
import Data.Word
import Foreign.Ptr ( FunPtr, castFunPtr )

import Control.Monad.Except

import LLVM.General.Target
import LLVM.General.Context
import LLVM.General.CodeModel
import LLVM.General.Module as Mod
import qualified LLVM.General.AST as AST

import LLVM.General.PassManager
import LLVM.General.Transforms
import LLVM.General.Analysis
import LLVM.General.PrettyPrint


import qualified LLVM.General.ExecutionEngine as EE


import qualified Data.Graph.Inductive as GI
import Graph
import LLVMCompile
import LLVM.General.AST as AST
import Data.Time

runJITG :: G -> IO (Either String AST.Module)
runJITG g = do
  --putStrLn $ "INPUTS:" ++ show (inputs g)
  --putStrLn $ "OUTPUTS:" ++ show (outputs g)
  --putStrLn $ "NODES:" ++ show (GI.nodes g)
  --putStrLn $ "Dotty:\n----------------\n" ++ showGraph g ++ "\n-------------\n"
  runJIT (defineModule g)

runJIT :: AST.Module -> IO (Either String AST.Module)
runJIT mod = do
  --putStrLn $ showPretty mod
  withContext $ \context ->
    jit context $ \executionEngine ->
      runExceptT $ withModuleFromAST context mod $ \m ->
        withPassManager passes $ \pm -> do
          -- Optimization Pass
          {-runPassManager pm m-}
          optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          --putStrLn s

          EE.withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- EE.getFunction ee (AST.Name "topLevel")
            case mainfn of
              Just fn -> do
                res <- do 
                        start <- getCurrentTime
                        run fn
                        stop <- getCurrentTime
                        putStrLn $ "Run in : " ++ show (diffUTCTime stop start)

                putStrLn $ "Evaluated to: " ++ show res
              Nothing -> error $ "Could not find function"
                --return ()

          -- Return the optimized module
          return optmod

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> (IO Double)

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }
