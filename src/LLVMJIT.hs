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
import Foreign.Ptr ( FunPtr, castFunPtr, Ptr )

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


import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

import qualified LLVM.General.ExecutionEngine as EE


import qualified Data.Graph.Inductive as GI
import Graph
import LLVMCompile
import LLVM.General.AST as AST
import Data.Time

runJITG :: G -> [Bool] -> Maybe AST.Module ->  IO (Either String (AST.Module, [Bool]))
runJITG g is mod  = do
  case mod of
    Nothing -> runJIT g is (defineModule g)
    Just m ->  runJIT g is m

runJIT :: G -> [Bool] -> AST.Module -> IO (Either String (AST.Module, [Bool]))
runJIT g is mod = do
  --putStrLn $ showPretty mod
  withContext $ \context ->
    jit context $ \executionEngine ->
      runExceptT $ withModuleFromAST context mod $ \m ->
        withPassManager passes $ \pm -> do
          -- Optimization Pass
          {-runPassManager pm m-}
          optmod <- moduleAST m
          --s <- moduleLLVMAssembly m
          --putStrLn s

          res' <- EE.withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- EE.getFunction ee (AST.Name "topLevel")
            case mainfn of
              Just fn -> do
                res <- run fn g is
                --putStrLn $ "Evaluated to: " ++ show res
                return res
              Nothing -> error $ "Could not find function"
                --return ()

          -- Return the optimized module
          return (optmod, res')

type FType = (Ptr Word32 -> Ptr Word32 -> IO (Ptr Word32))

foreign import ccall "dynamic" haskFun ::
  FunPtr FType -> FType


allocToPtr :: [Bool] -> IO (Ptr Bool)
allocToPtr vs = newArray vs

freeToBool :: Int -> Ptr Bool -> IO [Bool]
freeToBool s pvs =
  do bools <- peekArray s pvs
     --free pvs
     return bools

btw :: Bool -> Word32
btw True  = 1
btw False = 0

wtb :: Word32 -> Bool
wtb 1 = True
wtb 0 = False
wtb _ = error "fuck"


run :: FunPtr () -> G -> [Bool] -> IO [Bool]
run fn g ivs = do
  let ivs_32 = map btw ivs
      l_os = length $ outputs g
      f    = haskFun (castFunPtr (fn :: FunPtr ()) :: FunPtr FType)
  withArray ivs_32 (\ia_ptr ->
    withArray (replicate l_os 1) (\ra_ptr ->
      do ptr <- f ia_ptr ra_ptr
         --ws <- peekArray l_os ptr
         ws <- peekArray l_os ra_ptr
         return $ map wtb ws
         ))


 {-
  putStrLn $ "Allocating arrays of sizes: " ++ show (length $ inputs g, length $ outputs g)
  ia <- if length ivs == length (inputs g)
           then allocToPtr ivs :: IO (Ptr Bool)
           else error "Trying to allocate array with the wrong size of inputs"
  oa <- mallocArray (length $ outputs g) :: IO (Ptr Bool)
  freeToBool  (length $ outputs g) ret
  -}

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }
