module LLVMCompile where

import Graph
import LLVM.General.AST
import LLVM.General.AST.Global
import LLVM.General.AST.Type
import qualified LLVM.General.AST.Constant as C
import Control.Monad.State (get, put, execState, State)
import Control.Monad (mapM, foldM)


returnArray :: Name
returnArray = Name "returnArray"



defineModule :: G -> Module
defineModule g = defaultModule
  { moduleName = "topLevelModule"
  , moduleDefinitions = [GlobalDefinition $ defineFunction g] }

defineFunction :: G -> Global
defineFunction g =
    functionDefaults {
          name        = Name "topLevel"
        , parameters  = (ra:[Parameter i1 (mkname x) [] | x <- inputs g], False)
        , returnType  = void
        , basicBlocks =
            [BasicBlock (Name "bb0") is (Name "ret" := Ret Nothing [])]
        }
    where ctxs    = contexts g
          (_, is) = execState (mapM_ genContext ctxs) (0, [])
          ra = Parameter
                (ArrayType
                    (fromIntegral. length $ outputs g) i1) returnArray []

type Codegen a = State (Word, [Named Instruction]) a

fresh :: Codegen Name
fresh = do
    (n, is) <- get
    put (n + 1, is)
    return $ UnName n


genInstr :: (Named Instruction) ->  Codegen ()
genInstr ni = do
    (n, is) <- get
    put (n, ni:is)

mkname :: Int -> Name
mkname x = Name $ "node_" ++ show x

mkoperand :: Name -> Operand
mkoperand n = LocalReference i1 n

genAnd1 :: Name -> Name -> Codegen Name
genAnd1 n1 n2  = do
    n3 <- fresh
    genInstr $ n3 := And (mkoperand n1) (mkoperand n2) []
    return n3

genAnd :: [Name] -> Codegen Name
genAnd []     = error "generating and for empty list"
genAnd [n]    = return n
genAnd (n:ns) = foldM genAnd1 n ns


constOne :: Operand
constOne = ConstantOperand $ C.Int 1 1

constZero :: Operand
constZero = ConstantOperand $ C.Int 1 0

genNames1 :: (Bool, Int) -> Codegen Name
genNames1 (True, x ) = return $ mkname x
genNames1 (False, x) = do
    n <- fresh
    genInstr $ n := Xor (mkoperand $ mkname x) constOne []
    return n

genNames :: [(Bool, Int)] -> Codegen [Name]
genNames is = mapM genNames1 is

copyInstr :: Name -> Name -> Codegen ()
copyInstr n1 n2 = do
    genInstr $ n2 := Or (mkoperand n1) constZero []


genContext :: Ctx -> Codegen ()
genContext ([], n, (), _) = return ()
genContext (is, n, (), _) = do
    ns <- genNames is
    r <- genAnd ns
    copyInstr r (mkname n)


