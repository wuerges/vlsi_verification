module LLVMCompile where

import Graph
import LLVM.General.AST
import LLVM.General.AST.Global
import LLVM.General.AST.Type
import LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.Constant as C
import Control.Monad.State (get, put, execState, State)
import Control.Monad (mapM, foldM)


import Debug.Trace

returnArray :: Name
returnArray = Name "returnArray"

inputArray :: Name
inputArray = Name "inputArray"

mkarrayParameter :: [Int] -> Name -> Parameter
mkarrayParameter ns n =
  --Parameter (ArrayType (fromIntegral . length $ ns) i1) n []
  Parameter (ArrayType (fromIntegral . length $ ns) i32) n []


defineModule :: G -> Module
defineModule g = defaultModule
  { moduleName = "topLevelModule"
  , moduleDefinitions = [GlobalDefinition $ defineFunction g] }

defineFunction :: G -> Global
defineFunction g =
    functionDefaults {
          name        = Name "topLevel"
        , parameters  = ([ia, ra], False)
        , returnType  = mkArrType l
        , basicBlocks =
          [BasicBlock
            (Name "bb0")
            (reverse is)
            (Name "ret" := Ret (Just $ mkArrOp l returnArray) [])
          ]
        }
    where ctxs    = contexts g
          (_, is) = execState (generateCode g) (0, [])
          ia = mkarrayParameter (inputs g) inputArray
          ra = mkarrayParameter (outputs g) returnArray
          l  = length $ outputs g

type Codegen a = State (Word, [Named Instruction]) a



mkArrType :: Int -> Type
mkArrType n = ArrayType (fromIntegral n) i32

mkArrOp :: Int -> Name -> Operand
mkArrOp l name = LocalReference (mkArrType l) name

copyInput :: Int -> (Int, Int) -> Codegen ()
copyInput l (idx, tgt) = trace ("\nIDX : " ++ show idx ++ "\n") $
  do
    --n <- fresh
    --genInstr $ mkname idx := Alloca i32 Nothing 4 []
    --genInstr $ mkname idx := ExtractValue (mkoperand inputArray) [fromIntegral idx] []
    genInstr $ mkname tgt :=
      ExtractValue
        (mkArrOp l inputArray)
        [fromIntegral idx]
        []
    {-
    genInstr $ mkname idx :=
      ExtractElement
        (mkoperand inputArray)
        (ConstantOperand $ C.Int 32 (fromIntegral idx))
        []
     -}

copyOutput :: Int -> (Int, Int) -> Codegen ()
copyOutput l (idx, tgt) = do
  o <- fresh
  genInstr $ o :=
    InsertValue
      (LocalReference (mkArrType l) returnArray)
      (LocalReference i32 (mkname tgt))
      [fromIntegral idx]
      []

generateCode :: G -> Codegen ()
generateCode g = do
  let ctxs = contexts g
  mapM_ (copyInput (length $ inputs g)) (zip [0..] (inputs g))
  mapM_ genContext ctxs
  mapM_ (copyOutput (length $ outputs g)) (zip [0..] (outputs g))

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
mkoperand n = LocalReference i32 n

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
constOne = ConstantOperand $ C.Int 32 1

constZero :: Operand
constZero = ConstantOperand $ C.Int 32 0

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


