{-# LANGUAGE RecordWildCards #-}

module Verilog where

import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State


data Val = Input String
         | Output String
         | Wire String
         | ValZero
         | ValOne
  deriving (Eq, Ord, Show)

data Function = Fun { _op  :: Op
                    , _out :: [Val]
                    , _in  :: [Val] }
    deriving (Ord, Eq, Show)

--namesFun :: Function -> [Val]
--namesFun (Fun _ o i) = o ++ i

data Op = And
        | Or
        | Buf
        | Xor
        | Xnor
        | Not
        | Nor
        | Nand
    deriving (Ord, Eq, Show)

makeFunction :: Op -> [Val] -> Function
makeFunction Buf ps = Fun Buf (init ps) [last ps]
makeFunction Not ps = Fun Not (init ps) [last ps]
makeFunction op (p:ps) = Fun op [p] ps
makeFunction _ _      = error "Function must have at least 1 input and 1 output wire"

data Verilog = Verilog { _inputs :: [String]
                       , _outputs :: [String]
                       , _functions :: [Function]
                       }
    deriving Show

--names :: Verilog -> [String]
--names v = _inputs v ++ _outputs v ++ concatMap namesFun (_functions v)

emptyVerilog = Verilog { _inputs = []
                       , _outputs = []
                       , _functions = [] }
