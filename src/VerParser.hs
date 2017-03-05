module VerParser where

import Text.ParserCombinators.Parsec
import Verilog

import qualified Text.Parsec.Token as P
import Text.Parsec.Language (javaStyle)


verilog = javaStyle { P.reservedNames = ["endmodule", "module", "input", "output", "wire"] }

-- Lexer
lexer       = P.makeTokenParser verilog
whiteSpace  = P.whiteSpace lexer
parens      = P.parens lexer
braces      = P.braces lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer
commaSep    = P.commaSep lexer
semi        = P.semi lexer


wire :: GenParser Char st Val
wire = do
  name <- identifier
  return $ Wire name

-- Semantics
data CompExpr = WireExpr [String]
              | InputExpr [String]
              | OutputExpr [String]
              | FunctionExpr Function

makeVerilog :: [CompExpr] -> Verilog
makeVerilog = foldl addVerilog emptyVerilog

addVerilog :: Verilog -> CompExpr -> Verilog
addVerilog v (WireExpr ws)      = v
addVerilog v (InputExpr ws)     = v { _inputs    = _inputs v ++ ws }
addVerilog v (OutputExpr ws)    = v { _outputs   = _outputs v ++ ws }
addVerilog v (FunctionExpr f)   = v { _functions = f:_functions v }

literalZero :: GenParser Char st Val
literalZero = reserved "1'b0" >> whiteSpace >> return ValZero

literalOne :: GenParser Char st Val
literalOne = reserved "1'b1" >> whiteSpace >> return ValOne



parseWire :: GenParser Char st Val
parseWire = wire <|> literalOne <|> literalZero

-- Syntax
moduleExpr :: GenParser Char st (String, [String])
moduleExpr = do whiteSpace
                reserved "module"
                name <- identifier
                ports <- parens (commaSep identifier)
                _ <- semi
                return (name, ports)

parseOp :: GenParser Char st Op
parseOp =    (reserved "and"  >> return And )
         <|> (reserved "or"   >> return Or  )
         <|> (reserved "buf"  >> return Buf )
         <|> (reserved "xor"  >> return Xor )
         <|> (reserved "xnor" >> return Xnor)
         <|> (reserved "not"  >> return Not )
         <|> (reserved "nor"  >> return Nor )
         <|> (reserved "nand" >> return Nand)

functionExpr :: GenParser Char st CompExpr
functionExpr = do op <- parseOp
                  params <- parens (commaSep parseWire)
                  _ <- semi
                  return $ FunctionExpr $ makeFunction op params

outputExpr :: GenParser Char st CompExpr
outputExpr = do reserved "output"
                is <- commaSep identifier
                _ <- semi
                return $ OutputExpr is

inputExpr :: GenParser Char st CompExpr
inputExpr = do reserved "input"
               is <- commaSep identifier
               _ <- semi
               return $ InputExpr is

wireExpr :: GenParser Char st CompExpr
wireExpr = do reserved "wire"
              is <- commaSep identifier
              _ <- semi
              return $ WireExpr is

verilogExpr :: GenParser Char st CompExpr
verilogExpr = inputExpr <|> outputExpr <|> wireExpr <|> functionExpr

endModuleExpr :: GenParser Char st ()
endModuleExpr = do reserved "endmodule"
                   return ()

topModule :: GenParser Char st Verilog
topModule = do (name, ports) <- moduleExpr
               es <- many verilogExpr
               endModuleExpr
               eof
               return $  makeVerilog es

parseVerilog :: String -> IO (Either ParseError Verilog)
parseVerilog = parseFromFile topModule
