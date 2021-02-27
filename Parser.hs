module Parser where

import Prelude hiding ( sum, not )
import Lexer ( lexer )
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import AST
import PrettyPrint

identifier = Token.identifier lexer
parens = Token.parens lexer
reservedOp = Token.reservedOp lexer

var = Var <$> identifier

opArrow = (reservedOp "->" <|> reservedOp "→") >> return Arrow

opOr = (reservedOp "\\/" <|> reservedOp "∨") >> return Sum

opAnd = (reservedOp "/\\" <|> reservedOp "∧") >> return Product

opNot = (reservedOp "~" <|> reservedOp "¬") >> return (`Arrow` Bot)

opBot = reservedOp "⊥" >> return Bot

typeExpr = buildExpressionParser typeOpTable typeTerm

typeTerm =  parens typeExpr
        <|> opBot
        <|> typeVar

typeOpTable = [
               [Prefix opNot],
               [Infix opAnd AssocLeft],
               [Infix opOr AssocLeft],
               [Infix opArrow AssocRight]
              ]

arrow :: Parsec String () Type
-- arrow = Arrow <$> prod <* opArrow *> arrow <|> prod
arrow = chainl1 typeVar opArrow
prod  = Sum <$> arrow <* opOr *> sum <|> sum
sum = Product <$> sum <* opAnd *> not <|> not
-- not = (opNot >> bot >>= (`Arrow` Bot)) <|> bot
not = (\_ y -> Arrow y Bot) <$> opNot *> bot <|> bot
bot = (reservedOp "⊥" >> return Bot) <|> typeVar
typeVar = TypeVar <$> identifier

types = undefined

expr :: Parsec String () Expr
expr =  parens expr
    <|> undefined