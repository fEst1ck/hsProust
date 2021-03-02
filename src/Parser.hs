module Parser (parseExpr, parseType) where

import Lexer ( hsProustDef, lexer )
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import AST
import PrettyPrint

identifier = Token.identifier lexer
parens = Token.parens lexer
reservedOp = Token.reservedOp lexer
symbol = Token.symbol lexer
whiteSpace = Token.whiteSpace lexer

opArrow = (reservedOp "->" <|> reservedOp "→") >> return Arrow

opOr = (reservedOp "\\/" <|> reservedOp "∨") >> return Sum

opAnd = (reservedOp "/\\" <|> reservedOp "∧") >> return Product

opNot = (reservedOp "~" <|> reservedOp "¬") >> return (`Arrow` Bot)

opBot = reservedOp "⊥" >> return Bot

-- t = t → t
--   | t ∨ t
--   | t ∧ t
--   | ¬ t
--   | ⊥
--   | x
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

typeVar = TypeVar <$> identifier

lam = do {reservedOp "\\"; x <- identifier; reservedOp "=>"; e <- expr'''; return $ Lam x e}
-- lam = reservedOp "\\" >> Lam <$> identifier <*> expr'''
andIntro = symbol "/\\-intro" >> AndIntro <$> expr''' <*> expr'''
andElim0 = symbol "/\\-elim0" >> AndElim0 <$> expr'''
andElim1 = symbol "/\\-elim1" >> AndElim1 <$> expr'''
orIntro0 = symbol "\\/-intro0" >> OrIntro0 <$> expr'''
orIntro1 = symbol "\\/-intro1" >> OrIntro1 <$> expr'''
orElim = symbol "\\/-elim" >> OrElim <$> expr''' <*> expr''' <*> expr'''
botElim = symbol "⊥" >> BotElim <$> expr'''
ann = do { x <- expr'; reservedOp ":"; t <- typeExpr; return $ Ann x t }
hole = reservedOp "?" >> return (Hole 0)
-- app = do {f <- expr'; appSym; a <- expr; return $ App f a}
app = chainl1 expr (symbol "." >> return App)
appSym = whiteSpace >> notFollowedBy (choice . map reservedOp $ Token.reservedOpNames hsProustDef)
-- ann = chainr1 expr' (reservedOp ":" >> return $ Ann x t)
var = Var <$> identifier

expr :: Parsec String () Expr
expr =  try andIntro
    <|> try andElim0
    <|> try andElim1
    <|> try orIntro0
    <|> try orIntro1
    <|> try orElim
    <|> try botElim
    <|> try hole
    <|> try var
    <|> try (parens expr''')

expr' :: Parsec String () Expr
expr' = try app <|> expr <|> parens expr''

expr'' :: Parsec String () Expr
expr'' = try ann <|> expr'
      <|> parens expr'''

expr''' = try lam <|> expr'' <|> parens expr'''

parseExpr = expr'''

parseType = typeExpr
