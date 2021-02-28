module AST where

type Var = String

data Expr = Lam Var Expr
          | AndIntro Expr Expr
          | AndElim0 Expr
          | AndElim1 Expr
          | OrIntro0 Expr
          | OrIntro1 Expr
          | OrElim Expr Expr Expr
          | BotElim Expr
          | App Expr Expr
          | Ann Expr Type
          | Hole Int
          | Var Var

data Type = Arrow Type Type
          | Product { product0 :: Type, product1 :: Type }
          | Sum Type Type
          | Bot
          | TypeVar Var
    deriving Eq
