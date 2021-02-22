module Data where

type Var = String

data Expr = Lam Var Expr
          | AndIntro Expr Expr
          | AndElim0 Expr
          | AndElim1 Expr
          | OrIntro0 Expr
          | OrIntro1 Expr
          | OrElim Expr Expr Expr
          | ContraElim Expr
          | App Expr Expr
          | Ann Expr Type
          | Hole Int
          | Var Var

data Type = Arrow Type Type
          | Product Type Type
          | Sum Type Type
          | Contra
          | TypeVar Var
