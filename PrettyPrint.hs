module PrettyPrint where

import AST

instance Show Expr where
    show (Lam x e)         = "(λ " ++ x ++ " => " ++ show e ++ ")"
    show (AndIntro e1 e2)  = "(∧-intro " ++ show e1 ++ show e2 ++ ")"
    show (AndElim0 e)      = "(∧-elim0 " ++ show e ++ ")"
    show (AndElim1 e)      = "(∧-elim1 " ++ show e ++ ")"
    show (OrIntro0 e)      = "(∨-intro0 " ++ show e ++ ")"
    show (OrIntro1 e)      = "(∨-intro1 " ++ show e ++ ")"
    show (OrElim e1 e2 e3) = "(∨-elim " ++ show e1 ++ show e2 ++ show e3 ++ ")"
    show (BotElim e)    = "(⊥-elim " ++ show e ++ ")"
    show (App e1 e2)       = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Ann e t)         = "(" ++ show e ++ " : " ++ show t ++ ")"
    show (Hole n)          = "?" ++ show n
    show (Var x)           = x

instance Show Type where
    show (Arrow a b)   = "(" ++ show a ++ " -> " ++ show b ++ ")"
    show (Product a b) = "(" ++ show a ++ " ∧ " ++ show b ++ ")"
    show (Sum a b)     = "(" ++ show a ++ " ∨ " ++ show b ++ ")"
    show Bot        = "⊥"
    show (TypeVar t)   = t