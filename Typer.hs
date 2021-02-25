import AST
import Prelude hiding (lookup)
import Data.Map ( insert, lookup, Map )
import Control.Monad ( guard )

type Context = Map Var Type

typeCheck :: Context -> Expr -> Type -> Maybe ()
typeCheck ctx (Lam x e) (Arrow t w) = typeCheck (insert x t ctx) e w
typeCheck ctx (AndIntro e1 e2) (Product t w) = do
    typeCheck ctx e1 t
    typeCheck ctx e2 w
typeCheck ctx (AndElim0 e) t = do
    (Product t' _) <- typeSynth ctx e
    guard $ t == t'
typeCheck ctx (AndElim1 e) w = do
    (Product _ w') <- typeSynth ctx e
    guard $ w == w'
typeCheck ctx (OrIntro0 e) (Sum t _) = typeCheck ctx e t
typeCheck ctx (OrIntro1 e) (Sum _ w) = typeCheck ctx e w
typeCheck ctx (OrElim d f g) v = do
    (Product t w) <- typeSynth ctx d
    typeCheck ctx f (Arrow t v)
    typeCheck ctx g (Arrow w v)
typeCheck ctx (BotElim e) _ = typeCheck ctx e Bot
typeCheck ctx e t = do
    t' <- typeSynth ctx e
    guard $ t == t'

typeSynth :: Context -> Expr -> Maybe Type
typeSynth ctx (Var x) = lookup x ctx
typeSynth ctx (App f a) = do
    Arrow t w <- typeSynth ctx f
    typeCheck ctx a t
    return w
typeSynth ctx (Ann x t) = do
    typeCheck ctx x t
    return t
typeSynth ctx (AndIntro e1 e2) = Product <$> typeSynth ctx e1 <*> typeSynth ctx e2
typeSynth ctx (AndElim0 e) = product0 <$> typeSynth ctx e
typeSynth ctx (AndElim1 e) = product1 <$> typeSynth ctx e
