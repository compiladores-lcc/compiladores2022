{-|
Module      : Elab
Description : Elabora un término fully named a uno locally closed.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite elaborar términos y declaraciones para convertirlas desde
fully named (@STerm) a locally closed (@Term@)
-}

module Elab ( elab, elabDecl) where

import Lang
import Subst

-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado. 
elab :: STerm -> Term
elab = elab' []

elab' :: [Name] -> STerm -> Term
elab' env (SV p v) =
  -- Tenemos que ver si la variable es Global o es un nombre local
  -- En env llevamos la lista de nombres locales.
  if v `elem` env 
    then  V p (Free v)
    else V p (Global v)

elab' _ (SConst p c) = Const p c
elab' env (SLam p [(v, ty)] t) = Lam p v ty (close v (elab' (v:env) t))
elab' env (SLam p ((v, ty):xs) t) = Lam p v ty (close v (elab' (v:env) (SLam p xs t)))
elab' env (SFix p (f,fty) (x,xty) [] t) = Fix p f fty x xty (close2 f x (elab' (x:f:env) t))
elab' env (SFix p (f,fty) (x,xty) xs t) = Fix p f fty x xty (close2 f x (elab' (x:f:env) (SLam p xs t)))
elab' env (SIfZ p c t e)         = IfZ p (elab' env c) (elab' env t) (elab' env e)
-- Operadores binarios
elab' env (SBinaryOp i o t u) = BinaryOp i o (elab' env t) (elab' env u)
-- Operador Print
elab' env (SPrint i str t) = Print i str (elab' env t)
-- Aplicaciones generales
elab' env (SApp p h a) = App p (elab' env h) (elab' env a)
elab' env (SLet p (v,vty) def body) =  
  Let p v vty (elab' env def) (close v (elab' (v:env) body))
elab' env (SLetLam p False (f, [(v, tv)], tf) def body) =
  Let p f (FunTy tv tf) (Lam p v tv (close v (elab' (v:env) def))) (close f (elab' (f:env) body))
elab' env (SLetLam p False (f, ((v, tv):xs), tf) def body) =
  Let p f (FunTy tv (FunTy (typeConstructor xs) tf)) (Lam p v tv (close v (elab' (v:env) (SLam p xs def)))) (close f (elab' (f:env) body))
elab' env (SLetLam p True (f, [(v, tv)], tf) def body) =
  Let p f (FunTy tv tf) (Fix p f (FunTy tv tf) v tv (close2 f v (elab' (v:f:env) def))) (close f (elab' (f:env) body))
elab' env (SLetLam p True (f, ((v, tv):xs), tf) def body) =
  Let p f (FunTy tv (FunTy (typeConstructor xs) tf)) (Fix p f (FunTy tv (FunTy (typeConstructor xs) tf)) v tv (close2 f v (elab' (v:f:env) (SLam p xs def)))) (close f (elab' (f:env) body))

typeConstructor :: [(Name, Ty)] -> Ty
typeConstructor [(v, ty)] = ty
typeConstructor ((v, ty):xs) = FunTy ty (typeConstructor xs)

elabDecl :: SDecl STerm -> Decl Term
elabDecl (SDecl i f def) = (Decl i f (elab def))
elabDecl (SDeclLam i False f xs tf def) = (Decl i f (elab def'))
  where def' = (SLam i xs def)
elabDecl (SDeclLam i True f ([(v, tv)]) tf def) = (Decl i f def')
  where def' = (Fix i f (FunTy tv tf) v tv (close2 f v (elab' (v:[f]) def)))
elabDecl (SDeclLam i True f ((v, tv):xs) tf def) = (Decl i f def')
  where def' = (Fix i f (FunTy tv (FunTy (typeConstructor xs) tf)) v tv (close2 f v (elab' (v:[f]) (SLam i xs def))))


-- TODO
-- - Hacer tipo para las declaraciones superficiales.
-- - Parcear declaraciones a declaraciones superficiales
-- - En elab construir Decl, rearmando el termino y sacando la azucar
