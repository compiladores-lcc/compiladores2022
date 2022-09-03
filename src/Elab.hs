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
    then V p (Free v)
    else V p (Global v)

elab' _ (SConst p c) = Const p c
elab' env (SLam p [] t) = error "Hay un lambda sin argumentos wtf"
elab' env (SLam p [(v,ty)] t) = Lam p v ty (close v (elab' (v:env) t))
elab' env (SLam p ((v, ty) : args) t) = Lam p v ty (close v (elab' (v : env) (SLam p args t)))
elab' env (SFix p _ [] t) = error "Hay un fix sin argumentos wtf"
elab' env (SFix p (f,fty) [(x,xty)] t) = Fix p f fty x xty (close2 f x (elab' (x : f : env) t))
elab' env (SFix p (f, fty) ((x, xty) : args) t) = Fix p f fty x xty (close2 f x (elab' (x : f : env) (SLam p args t)))
elab' env (SIfZ p c t e) = IfZ p (elab' env c) (elab' env t) (elab' env e)
-- Operadores binarios
elab' env (SBinaryOp i o t u) = BinaryOp i o (elab' env t) (elab' env u)
-- Operador Print
elab' env (SPrint i str Nothing) = elab' env (SLam i [("printV", NatTy)] (SPrint i str (Just (SV i "printV")))) -- No hay problema con hardcodear un nombre para esa lambda? para mi no pero mauro si estas leyendo esto pone otro comentario abajo diciendo si funca o no
elab' env (SPrint i str (Just t)) = Print i str (elab' env t)
-- Aplicaciones generales
elab' env (SApp p h a) = App p (elab' env h) (elab' env a)
elab' env (SLet p _ [(v,vty)] def body) =
  Let p v vty (elab' env def) (close v (elab' (v : env) body))
elab' env (SLet p False ((fv, fty) : args) def body) =
  Let p fv (foldr1 FunTy (map snd args ++ [fty])) (elab' (map fst args ++ env) (SLam p args def)) (close fv $ elab' (fv : env) body)
elab' env (SLet p True ((fv, fty) : args) def body) =
  let fty' = foldr1 FunTy (map snd args ++ [fty])
   in Let p fv fty' (elab' (map fst args ++ env) (SFix p (fv, fty') args def)) (close fv $ elab' (fv : env) body) -- PARECIDO AL LAM
elab' _ _ = error "Error extraño"

elabDecl :: Decl STerm -> Decl Term
elabDecl = fmap elab
