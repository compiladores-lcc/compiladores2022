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

module Elab (elab, elabDecl, elabType, deElab) where

import Lang
import Subst
import MonadFD4 (MonadFD4, lookupTypeSyn, failPosFD4)
import Common (Pos(NoPos))

-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado.
elab :: MonadFD4 m => STerm -> m Term
elab = elab' []

elab' :: MonadFD4 m => [Name] -> STerm -> m Term
elab' env (SV p v) =
  -- Tenemos que ver si la variable es Global o es un nombre local
  -- En env llevamos la lista de nombres locales.
  return $ if v `elem` env
    then V p (Free v)
    else V p (Global v)
elab' _ (SConst p c) = return $ Const p c
elab' env (SLam p [] t) = error "Hay un lambda sin argumentos wtf"
elab' env (SLam p [(v,ty)] t) = Lam p v <$> elabType ty <*> (close v <$> elab' (v:env) t)
elab' env (SLam p ((v, ty) : args) t) = Lam p v <$> elabType ty <*> (close v <$> elab' (v : env) (SLam p args t))
elab' env (SFix p _ [] t) = error "Hay un fix sin argumentos wtf"
elab' env (SFix p (f,fty) [(x,xty)] t) = do
  fty' <- elabType fty
  xty' <- elabType xty
  body <- elab' (x : f : env) t
  return $ Fix p f fty' x xty' (close2 f x body)
elab' env (SFix p (f, fty) ((x, xty) : args) t) = do
  fty' <- elabType fty
  xty' <- elabType xty
  body <- elab' (x : f : env) (SLam p args t)
  return $ Fix p f fty' x xty' (close2 f x body)
elab' env (SIfZ p c t e) = IfZ p <$> elab' env c <*> elab' env t <*> elab' env e
-- Operadores binarios
elab' env (SBinaryOp i o t u) = BinaryOp i o <$> elab' env t <*> elab' env u
-- Operador Print
elab' env (SPrint i str Nothing) = elab' env (SLam i [("printV", NatSTy i)] (SPrint i str (Just (SV i "printV"))))
elab' env (SPrint i str (Just t)) = Print i str <$> elab' env t
-- Aplicaciones generales
elab' env (SApp p h a) = App p <$> elab' env h <*> elab' env a
elab' env (SLet p _ [(v,vty)] def body) =
  Let p v <$> elabType vty <*> elab' env def <*> (close v <$> elab' (v : env) body)
elab' env (SLet p False ((fv, fty) : args) def body) =
  Let p fv <$> elabType (foldr1 (FunSTy p) (map snd args ++ [fty])) <*> elab' (map fst args ++ env) (SLam p args def) <*> (close fv <$> elab' (fv : env) body)
elab' env (SLet p True ((fv, fty) : args) def body) = do
  let fty' = foldr1 (FunSTy p) (map snd args ++ [fty])
  Let p fv <$> elabType fty' <*> elab' (map fst args ++ env) (SFix p (fv, fty') args def) <*> (close fv <$> elab' (fv : env) body)
elab' _ _ = error "Error extraño"

elabType :: MonadFD4 m => SType -> m Ty
elabType (NatSTy _) = return NatTy
elabType (FunSTy _ t t') = FunTy <$> elabType t <*> elabType t'
elabType (SynSTy i t) = do
  a <- lookupTypeSyn t
  case a of
    Nothing -> failPosFD4 i ("No se encontró el sinónimo de tipo " ++ show t)
    Just ty -> return ty

elabDecl :: MonadFD4 m => SDecl -> m (Decl Term)
elabDecl (LetDecl pos s st) = Decl pos s <$> elab st

deElab :: Ty -> SType
deElab NatTy = NatSTy NoPos
deElab (FunTy ty ty') = FunSTy NoPos (deElab ty) (deElab ty')
