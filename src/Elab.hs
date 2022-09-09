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

module Elab (elab, elabDecl, elabType, elabTypeWithTag, deElabType, deElabDecl) where

import Lang
import Subst
import MonadFD4 (MonadFD4, lookupTypeSyn, failPosFD4, failFD4)
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
elab' env (SLam p [] t) = failFD4 "Intentando elaborar un lambda sin argumentos"
elab' env (SLam p [(v,ty)] t) = Lam p v <$> elabType ty <*> (close v <$> elab' (v:env) t)
elab' env (SLam p ((v, ty) : args) t) = Lam p v <$> elabType ty <*> (close v <$> elab' (v : env) (SLam p args t))
elab' env (SFix p _ [] t) = failFD4 "Intentando elaborar un fix sin argumentos"
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
elab' _ _ = failFD4 "Error extraño durante elaboración"

elabType :: MonadFD4 m => SType -> m Ty
elabType (NatSTy _) = return $ NatTy Nothing
elabType (FunSTy _ t t') = FunTy Nothing <$> elabType t <*> elabType t'
elabType (SynSTy i t) = do
  a <- lookupTypeSyn t
  case a of
    Nothing -> failPosFD4 i ("No se encontró el sinónimo de tipo " ++ show t)
    Just ty -> return ty

elabTypeWithTag :: MonadFD4 m => Name -> SType -> m Ty
elabTypeWithTag n x = do
  t <- elabType x
  case t of
    NatTy _ -> return $ NatTy $ Just n
    FunTy _ ty ty' -> return $ FunTy (Just n) ty ty'

elabDecl :: MonadFD4 m => SDecl -> m (Decl Term)
elabDecl (LetDecl pos _ [] _) = failPosFD4 pos "Declaración sin binders"
elabDecl (LetDecl pos _ [(f, t)] def) = Decl pos f <$> elabType t <*> elab def
elabDecl (LetDecl pos False ((f, t):args) def) = Decl pos f <$> elabType (foldr1 (FunSTy pos) (map snd args ++ [t])) <*> elab (SLam pos args def)
elabDecl (LetDecl pos True ((f, t):args) def) = Decl pos f <$> elabType (foldr1 (FunSTy pos) (map snd args ++ [t])) <*> elab (SFix pos (f, foldr1 (FunSTy pos) (t:map snd args)) args def)
elabDecl (TypeDecl pos s _) = failPosFD4 pos ("Se intentó elaborar una declaración de sinónimo de tipo: " ++ s)

deElabDecl :: Decl STerm -> SDecl
deElabDecl (Decl pos name ty (SLam _ args body)) = LetDecl pos False ((name, deElabType ty):args) body
deElabDecl (Decl pos name ty (SFix _ (f, fty) args body))
  | name == f = LetDecl pos True ((f, deElabType ty):args) body
deElabDecl (Decl pos name ty def) = LetDecl pos False [(name, deElabType ty)] def

deElabType :: Ty -> SType
deElabType (NatTy Nothing) = NatSTy NoPos
deElabType (NatTy (Just n)) = SynSTy NoPos n
deElabType (FunTy Nothing ty ty') = FunSTy NoPos (deElabType ty) (deElabType ty')
deElabType (FunTy (Just n) _ _) = SynSTy NoPos n
