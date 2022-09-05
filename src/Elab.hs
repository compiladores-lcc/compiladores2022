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

module Elab ( elab, elabDecl, elabDeclType, elabTermType ) where

import Lang
import Subst
import PPrint ( freshen, dbnd )
import MonadFD4
import Common ( Pos )
-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado. 
elab :: MonadFD4 m => STermTy -> m Term
elab t = do return (elab' [] t)

elab' :: [Name] -> STermTy -> Term
elab' env (SV p v) =
  -- Tenemos que ver si la variable es Global o es un nombre local
  -- En env llevamos la lista de nombres locales.
  if v `elem` env 
    then  V p (Free v)
    else V p (Global v)

elab' _ (SConst p c) = Const p c
elab' env (SLam p param t) = Lam p v ty (close v (elab' (v:env) (slam p ys t)))
  where ((v, ty):xs) = dbnd param
        ys = implode xs
elab' env (SFix p (f,fty) (x,xty) xs t) = Fix p f fty x xty (close2 f x (elab' (x:f:env) (slam p xs t)))
elab' env (SIfZ p c t e)         = IfZ p (elab' env c) (elab' env t) (elab' env e)
-- Operadores binarios
elab' env (SBinaryOp i o t u) = BinaryOp i o (elab' env t) (elab' env u)
-- Operador Print
elab' env (SPrint i str (Just t)) = Print i str (elab' env t)
elab' env (SPrint i str Nothing) = let v = (freshen env "x") in Lam i v NatTy (close v (elab' (v:env) (SPrint i str (Just (SV i v)))))
-- Aplicaciones generales
elab' env (SApp p h a) = App p (elab' env h) (elab' env a)
elab' env (SLet p (v,vty) def body) =
  Let p v vty (elab' env def) (close v (elab' (v:env) body))
elab' env (SLetLam p False (f, param, tf) def body) =
  Let p f (mkArr (tv:(map snd xs)) tf) (Lam p v tv (close v (elab' (v:env) (slam p ys def)))) (close f (elab' (f:env) body))
  where ((v, tv):xs) = dbnd param
        ys = implode xs
elab' env (SLetLam p True (f, param, tf) def body) =
  Let p f (mkArr (tv:(map snd xs)) tf) (Fix p f (mkArr (tv:(map snd xs)) tf) v tv (close2 f v (elab' (v:f:env) (slam p ys def)))) (close f (elab' (f:env) body))
  where ((v, tv):xs) = dbnd param
        ys = implode xs


implode :: [(Name, Ty)] -> [([Name], Ty)]
implode xs = map (\(x,y) -> ([x], y)) xs



slam :: Pos -> [([Name], Ty)] -> STermTy -> STermTy
slam p [] def = def
slam p xs def = (SLam p xs def)

mkArr :: [Ty] -> Ty -> Ty
mkArr [] cod = cod
mkArr (t:doms) cod = FunTy t (mkArr doms cod)

elabDecl :: MonadFD4 m => SDeclTy STermTy -> m (Maybe (Decl Term))
elabDecl (SDecl i f def) = do return (Just (Decl i f (elab' [] def)))
elabDecl (SDeclLam i False f xs tf def) = do return (Just (Decl i f (elab' [] def')))
  where def' = (SLam i xs def)
elabDecl (SDeclLam i True f param tf def) = do return (Just (Decl i f def'))
  where def' = (elab' [] (SFix i (f, mkArr (map snd ((v, tv):xs)) tf) (v, tv) ys def))
        ((v, tv):xs) = dbnd param
        ys = implode xs
elabDecl (SDeclType i n t) = do ty <- lookupTyDef n
                                case ty of
                                  Nothing -> do addTypeDef (n, t)
                                                return Nothing 
                                  Just _ -> failPosFD4 i $ n ++" ya está declarado"
                                

typeResolver :: MonadFD4 m => STy -> Pos -> m (Ty)
typeResolver SNatTy p = return NatTy
typeResolver (SNameTy n) p = do 
                              ty <- lookupTyDef n
                              case ty of
                                Nothing -> failPosFD4 p $ n ++" no está declarado"
                                Just t -> return t
typeResolver (SFunTy t1 t2) p = do
                                  t1' <- typeResolver t1 p
                                  t2' <- typeResolver t2 p
                                  return (FunTy t1' t2')

styBinder :: MonadFD4 m => Pos -> ([a], STy) -> m ([a], Ty)
styBinder i (v, ty) =
  do ty' <- typeResolver ty i
     return (v, ty')
  
listElemTypeResolver :: MonadFD4 m => Pos -> [([a], STy)] -> m [([a], Ty)]
listElemTypeResolver i bs = mapM (styBinder i) bs

-- Se resuelven los tipos superficiales.
elabTermType :: MonadFD4 m => STerm -> m (STermTy)
elabTermType (SV i v) = return (SV i v)
elabTermType (SConst i c) = return (SConst i c)
elabTermType (SLam i xs t) = do t' <- elabTermType t
                                xs' <- listElemTypeResolver i xs
                                return (SLam i xs' t')
elabTermType (SApp i t1 t2) = do t1' <- elabTermType t1
                                 t2' <- elabTermType t2
                                 return (SApp i t1' t2')
elabTermType (SFix i (v, tv) (v2, tv2) xs t) = do t1' <- typeResolver tv i
                                                  t2' <- typeResolver tv2 i
                                                  e1' <- elabTermType t
                                                  xs' <- listElemTypeResolver i xs
                                                  return (SFix i (v, t1') (v2, t2') xs' e1')
elabTermType (SPrint i str (Just t)) = do t' <- elabTermType t
                                          return (SPrint i str (Just t'))
elabTermType (SPrint i s Nothing) = return (SPrint i s Nothing)
elabTermType (SLet p (v, vty) def body) = do t1' <- typeResolver vty p
                                             e1' <- elabTermType def
                                             e2' <- elabTermType body
                                             return (SLet p (v, t1') e1' e2')
elabTermType (SLetLam p b (f, xs, tf) def body) = do xs' <- listElemTypeResolver p xs
                                                     t1' <- typeResolver tf p
                                                     e1' <- elabTermType def
                                                     e2' <- elabTermType body
                                                     return (SLetLam p b (f, xs', t1') e1' e2')
elabTermType (SBinaryOp i b t1 t2) = do t1' <- elabTermType t1
                                        t2' <- elabTermType t2
                                        return (SBinaryOp i b t1' t2')
elabTermType (SIfZ i t1 t2 t3) = do t1' <- elabTermType t1
                                    t2' <- elabTermType t2
                                    t3' <- elabTermType t3
                                    return (SIfZ i t1' t2' t3')

elabDeclType :: MonadFD4 m => SDecl STerm -> m (SDeclTy STermTy)
elabDeclType (SDecl i f def) = do e1' <- elabTermType def
                                  return (SDecl i f e1')
elabDeclType (SDeclLam i b f xs tf def) = do t1' <- typeResolver tf i
                                             e1' <- elabTermType def
                                             xs' <- listElemTypeResolver i xs
                                             return (SDeclLam i b f xs' t1' e1')
elabDeclType (SDeclType i n t) = do t1' <- typeResolver t i 
                                    return (SDeclType i n t1')
