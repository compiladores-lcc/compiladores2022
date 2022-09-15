module CEK where
import Lang
import MonadFD4
import Common (Pos)
import Subst
import Eval (semOp)

type Env = [Val]
data Clos = ClosFun (Pos, Ty) Env Name Ty TTerm | ClosFix (Pos, Ty) Env Name Ty Name Ty TTerm
  deriving (Show)
data Val = N (Pos, Ty) Const | Closure Clos
  deriving Show
data Frame
  = Fun {env :: Env, arg :: TTerm}
  | Arg {fun :: Clos}
  | IfzCond {env :: Env, ifzThen :: TTerm, ifzElse :: TTerm}
  | BinOpFst {env :: Env, binOp :: BinaryOp, binOpSnd :: TTerm}
  | BinOpSnd {binOp :: BinaryOp, binOpFst :: Val}
  | PrintFr {printString :: String}
  | LetDef {env :: Env, letBody :: TTerm}
type Kont = [Frame]

search :: MonadFD4 m => TTerm -> Env -> Kont -> m Val
search (V _ (Bound n)) e k = destroy (e !! n) k
search (V (p, _) (Free s)) e k = failPosFD4 p "La máquina encontró una variable libre"
search (V (p, _) (Global s)) e k = do
  mt <- lookupDecl s
  case mt of
    Nothing -> failPosFD4 p ("No existe la variable global " ++ show s)
    Just tm -> search tm e k
search (Const i n) e k = destroy (N i n) k
search (Lam i s sty (Sc1 t)) e k = destroy (Closure (ClosFun i e s sty t)) k
search (App _ tm tm') e k = search tm e (Fun e tm' : k)
search (Print _ s tm) e k = search tm e (PrintFr s : k)
search (BinaryOp _ bo tm tm') e k = search tm e (BinOpFst e bo tm' : k)
search (Fix i f fty x xty (Sc2 t)) e k = destroy (Closure (ClosFix i e f fty x xty t)) k
search (IfZ _ tm thenT elseT) e k = search tm e (IfzCond e thenT elseT : k)
search (Let (p, _) s _ tm (Sc1 body)) e k = search tm e (LetDef e body : k)

destroy :: MonadFD4 m => Val -> Kont -> m Val
destroy c [] = return c
destroy (N _ c) ((Fun _ _) : _) = failFD4 (show c ++ " No es una función")
destroy (Closure c) ((Fun vals tm) : k) = search tm vals (Arg c : k)
destroy c ((Arg (ClosFun _ vals _ _ tm)) : k) = search tm (c : vals) k
destroy c ((Arg f@(ClosFix i vals _ _ _ _ tm)) : k) = search tm (c : Closure f : vals) k
destroy (N _ (CNat n)) ((IfzCond vals thenT elseT) : k)
 | n == 0 = search thenT vals k
 | otherwise = search elseT vals k
destroy (Closure _) ((IfzCond {}) : _) = failFD4 "Se encontró un ifz con una función como condición"
destroy c ((BinOpFst vals bo tm) : k) = search tm vals (BinOpSnd bo c : k)
destroy (N i (CNat n')) ((BinOpSnd bo (N _ (CNat n))) : k) = destroy (N i $ CNat $ semOp bo n n') k
destroy (N _ (CNat n)) ((BinOpSnd bo (Closure cl)) : k) = failFD4 "Operación binaria cuyo primer argumento es una función"
destroy (Closure cl) ((BinOpSnd bo val) : frs) = failFD4 "Operación binaria cuyo segundo argumento es una función"
destroy c ((PrintFr s) : k) = printFD4 (s ++ show c) >> destroy c k
destroy c ((LetDef vals body) : k) = search body (c : vals) k

toTTerm :: Val -> TTerm
toTTerm (N info c) = Const info c
toTTerm (Closure (ClosFun info vals s sty tm)) = substN (map toTTerm vals) $ Lam info s sty (Sc1 tm)
toTTerm (Closure (ClosFix info vals f fty x xty tm)) = substN (map toTTerm vals) $ Fix info f fty x xty (Sc2 tm)
