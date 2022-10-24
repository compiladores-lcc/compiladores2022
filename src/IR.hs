module IR where

import Lang

data Ir = IrVar Name
        | IrGlobal Name
        | IrCall Ir [Ir] IrTy
                        -- ^ Tipo de expr final
        | IrConst Const
        | IrPrint String Ir
        | IrBinaryOp BinaryOp Ir Ir 
        | IrLet Name IrTy Ir Ir
        | IrIfZ Ir Ir Ir
        | MkClosure Name [Ir]
        | IrAccess Ir IrTy Int
  deriving Show

data IrTy = IrInt
          | IrClo
          | IrFunTy
  deriving Show

data IrDecl =
    IrFun { irDeclName :: Name
          , irRetTy :: IrTy
          , irDeclArgs :: [(Name, IrTy)]
          , irDeclBody :: Ir
    }
  | IrVal { irDeclName :: Name
          , irDeclTy :: IrTy
          , irDeclDef :: Ir
          }
  deriving Show

newtype IrDecls = IrDecls { irDecls :: [IrDecl] }

{-
La siguiente instancia es sólo para debugging
-}
instance Show IrDecls where
  show (IrDecls decls) =
   concatMap (\d -> show d ++ "\n") decls
