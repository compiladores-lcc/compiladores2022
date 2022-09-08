{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE TupleSections #-}
{-|
Module      : Parse
Description : Define un parser de términos FD40 a términos fully named.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Parse (tm, Parse.parse, decl, runP, P, program, declOrTm) where

import Prelude hiding ( const )
import Lang hiding (getPos)
import Common
import Text.Parsec hiding (runP,parse)
--import Data.Char ( isNumber, ord )
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language --( GenLanguageDef(..), emptyDef )
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Expr (Operator, Assoc)
import Control.Monad.Identity (Identity)
import Control.Monad (join)

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser langDef

langDef :: LanguageDef u
langDef = emptyDef {
         commentLine    = "#",
         reservedNames = ["let", "rec","fun", "fix", "then", "else","in",
                           "ifz", "print","Nat","type"],
         reservedOpNames = ["->",":","=","+","-"],
         commentStart = "{-",
         commentEnd = "-}"
        }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer
natural = Tok.natural lexer

stringLiteral :: P String
stringLiteral = Tok.stringLiteral lexer

parens :: P a -> P a
parens = Tok.parens lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

tyIdentifier :: P String
tyIdentifier = Tok.lexeme lexer $ do
  c  <- upper
  cs <- many (identLetter langDef)
  return (c:cs)

-----------------------
-- Parsers
-----------------------

num :: P Int
num = fromInteger <$> natural

var :: P Name
var = identifier

getPos :: P Pos
getPos = do pos <- getPosition
            return $ Pos (sourceLine pos) (sourceColumn pos)

tyatom :: P SType
tyatom = (reserved "Nat" >> NatSTy <$> getPos)
         <|> (SynSTy <$> getPos <*> var)
         <|> parens typeP

typeP :: P SType
typeP = try (do
          i <- getPos
          x <- tyatom
          reservedOp "->"
          y <- typeP
          return (FunSTy i x y))
      <|> tyatom

const :: P Const
const = CNat <$> num

printOp :: P STerm
printOp = do
  i <- getPos
  reserved "print"
  str <- option "" stringLiteral
  a <- optionMaybe atom
  return (SPrint i str a)

binary :: String -> BinaryOp -> Assoc -> Operator String () Identity STerm
binary s f = Ex.Infix (reservedOp s >> return (SBinaryOp NoPos f))

table :: [[Operator String () Identity STerm]]
table = [[binary "+" Add Ex.AssocLeft,
          binary "-" Sub Ex.AssocLeft]]

expr :: P STerm
expr = Ex.buildExpressionParser table tm

atom :: P STerm
atom =     (flip SConst <$> const <*> getPos)
       <|> flip SV <$> var <*> getPos
       <|> parens expr
       <|> printOp

-- parsea un par (variable : tipo)
binding :: P (Name, SType)
binding = do v <- var
             reservedOp ":"
             ty <- typeP
             return (v, ty)

multiBinding :: P [(Name, SType)]
multiBinding = do args <- many1 var 
                  t <- reservedOp ":" >> typeP
                  return (map (, t) args)

lam :: P STerm
lam = do i <- getPos
         reserved "fun"
         args <- many1 $ parens multiBinding
         reservedOp "->"
         t <- expr
         return (SLam i (join args) t)

-- Nota el parser app también parsea un solo atom.
app :: P STerm
app = do i <- getPos
         f <- atom
         args <- many atom
         return (foldl (SApp i) f args)

ifz :: P STerm
ifz = do i <- getPos
         reserved "ifz"
         c <- expr
         reserved "then"
         t <- expr
         reserved "else"
         e <- expr
         return (SIfZ i c t e)

fix :: P STerm
fix = do i <- getPos
         reserved "fix"
         (f, fty) <- parens binding
         args <- many1 $ parens multiBinding
         reservedOp "->"
         t <- expr
         return (SFix i (f,fty) (join args) t)

binders :: P [(Name, SType)]
binders = parens binders' <|> binders' where
  binders' = do
    f <- var
    args <- many $ parens multiBinding
    t <- reservedOp ":" >> typeP
    return ((f,t):join args)

letexp :: P STerm
letexp = do
  i <- getPos
  reserved "let"
  isR <- (reserved "rec" >> return True) <|> return False
  bindings <- binders
  reservedOp "="  
  def <- expr
  reserved "in"
  body <- expr
  return (SLet i isR bindings def body)

-- | Parser de términos
tm :: P STerm
tm = app <|> lam <|> ifz <|> printOp <|> fix <|> letexp

-- | Parser de declaraciones
decl :: P SDecl
decl = letDecl <|> typeSyn

typeSyn :: P SDecl
typeSyn = do
  i <- getPos
  reserved "type"
  t <- tyIdentifier
  reservedOp "="
  def <- typeP
  return $ TypeDecl i t def

letDecl :: P SDecl
letDecl = do 
     i <- getPos
     reserved "let"
     isR <- (reserved "rec" >> return True) <|> return False
     args <- binders
     reservedOp "="
     def <- expr
     notFollowedBy (reserved "in")
     return (LetDecl i isR args def)

-- | Parser de programas (listas de declaraciones) 
program :: P [SDecl]
program = many decl

-- | Parsea una declaración a un término
-- Útil para las sesiones interactivas
declOrTm :: P (Either SDecl STerm)
declOrTm =  try (Left <$> decl) <|> (Right <$> expr)

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> STerm
parse s = case runP expr s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s)
