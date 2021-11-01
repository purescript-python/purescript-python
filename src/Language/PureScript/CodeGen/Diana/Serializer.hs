-- -- required rts:
-- 1. `zfsr32`, which implements zero_fill_shift_right for 32-bit integers
-- 2. `Error(msg, self) = Exception(msg)`
-- 3. `import_module` from importlib (>=Python 3.5)
{-# LANGUAGE UndecidableInstances #-}

module Language.PureScript.CodeGen.Diana.Serializer where

import Control.Applicative hiding (optional)
import Control.Arrow ((&&&))
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Language.PureScript.AST.SourcePos
import Language.PureScript.CodeGen.Diana.Common
import Language.PureScript.CodeGen.Diana.Eval
import Language.PureScript.CoreImp.AST (BinaryOperator (..), UnaryOperator (..))
import StringEscape (escape)
import Text.Printf (printf)

-- serialize python

optional :: Maybe (Doc a) -> Doc a
optional (Just a) = a
optional Nothing = pretty "None"

pattern Optional a <- (optional -> a)

instance EvalJS (State (M.Map String Int) (Doc a)) where
  none = return $ pretty "None"
  intLit i = return $ pretty i
  doubleLit f = return $ pretty f
  strLit s = return $  pretty $ escape s
  boolLit b = return $ pretty $ if b then "1" else "0"
  objLit xs = do
    let meach (field, o) = do
            { o <- o 
            ; return (pretty (escape field) <> pretty ":" <+> o)
            }
    xs <- mapM meach xs
    return $ align (encloseSep (pretty "{|") (pretty "|}") comma xs)
        
  arrayLit xs = do
      xs <- sequence xs
      return $ list xs
  unary op e = do
    e <- e
    return $ case op of
      Negate -> pretty "-" <> e
      Not -> pretty "not" <+> e
      BitwiseNot -> pretty "~" <> e
      Positive -> pretty "+" <> e
      New -> error "impossible"

  binary op l r = do -- actually this will not be called in ImPureScript
    l <- l
    r <- r
    return $ case op of
      Add -> l <+> pretty "+" <+> r
      Subtract -> l <+> pretty "-" <+> r
      Multiply -> l <+> pretty "*" <+> r
      Divide -> polyDiv <> tupled [l, r]
      Modulus -> l <+> pretty "%" <+> r
      EqualTo -> l <+> pretty "==" <+> r
      NotEqualTo -> l <+> pretty "!=" <+> r
      LessThan -> l <+> pretty "<" <+> r
      LessThanOrEqualTo -> l <+> pretty "<=" <+> r
      GreaterThan -> l <+> pretty ">" <+> r
      GreaterThanOrEqualTo -> l <+> pretty ">=" <+> r
      And -> l <+> pretty "and" <+> r
      Or -> l <+> pretty "or" <+> r
      BitwiseAnd -> l <+> pretty "&" <+> r
      BitwiseOr -> l <+> pretty "|" <+> r
      BitwiseXor -> l <+> pretty "^" <+> r
      ShiftLeft -> l <+> pretty "<<" <+> r
      ShiftRight -> l <+> pretty ">>" <+> r
      ZeroFillShiftRight -> zeroFillShiftRight <> tupled [l, r]

  getAttr a attr = do
    a <- a
    return $ a <> pretty "." <> pretty attr
  setAttr a attr v = do
    a <- a
    v <- v
    return $ a <> pretty "." <> pretty attr <+> pretty "=" <+> v
  getItem a i = do
    i <- i
    a <- a
    return $ a <> pretty ".[" <> i <> pretty "]"
  setItem a i v = do
    a <- a
    i <- i
    v <- v
    return $ a <> pretty ".[" <> i <> pretty "]" <+> pretty "=" <+> v
  func n' args body = do
    body <- body
    let n = case n' of
          Nothing -> T.pack ""
          Just (MustNorm n) -> n
    return $ vsep [pretty "fun" <+> pretty n <> tupled (map (pretty . forceNorm) args), align $ vsep [ body, pretty "end"]]

  app f args = do
    f <- f
    args <- sequence args
    return $ f <> tupled args
  new f args = do
    f <- f
    args <- sequence args
    return $ newObject <> tupled (f : args)
  block suite = do
    suite <- sequence suite 
    return $ indent 4 $ vsep suite
  var (Normal n) = return $ pretty n
  var This = return thisName
  var Import = return importName
  assign (MustNorm n) v = do
    v <- v
    return $  pretty n <+> pretty "=" <+> v
  intro (MustNorm n) Nothing = return $  pretty "var" <+> pretty n
  intro (MustNorm n) (Just it) = do
    it <- it
    return $ vsep [ pretty "var" <+> pretty n,  pretty n <+> pretty "=" <+>  it ]
  while cond body = do
    cond <- cond
    body <- body
    return $ vsep [
      pretty "while" <+> cond <+> pretty "do",
      align $ vsep[ body
                  , pretty "end"]]
  forRange (MustNorm n) low high body = do
    low <- low
    high <- high
    body <- body
    return $ vsep
      [ pretty "for" <+> pretty "n" <+> pretty "in" <+> ranger <> tupled [low, high] <+> pretty "do"
      , align $ vsep [body, pretty "end"]
      ]
  ite cond te Nothing = do
    cond <- cond
    te <- te
    return $ vsep [pretty "if" <+> cond <+> pretty "then", align $ vsep [ te, pretty "end"]]
  ite cond te (Just fe) = do
    cond <- cond
    te <- te
    fe <- fe
    return $ vsep [pretty "if" <+> cond <+> pretty "then", align $ vsep [te, pretty "else", indent (-4) fe, pretty "end"]]
  ret v = do
    v <- v
    return $ pretty "return" <+> v
  retNoRes = return $ pretty "return"
  throw v = do
    v <- v
    return $ pretty "raise" <+> v
  isa inst ty = do
    inst <- inst
    ty <- ty
    return $ inst <> pretty ".TAG" <> pretty "===" <> ty
  comment cs exp = exp
    
  located SourceLoc {line, col, filename} isStmt term = do
    m <- get
    let op | Just v <- M.lookup filename m  = return v
           | otherwise = do
              let i = M.size m
              put $ M.insert filename i m
              return i
    i <- op
    term <- term
    if isStmt then
      return $ pretty "__META" <+> pretty i
               <> pretty ":" <> pretty (toInteger line)
               <> pretty ":" <> pretty (toInteger col)
               <+> pretty "do" <+> term
    else
      return $ pretty "__META" <+> pretty i
               <> pretty ":" <> pretty (toInteger line)
               <> pretty ":" <> pretty (toInteger col)
               <+> pretty "in" <+> term


runDoc :: State (M.Map String Int) (Doc a) -> Doc a
runDoc m =
    let (a, s) = runState m M.empty
    in
    vsep [ pretty "__SETMETA" <+> pretty i <+> pretty s  | (s, i) <-  M.toList s]
    <+> hardline <+> a
