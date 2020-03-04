-- -- required rts:
-- 1. `zfsr32`, which implements zero_fill_shift_right for 32-bit integers
-- 2. `Error(msg, self) = Exception(msg)`
-- 3. `import_module` from importlib (>=Python 3.5)
{-# LANGUAGE UndecidableInstances #-}
module Language.PureScript.CodeGen.Py.Serializer where

import Language.PureScript.CodeGen.Py.Common
import Language.PureScript.CodeGen.Py.Eval
import Language.PureScript.AST.SourcePos
import Language.PureScript.CoreImp.AST (UnaryOperator(..), BinaryOperator(..))
import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Text as T
import Text.Printf (printf)
import Control.Monad.State
import Control.Applicative hiding (optional)
import Control.Arrow ((&&&))
import Data.Maybe (fromJust)
import Data.Text.Prettyprint.Doc
import Topdown.Core


-- serialize python

data As a where
    AsCall  :: forall a. Topdown a => a -> As a
    AsBin   :: String -> As a
    AsCmp   :: String -> As a
    AsOther :: forall a. Topdown a => ((a, a) -> a) -> As a

py_inst_of :: forall a. Topdown a => a -> a -> a
py_inst_of l r =
    tfCons "call"
      [ tfCons "var" [tfStr "isinstance"]
      , l
      , r
      ]

py_attr_of :: forall a. Topdown a => a -> String -> a
py_attr_of subject attr =
  tfCons "getattr"
    [ subject
    , tfStr attr
    ]

applyAs :: forall a. Topdown a => As a -> a -> a -> a
applyAs a l r = case a of
    AsOther f -> f (l, r)
    AsCall f -> tfCons "call" [f, l, r]
    AsBin op -> tfCons "binop"
        [ l
        , py_attr_of (tfVar "BinOp") op
        , r
        ]
    AsCmp op -> tfCons "cmp"
        [ l
        , py_attr_of (tfVar "Compare") op
        , r
        ]

optional :: forall a. Topdown a => Maybe a -> a
optional (Just a) = a
optional Nothing = tfUnit

pattern Optional a <- (optional -> a)

instance Topdown a => EvalJS a where

    none = tfUnit
    intLit i = tfInt i
    doubleLit f = tfFloat f
    strLit s = tfStr s
    boolLit b = tfBool b
    objLit xs = tfCons "record" $ flip map xs $ \(field, o) -> tfCons "make_pair" [tfStr field, o]
    arrayLit xs = tfSeq xs
    unary op e =
        let
          op' :: String
          op'
            | Negate <- op = "NEGATIVE"
            | Not <- op = "NOT"
            | BitwiseNot <- op = "INVERT"
            | Positive <- op = "POSITIVE"
            | otherwise = error "impossible unary operator"
        in  tfCons "uop" [py_attr_of (tfVar "UOp") op', e]

    binary op l r =
        let
            is a = a == op
            op' :: As a
            op' | is Add       =  AsBin "ADD"
                | is Subtract  =  AsBin "SUBTRACT"
                | is Multiply  =  AsBin "MULTIPLY"
                | is Divide    =  AsOther $
                    \(l, r) ->
                        tfCons "ite"
                          [ py_inst_of l (tfCons "var" [tfStr "int"])
                          , applyAs (AsBin "FLOOR_DIVIDE") l r
                          , applyAs (AsBin "TRUE_DIVIDE")  l r
                          ]
                | is Modulus = AsBin "MODULO"
                | is EqualTo = AsCmp "EQ"
                | is NotEqualTo = AsCmp "NE"
                | is LessThan = AsCmp "LT"
                | is LessThanOrEqualTo = AsCmp "LE"
                | is GreaterThan = AsCmp "GT"
                | is GreaterThanOrEqualTo = AsCmp "GE"
                | is And = AsOther $
                    \(l, r) ->
                        tfCons "ite"
                          [  l
                          ,  r
                          , tfBool False
                          ]
                | is Or = AsOther $
                    \(l, r) ->
                        tfCons "ite"
                          [  l
                          ,  tfBool True
                          ,  r
                          ]
                | is BitwiseAnd = AsBin "AND"
                | is BitwiseOr = AsBin "OR"
                | is BitwiseXor = AsBin "XOR"
                | is ShiftLeft = AsBin "LSHIFT"
                | is ShiftRight = AsBin "RSHIFT"
                | is ZeroFillShiftRight = AsCall $ tfCons "var" [tfStr "zfsr32"]
        in   applyAs op' l r

    getAttr a attr = tfCons "get_attr" [a, tfStr attr]

    setAttr a attr v = tfCons "set_attr" [a, tfStr attr, v]
    getItem a i =  tfCons "get_item" [a, i]
    setItem a i v = tfCons "set_item" [a, i, v]
    func n' args body =
        let n = case n' of
                Nothing -> "None"
                Just (Unbox n) -> n
        in  tfCons "define"
                [ tfStr n
                ,  tfSeq (map (tfStr . fromJust . unbox) args)
                , body
                , tfSeq (map (const tfUnit) args)
                ]

    app f args = tfCons "call" (f:args)

    new f args = tfCons "new" (f:args)

    block xs   =  tfCons "block" xs

    var (Unbox n) = tfCons "var" [tfStr n]
    var This      = tfVar "this" -- TODO: correct mangling for this in ../Py.hs
    var Import    = tfCons "var" [tfStr "import_module"]
    assign (Unbox n) v = tfCons "assign" [tfStr n, v]
    intro (Unbox n) v  = tfCons "assign_star" [tfStr n, v]
    while cond body    = tfCons "loop" [cond, body]
    upRecord old new   = tfCons "lens" [old, new]

    forIn (Unbox n) seq body         = tfCons "for_in" [tfStr n, seq, body]
    forRange (Unbox n) low high body = tfCons "for_range" [tfStr n, low, high, body]
    ite cond te (Optional fe)        = tfCons "ite" [cond, te, fe]
    ret v       = tfCons "ret" [v]
    retNoRes    = tfCons "ret" [tfUnit]
    throw v     = tfCons "throw" [v]
    isa inst ty = tfCons "isa" [inst, ty]
    comment cs exp = tfCons "document" [tfStr (unlines (map (T.unpack) cs)), exp]
    located SourceLoc {line, col, filename} term =
        tfCons "metadata" [tfInt (toInteger line), tfInt (toInteger col), tfStr filename, term]
