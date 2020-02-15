module Language.PureScript.CodeGen.Py.Printer where

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


data Py


constOf :: String -> Doc Py
constOf s = viaShow $ (printf "const(%s)" s :: String)

just :: String -> Doc Py
just = viaShow
-- -- required rts:
-- -- 1. `zfsr64`, which implements zero_fill_shift_right for 64-bit integers
-- -- 2. `Error(msg, self) = Exception(msg)`, 



data As
    = AsCall String
    | AsUn String
    | AsBin String
    | AsCmp String
    | AsOther ((Doc Py, Doc Py) -> Doc Py)

py_inst_of :: Doc Py -> Doc Py -> Doc Py
py_inst_of l r =
    just "call" <> tupled
      [ just "var('isinstance')"
      , l
      , r
      ]
applyAs :: As -> Doc Py -> Doc Py -> Doc Py
applyAs a l r = case a of
    AsOther f -> f (l, r)
    AsCall f -> just "call" <> tupled [l, r]
    AsBin op -> just "binop" <> tupled
            [ l
            , just "BinOp." <> just op
            , r
            ]
    AsCmp op -> just "cmp" <> tupled
            [ l
            , just "Compare." <> just op
            , r
            ]

optional :: Maybe (Doc Py) -> Doc Py
optional (Just a) = a
optional Nothing = just "None"

pattern Optional a <- (optional -> a)

instance EvalJS (Doc Py) where

    none = just "None"
    intLit i = just $ printf "%d" i
    doubleLit f = just $ printf "%f" f
    strLit s = just $ printf "%s" s
    boolLit b = just $ printf "%s" $ show b
    objLit xs =
        just "record" <> tupled (map (\(a, b) -> tupled [just a, b]) xs)
    
    arrayLit xs = just "mktuple" <> tupled xs
    
    unary op e =
        let 
          op' :: String
          op'
            | Negate <- op = "UOp.NEGATIVE"
            | Not <- op = "UOp.NOT"
            | BitwiseNot <- op = "UOp.INVERT"
            | Positive <- op = "UOp.POSITIVE"
            | otherwise = error "impossible unary operator"
        in  just "uop" <>  tupled [just op', e]

    binary op l r =
        let
            is a = a == op
            
            op' :: As
            op' | is Add       =  AsBin "ADD"
                | is Subtract  =  AsBin "SUBTRACT"
                | is Multiply  =  AsBin "MULTIPLY"
                | is Divide    =  AsOther $
                    \(l, r) ->
                        just "ite" <> tupled
                          [ py_inst_of l (just "var('int')")
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
                        just "ite" <> tupled
                          [  l
                          ,  r
                          ,  just "False"
                          ]
                | is Or = AsOther $
                    \(l, r) ->
                        just "ite" <> tupled
                          [  l
                          ,  just "True"
                          ,  r
                          ]
                | is BitwiseAnd = AsBin "AND"
                | is BitwiseOr = AsBin "OR"
                | is BitwiseXor = AsBin "XOR"
                | is ShiftLeft = AsBin "LSHIFT"
                | is ShiftRight = AsBin "RSHIFT"
                | is ZeroFillShiftRight = AsCall "var('zfsr64')"
        in   applyAs op' l r
    
    getAttr a attr =
        just "get_attr" <> tupled [a, just attr]
    
    setAttr a attr v =
        just "set_attr" <> tupled [a, just attr, v]
    
    getItem a i =
        just "get_item" <> tupled [a, i]
    
    setItem a i v =
        just "set_item" <> tupled [a, i, v]
    
    func n' args body =
        let n = case n' of
                Nothing -> "None"
                Just (Unbox n) -> n
        in  just "define" <> tupled [just n, list (map (just . fromJust . unbox) args), body]
    
    app f args = just "call_or_specialize" <> tupled (f:args)

    new f args = just "new" <> tupled (f:args)

    block xs = just "block" <> just "(" <+> vsep xs <+> just ")"

    var (Unbox n) = just "var" <> tupled [just n]
    var This      = just "this"
    var Import    = just "var('load_module')"
    assign (Unbox n) v = just "assign" <> tupled [just n, v]
    while cond body    = just "loop"   <> tupled [cond, body]
    upRecord old new   = just "lens"   <> tupled [old, new]

    forIn (Unbox n) seq body         = just "for_in" <> tupled [just n, seq, body]
    forRange (Unbox n) low high body = just "for_range" <> tupled [just n, low, high, body]
    ite cond te (Optional fe)        = just "ite" <> tupled [cond, te, fe]
    ret v       = just "ret" <> tupled [v]
    retNoRes    = just "ret(None)"
    throw v     = just "throw" <> tupled [v]
    isa inst ty = just "isa"<> tupled [inst, ty]
    comment cs exp = just "document" <> tupled [vsep (map just cs), exp]
    located SourceLoc {line, col, filename} term =
        just "metadata" <> tupled [pretty line, pretty col, just filename, term]
