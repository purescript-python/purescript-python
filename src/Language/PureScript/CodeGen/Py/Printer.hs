-- -- required rts:
-- 1. `zfsr64`, which implements zero_fill_shift_right for 64-bit integers
-- 2. `Error(msg, self) = Exception(msg)`
-- 3. `import_module` from importlib (>=Python 3.5)
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

just :: String -> Doc Py
just = pretty


data As
    = AsCall String
    | AsUn String
    | AsBin String
    | AsCmp String
    | AsOther ((Doc Py, Doc Py) -> Doc Py)

py_inst_of :: Doc Py -> Doc Py -> Doc Py
py_inst_of l r =
    just "call" <> align_tupled
      [ just "var('isinstance')"
      , l
      , r
      ]

applyAs :: As -> Doc Py -> Doc Py -> Doc Py
applyAs a l r = case a of
    AsOther f -> f (l, r)
    AsCall f -> just "call" <> align_tupled [l, r]
    AsBin op -> just "binop" <> align_tupled
            [ l
            , just ("BinOp." <> op)
            , r
            ]
    AsCmp op -> just "cmp" <> align_tupled
            [ l
            , just ("Compare." <> op)
            , r
            ]

optional :: Maybe (Doc Py) -> Doc Py
optional (Just a) = a
optional Nothing = just "None"

pattern Optional a <- (optional -> a)

align_tupled = align . tupled

instance EvalJS (Doc Py) where

    none = just "None"
    intLit i = just $ printf "%d" i
    doubleLit f = just $ printf "%f" f
    strLit s = just $ printf "%s" (escape s)
    boolLit b = just $ printf "%s" $ show b
    objLit xs =
        just "record" <> align_tupled (map (\(a, b) -> align_tupled [just $ escape a, b]) xs)

    arrayLit xs = just "mktuple" <> align_tupled xs

    unary op e =
        let
          op' :: String
          op'
            | Negate <- op = "UOp.NEGATIVE"
            | Not <- op = "UOp.NOT"
            | BitwiseNot <- op = "UOp.INVERT"
            | Positive <- op = "UOp.POSITIVE"
            | otherwise = error "impossible unary operator"
        in  just "uop" <>  align_tupled [just op', e]

    binary op l r =
        let
            is a = a == op

            op' :: As
            op' | is Add       =  AsBin "ADD"
                | is Subtract  =  AsBin "SUBTRACT"
                | is Multiply  =  AsBin "MULTIPLY"
                | is Divide    =  AsOther $
                    \(l, r) ->
                        just "ite" <> align_tupled
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
                        just "ite" <> align_tupled
                          [  l
                          ,  r
                          ,  just "False"
                          ]
                | is Or = AsOther $
                    \(l, r) ->
                        just "ite" <> align_tupled
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
        just "get_attr" <> align_tupled [a, just $ escape attr]

    setAttr a attr v =
        just "set_attr" <> align_tupled [a, just $ escape attr, v]

    getItem a i =
        just "get_item" <> align_tupled [a, i]

    setItem a i v =
        just "set_item" <> align_tupled [a, i, v]

    func n' args body =
        let n = case n' of
                Nothing -> "None"
                Just (Unbox n) -> n
        in  just "define" <> align_tupled
                [ just n
                ,  list (map (just . fromJust . unbox) args)
                , body
                ]

    app f args = just "call" <> align_tupled (f:args)

    new f args = just "new" <> align_tupled (f:args)

    block xs   =    just "block" <> align_tupled xs

    var (Unbox n) = just "var" <> align_tupled [just n]
    var This      = just "this"
    var Import    = just "var('import_module')"
    assign (Unbox n) v = just "assign" <> align_tupled [just n, v]
    while cond body    = just "loop"   <> align_tupled [cond, body]
    upRecord old new   = just "lens"   <> align_tupled [old, new]

    forIn (Unbox n) seq body         = just "for_in" <> align_tupled [just n, seq, body]
    forRange (Unbox n) low high body = just "for_range" <> align_tupled [just n, low, high, body]
    ite cond te (Optional fe)        = just "ite" <> align_tupled [cond, te, fe]
    ret v       = just "ret" <> align_tupled [v]
    retNoRes    = just "ret(None)"
    throw v     = just "throw" <> align_tupled [v]
    isa inst ty = just "isa"<> align_tupled [inst, ty]
    comment cs exp = just "document" <> align_tupled [vsep (map (just . escape . T.unpack) cs), exp]
    located SourceLoc {line, col, filename} term =
        let fullpath = just "joinpath" <> tupled [just "project_path", just $ escape filename]
        in just "metadata" <> align_tupled [pretty line, pretty col, fullpath , term]

bindSExpr :: String -> Doc Py -> Doc Py
bindSExpr name sexpr = just name <+> just "=" <+> sexpr
