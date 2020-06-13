{-# LANGUAGE
      OverloadedStrings
    , PatternSynonyms
#-}

module Language.PureScript.CodeGen.Py.Eval where


import System.FilePath ((</>))

import Language.PureScript.AST.SourcePos
import Language.PureScript.CoreImp.AST
import Language.PureScript.Comments (Comment(..))
import Language.PureScript.CodeGen.Py.Common (BoxedName, SourceLoc(..), mkName)
import Language.PureScript.PSString (PSString, decodeStringWithReplacement)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map  as M
import Text.Printf (printf)
import Control.Monad.State
import Control.Applicative
import Control.Arrow ((&&&))

pattern Attr ps <- ArrayLiteral _ [StringLiteral _ ps]

class EvalJS repr where
    none       :: repr
    intLit     :: Integer -> repr
    doubleLit  :: Double -> repr
    strLit     :: String -> repr
    boolLit    :: Bool -> repr
    objLit     :: [(String, repr)] -> repr
    arrayLit   :: [repr] -> repr

    unary      :: UnaryOperator -> repr -> repr
    binary     :: BinaryOperator -> repr -> repr -> repr

    getAttr    :: repr -> String -> repr
    setAttr    :: repr -> String -> repr -> repr

    getItem    :: repr -> repr -> repr
    setItem    :: repr -> repr -> repr -> repr

    func    :: Maybe BoxedName -> [BoxedName] -> repr -> repr
    app     :: repr -> [repr] -> repr
    new     :: repr -> [repr] -> repr -- create class
    block   :: [repr] -> repr
    var     :: BoxedName -> repr

    intro   :: BoxedName -> repr -> repr
    assign  :: BoxedName -> repr -> repr

    while    :: repr -> repr -> repr
    upRecord :: repr -> repr -> repr
    forRange :: BoxedName -> repr -> repr -> repr -> repr
    -- [forIn]
    --  used only for iterating records:
    --    github.com/purescript/purescript@master
    --    src/Language/PureScript/CodeGen/JS.hs
    forIn   :: BoxedName -> repr -> repr -> repr

    ite :: repr -> repr -> Maybe repr -> repr
    ret :: repr -> repr
    retNoRes :: repr
    throw :: repr -> repr
    isa :: repr -> repr -> repr
    comment :: [Text] -> repr -> repr
    located :: SourceLoc -> repr -> repr

recurIndex :: (AST -> Maybe AST) -> AST -> (Int, AST)
recurIndex f ast =
    case f ast of
        Nothing -> (0, ast)
        Just inner ->
            let (j, inner') = recurIndex f inner
            in (1 + j, inner')

finally :: EvalJS repr => AST -> repr
finally n = loc $ case n of
    NumericLiteral _ (Left i) -> intLit i
    NumericLiteral _ (Right i) -> doubleLit i
    BooleanLiteral _ b -> boolLit b
    StringLiteral _ ps -> strLit $ decodeStringWithReplacement ps

    Unary _ New (App _ f args) ->
        new (finally f) (map finally args)

    Unary _ op a -> unary op $ finally a

    Binary _ op left right ->
        binary op (finally left) (finally right)

    ArrayLiteral _ xs ->
        arrayLit $ map finally xs

    ObjectLiteral _ xs ->
        objLit $ map (decodeStringWithReplacement . fst &&& finally . snd) xs

    Function _ n args body ->
        func (fmap mkName n) (map mkName args) $ finally body

    Indexer _ (Attr ps) base ->
        let tryRecur = \case
                Indexer _ (Attr ps') base' | ps == ps' -> Just base
                Comment _ _ exp -> tryRecur exp
                _ -> Nothing
            (depth, inner) = recurIndex tryRecur base
        in
        if depth == 0 then
            getAttr (finally base) (decodeStringWithReplacement ps)
        else
            -- this is for speed up compilation
            app (var "special@getattr_looper") [intLit (toInteger depth + 1), finally inner, strLit (decodeStringWithReplacement ps)]

    Indexer _ item base ->
        let (depth, inner)
                | StringLiteral _ item' <- item =
                    let tryRecur = \case
                            Indexer _ (Attr _) _ -> Nothing
                            Indexer _ (StringLiteral _ item'') inner | item'' == item' ->
                                Just inner
                            Comment _ _ exp -> tryRecur exp
                            _ -> Nothing
                    in recurIndex tryRecur base
                | otherwise = (0, base)
        in
        if depth == 0 then
            getItem (finally base) (finally item)
        else
            -- this is for speed up compilation
            app (var "special@getitem_looper") [intLit (toInteger depth + 1), finally inner, finally item]

    Assignment _ (Indexer _ (Attr ps) base) rhs ->
        setAttr (finally base) (decodeStringWithReplacement ps) (finally rhs)

    Assignment _ (Indexer _ item base) rhs ->
        setItem (finally base) (finally item) (finally rhs)

    Assignment _ (Var _ n) rhs -> assign (mkName n) (finally rhs)

    Assignment _ lhs _ -> error $ show lhs

    App _ (Indexer _ (Attr "special@record_update") old) [new] ->
        upRecord (finally old) (finally new)

    App _ f args ->
        app (finally f) (map finally args)

    -- special names must in form of be `Var _` and not LHS
    Var _ n -> var (mkName n)

    Block _ xs -> block $ map finally xs

    VariableIntroduction _ n Nothing ->
        intro (mkName n) none

    VariableIntroduction _ n (Just it) ->
        intro (mkName n) (finally it)

    While _ cond body ->
        while (finally cond) (finally body)

    For _ n low high body ->
        forRange (mkName n) (finally low) (finally high) (finally body)

    ForIn _ n itr body ->
        forIn (mkName n) (finally itr) (finally body)

    IfElse _ cond te fe ->
        ite (finally cond) (finally te) (fmap finally fe)

    Return _ e ->
        ret $ finally e

    ReturnNoResult _ -> retNoRes

    Throw _ e ->
        throw $ finally e

    InstanceOf _ inst cls ->
        isa (finally inst) (finally cls)

    Comment _ cs exp ->
        let f = \case
             LineComment x -> x
             BlockComment x -> x
        in comment (map f cs) (finally exp)

    where loc | Just loc <- getSourceSpan n =
                    let loc' = takeSourceLoc loc
                    in  if line loc' == 0 then id
                            -- This is actually invalid line number,
                            -- and will break the support of Python 3.5.
                        else
                            located loc'
              | otherwise = id

takeSourceLoc
    SourceSpan
        { spanName=filename
        , spanStart =
            SourcePos
            { sourcePosLine = line'
            , sourcePosColumn = col
            }
        }
     =
        let line = line' + 1 in
        -- Issue #8
        SourceLoc {line, col, filename}
