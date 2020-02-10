module PsPy where

import Language.PureScript.CoreImp.AST
import Language.PureScript.Comments (Comment(..))
import Language.PureScript.PSString (PSString)
import Language.PureScript.AST.SourcePos
import qualified Data.Text as T
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Map  as M
import Text.Printf (printf)
import Control.Monad.State
import Control.Applicative
import Control.Arrow ((&&&))


class PolyString s where
    asIs :: Text -> s


rtsPrefix :: PolyString s => s
rtsPrefix = asIs "rts_"
psPrefix :: PolyString s => s
psPrefix = asIs "ps_"

instance PolyString String where
    asIs = T.unpack

instance PolyString Text where
    asIs = id


-- getLocSet ss
--     | Just SourceSpan
--         { spanStart=SourcePos 
--             { sourcePosLine
--             , sourcePosColumn
--             }
--         } <- ss
--       = \x -> printf "setLoc(%s, %d, %d)"  sourcePosLine sourcePosColumn
--     | otherwise = id


class EvalJS repr where
    none :: repr
    intLit  :: Integer -> repr
    doubleLit  :: Double -> repr
    strLit  :: String -> repr
    boolLit :: Bool -> repr
    unary   :: UnaryOperator -> repr -> repr
    binary  :: BinaryOperator -> repr -> repr -> repr
    arrayLit :: [repr] -> repr
    getItem :: repr -> repr -> repr
    objLit  :: [(String, repr)] -> repr
    func    :: Maybe String -> [String] -> repr -> repr
    app     :: repr -> [repr] -> repr
    -- create class
    new     :: repr -> [repr] -> repr
    var     :: String -> repr
    block   :: [repr] -> repr
    assign  :: repr -> repr -> repr
    while   :: repr -> repr -> repr
    for     :: String -> repr -> repr -> repr -> repr
    -- [forIn]
    --  used only for iterating records:
    --    github.com/purescript/purescript@master 
    --    src/Language/PureScript/CodeGen/JS.hs
    forIn   :: String -> repr -> repr -> repr
    ite :: repr -> repr -> Maybe repr -> repr
    ret :: repr -> repr
    retNoRes :: repr
    throw :: repr -> repr
    isa :: repr -> repr -> repr
    comment :: [String] -> repr -> repr
    located :: SourceSpan -> repr -> repr

ofName :: Text -> String
ofName = show . T.append psPrefix

finally :: EvalJS repr => AST -> repr
finally n = loc $ case n of
    NumericLiteral _ (Left i) -> intLit i
    NumericLiteral _ (Right i) -> doubleLit i
    BooleanLiteral _ b -> boolLit b
    StringLiteral _ ps -> strLit $ show ps
    Unary _ New (App _ f args) ->
        new (finally f) (map finally args)
    Unary _ op a -> unary op $ finally a
    Binary _ op left right ->
        binary op (finally left) (finally right)
    ArrayLiteral _ xs ->
        arrayLit $ map finally xs
    Indexer _ item base -> getItem (finally base) (finally item)
    ObjectLiteral _ xs ->
        objLit $ map (show  . fst &&& finally . snd) xs
    Function _ n args body ->
        func (fmap ofName n) (fmap ofName args) $ finally body
    App _ f args ->
        app (finally f) (map finally args)
    Var _ n -> var $ ofName n
    Block _ xs -> block $ map finally xs
    VariableIntroduction _ n Nothing ->
        assign (var $ ofName n) none
    
    VariableIntroduction _ n (Just it) ->
        assign (var $ ofName n) (finally it)
    
    Assignment _ lhs rhs ->
        assign (finally lhs) (finally rhs)
    
    While _ cond body ->
        while (finally cond) (finally body)
    
    For _ n low high body ->
        for (ofName n) (finally low) (finally high) (finally body)
    
    ForIn _ n itr body ->
        forIn (ofName n) (finally itr) (finally body)
    
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
             LineComment x -> ofName x
             BlockComment x -> ofName x
        in comment (map f cs) (finally exp)
    
    where
        loc | Just pos <- getSourceSpan n = located pos
            | otherwise = id

data CFG
    = CFG {
      ncnt :: Int
    , lif :: [String] -- lambda lifted
    , lhs :: Bool
    }

gensym :: State CFG String
gensym = do
    CFG {ncnt} <- get
    let n = psPrefix ++ (printf "%d" ncnt :: String)
    modify $ \st@CFG {ncnt} -> st {ncnt = ncnt + 1}
    return n

newtype Py = Py String
unPy (Py s) = s

returnPy = return . Py
getCtx :: State CFG String
getCtx = do
    isLhs <- gets lhs
    return $
        if isLhs then "Store()"
        else "Load()"

constOf s = printf "Constant(value=%s)" s
joinByComma xs = L.intercalate "," xs

evalWithLamLift :: State CFG Py -> State CFG [String]
evalWithLamLift it = do
    st <- get
    let (Py a, st') = runState it (st {lif = []})
    let ret = reverse $ a : lif st'
    put $ st' {lif = lif st}
    return ret

lamLift :: String -> State CFG ()
lamLift s = modify $ \st@CFG {lif} -> st {lif = s:lif}

newDatum :: String -> String
newDatum cls = printf "Dict(keys=[Constant(value='.type')], values=[%s])" cls

-- required rts:
-- 1. `zfsr64`, which implements zero_fill_shift_right for 64-bit integers
-- 2. `Error(msg, self) = Exception(msg)`, 

instance EvalJS (State CFG Py) where

    none = returnPy "Constant(value=None)"
    intLit i = returnPy $ printf "Constant(value=%d)" i
    doubleLit f = returnPy $ printf "Constant(value=%f)" f
    strLit s = returnPy $ printf "Constant(value=%s)" s
    boolLit b = returnPy $ printf "Constant(value=%s)" $ show b
    arrayLit xs = do
        xs <- L.intercalate ", " . map unPy <$> sequence xs
        ctx <- getCtx
        returnPy $ printf "List(elts=[%s], ctx=%s)" xs ctx
    unary op e = do
        Py e <- e
        let op' :: String
            op' | Negate <- op = "USub()"
                | Not <- op = "Not()"
                | BitwiseNot <- op = "Invert()"
                | Positive <- op = "UAdd()"
                | otherwise = error "impossible unary operator"
        returnPy $ printf "UnaryOp(op=%s, operand=%s)" op' e
    binary op l r = do
        Py l <- l
        Py r <- r
        let
            mkBop :: String -> String -> String -> String
            mkBop op l r = printf "BinOp(left=%s,op=%s,right=%s)" l op r
            mkBoolOp :: String -> String -> String -> String
            mkBoolOp op l r = printf "BoolOp(op=%s,values=[%s, %s])" op l r
            
            is a = a == op
            op' :: String -> String -> String
            op' | is Add = mkBop "Add()"
                | is Subtract = mkBop "Sub()"
                | is Multiply = mkBop "Mult()"
                | is Divide = mkBop "Div()"
                | is Modulus = mkBop "Mod()"
                | is EqualTo = mkBoolOp "Eq()"
                | is NotEqualTo = mkBoolOp "Ne()"
                | is LessThan = mkBoolOp "Lt()"
                | is LessThanOrEqualTo = mkBoolOp "Le()"
                | is GreaterThan = mkBoolOp "Gt()"
                | is GreaterThanOrEqualTo = mkBoolOp "Ge()"
                | is And = mkBoolOp "And()"
                | is Or =  mkBoolOp "Or()"
                | is BitwiseAnd = mkBop "BitAnd()"
                | is BitwiseOr = mkBop "BitOr()"
                | is BitwiseXor = mkBop "BitXor()"
                | is ShiftLeft = mkBoolOp "LShift()"
                | is ShiftRight = mkBoolOp "RShift()"
                | is ZeroFillShiftRight =
                    \l r -> printf
                    "Call(func=Name(id='zfsr64', ctx=Load()), args=[%s, %s], keywords=[])" l r    
        returnPy $ op' l r
    
    getItem base item = do
        Py base <- base
        Py item <- item
        ctx <- getCtx
        returnPy $ printf "Subscript(value=%s, slice=%s, ctx=%s)" base item ctx
    
    objLit xs = do
        let (keys', values) = unzip xs
        let keys = keys' >>= pure . constOf
        values <- map unPy <$> sequence values
        returnPy $
            printf "Dict(keys=[%s], values=[%s])"
                (joinByComma keys)
                (joinByComma values)

    func n args' body = do
        n <- case n of
               Just n -> return n
               _      -> gensym
        let args = args' ++ [show ("this" :: String)] >>= \n ->
                    [printf "arg(arg=%s, annotation=None)" n]
        suite <- evalWithLamLift body
        
        returnPy $ 
          printf
            "FunctionDef(\
                \name=%s,\
                \args=arguments(\
                    \args=[%s],\
                    \vararg=None,\
                    \kwonlyargs=[],\
                    \kw_defaults=[],\
                    \kwarg=None,\
                    \defaults=[Constant(value=None)]),\
                \),\
                \body=[%s],\
                \decorator_list=[],\
                \returns=None,\
            \)"
            n
            (joinByComma args)
            (joinByComma suite)
    
    app f ps = do
        Py f <- f
        ps <- map unPy <$> sequence ps
        returnPy $ printf "Call(func=%s, args=[%s], keywords=[])" f (joinByComma ps)
    
    -- -- create class
    new f ps = do
        Py f <- f
        n <- gensym
        let inst :: String -> String
            inst ctx = printf "Name(id=%s, ctx=%s())" n ctx
        lamLift $ printf "Assign(targets=[%s], value=%s)" (inst "Store") (newDatum f)
        ps <- (++ [inst "Load"]) . map unPy <$> sequence ps
        lamLift $ printf "Call(func=%s, args=[%s], keywords=[])" f (joinByComma ps)
        returnPy $ inst "Load"

    var s = returnPy $ printf "Name(id=%s, ctx=Load())" s
    block [] = returnPy $ constOf "None"
    block xs = do
        let xs1 = init xs
        let end = last xs
        forM_ xs $ \x ->
            Py x <- x
            lamLift x
        Py x <- end
        returnPy x

    -- block   :: [repr] -> repr
    -- assign  :: repr -> repr -> repr
    -- while   :: repr -> repr -> repr
    -- for     :: String -> repr -> repr -> repr -> repr
    -- -- [forIn]
    -- --  used only for iterating records:
    -- --    github.com/purescript/purescript@master 
    -- --    src/Language/PureScript/CodeGen/JS.hs
    -- forIn   :: String -> repr -> repr -> repr
    -- ite :: repr -> repr -> Maybe repr -> repr
    -- ret :: repr -> repr
    -- retNoRes :: repr
    -- throw :: repr -> repr
    -- isa :: repr -> repr -> repr
    -- comment :: [String] -> repr -> repr
    -- located :: SourceSpan -> repr -> repr

-- cg :: AST -> State Py String
-- cg n = setLoc <$>
--     case n of
--     NumericLiteral _ l
--         | Left l <- l  -> return $ printf "Constant(value=%d)" l
--         | Right d <- l -> return $ printf "Constant(value=%f)" d
--     StringLiteral _ s  -> return $ printf "Constant(value=%s)" $ show s
--     BooleanLiteral _ b -> return $ printf "Constant(value=True)" $ show b
   
--     Unary _ Not op -> do
--         operand <- cg op
--         return $ printf "UnaryOp(op=Not(), operand=%s)" operand
    
--     -- https://github.com/purescript/purescript/issues/3786
--     Unary _ Negate op -> do
--         operand <- cg op
--         return $ printf "UnaryOp(op=USub(), operand=%s)" operand
    
--     Unary _ BitwiseNot op -> do
--         operand <- cg op
--         return $ printf "UnaryOp(op=Invert(), operand=%s)" operand
    
--     Unary _ Positive op -> do
--         operand <- cg op
--         return $ printf "UnaryOp(op=UAdd(), operand=%s)" operand

--     -- new A(x) ->
--     --   tmp = new()
--     --   A(x, self=tmp)
--     --   tmp
--     Unary _ New (App _) -> do
--         operand <- cg op
--         return $ printf "Call(func=Name(id=%s, ctx=Load()), operand=%s)" operand
    
    
    


--     _ -> return ""
--     where
--         setLoc :: String -> String
--         setLoc = getLocSet $ getSourceSpan n
          
