module Eval where

import Expr
import Control.Monad.State
import Data.Map ( lookup, empty, Map, insert )
import PrettyPrint
import Control.Monad.Except
import Parse
import Text.Parsec

type Evaluator a = StateT (Map String Expr) (Except String) a

runEvaluator :: Evaluator a -> Either String a
runEvaluator ev = runExcept $ evalStateT ev empty

parseEval :: String -> Evaluator Expr
parseEval s = parseWithError s >>= stepUntilValue

parseWithError :: String -> Evaluator Expr
parseWithError s = case parse parseExpr "" s of
    Right expr -> return expr
    Left parseerror -> throwError $ show parseerror 


isValue :: Expr -> Bool
isValue (Number _ _) = True
isValue (Boolean _) = True
isValue (Identifier _) = False
isValue (Die _) = False
isValue (Function _ _) = True
isValue (App _ _) = False
isValue (List _ False) = True
isValue (List _ True) = False
isValue (BinOp _ _ _) = False
isValue (UnaryOp _ _) = False
isValue (If _ _ _) = False
isValue (Parens _) = False

stepUntilValue :: Expr -> Evaluator Expr
stepUntilValue e = do
    e' <- step e
    if isValue e' then return e' else stepUntilValue e'

step :: Expr -> Evaluator Expr
step e@(Number _ _) = return e
step e@(Boolean _) = return e
step (Identifier id) = do
    ctx <- get
    case Data.Map.lookup id ctx of
        Just e -> return e
        Nothing -> throwError $ "Variable " ++ id ++ " not found."
step (Die _) = throwError "NYI"
step e@(Function _ _) = return e
step (App f args)
    | not $ isValue f = do
        f' <- step f 
        return $ App f' args
    | not $ all isValue args = do
        args' <- mapM step args
        return $ App f args'
    | otherwise = case f of
        (Function argnames exp) -> if length argnames /= length args then
            throwError $ "Function " ++ prettyPrint f ++ " expects " ++ show (length argnames) ++ " argument(s), got " ++ show (length args) ++ " argument(s)."
        else do
            ctx <- get
            put $ foldr (\(a,v) m -> Data.Map.insert a v m) ctx (zip argnames args)
            exp' <- stepUntilValue exp
            put ctx
            return exp'
        s -> throwError $ "Attempted to apply non-function " ++ prettyPrint s
step e@(List _ False) = return e
step (List l True) = if all isValue l then throwError "NYI" else do
    l' <- mapM step l
    return $ List l' True
step (BinOp o l r)
    | isValue l && isValue r = stepBinOp o l r
    | isValue l = do
        r' <- step r
        return $ BinOp o l r'
    | isValue r = do
        l' <- step l
        return $ BinOp o l' r
    | otherwise = do
        l' <- step l
        r' <- step r
        return $ BinOp o l' r'
step (UnaryOp s e)
    | isValue e = stepUnaryOp s e
    | otherwise = do
        e' <- step e
        return $ UnaryOp s e'
step (If c l r)
    | not $ isValue c = do
        c' <- step c
        return $ If c' l r
    | otherwise = case c of
        (Boolean b) -> if b then return l else return r
        s -> throwError $ "Attempted to use non-boolean " ++ prettyPrint s ++ " in if statement"
step (Parens e) = if isValue e then return e else do
    e' <- step e
    return $ Parens e'


stepBinOp :: String -> Expr -> Expr -> Evaluator Expr
stepBinOp "+" l r = liftNumeric "+" (+) l r
stepBinOp "-" l r = liftNumeric "-" (-) l r
stepBinOp "*" l r = liftNumeric "*" (*) l r
stepBinOp "/" l r = liftNumeric "/" (/) l r
stepBinOp "%" l r = liftNumeric "%" (\x y -> x - (y * fromInteger (floor (x/y)))) l r -- there has to be a better way
stepBinOp "^" l r = liftNumeric "^" (**) l r
stepBinOp "=" l r = liftComparison "=" (==) l r
stepBinOp "/=" l r = liftComparison "=" (/=) l r
stepBinOp ">" l r = liftComparison "=" (>) l r
stepBinOp "<" l r = liftComparison "=" (<) l r
stepBinOp ">=" l r = liftComparison "=" (>=) l r
stepBinOp "<=" l r = liftComparison "=" (<=) l r
stepBinOp "and" l r = liftBoolean "and" (&&) l r
stepBinOp "or" l r = liftBoolean "or" (||) l r
stepBinOp s _ _ = throwError $ "Unrecognized binary operator " ++ s

stepUnaryOp :: String -> Expr -> Evaluator Expr
stepUnaryOp "-" e = liftNumericU "-" (\x -> -x) e
stepUnaryOp "not" e = liftBooleanU "not" not e
stepUnaryOp s _ = throwError $ "Unrecognized unary operator " ++ s

liftNumeric :: String -> (Double -> Double -> Double) -> Expr -> Expr -> Evaluator Expr
liftNumeric _ f (Number nl cl) (Number nr cr) = return $ Number (f nl nr) (cl <> cr)
liftNumeric s _ l r = throwError $ "Tried to apply numeric operation " ++ s ++ " to non-numbers " ++ prettyPrint l ++ " and " ++ prettyPrint r

liftBoolean :: String -> (Bool -> Bool -> Bool) -> Expr -> Expr -> Evaluator Expr
liftBoolean _ f (Boolean l) (Boolean r) = return $ Boolean $ f l r
liftBoolean s _ l r = throwError $ "Tried to apply boolean operation " ++ s ++ " to non-boolean " ++ prettyPrint l ++ " and " ++ prettyPrint r

liftComparison :: String -> (Double -> Double -> Bool) -> Expr -> Expr -> Evaluator Expr
liftComparison _ f (Number nl _) (Number nr _) = return $ Boolean $ f nl nr 
liftComparison s _ l r = throwError $ "Tried to apply comparison " ++ s ++ " to non-numbers " ++ prettyPrint l ++ " and " ++ prettyPrint r

liftNumericU :: String -> (Double -> Double) -> Expr -> Evaluator Expr
liftNumericU _ f (Number n c) = return $ Number (f n) c
liftNumericU s _ e = throwError $ "Tried to apply numeric operation " ++ s ++ " to non-number " ++ prettyPrint e

liftBooleanU :: String -> (Bool -> Bool) -> Expr -> Evaluator Expr
liftBooleanU _ f (Boolean b) = return $ Boolean (f b)
liftBooleanU s _ e = throwError $ "Tried to apply boolean operation " ++ s ++ " to non-boolean " ++ prettyPrint e
