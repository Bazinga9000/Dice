module Expr where

data Criticality = Neither | Success | Failure | Both | Constant deriving Show

instance Semigroup Criticality where
    Both <> x = x
    x <> Both = x
    Constant <> x = x
    x <> Constant = x
    Success <> Success = Success
    Failure <> Failure = Failure
    _ <> _ = Neither

  
data DieExpr = Normal Expr Expr | Fudge Expr Expr | Pareto Expr Expr | Gaussian Expr Expr | Binomial Expr Expr Expr | Set Expr [Expr] deriving Show

data Expr =
      Number Double Criticality
    | Boolean Bool
    | Identifier String
    | Die DieExpr
    | Function [String] Expr
    | App Expr [Expr] 
    | List [Expr] Bool -- bool is for whether the list is to be automatically summed
    | BinOp String Expr Expr
    | UnaryOp String Expr
    | If Expr Expr Expr
    | Parens Expr
    deriving Show
