module PrettyPrint where
import Data.List

import Expr

ppCriticality :: Criticality -> String
ppCriticality Neither = ""
ppCriticality Constant = ""
ppCriticality Success = "★"
ppCriticality Failure = "†"
ppCriticality Both = "?"


ppDieExpr :: b -> String
ppDieExpr = const "NYI"

prettyPrint :: Expr -> String
prettyPrint (Number n c) = nstr ++ ppCriticality c where
    n' = round n
    nstr = if n == fromInteger n' then show n' else show n
prettyPrint (Boolean b) = show b
prettyPrint (Identifier id) = id
prettyPrint (Die de) = ppDieExpr de
prettyPrint (Function id expr) = "(\\" ++ intercalate "," id ++ " -> " ++ prettyPrint expr ++ ")"
prettyPrint (List lexp _) = show (map prettyPrint lexp)
prettyPrint (BinOp op a b) = prettyPrint a ++ op ++ prettyPrint b
prettyPrint (UnaryOp op a) = op ++ prettyPrint a
prettyPrint (Parens e) = "(" ++ prettyPrint e ++ ")"