{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- The above pragma enables all warnings

module Task3 where
import Task1 (Parse (parse))
import Task2 (Eval (evalBinOp), Expr (..), evalExpr)
import Data.Foldable (foldl')

-- | Solves SAT problem for given boolean formula written in Reverse Polish Notation
--
-- Returns whether given formula is satifiable
-- wrapped into 'Maybe' with 'Nothing' indicating parsing failure.
--
-- Only following binary operations are allowed:
-- - and
-- - or
-- - xor
--
-- All other words are considered as variables.
--
-- Usage example:
--
-- >>> solveSAT "x y and"
-- Just True
-- >>> solveSAT "x y xor x y and and"
-- Just False
-- >>> solveSAT "x y xor x y and or"
-- Just True
-- >>> solveSAT "x xor" 
-- Nothing
--
solveSAT :: String -> Maybe Bool
solveSAT = solve 


data BoolOp = And | Or | Xor
  deriving Show


instance Parse Bool where
  parse "True"  = Just True
  parse "False" = Just False
  parse _       = Nothing


instance Parse BoolOp where
  parse "xor" = Just Xor
  parse "and" = Just And
  parse "or"  = Just Or
  parse   _   = Nothing


instance Eval Bool BoolOp where
  evalBinOp And = (&&)
  evalBinOp Or  = (||)
  evalBinOp Xor = (/=)


evaluteBool :: [(String, Bool)] -> String -> Maybe Bool
evaluteBool list s = case parse s :: Maybe (Expr Bool BoolOp) of
                      Nothing -> Nothing
                      Just x  -> evalExpr list x

solve :: String -> Maybe Bool
solve x = case (parse x :: Maybe (Expr Bool BoolOp)) of 
            (Just expr) -> foldl' combine (Just False) (map (`evaluteBool` x) (constructList (findAllVars expr)))
            Nothing     -> Nothing


constructList :: [String] -> [[(String, Bool)]]
constructList []     = [[]]
constructList (x:xs) = map (\s -> (x, True) : s) (constructList xs) ++  map (\s -> (x, False) : s) (constructList xs)


findAllVars :: Expr Bool BoolOp -> [String]
findAllVars (Lit _) = []
findAllVars (Var x) = [x]
findAllVars (BinOp _ x y) = findAllVars x ++ findAllVars y


combine:: Maybe Bool -> Maybe Bool -> Maybe Bool
combine (Just x) (Just y) = Just (x || y)
combine _ _ = Nothing


