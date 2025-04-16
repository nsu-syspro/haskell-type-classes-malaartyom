{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where
import Data.Char (isDigit)


-- * Expression data type

-- | Representation of integer arithmetic expressions comprising
-- - Literals of type 'a'
-- - Binary operations 'Add' and 'Mul'
data IExpr =
    Lit Integer
  | Add IExpr IExpr
  | Mul IExpr IExpr
  deriving Show

-- * Evaluation

-- | Evaluates given 'IExpr'
--
-- Usage example:
--
-- >>> evalIExpr (Lit 2)
-- 2
-- >>> evalIExpr (Add (Lit 2) (Lit 3))
-- 5
-- >>> evalIExpr (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- 9
--
evalIExpr :: IExpr -> Integer
evalIExpr (Lit a) = a
evalIExpr (Add x y) = evalIExpr x + evalIExpr y
evalIExpr (Mul x y) = evalIExpr x * evalIExpr y
-- * Parsing

-- | Class of parseable types
class Parse a where
  -- | Parses value 'a' from given string
  -- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
  parse :: String -> Maybe a

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe IExpr
-- Just (Lit 2)
-- >>> parse "2 3 +" :: Maybe IExpr
-- Just (Add (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe IExpr
-- Just (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe IExpr
-- Nothing
-- >>> parse "2 3" :: Maybe IExpr
-- Nothing
--
instance Parse IExpr where
  parse x = parseSplitted (map toToken (words x)) []


-- * Evaluation with parsing

-- | Parses given expression in Reverse Polish Notation and evaluates it
--
-- Returns 'Nothing' in case the expression could not be parsed.
--
-- Usage example:
--
-- >>> evaluateIExpr "2"
-- Just 2
-- >>> evaluateIExpr "2 3 +"
-- Just 5
-- >>> evaluateIExpr "3 2 * 3 +"
-- Just 9
-- >>> evaluateIExpr "2 +"
-- Nothing
-- >>> evaluateIExpr "2 3"
-- Nothing
--
evaluateIExpr :: String -> Maybe Integer
evaluateIExpr s = case parse s of
                    Nothing -> Nothing
                    Just x  -> Just (evalIExpr x)


data Token = Num Integer | AddOp | MulOp
  deriving Show



toToken:: String -> Maybe Token
toToken "+" = Just AddOp
toToken "*" = Just MulOp
toToken  x  = if all isDigit x then Just (Num (read x)) else Nothing


parseSplitted :: [Maybe Token] -> [IExpr] -> Maybe IExpr
parseSplitted [] [] = Nothing
parseSplitted (x:xs) stack@(y:z:ys) = case x of
                                  Nothing -> Nothing
                                  Just AddOp -> parseSplitted xs (Add y z : ys)
                                  Just MulOp -> parseSplitted xs (Mul y z : ys)
                                  Just (Num n) -> parseSplitted xs (Lit n : stack)
parseSplitted [] [x]  = Just x
parseSplitted [] _    = Nothing
parseSplitted (x:xs) y = parseNum x xs y



parseNum :: Maybe Token -> [Maybe Token] -> [IExpr] -> Maybe IExpr
parseNum (Just (Num x)) xs y = parseSplitted xs (Lit x:y)
parseNum (Just _) _ _ = Nothing
parseNum Nothing _ _= Nothing

