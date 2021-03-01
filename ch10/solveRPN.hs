-- Reverse Polish Notation

module SolveRPN (
  solveRPN
) where

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
  where foldingFunction (x:y:xs) "*" = (y * x):xs
        foldingFunction (x:y:xs) "+" = (y + x):xs
        foldingFunction (x:y:xs) "-" = (y - x):xs
        foldingFunction (x:y:xs) "/" = (y / x):xs
        foldingFunction (x:y:xs) "^" = (y ** x):xs
        foldingFunction (x:xs) "ln" = log x: xs
        foldingFunction xs "sum" = [sum xs]
        foldingFunction xs numberString = read numberString:xs