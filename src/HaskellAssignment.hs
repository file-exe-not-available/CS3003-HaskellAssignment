module HaskellAssignment where

------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"
findFirst :: Eq a => (a -> Bool) -> [a] -> Found
findFirst predicate xs = go 0 xs
  where
    go _ [] = NoMatch
    go i (x:rest)
      | predicate x = Match i
      | otherwise   = go (i + 1) rest

------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool
palindrome s = s == reverse s

------------------------------------------------
-- mergesort
------------------------------------------------
mergesort :: (a -> a -> Bool) -> [a] -> [a]
mergesort _ []  = []
mergesort _ [x] = [x]
mergesort cmp xs = merge (mergesort cmp left) (mergesort cmp right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | cmp x y   = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

------------------------------------------------
-- runLengthEncode
------------------------------------------------
data RunLength = Span Int Char deriving (Eq)

instance Show RunLength where
  show (Span n c) = "(Span " ++ show n ++ " '" ++ [c] ++ "')"

runLengthEncode :: String -> [RunLength]
runLengthEncode [] = []
runLengthEncode (x:xs) = encode 1 x xs
  where
    encode n c [] = [Span n c]
    encode n c (y:ys)
      | y == c    = encode (n + 1) c ys
      | otherwise = Span n c : encode 1 y ys