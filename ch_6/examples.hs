import Data.List 
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

-- Caesar Cipher
encode :: Int -> String -> String
-- As a function composition map (chr . (+ offset) . ord)
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]
