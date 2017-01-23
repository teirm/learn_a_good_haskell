import Data.List

encode :: (Eq a) => [a] -> [([a], Int)]
encode = map (\word -> (word, length word)) . group 
