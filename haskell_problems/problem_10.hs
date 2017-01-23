import Data.List
-- Technically cheating since I did not write my own
-- group
-- Will fix when possible

encode :: (Eq a) => [a] -> [([a], Int)]
encode = map (\word -> (word, length word)) . group 
