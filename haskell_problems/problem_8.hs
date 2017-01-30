import Data.List 

compress :: (Eq a) => [a] -> [a]
compress x = map head $ group x 
