myLength :: [a] -> Int
myLength [] = 0
myLength xs = sum [1 | _ <- xs] 

