myButLast :: [a] -> a
myButLast [] = error "Empty List"
myButLast [x] = error "Singleton List"
myButLast (x:y:[]) = x
myButLast (_:xs) = myButLast xs
