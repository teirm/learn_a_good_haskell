myLast :: [a] -> a
myLast [] = error "Empty List"
myLast [x] = x
myLast (_:xs) = myLast xs
