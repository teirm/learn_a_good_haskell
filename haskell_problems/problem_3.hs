elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt [] _ = error "Error empty list"
elementAt (_:xs) n = elementAt xs (n - 1)
