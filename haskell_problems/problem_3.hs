elementAt :: (Num a, Integral b) => [a] -> b -> a
elementAt (x:_) 1 = x
elementAt [] _ = error "Error empty list"
elementAt (_:xs) n = elementAt xs (n - 1)
