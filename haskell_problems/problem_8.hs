compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) =
    let no_dups = [a | a <- xs, a /= x]
    in x : compress no_dups
