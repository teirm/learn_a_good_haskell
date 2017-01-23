isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [_] = True
isPalindrome (x:xs) =
    let end = last xs
        middle = init xs
    in x == end && isPalindrome middle

-- From Solutions
isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = foldl (\acc (a,b) ->
                                if a == b
                                then acc
                                else False) True input
        where
        input = zip xs (reverse xs)
