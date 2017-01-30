pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = 
    let (prefix, postfix) = span ( == x) xs
    in [prefix] ++ pack postfix
