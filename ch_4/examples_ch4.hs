myFib :: Int -> Int
myFib n
    | n < 0     = error "Negative number!"
    | n == 0    = 0
    | n == 1    = 1
    | otherwise = myFib (n-1) + myFib (n-2)

--myFib 0 = 0
--myFib 1 = 1
--myFib x = myFib (x-1) + myFib (x-2)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _        
    | n <= 0    = []
take' _ []      = []
take' n (x:xs)  = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ []   = []
zip' [] _   = []
zip' (x:xs) (y:ys)  = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a []  = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs

-- Six Line Quick Sort --
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in  quicksort smallerOrEqual ++ [x] ++ quicksort larger

sum' :: (Num a) => [a] -> a
sum' []  = error "Sum of an empty list!"
sum' [x] = x
sum' (x:xs) = x + sum xs

minimum' :: (Ord a) => [a] -> a
minimum' []     = error "Minimum of an empty list!"
minimum' [x]    = x
minimum' (x:xs) = min x (minimum' xs)

orArray :: [Bool] -> Bool
orArray []          = error "OR of empty list!"
orArray [x]         = x 
orArray (x:xs)      = x || orArray xs

andArray :: [Bool] -> Bool
andArray xs =
    case xs of []       -> error "AND of empty list!"
               [x]      -> x
               (x:xs)   -> x && andArray xs

--andArray []         = error "AND of empty list!"
--andArray [x]        = x
--andArray (x:xs)     = x && andArray xs


