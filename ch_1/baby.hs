doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

boombBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

rightTriangles = [(a,b,c) |
                  a <- [1..10], b <- [1..a], c <- [1..b],
                  c^2 + b^2 == a^2,
                  a+b+c == 24
                 ]
