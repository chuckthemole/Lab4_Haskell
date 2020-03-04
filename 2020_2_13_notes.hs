--double [] = []
double (x:s) =  (x * 2) : double s --first element, x, and the rest, s.

--let ints = 1 : map (\x -> x + 1) ints --int f(int x) {
                                      --  return x + 1;}
--take 5 ints

addLists _ [] = [] -- underscore means we don't care
addLists [] _ = [] -- if one list is empty, return empty list
addLists (s:xs) (y:ys) = (s + y) : addLists xs ys

--base = 0
myfoldl f base [] = base
myfoldl f base (x:xs) = myfoldl f (f base x) xs

myfoldr f base [] = base
myfoldr f base (x:xs) = f x (myfoldr f base xs)

--Lab stuff
--factors n = (filter (\x -> (mod n x) == 0) [1..(n-1)])
factors n = [x | x <- [1..(n - 1)], mod n x == 0]
isPerfect n = ((foldl (+) 0 (factors n)) == n) -- take those factors and add them and see if they're equal to n
--isPerfectRange = [x | x <- [1..10000], isPerfect x]
isPerfectRange = (filter (\x -> isPerfect x)) [1..10000]

isPointy n = [mod x 2 == 0 | let x = length (factors n)]
isPointyRange = [x | x <- [1..10000], isPerfect x]

isPrime :: Integer -> Bool
isPrime n =
  if length(factors n) > 1
    then False
  else
    True

--addLists (s:xs) (y:ys) = (s + y) : addLists xs ys
divisors :: Integer -> [Bool]
divisors n =
  (map (\x ->
    if mod n x == 0
      then False
      else
        True
  ) [2..(n-1)])

checkList n (x:xs) =
  if mod n x == 0
    then False
  else
    checkList n xs

--takes two arguments: an iterator and number to test.
prime :: Integer -> Integer -> Bool
prime n x =
  if x >= n-1
    then True
    else if mod n x == 0
      then False
      else
        prime n (x+1)

isPrime2 :: Integer -> Bool
isPrime2 n =
  if n == 2
    then True
    else if mod n 2 == 0
      then False
      else
        prime n 3 -- call prime to test divisors beyond 2


isPrimeRange :: [Integer]
isPrimeRange = [x | x <- [2..10000], isPrime x]

  --[x | x <- [2..50], mod n x == 0]

main = do
  print(isPrime 13)
  print(isPrime 14)
  print(isPrime 21)
  print(isPrimeRange)
  --print(isPrime2 11)
  --print(checkList 12 [1, 2, 3, 4])


   --putStrLn "The factors of 5 is:"
   --print (factors 5)
