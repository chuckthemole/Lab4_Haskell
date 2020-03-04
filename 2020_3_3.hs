

addTwo x = x + 2

subtract a b = a - b

isEven x = (mod x 2) == 0

startsWith17 [] = False
startsWith17 (x:xs) = x == 17

--  (++) this is append in haskell
--(++) :: [a] -> [a] -> [a]


--mysubtract :: Num a => a -> a -> a

--lab labStuff
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _==_ = False
