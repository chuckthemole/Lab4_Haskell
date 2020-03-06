data MyNum = Fraction {numerator :: Integer, denominator :: Integer}
  | Mixed {whole :: Integer, numerator :: Integer, denominator :: Integer}
    --deriving (Eq, Show)

instance Show MyNum where
  show (Fraction n d) = (show n) ++ "/" ++ (show d)
  show (Mixed w n d) = (show w) ++ " " ++ (show n) ++ "/" ++ (show d)

instance Eq MyNum where
  (Fraction n1 d1) == (Fraction n2 d2) =
    (helper (Fraction {numerator = n1, denominator = d1})) ==
      (helper (Fraction {numerator = n2, denominator = d2}))
  (Mixed w1 n1 d1) == (Mixed w2 n2 d2) =
    (helper (Mixed {whole = w1, numerator = n1, denominator = d1})) ==
      (helper (Mixed {whole = w2, numerator = n2, denominator = d2}))
  (Fraction n1 d1) == (Mixed w2 n2 d2) =
    (helper (Fraction {numerator = n1, denominator = d1})) ==
      (helper (Mixed {whole = w2, numerator = n2, denominator = d2}))
  (Mixed w1 n1 d1) == (Fraction n2 d2) =
    (helper (Mixed {whole = w1, numerator = n1, denominator = d1})) ==
      (helper (Fraction {numerator = n2, denominator = d2}))

myCompare :: MyNum -> MyNum -> Ordering
myCompare (Fraction {numerator = n1, denominator = d1})
          (Fraction {numerator = n2, denominator = d2})
  = compare (helper (Fraction {numerator = n1, denominator = d1}))
            (helper (Fraction {numerator = n2, denominator = d2}))
myCompare (Mixed {whole = w1, numerator = n1, denominator = d1})
          (Mixed {whole = w2, numerator = n2, denominator = d2})
  = compare (helper (Mixed {whole = w1, numerator = n1, denominator = d1}))
            (helper (Mixed {whole = w2, numerator = n2, denominator = d2}))
myCompare (Fraction {numerator = n1, denominator = d1})
          (Mixed {whole = w2, numerator = n2, denominator = d2})
  = compare (helper (Fraction {numerator = n1, denominator = d1}))
            (helper (Mixed {whole = w2, numerator = n2, denominator = d2}))
myCompare (Mixed {whole = w1, numerator = n1, denominator = d1})
          (Fraction {numerator = n2, denominator = d2})
  = compare (helper (Mixed {whole = w1, numerator = n1, denominator = d1}))
            (helper (Fraction {numerator = n2, denominator = d2}))

--Converts a MyNum, Fraction or Mixed, to a type Double
helper :: MyNum -> Double
helper (Fraction {numerator = n, denominator = d}) = (fromIntegral n) / (fromIntegral d)
helper (Mixed {whole = x, numerator = n, denominator = d}) =
  ((fromIntegral d) * (fromIntegral x) + (fromIntegral n)) / (fromIntegral d)

--Tests
main = do
  let f1 = Fraction{numerator = 1, denominator = 2}
  let f2 = Fraction{numerator = 2, denominator = 4}
  let f3 = Fraction{numerator = 14, denominator = 2}
  let m1 = Mixed{whole = 12, numerator = 1, denominator = 2}
  let m2 = Mixed{whole = 0, numerator = 1, denominator = 2}
  let m3 = Mixed{whole = 4, numerator = 12, denominator = 4}

  --Test Fraction == Fraction, Fraction /= Fraction,
  --Fractions are equal but one is not reduced
  print("*************************************************************************")
  print("Test for f1 and f2 (both Fractions). f1 is printed first and f2 second.")
  print(f1)
  print(f2)
  print("f1 == f2")
  print(f1 == f2) --True
  print("f2 /= f1")
  print(f2 /= f1) --False
  print("*************************************************************************")

  --Test Mixed == Mixed, Mixed /= Mixed,
  --Mixed are unequal
  print("Test for m1 and m2 (both Mixed). m1 is printed first and m2 second.")
  print(m1)
  print(m2)
  print("m1 == m2")
  print(m1 == m2) --False
  print("m2 /= m1")
  print(m2 /= m1) --True
  print("*************************************************************************")


  --Test Mixed == Fraction, Fraction == Mixed, Fraction /= Mixed
  print("Test for m3 (Mixed) and f3 (Fraction). m3 is printed first and f3 second.")
  print(m3)
  print(f3)
  print("m3 == f3")
  print(m3 == f3) --True
  print("f3 == m3")
  print(f3 == m3) --True
  print("f3 /= m3")
  print(f3 /= m3) --False
  print("*************************************************************************")

  --Test for myCompare
  print("f1 = 1/2, f2 = 2/4, f3 = 14/2")
  print("m1 = 12 1/2, m2 = 0 1/2, m3 = 4 12/4")
  print("*************************************************************************")
  print("myCompare f1 f2")
  print(myCompare f1 f2)
  print("myCompare m1 m2")
  print(myCompare m1 m2)
  print("myCompare m2 f1")
  print(myCompare m2 f1)
  print("myCompare f3 m3")
  print(myCompare f3 m3)
  print("myCompare f2 m2")
  print(myCompare f2 m2)
  print("myCompare f1 m3")
  print(myCompare f1 m3)
  print("*************************************************************************")

  --Below is the output from main
  {-
  *Main> main
  "*************************************************************************"
  "Test for f1 and f2 (both Fractions). f1 is printed first and f2 second."
  1/2
  2/4
  "f1 == f2"
  True
  "f2 /= f1"
  False
  "*************************************************************************"
  "Test for m1 and m2 (both Mixed). m1 is printed first and m2 second."
  12 1/2
  0 1/2
  "m1 == m2"
  False
  "m2 /= m1"
  True
  "*************************************************************************"
  "Test for m3 (Mixed) and f3 (Fraction). m3 is printed first and f3 second."
  4 12/4
  14/2
  "m3 == f3"
  True
  "f3 == m3"
  True
  "f3 /= m3"
  False
  "*************************************************************************"
  "f1 = 1/2, f2 = 2/4, f3 = 14/2"
  "m1 = 12 1/2, m2 = 0 1/2, m3 = 4 12/4"
  "*************************************************************************"
  "myCompare f1 f2"
  EQ
  "myCompare m1 m2"
  GT
  "myCompare m2 f1"
  EQ
  "myCompare f3 m3"
  EQ
  "myCompare f2 m2"
  EQ
  "myCompare f1 m3"
  LT
  "*************************************************************************"
  (0.01 secs, 876,288 bytes)
  -}
