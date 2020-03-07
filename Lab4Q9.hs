data MyNum = Fraction {numerator :: Integer, denominator :: Integer}
  | Mixed {whole :: Integer, numerator :: Integer, denominator :: Integer}
    deriving (Eq, Show)


main = do
  let f1 = Fraction{numerator = 1, denominator = 2}
  let f2 = Fraction{numerator = 2, denominator = 4}
  let f3 = Fraction{numerator = 3, denominator = 2}
  let m1 = Mixed{whole = 1, numerator = 1, denominator = 2}
  let m2 = Mixed{whole = 0, numerator = 3, denominator = 2}
  let m3 = Mixed{whole = 1, numerator = 1, denominator = 2}

  print("1/2 == 2/4?")
  print(f1 == f2)
  print("3/2 == 1 1/2?")
  print(f3 == m1)
  print("0 3/2 == 1 1/2?")
  print(m2 == m3)

  {-
*Main> main
"1/2 == 2/4?"
False
"3/2 == 1 1/2?"
False
"0 3/2 == 1 1/2?"
False
(0.00 secs, 101,944 bytes)
*Main>
  -}
