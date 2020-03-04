quicksort [] = []
quicksort (x:xs) =
  let smallerStuff = quicksort [a | a <- xs, a < x]
    largerStuff = quicksort [b | b <- xs, b > x]
  in
    smallerStuff ++ [x] ++ largerStuff
