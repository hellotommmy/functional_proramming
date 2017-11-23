module F where
{-reverse ::[Int] ->  [Int]
reverse [] = []
reverse (x:xs) = reverse xs ++[x]
 -}
revk :: [Int] -> ([Int] -> [Int]) -> [Int]
--      [Int] -> M [Int]
revk [] k  = k []
revk (x:xs) k = revk xs (\z -> k (z++[x] ) )
{-eg for iterative  writing
>sumList :: [Integer] -> Integer
>sumList lst = sumLoop lst 0 where
>    sumLoop (x:xs) i = sumLoop xs (i+x)
>    sumLoop [] i = i
 -}

revi :: [Int] -> [Int]
revi xs = (reviLoop xs []) where
  reviLoop (x1:xs1) lst = reviLoop xs1 ([x1]++lst)
  reviLoop [] lst = lst
