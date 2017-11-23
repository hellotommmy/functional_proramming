module F where
{-reverse ::[Int] ->  [Int]
reverse [] = []
reverse (x:xs) = reverse xs ++[x]
 -}
revk :: [Int] -> ([Int] -> [Int]) -> [Int]
--      [Int] -> M [Int]
revk [] k  = k []
revk (x:xs) k = revk xs (\z -> k (z++[x] ) )
