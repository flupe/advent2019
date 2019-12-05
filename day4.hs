high = [5, 7, 6, 7, 2, 3]

main :: IO ()
main = do
    print $ count (>= 2)
    print $ count (== 2)
    where count = wingIt 6 1 high False 0

wingIt 0 _ _ hc sl cond = fromEnum (hc || cond sl)
wingIt n l (h:xs) hc sl cond =
    sum [ wingIt (n - 1) x (hi x) (hc' x) (sl' x) cond | x <- [l .. h] ]
    where hi x | x == h = xs
               | otherwise = repeat 9
          hc' x = hc || (x /= l && cond sl)
          sl' x = if x == l then sl + 1 else 1
