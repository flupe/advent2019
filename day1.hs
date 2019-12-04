main :: IO ()
main = do
    input <- readFile "inputs/day1"

    print . sum . map (getFuel1 . read) . lines $ input
    print . sum . map (getFuel2 . read) . lines $ input

    where
        getFuel1 m = m `div` 3 - 2
        getFuel2 m
            | m <= 8 = 0
            | otherwise = fuel + getFuel2 fuel
                  where fuel = getFuel1 m
