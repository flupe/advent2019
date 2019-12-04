import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

data Dir  = U | D | L | R deriving (Read)
data S    = Up | Dn
data Line = E Int Int Int S Int

main :: IO ()
main = do
    [wire1, wire2] <- map (map toCmd . splitOn ",") . lines <$> readFile "inputs/day3"

    let
        (horiz1, verts1) = getLines wire1
        (horiz2, verts2) = getLines wire2

        intersections = mapMaybe intersection ([ (h, v) | h <- horiz1, v <- verts2 ]
                                            ++ [ (h, v) | h <- horiz2, v <- verts1 ])

    print $ smallest day1 intersections
    print $ smallest day2 intersections

    where
        toCmd (x : xs) = (read [x], read xs)
        getLines = computeLines 0 (0, 0) ([], [])
        inRange x a b = a <= x && x <= b
        intersection a@(E yH x1 x2 _ _, E xV y1 y2 _ _)
            | inRange yH y1 y2 && inRange xV x1 x2 = Just a
            | otherwise = Nothing
        smallest f = head . sort . map f

day1 (E yH _ _ _ _, E xV _ _ _ _) = abs xV + abs yH
day2 (E yH x1 x2 s1 d1, E xV y1 y2 s2 d2) =
    d1 + d2 + delta s1 xV x1 x2 + delta s2 yH y1 y2
    where delta Up x x1 x2 = x - x1
          delta Dn x x1 x2 = x2 - x

computeLines _ _ lines [] = lines
computeLines dist (x, y) (horiz, verts) ((d, i):xs) =
    case d of
        U -> comp (x, y + i) (horiz, E x y (y + i) Up dist : verts) xs
        D -> comp (x, y - i) (horiz, E x (y - i) y Dn dist : verts) xs
        R -> comp (x + i, y) (E y x (x + i) Up dist : horiz, verts) xs
        L -> comp (x - i, y) (E y (x - i) x Dn dist : horiz, verts) xs
    where comp = computeLines (dist + i)

