import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

type Tree = M.Map String (Maybe String, [String])

main :: IO ()
main = do
    tree <- foldr addOrbit M.empty . lines <$> readFile "inputs/day6"

    print $ count tree 0 "COM"

    let you = reverse $ ancestors tree "YOU"
        san = reverse $ ancestors tree "SAN"

    print $ uncurry (+) $ map' length (strip you san)

    where
        addOrbit :: String -> Tree -> Tree
        addOrbit orbit tree =
            let [a, b] = splitOn ")" orbit
            in M.alter (\case Nothing      -> Just (Just a, [])
                              Just (_, xs) -> Just (Just a, xs)) b
             $ M.alter (\case Nothing      -> Just (Nothing, [b])
                              Just (p, xs) -> Just (p, b:xs)) a tree

        count :: Tree -> Int -> String -> Int
        count tree depth root =
            case M.lookup root tree of
                Nothing      -> depth
                Just (_, xs) -> depth + (sum $ map (count tree (depth + 1)) xs)

        ancestors :: Tree -> String -> [String]
        ancestors tree leaf =
            case M.lookup leaf tree of
                Just (Just p, _)  -> p:ancestors tree p
                _                 -> []

        strip :: [String] -> [String] -> ([String], [String])
        strip [] xs = ([], xs)
        strip xs [] = (xs, [])
        strip a@(x:xs) b@(y:ys) | x == y    = strip xs ys
                                | otherwise = (a, b)

        map' f (x, y) = (f x, f y)
