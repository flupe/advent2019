import Parser

type Chemical    = String
type Compound    = (Chemical, Int)
type Reaction    = (Chemical, (Int, [Compound]))
type Ingredients = [Compound]

compound :: Parser Compound
compound = (\a b -> (b, a)) <$> integer <* space <*> identifier

reaction :: Parser Reaction
reaction = (\a (b, c) -> (b, (c, a)))
         <$> sepBy ", " compound
         <*  space <* string "=>" <* space
         <*> compound

main :: IO ()
main = do
    reactions <- map (eat reaction) . lines <$> readFile "inputs/day14"
    print . snd $ synthesize reactions [] ("FUEL", 1)

    print . snd $ synthesize reactions [] ("FUEL", 1120408)
    print . snd $ synthesize reactions [] ("FUEL", 1120409)

-- | Add a given quantity of chemical to the bank.
refuel :: Chemical -> Int -> Ingredients -> Ingredients
refuel k x [] = [(k, x)]
refuel k x ((n, b):xs) | n == k     = (n, b + x):xs
                       | otherwise  = (n, b):refuel k x xs

-- | Remove the available chemical quantity from the bank,
-- | then return whether more chemical needs to be synthesized.
retrieve :: Ingredients -> Compound -> (Ingredients, Maybe Int)
retrieve [] (c, x) = ([], Just x)
retrieve ((c, a):xs) s@(n, q) | n == c && q <= a = ((c, a - q):xs, Nothing)
                              | n == c         = ((c, 0):xs, Just (q - a))
                              | otherwise     =
                                    let (ing', r) = retrieve xs s in ((c, a):ing', r)

-- | Given a set of available reactions and available compounds,
-- | synthesizes compound and returns the resulting bank and amount of ORE necessary
synthesize :: [Reaction] -> Ingredients -> Compound -> (Ingredients, Int)
synthesize _ bank ("ORE", x) = (bank, x)
synthesize rules bank q@(name, _) =
    case retrieve bank q of
        (bank', Nothing) -> (bank', 0)
        (bank', Just x)  ->
            let Just (n, deps) = lookup name rules
                multiplier     = ceiling ((fromIntegral x) / (fromIntegral n) :: Double)
                bank           = refuel name (multiplier * n - x) bank'
            in  foldl (\(bank, ore) (c, q) ->
                    let (bank', ore') = synthesize rules bank (c, multiplier * q)
                    in (bank', ore + ore')) (bank, 0) deps
