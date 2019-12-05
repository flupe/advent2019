import Control.Monad ((>=>))
import Data.List.Split (splitOn)
import Data.Array.IO

type Program = IOUArray Int Int
data Mode = Pos | Imm deriving (Enum)

main :: IO ()
main = do
    input <- map read . splitOn "," . unlines . lines <$> readFile "inputs/day5"
    let prog = newListArray (0, length input) input

    prog >>= compute 1 >>= print
    prog >>= compute 5 >>= print

    where compute i = run 0 i 0

run :: Int -> Int -> Int -> Program -> IO Int
run i input output mem = do
    (op, modes) <- split <$> get i

    case op of
        x | x `elem` [1, 2, 7, 8] -> do
            let ma:mb:_ = modes
            a <- read ma (i + 1)
            b <- read mb (i + 2)
            c <- get (i + 3)

            let cond 1 = (+)
                cond 2 = (*)
                cond 7 = \a b -> fromEnum $ a < b
                cond 8 = \a b -> fromEnum $ a == b

            put c $ (cond x) a b
            run (i + 4) input output mem

        3 -> do
            let m:_ = modes
            d <- get (i + 1)
            put d input
            run (i + 2) input output mem

        4 -> do
            let m:_ = modes
            v <- read m (i + 1)
            run (i + 2) input v mem

        x | x `elem` [5, 6] -> do
            let ma:mb:_ = modes
            a <- read ma (i + 1)
            b <- read mb (i + 2)
            run (if (x == 5 && a /= 0 || x == 6 && a == 0) then b else i + 3) input output mem

        99 -> return output

    where
        get = readArray mem
        put = writeArray mem
        read Pos = get >=> get
        read Imm = get
        split x = (x `rem` 100, modes (x `div` 100))
        modes x = toEnum (x `rem` 10) : modes (x `div` 10)
