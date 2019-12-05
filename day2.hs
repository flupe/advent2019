{-# LANGUAGE BlockArguments #-}

import Data.List.Split (splitOn)
import Data.Array.IO

type Program = IOUArray Int Int

main :: IO ()
main = do
    input <- map read . splitOn "," <$> readFile "inputs/day2"
    let prog = newListArray (0, length input) input

    day1 prog >>= print
    day2 prog >>= print

day1 :: IO Program -> IO Int
day1 p = compute p 12 2

day2 :: IO Program -> IO Int
day2 p =
    foldr check (return 0) [ (x, y, compute p x y) | x <- [0 .. 99], y <- [0 .. 99] ]
    where
        check (x, y, res) rest = do
            res <- res
            if res == 19690720 then return (x * 100 + y)
            else rest

compute :: IO Program -> Int -> Int -> IO Int
compute p x y = do
    p <- p
    writeArray p 1 x
    writeArray p 2 y
    run p 0

run :: Program -> Int -> IO Int
run mem i = do
    op <- get i

    if op == 99 then get 0

    else do
        a <- get (i + 1)
        b <- get (i + 2)
        c <- get (i + 3)

        (if op == 1 then (+) else (*)) <$> get a <*> get b >>= put c

        run mem (i + 4)

    where
        get = readArray mem
        put = writeArray mem
