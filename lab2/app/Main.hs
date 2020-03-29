module Main
where

import SimpsonIntegration

func :: Double -> Double
func x = (x * x + 2.0 * x + 1.0)

main = do
    putStrLn "a: "
    a_input <- getLine
    let a = (read a_input :: Double)
    putStrLn "b: "
    b_input <- getLine
    let b = (read b_input :: Double)
    putStrLn "Desired precision: "
    input <- getLine
    let eps = (read input :: Double)
    let start = (min a b)
    let finish = (max a b)
    print (integrate func (start, finish) eps)
