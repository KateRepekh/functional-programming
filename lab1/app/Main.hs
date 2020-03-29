module Main where

import Database

main :: IO ()
main = do
    db <- defaultFacultyDB
    db <- login db
    work db
