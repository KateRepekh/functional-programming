{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module CRUD where

import Database.MySQL.Simple
import Control.Exception
import TablesPrint

class CRUD a where
  create :: a -> Connection -> IO()
  readTable :: a -> Connection -> IO([[String]])
  readRow :: a -> Connection -> Int -> IO(Maybe a)
  update :: a -> Connection -> IO()
  delete :: a -> Connection -> IO()
  canChangeTable :: a -> Connection -> Bool -> Int -> IO(Bool)
  input :: a -> IO(a)


showTable :: (CRUD a) => a -> Connection -> IO()
showTable table connection = do
  tableStrings <- readTable table connection
  printTable tableStrings


change :: (CRUD a) => (a -> Connection -> IO()) -> a -> Connection -> Bool -> Int -> IO()
change func table connection permission userId = do
  finalPermission <- try(canChangeTable table connection permission userId) :: IO (Either IOError Bool)
  case finalPermission of
    Left err              -> putStrLn "ERROR: Id doesn't exist."
    Right finalPermission -> case finalPermission of
                               True  -> func table connection 
                               False -> putStrLn "ERROR: Permission denied."

askFor :: String -> IO()
askFor str = putStrLn (str ++ " >")

readString :: String -> IO(String)
readString inputName = do
  askFor inputName
  getLine

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
    [(x, _)] -> Just x
    _        -> Nothing

readInt :: String -> IO(Int)
readInt inputName = do
  askFor inputName
  s <- getLine
  let res = maybeRead s :: Maybe Int
  case res of
    Just x -> return x
    Nothing -> do
      putStrLn "ERROR: You have to enter an integer."
      readInt inputName