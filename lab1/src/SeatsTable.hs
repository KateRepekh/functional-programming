{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module SeatsTable where

import CRUD
import Database.MySQL.Simple

data Material = Wood | Plastic | Cotton | Leather | Vynil | Metal | NoMaterial deriving (Eq, Show, Enum, Read)

readMaterial :: IO(Material)
readMaterial = do
  askFor "material"
  s <- getLine
  if s == ""
    then return Wood
    else do
      let res = maybeRead s :: Maybe Material
      case res of
        Just x -> return x
        Nothing -> do
          putStrLn "Possible values: Wood | Plastic | Cotton | Leather | Vynil | Metal"
          readMaterial


data SeatsTable = SeatsTable {seatsTableId :: Int, roomId :: Int, material :: Material}
defaultSeatsTable :: SeatsTable
defaultSeatsTable = SeatsTable {seatsTableId = 0, roomId = 0, material = NoMaterial}


seatsRowToString :: (Int, Int, String) -> [String]
seatsRowToString (id, roomId, material) = (show id) : (show roomId) : [show material]

seatsTableToString :: [(Int, Int, String)] -> [[String]]
seatsTableToString (x:other) = (seatsRowToString x) : (seatsTableToString other)
seatsTableToString []        = []


instance CRUD SeatsTable where
  input table = do
    classroomIdRes <- readInt "classroom_id"
    materialRes    <- readMaterial
    return table {roomId = classroomIdRes, material = materialRes}
  
  readTable table connection = do
    (rs :: [(Int, Int, String)]) <- query_ connection "SELECT seat_id, classroom_id, material FROM seats"
    return (("seat_id":"classroom_id":["material"]) : (seatsTableToString rs))

  create table connection = do
    _ <- execute connection 
           "INSERT INTO seats (classroom_id, material) VALUES (?, ?)"
           (roomId table, show (material table))
    putStrLn "Inserted"

  readRow table connection id = do
    (rs :: [(Int, Int, String)]) <- query connection 
                                      "SELECT seat_id, classroom_id, material FROM seats where seat_id = ?"
                                      [id]
    case length rs of
      0 -> return Nothing
      _ -> do
        let row = rs !! 0
        let (sId, rId, mat) = row
        return (Just SeatsTable {seatsTableId = sId, roomId = rId, material = (read mat :: Material)})

  update table connection = do
    _ <- execute connection
            "UPDATE seats SET classroom_id = ?, material = ? WHERE seat_id = ?"
            (roomId table, show (material table), seatsTableId table)
    putStrLn "Updated"

  delete table connection = do
    _ <- execute connection
            "DELETE FROM seats WHERE classroom_id = ?"
            [seatsTableId table]
    putStrLn "Deleted"

  canChangeTable table connection permission _ = do
    if permission == False 
      then return False
      else do
        [Only (existsInt :: Int)] <- query connection 
                                       "SELECT count(*) = 0 FROM classrooms WHERE classroom_id = ?"
                                       [roomId table]
        let exists = (existsInt == 1)
        if exists == False
          then do
            putStrLn "ERROR: Classroom doesn't exist."
            return False
          else do
            [Only (addedSeats :: Int)] <- query connection 
                                            "SELECT COUNT(*) FROM seats WHERE classroom_id = ? AND seat_id <> ?"
                                            (roomId table, seatsTableId table)
            [Only (maxSeats :: Int)] <- query connection 
                                          "SELECT capacity FROM classrooms WHERE classroom_id = ?"
                                          [roomId table]
            let enoughSeats = (addedSeats < maxSeats)
            if enoughSeats == False
              then putStrLn "ERROR: Classroom capacity is too small."
              else return()
            return enoughSeats
    
