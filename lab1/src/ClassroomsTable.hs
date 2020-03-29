{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module ClassroomsTable where

import CRUD
import Database.MySQL.Simple


data ClassroomsTable = ClassroomsTable {classroomsTableId :: Int, capacity :: Int, floorNumber :: Int}

defaultClassroomsTable :: ClassroomsTable
defaultClassroomsTable = ClassroomsTable {classroomsTableId = 0, capacity = 0, floorNumber = 0}


classroomsRowToString :: (Int, Int, Int) -> [String]
classroomsRowToString (id, capacity, floor) = (show id) : (show capacity) : [show floor]

classroomsTableToString :: [(Int, Int, Int)] -> [[String]]
classroomsTableToString (x:other) = (classroomsRowToString x) : (classroomsTableToString other)
classroomsTableToString []        = []


instance CRUD ClassroomsTable where
  input table = do
    capacityRes <- readInt "capacity"
    floorRes    <- readInt "floor"
    return table {capacity = capacityRes, floorNumber = floorRes}

  readTable table connection = do
    (rs :: [(Int, Int, Int)]) <- query_ connection "SELECT classroom_id, capacity, floor FROM classrooms"
    return (("classroom_id":"capacity":["floor"]) : (classroomsTableToString rs))

  create table connection = do
    _ <- execute connection 
            "INSERT INTO classrooms (capacity, floor) VALUES (?, ?)"
            (capacity table, floorNumber table)
    putStrLn "Inserted"

  readRow table connection id = do
    (rs :: [(Int, Int, Int)]) <- query connection 
                                   "SELECT classroom_id, capacity, floor FROM classrooms where classroom_id = ?"
                                   [id]
    case length rs of
      0 -> return Nothing
      _ -> do
        let row = rs !! 0
        let (crId, cap, floor) = row
        return (Just ClassroomsTable {classroomsTableId = crId, capacity = cap, floorNumber = floor})

  update table connection = do
    _ <- execute connection
            "UPDATE classrooms SET capacity = ?, floor = ? WHERE classroom_id = ?"
            (capacity table, floorNumber table, classroomsTableId table)
    putStrLn "Updated"

  delete table connection = do
    _ <- execute connection
            "DELETE FROM classrooms WHERE classroom_id = ?"
            [classroomsTableId table]
    putStrLn "Deleted"
    
  canChangeTable table connection permission _ = do
    if permission == False
      then return False
      else do
        [Only (addedSeats :: Int)] <- query connection 
                                "SELECT COUNT(*) FROM seats WHERE classroom_id = ?"
                                [classroomsTableId table]
        let enoughSeats = (addedSeats < (capacity table))
        if enoughSeats == False
          then putStrLn "ERROR: Classroom capacity is too small."
          else return()
        return enoughSeats
