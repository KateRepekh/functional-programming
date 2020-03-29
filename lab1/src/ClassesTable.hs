{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module ClassesTable where

import CRUD
import Database.MySQL.Simple

data ClassesTable = ClassesTable {classesTableId :: Int, teacherId :: Int, className :: String}

defaultClassesTable :: ClassesTable
defaultClassesTable = ClassesTable {classesTableId = 0, teacherId = 0, className = ""}


classesRowToString :: (Int, Int, String) -> [String]
classesRowToString (id, teacherId, className) = (show id) : (show teacherId) : [show className]

classesTableToString :: [(Int, Int, String)] -> [[String]]
classesTableToString (x:other) = (classesRowToString x) : (classesTableToString other)
classesTableToString []        = []


instance CRUD ClassesTable where
  input table = do
    idRes   <- readInt "teacher_id"
    nameStr <- readString "class_name"
    return table {teacherId = idRes, className = nameStr}

  readTable table connection = do
    (rs :: [(Int, Int, String)]) <- query_ connection "SELECT class_id, teacher_id, class_name FROM faculty_classes"
    return (("class_id":"teacher_id":["class_name"]) : (classesTableToString rs))

  create table connection = do
    _ <- execute connection 
            "INSERT INTO faculty_classes (teacher_id, class_name) VALUES (?, ?)"
            (teacherId table, className table)
    putStrLn "Inserted"

  readRow table connection id = do
    (rs :: [(Int, Int, String)]) <- query connection 
                                   "SELECT class_id, teacher_id, class_name FROM faculty_classes where class_id = ?"
                                   [id]
    case length rs of
      0 -> return Nothing
      _ -> do
        let row = rs !! 0
        let (cId, tId, cName) = row
        return (Just ClassesTable {classesTableId = cId, teacherId = tId, className = cName})

  update table connection = do
    _ <- execute connection
            "UPDATE faculty_classes SET teacher_id = ?, class_name = ? WHERE class_id = ?"
            (teacherId table, className table, classesTableId table)
    putStrLn "Updated"

  delete table connection = do
    _ <- execute connection
            "DELETE FROM faculty_classes WHERE class_id = ?"
            [classesTableId table]
    putStrLn "Deleted"

  canChangeTable table connection permission userId = do
    if (permission || (userId == (teacherId table))) == False
      then return False
      else do
        [Only (isTeacherInt :: Int)] <- query connection 
                                    "SELECT status = 'Teacher' FROM faculty_users WHERE user_id = ?" 
                                    [userId]
        let isTeacher = (isTeacherInt == 1)
        if isTeacher == False
          then do 
            putStrLn "ERROR: Only teachers can have classes."
            return False
          else do
            [Only (uniqueInt :: Int)] <- query connection
                                          "SELECT COUNT(*) = 0 FROM \
                                          \(SELECT COUNT(*) lessons_at_the_same_time FROM \
                                          \(SELECT class_id, day_of_week, time_slot FROM lessons \
                                          \WHERE class_id = ? UNION \
                                          \(SELECT lessons.class_id, day_of_week, time_slot FROM lessons \
                                          \JOIN faculty_classes ON lessons.class_id = faculty_classes.class_id AND teacher_id = ?)) \
                                          \all_lessons GROUP BY time_slot, day_of_week) lessons_at_the_same_time_count \
                                          \WHERE lessons_at_the_same_time > 1"
                                          (classesTableId table, teacherId table)
            let unique = (uniqueInt == 1)
            if unique == False
              then putStrLn "ERROR: Can't have several lessons at the same time."
              else return ()
            return unique
    
