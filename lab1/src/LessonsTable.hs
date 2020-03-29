{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, BlockArguments #-}

module LessonsTable where

import CRUD
import Database.MySQL.Simple
import Data.Strings (strDrop, strReplace)

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday | NoDay deriving (Eq, Show, Enum, Read)

readDayOfWeek :: IO(DayOfWeek)
readDayOfWeek = do
  askFor "day_of_week"
  s <- getLine
  let res = maybeRead s :: Maybe DayOfWeek
  case res of
    Just x -> return x
    Nothing -> do
      putStrLn "Possible values: Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday"
      readDayOfWeek


data TimeSlot = S7_8 | S8_9 | S9_10 | S10_11 | S11_12 | S12_13 | S13_14 | S14_15 | S15_16 | S16_17 | S17_18 | S18_19 | S19_20 | S0 deriving (Eq, Show, Enum, Read)

showTimeSlot :: TimeSlot -> String
showTimeSlot slot = strReplace "_" "-" (strDrop 1 (show slot))

adaptTimeSlot :: String -> String
adaptTimeSlot slot = ("S" ++ (strReplace "-" "_" slot))

readTimeSlot :: IO(TimeSlot)
readTimeSlot = do
  askFor "time_slot"
  userString <- getLine
  let s = (adaptTimeSlot userString)
  let res = maybeRead s :: Maybe TimeSlot
  case res of
    Just x -> return x
    Nothing -> do
      putStrLn "Possible values: 7-8 | 8-9 | 9-10 | 10-11 | 11-12 | 12-13 | 13-14 | 14-15 | 15-16 | 16-17 | 17-18 | 18-19 | 19-20"
      readTimeSlot

data LessonsTable = LessonsTable {lessonsTableId :: Int, classId :: Int, classroomId :: Int,
                                  dayOfWeek :: DayOfWeek, timeSlot :: TimeSlot}

defaultLessonsTable :: LessonsTable
defaultLessonsTable = LessonsTable {lessonsTableId = 0, classId = 0, classroomId = 0, dayOfWeek = NoDay, timeSlot = S0}


lessonsRowToString :: (Int, Int, Int, String, String) -> [String]
lessonsRowToString (id, classId, roomId, dayOfWeek, timeSlot) = (show id) : (show classId) : (show roomId) : (show dayOfWeek) : [show timeSlot]

lessonsTableToString :: [(Int, Int, Int, String, String)] -> [[String]]
lessonsTableToString (x:other) = (lessonsRowToString x) : (lessonsTableToString other)
lessonsTableToString []        = []


instance CRUD LessonsTable where
  input table = do
    classIdRes     <- readInt "class_id"
    classroomIdRes <- readInt "classroom_id"
    dayOfWeekRes   <- readDayOfWeek
    timeSlotRes    <- readTimeSlot
    return table {classId = classIdRes, classroomId = classroomIdRes, dayOfWeek = dayOfWeekRes, timeSlot = timeSlotRes}

  readTable table connection = do
    (rs :: [(Int, Int, Int, String, String)]) <- query_ connection 
                                                   "SELECT lesson_id, class_id, classroom_id, day_of_week, time_slot FROM lessons"
    return (("lesson_id":"class_id":"classroom_id":"day_of_week":["time_slot"]) : (lessonsTableToString rs))

  create table connection = do
    _ <- execute connection 
            "INSERT INTO lessons (class_id, classroom_id, day_of_week, time_slot) VALUES (?, ?, ?, ?)"
            (classId table, classroomId table, show (dayOfWeek table), showTimeSlot (timeSlot table))
    putStrLn "Inserted"

  readRow table connection id = do
    (rs :: [(Int, Int, Int, String, String)]) <- query connection 
                                                   "SELECT lesson_id, class_id, classroom_id, day_of_week, time_slot FROM lessons where lesson_id = ?"
                                                   [id]
    case length rs of
      0 -> return Nothing
      _ -> do
        let row = rs !! 0
        let (lId, cId, crId, day, time) = row
        return (Just LessonsTable {lessonsTableId = lId, classId = cId, classroomId = crId,
                                   dayOfWeek = (read day :: DayOfWeek), timeSlot = (read (adaptTimeSlot time) :: TimeSlot)})

  update table connection = do
    _ <- execute connection
            "UPDATE lessons SET class_id = ?, classroom_id = ?, day_of_week = ?, time_slot = ? WHERE lesson_id = ?"
            (classId table, classroomId table, show (dayOfWeek table), showTimeSlot (timeSlot table), lessonsTableId table)
    putStrLn "Updated"

  delete table connection = do
    _ <- execute connection
            "DELETE FROM lessons WHERE lesson_id = ?"
            [lessonsTableId table]
    putStrLn "Deleted"

  canChangeTable table connection permission userId = do
    [Only (teacherId :: Int)] <- query connection
                                 "SELECT teacher_id FROM faculty_classes WHERE class_id = ?"
                                 [classId table]
    if (permission == False) && (teacherId /= userId)
      then return False
      else do
        [Only (freeInt :: Int)] <- query connection
                                   "SELECT COUNT(*) = 0 FROM lessons \
                                   \JOIN faculty_classes ON lessons.class_id = faculty_classes.class_id \
                                   \AND faculty_classes.teacher_id = ? AND day_of_week = ? AND time_slot = ? \
                                   \AND lesson_id <> ?"
                                   (teacherId, show (dayOfWeek table), showTimeSlot (timeSlot table), lessonsTableId table)
        let free = (freeInt == 1)
        if free == False
          then putStrLn "ERROR: You have another lesson at the same time."
          else return ()
        return free
    