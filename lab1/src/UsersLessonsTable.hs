{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module UsersLessonsTable where

import CRUD
import Database.MySQL.Simple

data UsersLessonsTable = UsersLessonsTable {usersLessonsTableId :: Int, scheduleId :: Int, seatId :: Int, userId :: Int}

defaultUsersLessonsTable :: UsersLessonsTable
defaultUsersLessonsTable = UsersLessonsTable {usersLessonsTableId = 0, scheduleId = 0, seatId = 0, userId = 0}


usersLessonsRowToString :: (Int, Int, Int, Int) -> [String]
usersLessonsRowToString (id, seatId, lessonId, userId) = (show id) : (show lessonId) : (show seatId) : [show userId]

usersLessonsTableToString :: [(Int, Int, Int, Int)] -> [[String]]
usersLessonsTableToString (x:other) = (usersLessonsRowToString x) : (usersLessonsTableToString other)
usersLessonsTableToString []        = []


instance CRUD UsersLessonsTable where
  input table = do
    lessonIdRes <- readInt "lesson_id"
    seatIdRes   <- readInt "seat_id"
    userIdRes   <- readInt "user_id"
    return table {scheduleId = lessonIdRes, seatId = seatIdRes, userId = userIdRes}

  readTable table connection = do
    (rs :: [(Int, Int, Int, Int)]) <- query_ connection "SELECT user_to_lesson_id, lesson_id, seat_id, user_id FROM users_to_lessons"
    return (("user_to_lesson_id":"lesson_id":"seat_id":["user_id"]) : (usersLessonsTableToString rs))

  create table connection = do
    _ <- execute connection 
           "INSERT INTO users_to_lessons (lesson_id, seat_id, user_id) VALUES (?, ?, ?)"
           (scheduleId table, seatId table, userId table)
    putStrLn "Inserted"

  readRow table connection id = do
    (rs :: [(Int, Int, Int, Int)]) <- query connection 
                                        "SELECT user_to_lesson_id, lesson_id, seat_id, user_id FROM users_to_lessons where user_to_lesson_id = ?"
                                        [id]
    case length rs of
      0 -> return Nothing
      _ -> do
        let row = rs !! 0
        let (ulId, lId, sId, uId) = row
        return (Just UsersLessonsTable {usersLessonsTableId = ulId, scheduleId = lId, seatId = sId, userId = uId})

  update table connection = do
    _ <- execute connection
           "UPDATE users_to_lessons SET lesson_id = ?, seat_id = ?, user_id = ? WHERE user_to_lesson_id = ?"
           (scheduleId table, seatId table, userId table, usersLessonsTableId table)
    putStrLn "Updated"

  delete table connection = do
    _ <- execute connection
          "DELETE FROM users_to_lessons WHERE user_to_lesson_id = ?"
          [usersLessonsTableId table]
    putStrLn "Deleted"

  canChangeTable table connection permission uId = do
    if (permission == False) && ((userId table) /= uId)
      then return False
      else do
        [Only (lessonRoomId :: Int)] <- query connection
                                        "SELECT classroom_id FROM lessons WHERE lesson_id = ?"
                                        [scheduleId table]
        [Only (seatRoomId :: Int)] <- query connection
                                      "SELECT classroom_id FROM seats WHERE seat_id = ?"
                                      [seatId table]
        if lessonRoomId /= seatRoomId
          then do
            putStrLn "ERROR: The seat is not in the room where the lesson takes place."
            return False
          else do
            [Only (freeInt :: Int)] <- query connection
                                       "SELECT COUNT(*) = 0 FROM \
                                       \(SELECT day_of_week, time_slot FROM users_to_lessons \
                                       \JOIN lessons ON users_to_lessons.lesson_id = lessons.lesson_id \
                                       \AND users_to_lessons.user_id = ? AND user_to_lesson_id <> ?) AS busy_time \
                                       \JOIN \
                                       \(SELECT day_of_week, time_slot FROM lessons \
                                       \WHERE lessons.lesson_id = ?) AS new_lesson \
                                       \ON busy_time.day_of_week = new_lesson.day_of_week AND busy_time.time_slot = new_lesson.time_slot"
                                       (userId table, usersLessonsTableId table, scheduleId table)
            let free = (freeInt == 1)
            if free == False
              then putStrLn "ERROR: You have another lesson at the same time."
              else return ()
            return free


    

