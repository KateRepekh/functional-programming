module Database where

import CRUD
import UserTable
import ClassroomsTable
import SeatsTable
import ClassesTable
import UsersLessonsTable
import LessonsTable
import Database.MySQL.Simple


data Command = NoCommand | Create | Read | Update | Delete | Exit deriving (Eq, Show, Enum, Read)

readCommand :: IO(Command)
readCommand = do
  askFor "command"
  s <- getLine
  let res = maybeRead s :: Maybe Command
  case res of
    Just x -> return x
    Nothing -> do
      putStrLn "Possible values: Create | Read | Update | Delete | Exit"
      readCommand

data FacultyTable = NoTable | Classrooms | Classes | Users | Lessons | Seats | UsersLessons deriving (Eq, Show, Enum, Read)

readFacultyTable :: IO(FacultyTable)
readFacultyTable = do
  askFor "table"
  s <- getLine
  let res = maybeRead s :: Maybe FacultyTable
  case res of
    Just x -> return x
    Nothing -> do
      putStrLn "Possible values: Classrooms | Classes | Users | Lessons | Seats | UsersLessons"
      readFacultyTable


data FacultyDB = FacultyDB {connection       :: Connection, 
                            activeUserStatus :: UserStatus, 
                            activeUserId     :: Int,
                            curTable         :: FacultyTable}

defaultFacultyDB :: IO FacultyDB
defaultFacultyDB = do 
  conn <- connect defaultConnectInfo { connectUser = "classrooms_db_user", connectDatabase = "faculty_classrooms_db"}
  return FacultyDB {connection = conn, activeUserStatus = NoStatus, activeUserId = 0, curTable = NoTable}

login :: FacultyDB -> IO FacultyDB
login db = do
  (userId, userStatus) <- readPermissionByCredentials defaultUserTable (connection db)
  return db {activeUserStatus = userStatus, activeUserId = userId}

checkPermission :: Command -> UserStatus -> Bool
checkPermission command status
  | (status == Teacher) && (command == Create) = True
  | command == Read                            = True
  | command == Exit                            = True
  | status  == Admin                           = True
  | otherwise                                  = False

isOwningRowsPossible :: UserStatus -> FacultyTable -> Bool
isOwningRowsPossible status table
  | ((table == Classes) || (table == Lessons)) && (status == Teacher) = True
  | (table == Users)    || (table == UsersLessons)                    = True
  | otherwise                                                         = False

editTable :: (CRUD table) => table -> Connection -> Command -> Bool -> Int -> IO()
editTable table connection command permission activeUserId = do
  case command of
    Delete -> do
      rowId <- readInt "row_id"
      maybeTable <- readRow table connection rowId
      case maybeTable of
        Nothing -> putStrLn "ERROR: Incorrect row id."
        Just table -> change delete table connection permission activeUserId
    Update -> do
      rowId <- readInt "row_id"
      maybeTable <- readRow table connection rowId
      case maybeTable of
        Nothing -> putStrLn "ERROR: Incorrect row id."
        Just table -> do 
          editPermission <- canChangeTable table connection permission activeUserId
          if editPermission
            then do
              table <- input table
              change update table connection permission activeUserId
            else putStrLn "ERROR: Permission denied."
    Create -> do
      table <- input table
      change create table connection permission activeUserId

work :: FacultyDB -> IO()
work db = do
  command <- readCommand
  if command == Exit
    then putStrLn "------------------------------------ BYE ------------------------------------"
    else do
      if ((curTable db) == NoTable) || (command == Read)
        then do
          table <- readFacultyTable
          case table of
            Users        -> showTable defaultUserTable         (connection db)
            Classrooms   -> showTable defaultClassroomsTable   (connection db)
            Seats        -> showTable defaultSeatsTable        (connection db)
            Classes      -> showTable defaultClassesTable      (connection db)
            Lessons      -> showTable defaultLessonsTable      (connection db)
            UsersLessons -> showTable defaultUsersLessonsTable (connection db)
          work db {curTable = table}
        else do
          let mainPermission = (checkPermission command (activeUserStatus db))
          let possiblePermission = (isOwningRowsPossible (activeUserStatus db) (curTable db))
          if (mainPermission || possiblePermission) == False
            then putStrLn "ERROR: Permission denied. You can't edit this table."
            else do
              case (curTable db) of
                Users        -> editTable defaultUserTable         (connection db) command mainPermission (activeUserId db)
                Classrooms   -> editTable defaultClassroomsTable   (connection db) command mainPermission (activeUserId db)
                Seats        -> editTable defaultSeatsTable        (connection db) command mainPermission (activeUserId db)
                Classes      -> editTable defaultClassesTable      (connection db) command mainPermission (activeUserId db)
                Lessons      -> editTable defaultLessonsTable      (connection db) command mainPermission (activeUserId db)
                UsersLessons -> editTable defaultUsersLessonsTable (connection db) command mainPermission (activeUserId db)
          work db
