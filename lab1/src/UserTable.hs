{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module UserTable where

import CRUD
import Database.MySQL.Simple


data UserStatus = Admin | Teacher | Student | NoStatus deriving (Eq, Show, Enum, Read)

readUserStatus :: IO(UserStatus)
readUserStatus = do
  askFor "status"
  s <- getLine
  if s == ""
    then return Student
    else do
      let res = maybeRead s :: Maybe UserStatus
      case res of
        Just x -> return x
        Nothing -> do
          putStrLn "Possible values: Admin | Teacher | Student"
          readUserStatus


data UserTable = UserTable {userTableId :: Int, userStatus :: UserStatus,
                            username :: String, password :: String, 
                            firstName :: String, lastName :: String}

defaultUserTable :: UserTable
defaultUserTable = UserTable {userTableId = 0, userStatus = NoStatus, username = "", password = "", firstName = "", lastName = ""}


userRowToString :: (Int, String, String, String, String) -> [String]
userRowToString (id, status, username, firstName, lastName) = (show id) : (show status) : (show username) : (show firstName) : [show lastName]

userTableToString :: [(Int, String, String, String, String)] -> [[String]]
userTableToString (x:other) = (userRowToString x) : (userTableToString other)
userTableToString []        = []


instance CRUD UserTable where
  input table = do
    usernameStr  <- readString "username"
    passwordStr  <- readString "password"
    firstNameStr <- readString "first_name"
    lastNameStr  <- readString "last_name"
    return table {username = usernameStr, password = passwordStr, firstName = firstNameStr, lastName = lastNameStr}

  readTable table connection = do
    (rs :: [(Int, String, String, String, String)]) <- query_ connection "SELECT user_id, status, username, first_name, last_name FROM faculty_users"
    return (("user_id":"status":"username":"first_name":["last_name"]) : (userTableToString rs))

  create table connection = do
    status <- readUserStatus
    _ <- execute connection 
            "INSERT INTO faculty_users (status, username, password, first_name, last_name) VALUES (?, ?, ?, ?, ?)"
            (show status, username table, password table, firstName table, lastName table)
    putStrLn "Inserted"

  readRow table connection id = do
    (rs :: [(Int, String, String, String, String, String)]) <- query connection 
                                                                  "SELECT user_id, status, username, password, first_name, last_name FROM faculty_users where user_id = ?"
                                                                  [id]
    case length rs of
      0 -> return Nothing
      _ -> do
        let row = rs !! 0
        let (uId, stat, uName, pass, fName, lName) = row
        return (Just UserTable {userTableId = uId, userStatus = (read stat :: UserStatus), 
                                username = uName, password = pass, firstName = fName, lastName = lName})

  update table connection = do
    _ <- execute connection
            "UPDATE faculty_users SET status = ?, username = ?, password = ?, first_name = ?, last_name = ? WHERE user_id = ?"
            (show (userStatus table), username table, password table, firstName table, lastName table, userTableId table)
    putStrLn "Updated"

  delete table connection = do
    _ <- execute connection
            "DELETE FROM faculty_users WHERE user_id = ?"
            [userTableId table]
    putStrLn "Deleted"

  canChangeTable table _ permission userId = do
    return (permission || (userId == (userTableId table)))
    
readPermissionByCredentials :: UserTable -> Connection -> IO((Int, UserStatus))
readPermissionByCredentials table connection = do
  usernameStr <- readString "username"
  passwordStr <- readString "password"
  (rs :: [(Int, String)]) <- query connection 
                               "SELECT user_id, status FROM faculty_users WHERE username = ? AND password = ?"
                               (usernameStr, passwordStr)
  if (length rs) == 0
    then do
      putStrLn "ERROR: Incorrect username and/or password."
      res <- (readPermissionByCredentials table connection)
      return res
    else do
      let row = rs !! 0
      let (userId, status) = row
      return (userId, read status :: UserStatus)