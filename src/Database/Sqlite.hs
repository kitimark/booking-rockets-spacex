{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}


module Database.Sqlite where

import System.Directory
import qualified System.IO 
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Maybe
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField
import Crypto.BCrypt
import qualified Data.ByteString.Char8 as B

data UserField = UserField
    { id :: Int 
    , username :: String
    , password ::String
    } deriving (Show)

instance FromRow UserField where
  fromRow = UserField <$> field <*> field <*> field

data BookField = BookField     
    { bookid :: Int 
    , userID :: Int
    , flightNumber :: Int
    } deriving (Show)

instance FromRow BookField where
  fromRow = BookField <$> field <*> field <*> field

-- instance FromField BookField where 
--     fromField UserField = Ok UserField
dropdb :: IO ()
dropdb = do 
    removeFile "data.db"
    putStrLn "data.db has been removed"

insertUser :: String -> String -> IO()
insertUser name pass = do
    conn <- open "data.db" 
    let inq = "INSERT INTO User ( username, password) VALUES(?, ?);"
    hash <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (B.pack pass)
    execute conn inq  ((name, B.unpack $ fromJust hash) :: (String, String))
    id1 <- lastInsertRowId conn
    putStrLn ("user id "++ show (id1) ++" has been added")
    close conn

insertBooking :: Int -> Int -> IO()
insertBooking userID flightNum = do
    conn <- open "data.db" 
    let inq = "INSERT INTO Booking ( userID, flightNumber) VALUES(?, ?);"
    execute conn inq  ((userID, flightNum) :: (Int, Int))
    id1 <- lastInsertRowId conn
    putStrLn ("id " ++ show (id1) ++ " booked to " ++ show(flightNum))
    close conn

initdb :: IO ()
initdb = do
    conn <- open "data.db" 
    let code_user = "CREATE TABLE IF NOT EXISTS User(\
                        \id INTEGER NOT NULL PRIMARY KEY,\
                        \username varchar(256) not null unique,\
                        \password varchar(256) not null);"
    let code_booking = "CREATE TABLE IF NOT EXISTS Booking (\
                        \id INTEGER NOT NULL PRIMARY KEY,\
                        \userID INTEGER not null,\
                        \flightNumber INTEGER not null,\
                        \FOREIGN KEY (userID) REFERENCES User (id) ON DELETE CASCADE);"
    execute_ conn code_user
    execute conn code_booking ()
    putStrLn "all table has been created"
    close conn


-------------------------------------- function query from user table --------------------------------------
queryUsers :: IO [UserField]
queryUsers = do 
    conn <- open "data.db" 
    let sql_code = "select * from User" 
    r <- query_ conn sql_code:: IO [UserField]
    close conn 
    return r

queryUserByID :: Int -> IO UserField
queryUserByID uid = do 
    conn <- open "data.db" 
    let sql_code = "select * from User where id = :qid" 
    [r] <- queryNamed conn sql_code [":qid" := (uid :: Int)]:: IO [UserField]
    close conn 
    return r

queryUserByUsername :: String -> IO UserField
queryUserByUsername name = do 
    conn <- open "data.db" 
    let sql_code = "select * from User where username = :qid" 
    [r] <- queryNamed conn sql_code [":qid" := (name :: String)]:: IO [UserField]
    close conn 
    return r

-------------------------------------- query from booking table --------------------------------------
queryBookingByID :: Int -> IO [BookField]
queryBookingByID bookingId = do
    conn <- open "data.db"
    let sql_code = "select * from Booking where id = :qid"
    r <- queryNamed conn sql_code [":qid" := (bookingId :: Int)]:: IO [BookField]
    close conn
    return r

queryBookingByUserID :: Int -> IO [BookField]
queryBookingByUserID userId = do
    conn <- open "data.db"
    let sql_code = "select * from Booking where userID = :qid"
    r <- queryNamed conn sql_code [":qid" := (userId :: Int)]:: IO [BookField]
    close conn
    return r

queryBookingByFlightID :: Int -> IO [BookField]
queryBookingByFlightID flightId = do
    conn <- open "data.db"
    let sql_code = "select * from Booking where flightNumber = :qid"
    r <- queryNamed conn sql_code [":qid" := (flightId :: Int)]:: IO [BookField]
    close conn
    return r

queryBookings :: IO [BookField]
queryBookings = do 
    conn <- open "data.db" 
    let sql_code = "select * from Booking" 
    r <- query_ conn sql_code :: IO [BookField]
    close conn
    return r
    
login :: String -> String -> IO ()
login user pass = do 
    conn <- open "data.db"
    [Only getque] <- liftIO $ query conn "select password from User where username = ?" (Only (user :: String)) :: IO [Only String]
    close conn
    if validatePassword (B.pack getque) (B.pack pass) 
        then print "successs"
        else print "fail"
    

