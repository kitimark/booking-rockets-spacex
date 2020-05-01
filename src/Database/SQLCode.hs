module Database.SQLCode where

code_user = "CREATE TABLE IF NOT EXISTS User( \ 
\ id INTEGER NOT NULL PRIMARY KEY, \ 
\ username varchar(256) not null unique, \ 
\ password varchar(256) not null);"

code_booking = "CREATE TABLE IF NOT EXISTS Booking ( \ 
\ id INTEGER NOT NULL PRIMARY KEY,\ 
\ userID INTEGER not null, \ 
\ flightNumber INTEGER not null, \ 
\ FOREIGN KEY (userID) REFERENCES User (id) ON DELETE CASCADE);"