module AKDatabase where

import AKDownload
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad(when)

conn = do
	d <- connectSqlite3 "test1.db"
	prepDB d
	--addData d
	return d

prepDB :: IConnection conn => conn -> IO ()
prepDB dbh =
    do tables <- getTables dbh
       when (not ("movies" `elem` tables)) $
           do run dbh "CREATE TABLE movies (\
                       \id TEXT NOT NULL PRIMARY KEY,\
                       \title TEXT NOT NULL)" []
              return ()
       commit dbh


--addData :: IConnection conn => conn -> Podcast -> IO Podcast
addData bO= 
    handleSql errorHandler $
      do -- Insert the castURL into the table.  The database
         -- will automatically assign a cast ID.
--	 let dbh = conn
	 d <- connectSqlite3 "test1.db"
	 prepDB d
         run d "INSERT INTO movies (id, title) VALUES (?,?)" [toSql (AKDownload.id bO), toSql (AKDownload.title bO)]
	 commit d
	 disconnect d
         -- Find out the castID for the URL we just added.
    where errorHandler e = 
              do fail $ "Error adding podcast; does this URL already exist?\n"
                     ++ show e


--getData dbh = 
--	do 
--	run' dbh "SELECT title FROM movies"
		--WHERE id = ?" [toSql ("123")]
	--show r
--	return r
