module AKDatabase where

import AKDownload
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad(when)

conn = do
	d <- connectSqlite3 "test3.db"
	prepDB d
	return d

prepDB :: IConnection conn => conn -> IO ()
prepDB dbh =
    do tables <- getTables dbh
       when (not ("movies" `elem` tables)) $
           do run dbh "CREATE TABLE movies (\
                       \id TEXT NOT NULL PRIMARY KEY,\
                       \title TEXT NOT NULL,\
			\year INTEGER NOT NULL,\
			\synopsis TEXT,\
			\)" []
              return ()
       when (not ("cast" `elem` tables)) $
           do run dbh "CREATE TABLE cast (\
                       \id TEXT NOT NULL PRIMARY KEY,\
                       \name TEXT NOT NULL)" []
              return ()
       when (not ("cast_bridge" `elem` tables)) $
           do run dbh "CREATE TABLE cast_bridge (\
                       \movie_id TEXT NOT NULL,\
                       \cast_id TEXT NOT NULL)" []
              return ()
       commit dbh


--addData :: IConnection conn => conn -> Podcast -> IO Podcast
addData bO= 
	handleSql errorHandler $
		do -- Insert the castURL into the table.  The database
         -- will automatically assign a cast ID.
--	 let dbh = conn
			d <- connectSqlite3 "test3.db"
			prepDB d
			check <- getMovieData d $ AKDownload.id bO
			case check of
				[] -> (run d "INSERT INTO movies (id, title, year, synopsis) VALUES (?,?,?,?)"
					[toSql (AKDownload.id bO), toSql (AKDownload.title bO),
						toSql (AKDownload.year bO), toSql (AKDownload.synopsis bO)])
				[x] -> return 0
			commit d
			disconnect d
			mapM (addCastData (AKDownload.id bO)) $ AKDownload.abridged_cast bO
         -- Find out the castID for the URL we just added.
		where errorHandler e = 
			do fail $ "Error adding podcast; does this URL already exist?\n" ++ show e

addCastData movie_id cast = print ((AKDownload.name cast) ++ movie_id)

getMovieData dbh movieId = 
	do 
	r <- quickQuery' dbh "SELECT * from movies where id = ?" [toSql (movieId)]
	return r
