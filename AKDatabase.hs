module AKDatabase where

import AKDownload
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Maybe
import Control.Monad(when)

-- Name of the Database
dbName :: String
dbName = "movie.db"

-- Function to create table if table does not exists already.
prepDB :: IConnection conn => conn -> IO ()
prepDB dbh =
	-- get list of tables in the databse
    do tables <- getTables dbh
	-- if movies, cast and cast_bridge tables exist or not.
	-- if does not exists, then create the tables.
       when (not ("movies" `elem` tables)) $
           do run dbh "CREATE TABLE movies (\
                       \id TEXT NOT NULL PRIMARY KEY,\
                       \title TEXT NOT NULL,\
		       \year INTEGER NOT NULL,\
		       \synopsis TEXT\
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
                       \cast_id TEXT NOT NULL,\
		       \characters TEXT)" []
              return ()
       commit dbh

-- Function to get the List of data fetched from remote and insert one by one into database.
insertInDB :: IO Movies -> IO [String]
insertInDB jsonData = do
	-- create connection to the database.
	dbh <- connectSqlite3 dbName
	-- Confirm existence of tables
	prepDB dbh
	-- Extract Movies from IO Movies
	d <- jsonData
	-- get the list of Movie from movies
	let a = movies d
	-- insert data in database one by one
	-- mapM returns the return values in a list.
	returnData <- mapM (addData dbh) a
	-- terminate the connection with the database
	disconnect dbh
	-- return the returned data
	return returnData

-- function to add a single movie data in the database
addData :: IConnection conn => conn -> Movie -> IO String
addData d bO = 
	handleSql errorHandler $
		do
			-- check if the given movie id exists in the database or not
			check <- getMovieData d $ AKDownload.id bO
			case check of
				-- if it does not exist, then insert the values
				[] -> (run d "INSERT INTO movies (id, title, year, synopsis) VALUES (?,?,?,?)"
						[toSql (AKDownload.id bO), toSql (AKDownload.title bO),
							toSql (AKDownload.year bO), toSql (AKDownload.synopsis bO)])
				-- else update the existing data
				[x] -> (run d "UPDATE movies SET title = ?, year = ?, synopsis = ? WHERE id = ?"
						[toSql (AKDownload.title bO), toSql (AKDownload.year bO),
							toSql (AKDownload.synopsis bO), toSql (AKDownload.id bO)])
			-- Add cast into database
			mapM_ (addCastData d (AKDownload.id bO)) $ AKDownload.abridged_cast bO
			-- commit the changes
			commit d
			-- return the id and name of the movie inserted or updated
			return $ ((AKDownload.id bO) ++ " - " ++ (AKDownload.title bO))
		where errorHandler e = 
			do fail $ "Error adding data;\n" ++ show e

-- add Cast data into the database according to the movie
addCastData :: IConnection conn => conn -> String -> Cast -> IO Integer
addCastData dbh movie_id cast =
	do
	-- check if particular cast details already exist
	let charactersList = fromMaybe [""] $ AKDownload.characters cast
	let characters = unwords charactersList
	check <- getCastData dbh $ AKDownload.cast_id cast
	case check of
		-- if it does not exist, then insert the values
		[] -> (run dbh "INSERT INTO cast (id, name) VALUES (?,?)"
				[toSql (AKDownload.cast_id cast), toSql (AKDownload.name cast)])
		-- else update the existing data
		[x] -> (run dbh "UPDATE cast SET name = ? WHERE id = ?"
				[toSql (AKDownload.name cast), toSql (AKDownload.cast_id cast)])
	check <- getCastBridgeData dbh movie_id $ AKDownload.cast_id cast
	case check of
		-- if it does not exist, then insert the values
		[] -> (run dbh "INSERT INTO cast_bridge (movie_id, cast_id, characters) VALUES (?,?,?)"
				[toSql movie_id, toSql (AKDownload.cast_id cast), toSql characters])
		-- else update the existing data
		[x] -> (run dbh "UPDATE cast_bridge SET characters = ? WHERE cast_id = ? AND movie_id = ?"
				[toSql (characters), toSql (AKDownload.cast_id cast), toSql movie_id])

-- Function which executes a query to return data from movie table w.r.t movie id
getMovieData :: IConnection conn => conn -> String -> IO [[SqlValue]]
getMovieData dbh movieId = 
	do 
	r <- quickQuery' dbh "SELECT * from movies WHERE id = ?" [toSql movieId]
	return r

-- Function which executes a query to return data from cast table w.r.t. cast id
getCastData :: IConnection conn => conn -> String -> IO [[SqlValue]]
getCastData dbh cast_id = 
	do 
	r <- quickQuery' dbh "SELECT * from cast WHERE id = ?" [toSql cast_id]
	return r

-- Function to execute query to return data from cast_bridge table w.r.t movie id and cast id
getCastBridgeData :: IConnection conn => conn -> String -> String -> IO [[SqlValue]]
getCastBridgeData dbh movie_id cast_id = 
	do 
	r <- quickQuery' dbh "SELECT * from cast_bridge WHERE movie_id = ? AND cast_id = ?" [toSql movie_id, toSql cast_id]
	return r

-- Function to get all the Casts name by movie. Here we are using GROUP_CONCAT() to get the names of the cast in a single cell separated by comma
getCastInAllMovieFromDB :: IO [[SqlValue]]
getCastInAllMovieFromDB = 
	do
	d <- connectSqlite3 dbName
	r <- quickQuery' d "SELECT m.title, GROUP_CONCAT(c.name) FROM movies as m, cast as c, cast_bridge as b WHERE m.id = b.movie_id AND b.cast_id = c.id GROUP BY m.title" []
	disconnect d
	return r

-- Function to get the name of the movie and charcaters by the castName
getCharacterNameFromDB :: String -> IO [[SqlValue]]
getCharacterNameFromDB castName = 
	do
	d <- connectSqlite3 dbName
	r <- quickQuery' d "SELECT c.name, m.title, GROUP_CONCAT(b.characters) FROM movies as m, cast as c, cast_bridge as b WHERE m.id = b.movie_id AND b.cast_id = c.id AND c.name LIKE ? GROUP BY c.name" [toSql ("%" ++ castName ++ "%")]
	disconnect d
	return r

-- Function to get all the movies by the cast name.
-- Here we get the Name of the cast and the comma separated name of movies the person has acted in.
getMovieByCastFromDB :: String -> IO [[SqlValue]]
getMovieByCastFromDB castName = 
	do
	d <- connectSqlite3 dbName
	r <- quickQuery' d "SELECT c.name, GROUP_CONCAT(m.title) FROM movies as m, cast as c, cast_bridge as b WHERE m.id = b.movie_id AND b.cast_id = c.id AND c.name LIKE ? GROUP BY c.name" [toSql ("%" ++ castName ++ "%")]
	disconnect d
	return r

-- Function to get all the details of a movie.
getAllInfoByMovieFromDB :: String -> IO [[SqlValue]]
getAllInfoByMovieFromDB movieTitle = 
	do
	d <- connectSqlite3 dbName
	r <- quickQuery' d "SELECT m.id, m.title, m.year, m.synopsis, GROUP_CONCAT(c.name) FROM movies as m, cast as c, cast_bridge as b WHERE m.id = b.movie_id AND b.cast_id = c.id AND m.title LIKE ? GROUP BY m.id" [toSql ("%" ++ movieTitle ++ "%")]
	disconnect d
	return r

-- Function to get the Co-Stars of a cast by Movie name from DB
getCoStarsFromDB :: String -> IO [[SqlValue]]
getCoStarsFromDB castName = 
	do
	d <- connectSqlite3 dbName
	r <- quickQuery' d "SELECT c1.name, GROUP_CONCAT(c2.name), m.title FROM movies as m, cast as c1, cast as c2, cast_bridge as b1, cast_bridge as b2 where b1.movie_id = b2.movie_id AND b1.cast_id <> b2.cast_id AND c2.id = b2.cast_id AND b1.movie_id = m.id AND c1.id = b1.cast_id AND c1.name LIKE ? GROUP BY m.id" [toSql ("%" ++ castName ++ "%")]
	disconnect d
	return r

-- Function to make data returned by getMovieByCastFromDB printable on IO
showMovieNamesByCast :: [SqlValue] -> String
showMovieNamesByCast [c, m] =
	show "" ++ "\n" ++ "Cast: " ++ cString ++ " \nMovie(s): " ++ mString ++ "\n"
	where 
		mString = (fromSql m)::String
		cString = (fromSql c)::String
showMovieNamesByCast x = fail $ "Unexpected result: " ++ show x

-- Function to make data returned by getCastInAllMovieFromDB printable on IO
showCastData :: [SqlValue] -> String
showCastData [m, c] = 
	show "" ++ "\n" ++ "Movie: " ++ mString ++ " \nCast: " ++ cString ++ "\n"
	where 
		mString = (fromSql m)::String
		cString = (fromSql c)::String
showCastData x = fail $ "Unexpected result: " ++ show x

--Function to make data returned by getCoStarsFromDB printable on IO
showCoStarsInfo :: [SqlValue] -> String
showCoStarsInfo [c1, c2, m] = 
	show "" ++ "\n" ++ "Cast searched for: " ++ castName ++ " \nMovie: " ++ movieName ++ "\nCo-Stars: " ++ coStars ++ "\n"
	where 
		castName = (fromSql c1)::String
		coStars = (fromSql c2)::String
		movieName = (fromSql m)::String
showCoStarsInfo x = fail $ "Unexpected result: " ++ show x

-- Function to make data from getCharactersNameFromDB printable on IO
showCharacterInfo :: [SqlValue] -> String
showCharacterInfo [c1, m, c2] = 
	show "" ++ "\n" ++ "Cast searched for: " ++ castName ++ " \nMovie: " ++ movieName ++ "\nCharacter(s) played: " ++ charactersPlayed ++ "\n"
	where 
		castName = (fromSql c1)::String
		charactersPlayed = (fromSql c2)::String
		movieName = (fromSql m)::String
showCharacterInfo x = fail $ "Unexpected result: " ++ show x

-- Function to make data returned by getAllInfoByMovieFromDB printable on IO	
showAllMovieInfo :: [SqlValue] -> String
showAllMovieInfo [mID, mTitle, mYear, mSynopsis, mCast] =
	show "" ++ "\n" ++ "ID: " ++ mIDS ++ "\nTitle: " ++ mTitleS ++ "\nYear: " ++ mYearS ++ "\nCast: " ++ mCastS ++ "\nSynopsis: " ++ mSynopsisS ++ "\n"
	where
		mIDS = (fromSql mID)::String
		mTitleS = (fromSql mTitle)::String
		mYearS = (fromSql mYear)::String
		mSynopsisS = (fromSql mSynopsis)::String
		mCastS = (fromSql mCast)::String
showAllMovieInfo x = fail $ "Unexpected result: " ++ show x

