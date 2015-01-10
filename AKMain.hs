import AKDownload
import AKDatabase
import Database.HDBC
import Database.HDBC.Sqlite3

insertInDB :: IO Movies -> IO ()
insertInDB jsonData = do
	d <- jsonData
	let a = movies d
	returnData <- mapM addData a
	print "Movie Titles added/updated in the Database :-"
	print returnData

getCastInMovie =
	do
	cast_data <- getCastInMovieFromDB
	-- Convert each row into a String
	let stringRows = map convRow cast_data
	mapM_ putStr stringRows

--printCastData (m:c) = show ((fromSql m)::String) ++ "mo"

convRow :: [SqlValue] -> String
convRow [m, c] = 
              show "" ++ "\n" ++ "Movie: " ++ mString ++ " \nCast: " ++ cString ++ "\n"
              where mString = (fromSql m)::[Char]
                    cString = (fromSql c)::String
convRow x = fail $ "Unexpected result: " ++ show x

