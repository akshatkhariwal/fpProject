import AKDownload
import AKDatabase
import Data.List (nub)

-- Function to initialize, insert and update data in the Database
callInsert :: IO ()
callInsert =
	do
	data1 <- insertInDB boxOffice
	data2 <- insertInDB inTheaters
	data3 <- insertInDB opening
	let allMovies = data1 ++ data2 ++ data3
	print "Movie Titles added/updated in the Database :-"
	mapM_ print $ nub allMovies

-- Function to get Cast in all the Movies
getCastInMovie :: IO ()
getCastInMovie =
	do
	cast_data <- getCastInAllMovieFromDB
	let stringRows = map showCastData cast_data
	mapM_ putStr stringRows

-- Function to get the names of movies a Cast has Acted in.
getMovieByCast :: String -> IO ()
getMovieByCast castName = 
	do
	movieNames <- getMovieByCastFromDB castName
	let stringRows = map showMovieNamesByCast movieNames
	mapM_ putStr stringRows

-- Function to get all the info about the given movie.
getAllInfoByMovie :: String -> IO ()
getAllInfoByMovie movieTitle = 
	do
	movieInfo <- getAllInfoByMovieFromDB movieTitle
	let stringRows = map showAllMovieInfo movieInfo
	mapM_ putStr stringRows

-- Function to get all the Co-Stars of the given actor by movie.
getCoStars :: String -> IO ()
getCoStars castName = 
	do
	coStarsInfo <- getCoStarsFromDB castName
	let stringRows = map showCoStarsInfo coStarsInfo
	mapM_ putStr stringRows

-- Function to get the names of charcaters played and movies by the name of actor.
getCharacterName :: String -> IO()
getCharacterName castName =
	do
	characterInfo <- getCharacterNameFromDB castName
	let stringRows = map showCharacterInfo characterInfo
	mapM_ putStr stringRows


