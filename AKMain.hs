import AKDownload
import AKDatabase
import Data.List (nub)


callInsert :: IO ()
callInsert =
	do
	data1 <- insertInDB boxOffice
	data2 <- insertInDB inTheaters
	data3 <- insertInDB opening
	let allMovies = data1 ++ data2 ++ data3
	print "Movie Titles added/updated in the Database :-"
	mapM_ print $ nub allMovies

getCastInMovie :: IO ()
getCastInMovie =
	do
	cast_data <- getCastInAllMovieFromDB
	let stringRows = map showCastData cast_data
	mapM_ putStr stringRows

getMovieByCast :: String -> IO ()
getMovieByCast castName = 
	do
	movieNames <- getMovieByCastFromDB castName
	let stringRows = map showMovieNamesByCast movieNames
	mapM_ putStr stringRows

getAllInfoByMovie :: String -> IO ()
getAllInfoByMovie movieTitle = 
	do
	movieInfo <- getAllInfoByMovieFromDB movieTitle
	let stringRows = map showAllMovieInfo movieInfo
	mapM_ putStr stringRows

getCoStars :: String -> IO ()
getCoStars castName = 
	do
	coStarsInfo <- getCoStarsFromDB castName
	let stringRows = map showCoStarsInfo coStarsInfo
	mapM_ putStr stringRows

