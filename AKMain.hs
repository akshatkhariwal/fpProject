import AKDownload
import AKDatabase

insertInDB :: IO Movies -> IO ()
insertInDB jsonData = do
	d <- jsonData
	let a = movies d
	returnData <- mapM addData a
	print "Movie Titles added/updated in the Database :-"
	print returnData

