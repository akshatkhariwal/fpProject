import AKDownload
import AKDatabase

insertInDB = do
	d <- boxOffice
	let a = movies d
	mapM addData a
