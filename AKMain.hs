import AKDownload
import AKDatabase

insertInDB = do
	d <- boxOffice
	let a = movies d
	mapM callInsertQuery a
	--return a

callInsertQuery bO = addData bO

