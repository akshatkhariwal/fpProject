import AKDownload
import AKDatabase

--insertInDB (movieData:[]) = insertQuery movieData
--insertInDB (movieData:movieDataList) = insertQuery movieData : insertInDB movieDataList

vari = do
	d <- boxOffice
	let a = movies d
	mapM decodeBoxOffice a
	--return a

--insertQuery = decodeBoxOffice vari

--decodeBoxOffice (bO:[]) = print $ AKDownload.id bO
--decodeBoxOffice (bO:bOs) = print $ AKDownload.id bO : decodeBoxOffice bOs

decodeBoxOffice bO = addData bO

