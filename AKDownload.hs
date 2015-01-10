{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module AKDownload where

import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import GHC.Generics

data Movies = Movies {
	movies :: [Movie]
} deriving (Show,Generic)

data Movie = Movie {
	id :: String
	, title :: String
	, year :: Int
	, synopsis :: String
	, abridged_cast :: [Cast]
} deriving (Show,Generic)

type Characters = [String]

data Cast = Cast {
	name :: String
	, cast_id :: String
	, characters :: Maybe Characters
} deriving (Show,Generic)

instance FromJSON Cast where
	parseJSON (Object v) = Cast <$>
		v .: "name" <*>
		v .: "id" <*>
		v .:? "characters"
	parseJSON _ = mzero

instance ToJSON Cast where
     toJSON (Cast name cast_id characters) = object ["name" .= name, "id" .= cast_id, "characters" .= characters]

instance FromJSON ReleaseDates
instance ToJSON ReleaseDates

instance FromJSON Movie
instance ToJSON Movie

instance FromJSON Movies
instance ToJSON Movies

-- the URL to get the data
url1 = "http://api.rottentomatoes.com/api/public/v1.0/lists/movies/box_office.json?apikey=mb8ewcmfn82ejdf85ppzb87p"
url2 = "http://api.rottentomatoes.com/api/public/v1.0/lists/movies/in_theaters.json?apikey=mb8ewcmfn82ejdf85ppzb87p"
url3 = "http://api.rottentomatoes.com/api/public/v1.0/lists/movies/opening.json?apikey=mb8ewcmfn82ejdf85ppzb87p"

boxOffice :: IO Movies
boxOffice = do
        d <- (eitherDecode <$> (simpleHttp url1)) :: IO (Either String Movies)
        case d of
		Right ps -> return ps

inTheaters :: IO Movies
inTheaters = do
        d <- (eitherDecode <$> (simpleHttp url2)) :: IO (Either String Movies)
        case d of
		Right ps -> return ps

opening :: IO Movies
opening = do
        d <- (eitherDecode <$> (simpleHttp url3)) :: IO (Either String Movies)
	case d of
		Right ps -> return ps

