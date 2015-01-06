{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module AKDownload where

import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics

data Movies = Movies {
	movies :: [Movie]
} deriving (Show,Generic)

data Movie = Movie {
	id :: String
	, title :: String
	, year :: Int
	, mpaa_rating :: String
	, runtime :: Int
	, critics_consensus :: Maybe String
	, release_dates :: ReleaseDates
	, ratings :: Ratings
	, synopsis :: String
	, posters :: Posters
	, abridged_cast :: [Cast]
	, alternate_ids :: AlternateIDs
	, links :: Links
} deriving (Show,Generic)

data ReleaseDates = ReleaseDates {
	theatre :: Maybe String
	, dvd :: Maybe String
} deriving (Show,Generic)

data Ratings = Ratings {
	critics_rating :: Maybe String
	, critics_score :: Maybe Int
	, audience_rating :: Maybe String
	, audience_score :: Maybe Int
} deriving (Show,Generic)

data Posters = Posters {
	thumbnail :: String
	, profile :: String
	, detailed :: String
	, original :: String
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

data AlternateIDs = AlternateIDs {
	imdb :: String
} deriving (Show,Generic)

data Links = Links {
	self:: String
	, alternate :: String
	, cast :: String
	, reviews :: String
	, similar :: String
} deriving (Show,Generic)

instance FromJSON Links
instance ToJSON Links

instance FromJSON AlternateIDs
instance ToJSON AlternateIDs

instance FromJSON Posters
instance ToJSON Posters

instance FromJSON Ratings
instance ToJSON Ratings

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
url4 = "http://api.rottentomatoes.com/api/public/v1.0/lists/movies/upcoming.json?apikey=mb8ewcmfn82ejdf85ppzb87p"

--boxOffice :: IO (Either String Movies)
boxOffice = do
        d <- (eitherDecode <$> (simpleHttp url1)) :: IO (Either String Movies)
        case d of
		Right ps -> return ps

inTheaters :: IO (Either String Movies)
inTheaters = do
        d <- (eitherDecode <$> (simpleHttp url2)) :: IO (Either String Movies)
        return d

--opening :: IO (Either String Movies)
opening = do
        d <- (eitherDecode <$> (simpleHttp url3)) :: IO (Either String Movies)
	case d of
		Right ps -> return ps

upcoming :: IO (Either String Movies)
upcoming = do
        d <- (eitherDecode <$> (simpleHttp url4)) :: IO (Either String Movies)
        return d
