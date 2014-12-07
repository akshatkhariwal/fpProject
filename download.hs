{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

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
	, critics_consensus :: String
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
	critics_rating :: String
	, critics_score :: Int
	, audience_rating :: String
	, audience_score :: Int
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

--instance FromJSON Cast
--instance ToJSON Cast

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

-- the URL we're going to search
url = "http://api.rottentomatoes.com/api/public/v1.0/lists/movies/box_office.json?apikey=mb8ewcmfn82ejdf85ppzb87p"

-- test
boxOffice = simpleHttp url

--(Just jsonParser) = decode boxOffice :: Maybe Movies

jsonParser = do
	json <- boxOffice
	abc <- decode json :: Maybe Movies
	return abc

--jsonParse :: IO ()
--jsonParse = do
-- Get JSON data and decode it
--        d <- (eitherDecode <$> boxOffice) :: IO (Either String Movies)
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, print it in XML.
--        case d of
--                Left err -> putStrLn err
--                Right ps -> print ps

