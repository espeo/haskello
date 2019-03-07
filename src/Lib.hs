{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson hiding(decode)
import Data.Csv hiding (defaultOptions)
import qualified Data.Vector as V
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)

data Rating = Rating {
	player::String,
	ranking::Int,
	matches::Int,
	winStreak::Int,
	highestRating::Int,
	victories::Int
} deriving(Eq, Show, Generic)  

instance FromRecord Rating

$(deriveJSON defaultOptions ''Rating)

type API = "get_ratings" :> Capture "game" String :> Get '[JSON] [Rating]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = ratings

ratings :: String -> Handler [Rating]
ratings player =  do
	csvData <- liftIO $ BL.readFile "ratings.txt"
	case decode NoHeader csvData of
		Left err -> liftIO (putStrLn err) >> return []
		Right x -> return $ V.toList x

