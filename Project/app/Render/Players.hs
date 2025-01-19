{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Render.Players (addPlayerRoute) where

import Web.Scotty
import Database.SQLite.Simple (Connection, query_)
import Database.Beam
import Database.Beam.Sqlite
import Database
import Network.Wai (requestBody)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int32)

-- Function to handle the /players route and render both the list of players and the form
addPlayerRoute :: Connection -> ScottyM ()
addPlayerRoute conn = do
    -- Render the players and the form to add a new player
    get "/players" $ do
        -- Fetch players from the database
        players <- liftIO $ fetchPlayers conn
        
        -- Render the players in an HTML table and include the form
        html $ renderPlayerTable players
            `TL.append` renderAddPlayerForm

    -- Handle form submission to add a new player
    post "/addPlayer" $ do
      -- Fetch the raw request body
      req <- request
      (paramsRaw, _) <- liftIO $ parseRequestBody lbsBackEnd req

      -- Debug output for raw body and parsed parameters
      liftIO $ putStrLn $ "Raw Params: " ++ show paramsRaw

      -- Convert ByteString params to Text
      let params = map (\(k, v) -> (TE.decodeUtf8 k, TE.decodeUtf8 v)) paramsRaw
      liftIO $ putStrLn $ "Decoded Params: " ++ show params

      -- Extract parameters safely
      let lookupParam key = maybe (error $ "Missing param: " ++ T.unpack key) id (lookup key params)
      let name = TL.fromStrict $ lookupParam "name"

      -- Convert Lazy Text to Strict Text
      let nameStrict = TL.toStrict name

      -- Insert player into the database
      liftIO $ insertPlayer conn nameStrict

      -- Redirect to /players
      redirect "/players"

-- Function to render the players in an HTML table
renderPlayerTable :: [Player] -> TL.Text
renderPlayerTable players = 
    let tableRows = map renderPlayerRow players
    in TL.concat
       [ TL.pack "<link rel='stylesheet' type='text/css' href='/style.css'>"
       , TL.pack "<h1>List of Players</h1>"
       , TL.pack "<table border=\"1\">"
       , TL.pack "<thead>"
       , TL.pack "<tr>"
       , TL.pack "<th>ID</th>"
       , TL.pack "<th>Name</th>"
       , TL.pack "</tr>"
       , TL.pack "</thead>"
       , TL.pack "<tbody>"
       , TL.concat (map renderPlayerRow players)
       , TL.pack "</tbody>"
       , TL.pack "</table>"
       ]

-- Function to render a single row of a player in the table
renderPlayerRow :: Player -> TL.Text
renderPlayerRow player = 
    let Player { playerId = pid, playerName = name } = player
    in TL.concat
       [ "<tr>"
       , "<td>" `TL.append` TL.pack (show pid) `TL.append` "</td>"
       , "<td>" `TL.append` TL.pack (show name) `TL.append` "</td>"
       , "</tr>"
       ]

-- Function to render the form for adding a new player
renderAddPlayerForm :: TL.Text
renderAddPlayerForm = 
    TL.concat
        [ TL.pack "<h1>Add New Player</h1>"
        , TL.pack "<form action=\"/addPlayer\" method=\"POST\">"
        , TL.pack "<label for=\"name\">Name:</label>"
        , TL.pack "<input type=\"text\" id=\"name\" name=\"name\" required><br><br>"
        , TL.pack "<button type=\"submit\">Submit</button>"
        , TL.pack "</form>"
        ]