{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Render.Characters (addCharacterRoute) where

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

-- Function to handle the /characters route and render both the list of characters and the form
addCharacterRoute :: Connection -> ScottyM ()
addCharacterRoute conn = do
    -- Render the characters and the form to add a new character
    get "/characters" $ do
        -- Fetch characters from the database
        characters <- liftIO $ fetchCharacters conn
        
        -- Render the characters in an HTML table and include the form
        html $ renderCharacterTable characters
            `TL.append` renderAddCharacterForm

    -- Handle form submission to add a new character
    post "/addCharacter" $ do
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
      let className = TL.fromStrict $ lookupParam "class"
      let race = TL.fromStrict $ lookupParam "race"
      let level = read $ T.unpack $ lookupParam "level" :: Int32
      let playerId = read $ T.unpack $ lookupParam "player_id" :: Int32

      -- Convert Lazy Text to Strict Text
      let nameStrict = TL.toStrict name
      let classNameStrict = TL.toStrict className
      let raceStrict = TL.toStrict race

      -- Insert character into the database
      liftIO $ insertCharacter conn nameStrict classNameStrict raceStrict level playerId

      -- Redirect to /characters
      redirect "/characters"

-- Function to render the characters in an HTML table
renderCharacterTable :: [Character] -> TL.Text
renderCharacterTable characters = 
    let tableRows = map renderCharacterRow characters
    in TL.concat
       [ TL.pack "<h1>List of Characters</h1>"
       , TL.pack "<table border=\"1\">"
       , TL.pack "<thead>"
       , TL.pack "<tr>"
       , TL.pack "<th>ID</th>"
       , TL.pack "<th>Name</th>"
       , TL.pack "<th>Class</th>"
       , TL.pack "<th>Race</th>"
       , TL.pack "<th>Level</th>"
       , TL.pack "<th>Player ID</th>"
       , TL.pack "</tr>"
       , TL.pack "</thead>"
       , TL.pack "<tbody>"
       , TL.concat (map renderCharacterRow characters)
       , TL.pack "</tbody>"
       , TL.pack "</table>"
       ]

-- Function to render a single row of a character in the table
renderCharacterRow :: Character -> TL.Text
renderCharacterRow character = 
    let Character { characterId = cid, characterName = name, characterClass = className, 
                    characterRace = race, characterLevel = level, characterPlayerId = playerId } = character
    in TL.concat
       [ "<tr>"
       , "<td>" `TL.append` TL.pack (show cid) `TL.append` "</td>"
       , "<td>" `TL.append` TL.pack (show name) `TL.append` "</td>"
       , "<td>" `TL.append` TL.pack (show className) `TL.append` "</td>"
       , "<td>" `TL.append` TL.pack (show race) `TL.append` "</td>"
       , "<td>" `TL.append` TL.pack (show level) `TL.append` "</td>"
       , "<td>" `TL.append` TL.pack (show playerId) `TL.append` "</td>"
       , "</tr>"
       ]

-- Function to render the form for adding a new character
renderAddCharacterForm :: TL.Text
renderAddCharacterForm = 
    TL.concat
        [ TL.pack "<h1>Add New Character</h1>"
        , TL.pack "<form action=\"/addCharacter\" method=\"POST\">"
        , TL.pack "<label for=\"name\">Name:</label>"
        , TL.pack "<input type=\"text\" id=\"name\" name=\"name\" required><br><br>"
        , TL.pack "<label for=\"class\">Class:</label>"
        , TL.pack "<input type=\"text\" id=\"class\" name=\"class\" required><br><br>"
        , TL.pack "<label for=\"race\">Race:</label>"
        , TL.pack "<input type=\"text\" id=\"race\" name=\"race\" required><br><br>"
        , TL.pack "<label for=\"level\">Level:</label>"
        , TL.pack "<input type=\"number\" id=\"level\" name=\"level\" required><br><br>"
        , TL.pack "<label for=\"player_id\">Player ID:</label>"
        , TL.pack "<input type=\"number\" id=\"player_id\" name=\"player_id\" required><br><br>"
        , TL.pack "<button type=\"submit\">Submit</button>"
        , TL.pack "</form>"
        ]
