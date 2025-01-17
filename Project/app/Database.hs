{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleContexts, TypeFamilies, FlexibleInstances, OverloadedStrings, StandaloneDeriving #-}

module Database where

import Database.Beam
import Database.Beam.Sqlite
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.SQLite.Simple (Connection, execute_)
import Data.Int (Int32)

-- Define the Character table
data CharacterT f = Character
  { characterId       :: Columnar f Int32
  , characterName     :: Columnar f Text
  , characterClass    :: Columnar f Text
  , characterRace     :: Columnar f Text
  , characterLevel    :: Columnar f Int32
  , characterPlayerId :: Columnar f Int32
  } deriving (Generic, Beamable)

-- Define the Character type
type Character = CharacterT Identity
deriving instance Show Character

type CharacterId = PrimaryKey CharacterT Identity
deriving instance Show CharacterId

-- Make the Character type an instance of the Table type class
instance Table CharacterT where
  data PrimaryKey CharacterT f = CharacterId (Columnar f Int32) deriving (Generic, Beamable)
  primaryKey = CharacterId . characterId

-- Define the database schema
data ProjectDb f = ProjectDb
  { _character :: f (TableEntity CharacterT)
  } deriving (Generic, Database Sqlite)

-- Make ProjectDb an instance of the Database type class for Sqlite
projectDb :: DatabaseSettings Sqlite ProjectDb
projectDb = defaultDbSettings

-- Initialize the database
initializeDb :: Connection -> IO ()
initializeDb conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS character (\
    \id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,\ 
    \name TEXT NOT NULL,\ 
    \class TEXT NOT NULL,\ 
    \race TEXT NOT NULL,\ 
    \level INTEGER NOT NULL,\ 
    \player_id INTEGER NOT NULL)"
  putStrLn "Database initialized"

-- Function to fetch all characters from the database
fetchCharacters :: Connection -> IO [Character]
fetchCharacters conn = runBeamSqlite conn $ runSelectReturningList $ select $ all_ (_character projectDb)

-- Function to insert a character into the database
insertCharacter :: Connection -> Text -> Text -> Text -> Int32 -> Int32 -> IO ()
insertCharacter conn name className race level playerId = runBeamSqlite conn $ runInsert $ insert (_character projectDb) $
  insertExpressions [Character default_ (val_ name) (val_ className) (val_ race) (val_ level) (val_ playerId)]
