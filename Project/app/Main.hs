{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import qualified Data.Text.Lazy as TL
import Render.Characters (addCharacterRoute)
import Database (initializeDb)
import Database.SQLite.Simple (open)

main :: IO ()
main = do
    conn <- open "project.db"
    initializeDb conn

    scotty 3000 $ do
        -- Define a route for the home page
        get "/" $ do
            html "<h1>Welcome to the Scotty Server!</h1><a href='/characters'><button>Go to Characters</button></a>"

        -- Add the /characters route from the Characters module
        addCharacterRoute conn

        -- Define a not found route
        notFound $ do
            text "404 - Not Found"
