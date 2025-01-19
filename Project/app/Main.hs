{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import qualified Data.Text.Lazy as TL
import Render.Characters (addCharacterRoute)
import Render.Players (addPlayerRoute)
import Database (initializeDb)
import Database.SQLite.Simple (open)
import Network.Wai.Middleware.Static

main :: IO ()
main = do
    conn <- open "project.db"
    initializeDb conn

    scotty 3000 $ do
        middleware $ staticPolicy (addBase "static")
        
        -- Define a route for the home page
        get "/" $ do
            html "<link rel='stylesheet' type='text/css' href='/style.css'><h1>Welcome to the Scotty Server!</h1><a href='/players'><button>Go to Players</button></a><br><a href='/characters'><button>Go to Characters</button></a>"

        -- Add the /players route from the Players module
        addPlayerRoute conn

        -- Add the /characters route from the Characters module
        addCharacterRoute conn

        -- Define a not found route
        notFound $ do
            text "404 - Not Found"