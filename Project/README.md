# Haskell Web App with Scotty and Beam

This project is a Haskell-based web application using the Scotty web framework and Beam for database interaction. It serves a web interface and connects to an SQLite database to manage a table of players and a table of characters.

## Features

- **Web Framework**: Scotty for routing and serving HTTP requests.
- **Database**: Beam for type-safe interaction with an SQLite database.

## Prerequisites

- **Haskell Toolchain**: Install [GHCup](https://www.haskell.org/ghcup/) to get GHC, Cabal, and Stack.
- **SQLite**: Ensure SQLite is installed on your system.

## Running the Application

1. Install dependencies using Stack:
   ```bash
   stack build
   ```

2. Start the server:
   ```bash
   stack run
   ```

2. Open your browser and navigate to:
   - Home page: [http://localhost:3000/](http://localhost:3000/)