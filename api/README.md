# Metalborn API

## Running

### Database

You can run a local database if you have docker and docker compose V2 installed
simply by running `docker compose up --build`.  If you want to connect to a different database,
you can change the database connection data in [`src/Config.hs`](./src/Config.hs).

### Build

Install [GHCup](https://www.haskell.org/ghcup/) and use `ghcup tui` to make sure
GHC 9.2.4 and Cabal are installed. Then run inside this directory:

```sh
cabal update
cabal install hpack
hpack
```

`hpack` will generate `metalborn-api.cabal` which is not version controlled.

Then run:

`cabal run metalborn-api-exe`

You should see this output:

```
Endpoint documentation generated.
Elm API types and functions generated.
Starting Metalborn API server.
```

You can then check that the database is up and accessible by running:

```
curl -H 'Content-Type: application/json' localhost:8081/api/character/ -d '["Kaladin", "Male"]
```
