help-event-muncher.hs
===========

The muncher that persists events into the database.

## Microservice Info
Channel: `event-muncher`

Group: `event-messages`

## Installation and Execution
To use `HelpEventMuncher`, do:

1. Install the [haskell-platform](https://www.haskell.org/platform).
2. `cabal install help-esb`
4. `git clone https://github.com/helpdotcom/help-event-muncher.hs`
5. `cd help-event-muncher.hs`
6. `ghc HelpEventMuncher.hs`
7. `./HelpEventMuncher`

If you wish to build `HelpEsbClient` from source, do:

1. Install the [haskell-platform](https://www.haskell.org/platform).
2. `cabal install missingh aeson uuid`
3. `git clone https://github.com/helpdotcom/help-esb.hs` (At least until haskell/hackage-server#275 is fixed. Then it will become `cabal install help-esb`.)
4. `git clone https://github.com/helpdotcom/help-event-muncher.hs`
5. `cd help-event-muncher.hs`
6. `ghc -i../help-esb.hs HelpEventMuncher.hs`
7. `./HelpEventMuncher`

## test/Auto.hs
Included is a [automated test](test/Auto.hs) that simply logs into the ESB, verifies the login,
subscribes to `event-messages` and then publishes test messages.

To run it for yourself, do:

1. `./HelpEventMuncher`
2. `cd test`
3. `ghc -i../ Auto.hs`

Or if you built `HelpEsbClient` from source, do:

1. `./HelpEventMuncher`
2. `cd test`
3. `ghc -i../ -i../../help-esb.hs Auto.hs`
4. `./Auto`
