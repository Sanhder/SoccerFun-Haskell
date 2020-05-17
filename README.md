# SoccerFun-Haskell
A truncated SoccerFun made in Haskell for a thesis

# Usage of the software
## Setup
The program has two dependencies, both downloadable from hackage: Lens: [Control.Lens](https://hackage.haskell.org/package/lens "Lens on Hackage") and Gloss: [Graphics.Gloss](https://hackage.haskell.org/package/gloss "Gloss on Hackage")
These can be installed using the following commands in the terminal.
cabal update
cabal install lens
cabal install gloss

## Running the game
The game can then be run by running soccer.hs’s main function, either interpreted or compiled by ghc. When compiling make sure to include the -threaded flag.
The game as standard runs in a small window at 60 fps. This can be edited in base.hs, either by changing the window to FullScreen, or editing the screen size. The size of the pitch can be changed width and height in pixels. Fps can be changed as well.

## Participating
The ai.hs file contains an example AI. Editing this file with your own implementation and then selecting this AI in soccer.hs implements this.
minies is the name of the example AI. It declares a shirt Color “green”, a start position “standard”, a name “Minies” and an Agent miniAgent.
standard is an example starting Configuration that can be reversed with reverseConfig.
miniAgent is an example Agent that simply takes the gamestate and the player’s Designation and returns the Deed that the player should perform.
