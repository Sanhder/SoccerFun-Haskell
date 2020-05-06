module Base
where

import Graphics.Gloss
import Control.Monad (join)
import Control.Lens hiding ((...))
import Data.Function (on)
import Data.Maybe (fromJust)
import Lens
----------------------------------------------------------------- Helper functions
infixr 8 ...
(...) = (.) . (.)

zipTuple :: (t1 -> t2 -> b) -> (t1, t1) -> (t2, t2) -> (b, b) -- ZipWith but on tuples
zipTuple f (x, y) (p, q) = (f x p, f y q)
miniaturize :: Float -> Float -- Translates meters to pixels based on screensize
miniaturize size = width/(105/size)
mini f = f `on` miniaturize

right, top :: Float -- Edges of the screen
right = width/2
top   = height/2

resize :: Point -> Float -> (Float, Float)
resize v l = join bimap (l/magnitude (0, 0) v*) v
magnitude :: Point -> Point -> Float
magnitude = sqrt . (\(x, y) -> x+y) . join bimap ((^2) . abs . miniaturize) ... zipTuple (-)

boolToSide :: Functor f => Bool -> (Contender -> f Contender) -> SoccerGame -> f SoccerGame
boolToSide s = if s then leftPlayer else rightPlayer  -- Converts between the lens that points to a team to bool that represents the same thing

ballRange :: Designation -> SoccerGame -> Float
ballRange des game = magnitude (game^.ballLoc) $ playerLoc des game -- Gets the distance of a player to the ball

playerLoc (t, i) game = fromJust $ game^?(boolToSide t).configuration.ix i  -- Gets the player's location

possible :: Designation -> Deed -> SoccerGame -> Update -> Update -- Checks if the action is in range
possible player deed game action = if ballRange player game <= deedRange deed then action else id

extent :: Float -> Deed -> (Float, Float) -- Scales the extent to the time and permissible size
extent s d =  if permissible < magnitude (0, 0) scaled then resize scaled permissible else scaled
  where scaled = join bimap ((*s) . miniaturize) $ unDeed d
        permissible = s*deedPower d

----------------------------------------------------------------- Values - Renderer
window      = InWindow "Football" screensize (offset, offset) :: Display
background  = white
fps         = 144 :: Int
width       = 1100
height      = miniaturize 68
offset      = 100
screensize  = (round $ width + miniaturize 6, round $ height + miniaturize 6) -- Same ratio as pitch; 105 by 68 meters
----------------------------------------------------------------- Values - Update
resistance = 0.6 :: Float
devSpeed  = miniaturize 5
gainRange = miniaturize 7
kickRange = miniaturize 0.1
kickPower = miniaturize 15000
movePower = miniaturize 15

deedRange :: Deed -> Float
deedRange ( Kick _ _) = kickRange
deedRange   Gain      = gainRange
deedRange ( Move _ _) = 1000000000000

deedPower :: Deed -> Float
deedPower (Kick _ _) = kickPower
deedPower (Move _ _) = movePower

unDeed (Kick x y) = (x, y)
unDeed (Move x y) = (x, y)
unDeed  Gain      = (0, 0)
