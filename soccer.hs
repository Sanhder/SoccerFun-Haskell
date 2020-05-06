module Main(main) where

import Graphics.Gloss
import Control.Lens hiding ((...))
import Control.Monad (join)
import Assets
import Base
import Lens
import Ai

----------------------------------------------------------------- Ai players
leftTeam, rightTeam :: Contender  -- Contenders of the game
leftTeam = reverseConfig minies
rightTeam = minies
----------------------------------------------------------------- Initial state
initialState :: Contender -> Contender -> SoccerGame
initialState = Game (0, 0) (0, 0) Nothing (0, 0) -- Constructs the initial state given the contenders
----------------------------------------------------------------- Code
main :: IO ()
main = simulate window background fps (initialState leftTeam rightTeam) render update -- Main function assembling all components
----------------------------------------------------------------- View
render :: SoccerGame -> Picture -- Function rendering the state to a picture
render game = pictures $ pictures <$> [field, residents]
  where field     = [pitch, scoreboard $ game^.score, uncurry translate (game^.ballLoc) football]
        residents = [team (game^.leftPlayer), team (game^.rightPlayer), names (game^.leftPlayer.name) (game^.rightPlayer.name)]
          where team t = pictures $ zipWith (uncurry translate) (t^.configuration) $ repeat $ player $ t^.shirt
----------------------------------------------------------------- Controller
moveBall, rollResistance, execute :: Float -> Update  -- Update functions composing to the function that steps the world one iteration
update _ seconds = execute seconds . possession . side . rollResistance seconds . moveBall seconds
moveBall seconds game   = ballLoc %~ (zipTuple (+) $ join bimap (*seconds) $ game^.ballVel) $ game
rollResistance seconds  = ballVel %~ (join bimap $ \x -> x-x*resistance*seconds)
execute s = exec True . exec False -- Function to execute player moves.
  where exec t game = foldr (uncurry $ act s) game $ zip roster $ (game^.(boolToSide t).agent) game <$> roster
          where roster = zip (repeat t) [0..pred $ length $ game^.(boolToSide t).configuration] -- Number of players per team

act :: Float -> Designation -> Deed -> Update -- Acts out the moves, truncating them to be allowed if necessary
act s des@(t, i) deed game = possible des deed game change game
  where change = let e = extent s deed in case deed of
                    Move x y  -> (boolToSide t) . configuration . ix i %~ zipTuple (+) e
                    Kick x y  -> (gained .~ Nothing ) . (ballVel .~ e)
                    Gain      -> (gained .~ Just des)

side, possession :: Update
side game | (not ... (||)) (bound _2 top) $ bound _1 right = game -- Update function taking care of scoring and out of field
          | otherwise = if bound _2 $ miniaturize 3.66 then kickoff else goal --Out of game
          where bound orientation = (>) $ abs $ game^.ballLoc.orientation
                goal = (score . if (>) 0 $ game^.ballLoc._1 then _1 else _2) %~ succ $ kickoff
                kickoff = score .~ (game^.score) $ initialState leftTeam rightTeam
possession game = case game^.gained of Nothing -> game; Just des -> ballLoc .~ (playerLoc des game) $ game -- Update possession
