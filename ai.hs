module Ai
where

import Graphics.Gloss
import Control.Lens
import Control.Monad (join)
import Data.Bifunctor (bimap)
import Base
import Lens

mini1 = Contender blue [(10, 0)] "Mini" miniAgent
noOne = Contender blue [] "No one" (const $ const $ Move 0 0)

minies :: Contender
minies = Contender shirt configuration name agent
  where shirt = green
        configuration = standard
        name = "Minies"
        agent = miniAgent

miniAgent :: Agent
miniAgent game des@(t, i) = if ballRange des game < kickRange then kick else move
  where kick, move :: Deed
        kick = uncurry Kick $ flip resize 1000000 $ zipTuple (-) (if t then right else -right, 0) (game^.ballLoc)
        move = uncurry Move $ flip resize 1000000 $ zipTuple (-) (game^.ballLoc) $ playerLoc des game

reverseConfig :: Contender -> Contender
reverseConfig = configuration %~ map (join bimap $ (+5) . negate)

standard :: [Point]
standard = centerLine ++ sides id ++ sides negate ++ line
    where sections = map ((*right) . (/4)) [1..3]
          centerLine =  zip sections $ repeat 0
          sides n = init $ zip sections $ repeat $ (3/4)*(n top)
          line = zip (repeat $ right*(3/5)) $ map (*top) $ [1/4, 3/4] ++ [-1/4, -3/4]
