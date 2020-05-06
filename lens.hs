{-# LANGUAGE TemplateHaskell #-}

module Lens
where

import Graphics.Gloss
import Control.Lens

----------------------------------------------------------------- SoccerGame data
type Update = SoccerGame -> SoccerGame

data SoccerGame = Game
  { _ballLoc         :: Point
  , _ballVel         :: (Float, Float)
  , _gained          :: Maybe Designation
  , _score           :: (Int, Int)
  , _leftPlayer      :: Contender
  , _rightPlayer     :: Contender
  }

type Designation = (Bool, Int) -- True is left, False is right

data Contender = Contender
  { _shirt           :: Color
  , _configuration   :: [Point]
  , _name            :: String
  , _agent           :: Agent
  }

type Agent = SoccerGame -> Designation -> Deed
data Deed = Kick Float Float | Gain | Move Float Float

makeLenses ''Contender
makeLenses ''SoccerGame
