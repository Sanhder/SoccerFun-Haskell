{-# LANGUAGE TemplateHaskell #-}

module Examples
where

import Graphics.Gloss
import Control.Lens
import Data.Maybe

{-# LANGUAGE TemplateHaskell #-}
data Gender = Male | Female deriving Show
data Cat = Cat
  { _colour         :: Color --Color of fur
  , _body           :: (Float, Float) --Length and weight
  , _gender         :: Gender
  }
makeLenses ''Cat

milo = Cat orange (45, 4) Male
ex1 = view gender milo
ex2 = milo^.gender
ex3 = milo^.body._1



data Matryoshka = Doll
  { _size     :: Int
  , _content  :: Maybe Matryoshka
  }
  deriving Show
makeLenses ''Matryoshka

nestingDoll = Doll 12 $ Just $ Doll 8 $ Just $ Doll 4 $ Nothing

ex4 = (_size.fromJust._content) nestingDoll
ex5 = nestingDoll {_size = _size nestingDoll + 1 }
ex6 = nestingDoll {_content = _content $ fromJust $ _content nestingDoll}
