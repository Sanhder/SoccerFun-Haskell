{-# LANGUAGE TemplateHaskell #-}

module Examples
where

import Graphics.Gloss
import Control.Lens hiding ((...))
import Data.Maybe

type State = Int
iteration, jump, collide, gravity, move :: State -> State
jump = undefined; collide = undefined; gravity = undefined; move = undefined

iteration = jump . collide . gravity . move
iteration' state = jump(collide(gravity(move(state))))

(...) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
infixr 8 ...
(...) = (.) . (.)

exG = simulate FullScreen blue 60 0 circleSolid $ const $ const $ succ

{-# LANGUAGE TemplateHaskell #-}
data Gender = Male | Female deriving Show
data Cat = Cat
  { _colour         :: Color --Color of fur
  , _body           :: (Float, Float) --Length and weight
  , _gender         :: Gender
  }
makeLenses ''Cat

milo :: Cat
milo = Cat orange (45, 4) Male
ex1 = view gender milo
ex2 = milo^.gender
ex3 = milo^.body._1
ex4 = body._1 %~ succ $ milo


data Matryoshka = Doll
  { _size     :: Int
  , _content  :: Maybe Matryoshka
  }
  deriving Show
makeLenses ''Matryoshka

nestingDoll = Doll 12 $ Just $ Doll 8 $ Just $ Doll 4 $ Nothing
newDoll = Doll 2 $ Nothing

ex5 = _size $ fromJust $ _content nestingDoll
ex6 = (_size.fromJust._content) nestingDoll
ex7 = nestingDoll {_size = _size nestingDoll + 1 }
ex8 = nestingDoll {_content = Just (fromJust $ _content nestingDoll) {_content = Just (fromJust $ _content (fromJust $ _content nestingDoll)) {_content = Just newDoll } } }
ex9 = nestingDoll {_content = Just doll1 {_content = Just doll2 {_content = Just newDoll } } }
  where doll1 = fromJust $ _content nestingDoll
        doll2 = fromJust $ _content doll1

ex10 = content._Just.content._Just.content._Just .~ newDoll $ nestingDoll
ex11 = ((!!2) . iterate (content._Just)) .~ newDoll $ nestingDoll
