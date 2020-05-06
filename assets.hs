module Assets
where
import Graphics.Gloss
import Data.Function (on)
import Control.Monad (join, replicateM)
import Base

tile :: Picture -> Picture  -- Takes a corner tile and reflects it to the entire screen
tile = pictures . zipWith (\[x, y]-> scale x y) (replicateM 2 [-1, 1]) . repeat -- Apply all 2-dimensional reflections to the tiles

----------------------------------------------------------------- Assets
pitch :: Picture  --Combines grass and linework into the full pitch
pitch = pictures [grass, linework]

grass :: Picture  -- Draws the grass
grass = color (dark $ dark green) $ rectangleSolid (width + miniaturize 5) $ height + miniaturize 5

linework :: Picture -- Lines on the field
linework = color white $ tile $ pictures [middle, penaltyArea, penaltySpot, goalArea, cornerArc, fieldBorder, goal]
  where
    middle      = pictures [circle $ miniaturize 9.15, line [(0, -top), (0, top)], circleSolid $ miniaturize 0.25]
    area corner = line [(right, snd corner), (right - fst corner, snd corner), (right - fst corner, 0)] -- Given a corner point create a box
    penaltyArea = area $ mini (,) 16.5 20.15
    goalArea    = area $ mini (,) 5.5 9.15
    fieldBorder = area (width, top)
    cornerArc   = translate right top $ arc 180 270 $ miniaturize 1
    goal        = area $ mini (,) (-2.44) 3.66
    penaltySpot = translate (right - miniaturize 11) 0 $ pictures [penaltyArc, dot]
      where
        dot         = circleSolid $ miniaturize 0.25
        penaltyArc  = arc (180-53.13) 180 $ miniaturize 9.15  -- Pythogoras with penaltyArea

football :: Picture -- Draws the ball
football = pictures [circle size, color white $ circleSolid size, tile pattern]
  where size      = miniaturize 0.6 -- miniaturize 0.22
        tile      = mosaic 2 (scale (-1) 1) . mosaic 5 (rotate 72)
          where mosaic n = pictures . take n ... iterate

        pattern = pictures [line path, polygon midSpot, polygon sideSpot]
          where path      = [penta, poly, top, diag, halfpol, diag, lastP]
                midSpot   = [penta, poly, center]
                sideSpot  = [diag, halfpol, fill, lastP]

                center  = join (,) 0
                penta   = shift center  0.208   0.182
                poly    = shift center  0       (1/3)
                top     = shift center  0       (2/3)
                diag    = shift top     0.255   0.109
                halfpol = shift diag    0.0727  0.182
                lastP   = shift diag    0.213   (-0.156)
                fill    = shift halfpol 0.311   (-0.18)

                shift p = zipTuple (+) p ... (,) `on` (*size)

scoreboard :: (Int, Int) -> Picture -- Draws the scoreboard given the score
scoreboard score = translate 0 (top + miniaturize 1.5) $ pictures [sign, readout score]
  where sign    = color (greyN 0.25) $ rectangleSolid (miniaturize 6) $ miniaturize 3
        readout = color magenta . mini translate (-2) (-1) . mini scale 0.015 0.015 . text . show

player :: Color -> Picture  -- Draws players given the color of their shirt
player shirt = pictures [shoulders, hair]
  where hair      = mini translate 0 0.04 $ scale 0.8 1 $ color black $ circleSolid $ miniaturize 0.20  -- Average male head length
        shoulders = scale 1 0.4 $ color shirt $ circleSolid $ miniaturize 0.40   -- Average male shoulder width

names :: String -> String -> Picture  -- Draws the team names at the top of the screen
names l r = translate offset (top + miniaturize 0.5) $ pictures [side negate l, side id r]
  where side m  = translate (m right / 1.6) 0 . mini scale 0.015 0.015 . text
        offset  = negate . miniaturize . fromIntegral . length $ r
