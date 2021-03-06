module Model.FlyingBall where

import Model.Board ( Pos, Board , Ball(..), Color(..),genRandBall, bheight, theight, bwidth)
import GHC.Float (int2Float)


data FlyingBall = FlyingBall
  { x  :: Float
  , y  :: Float
  , dx :: Float
  , dy :: Float
  , ball :: Ball
  }

fbwidth :: Float
fbwidth = int2Float bwidth

ftheight :: Float
ftheight = int2Float theight



init :: FlyingBall
init  = FlyingBall 0 0 0 0 (Ball EMPTY)

setFlyingBall :: ((Int,Int),Ball) -> FlyingBall
setFlyingBall (angle,ball) = FlyingBall ( (fbwidth + 1)  / 2)  ftheight (angle2dx angle) (angle2dy angle) ball

hasFlyingBall :: FlyingBall -> Bool
hasFlyingBall fb = ball fb /= Ball EMPTY

-- >>> - (sin (int2Float 90 * (pi/180)))
-- -1.0

-- Only Move for 0.1
dis ::  Int -> Int -> Float
dis x y = sqrt ( int2Float (x*x) + int2Float (y*y))

angle2dy :: (Int, Int) -> Float
angle2dy (dx,dy) = - (int2Float dy / (dis dx dy / 0.1))

-- Only Move for 0.1
angle2dx :: (Int, Int) -> Float
angle2dx (dx,dy) =  int2Float dx / (dis dx dy / 0.1)

nextFlyingBall :: FlyingBall -> FlyingBall
nextFlyingBall fb =
    case (fball == Ball EMPTY, x' + dx' < 1, x' + dx' > fbwidth , y' + dy' < 1) of
        (True, _, _, _) -> fb
        (_, True, _, True) -> fb {x = 1, dx = -dx', y = 1, dy = -dy'}
        (_, _, True, True) -> fb {x = fbwidth, dx = -dx', y = 1, dy = -dy'}
        (_, True, _, _) -> fb {x = 1, dx = -dx', y = y' + dy'}
        (_, _, True, _) -> fb {x = 10, dx = -dx', y = y' + dy'}
        (_, _, _, True) -> fb {x = x' + dx', y = 1, dy = -dy'}
        (_, _, _, _) -> fb {x = x' + dx', y = y' + dy'}
    where
        fball = ball fb
        dx' = dx fb
        dy' = dy fb
        x' = x fb
        y' = y fb

getFlyingBall :: FlyingBall -> (Float ,Float, Ball)
getFlyingBall fb = (x fb, y fb, ball fb)
