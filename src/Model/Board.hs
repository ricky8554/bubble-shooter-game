{-# LANGUAGE DeriveFunctor #-}
module Model.Board
  ( -- * Types
    Board,
    Ball (..),
    Color (..)
  , Pos (..)
  , ResultB (..)
    -- * Board API
  , bheight
  , theight
  , bwidth
  , (!)
  , init
  , genRandBall
  , updateBoard
  , isBoardFinished
  , failBoard
  , getExistBall
  , testBoard
  )
  where

import Prelude hiding (init)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Matrix as MX
import Data.Maybe (isJust, isNothing)
import System.Random
import GHC.Float (int2Float)
import Data.Bool (Bool)

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------
data Color = RED | BLUE | YELLOW | BLACK | GREEN | EMPTY | HALF | SPECIAL deriving (Eq, Ord, Show)
data Ball = Ball Color deriving (Eq, Ord, Show)

data ResultB a 
  = Win Int
  | Fail 
  | Cont a
  deriving (Eq, Functor, Show)

data Pos = Pos
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord, Show)

type Board = MX.Matrix Ball
type PosSet = S.Set Pos

(!) :: Board -> Pos -> Maybe Ball
board ! (Pos r c) = case MX.safeGet r c board of
                      Just (Ball EMPTY) -> Nothing
                      x -> x
validPos :: Pos -> Bool
validPos (Pos r c) = r > 0 && c > 0 && r <= theight && ((odd r && c <= bwidth) || (even r && c < bwidth))


bheight :: Int
bheight = 4

theight :: Int
theight = bheight + 4

bwidth :: Int
bwidth = 10

colorNum :: Int
colorNum = 5

colors :: [Color]
colors = [RED, BLUE,  BLACK, YELLOW, GREEN]

allPos :: [Pos]
allPos = filter validPos [ Pos i j | i <- [1..theight], j <- [1..bwidth] ]

init :: Int -> Board
init idx = [init1, init2, init3, init4] !! idx

testBoard :: Int -> Board
testBoard idx = [test1, test2, test3, test4] !! idx

-- >>> (updateBoard (5.1, 5.04, (Ball BLACK )) (Model.Board.init 0))                                                                                                     
init1 :: Board
init1 = MX.fromList theight bwidth [Ball YELLOW,  Ball BLACK,  Ball BLACK,     Ball BLACK,   Ball BLACK, Ball BLACK,  Ball BLACK,  Ball BLACK,     Ball BLACK,   Ball YELLOW,
                                    Ball YELLOW,    Ball BLUE,  Ball BLUE,   Ball YELLOW,   Ball EMPTY ,Ball YELLOW,    Ball BLUE,  Ball BLUE,   Ball YELLOW,   Ball EMPTY,
                                    Ball YELLOW,  Ball YELLOW,  Ball YELLOW,     Ball YELLOW,   Ball YELLOW, Ball YELLOW,  Ball YELLOW,  Ball YELLOW,     Ball YELLOW,   Ball YELLOW,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY]

init2 :: Board
init2 = MX.fromList theight bwidth [Ball RED,  Ball RED,  Ball RED,     Ball RED,   Ball BLACK, Ball BLACK,  Ball BLACK,  Ball RED, Ball RED,   Ball RED,
                                  Ball BLUE,    Ball BLUE,  Ball BLUE,   Ball BLUE,   Ball BLUE, Ball BLUE,    Ball BLUE,  Ball BLUE,   Ball BLUE,   Ball EMPTY,
                                  Ball BLACK,  Ball BLACK,  Ball BLACK,     Ball BLACK,   Ball BLACK, Ball BLACK,  Ball BLACK,  Ball BLACK, Ball BLACK,   Ball BLACK,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY]

init3 :: Board
init3 = MX.fromList theight bwidth [Ball BLUE,  Ball BLUE,  Ball BLUE,     Ball BLUE,   Ball BLUE, Ball BLUE,  Ball BLUE,  Ball BLUE,     Ball BLUE,   Ball BLUE,
                                   Ball BLUE,    Ball BLUE,  Ball BLUE,   Ball GREEN,   Ball BLACK, Ball GREEN,    Ball BLUE,  Ball BLUE,   Ball BLUE,   Ball EMPTY,
                                  Ball BLUE,  Ball BLUE,  Ball BLUE,     Ball GREEN,   Ball GREEN, Ball GREEN,  Ball BLUE,  Ball BLUE,     Ball BLUE,   Ball BLUE,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY]

init4 :: Board
init4 = MX.fromList theight bwidth [Ball BLUE,   Ball BLUE,   Ball YELLOW,  Ball YELLOW,  Ball BLUE,  Ball BLUE,   Ball BLUE,    Ball BLUE,    Ball RED,    Ball RED,
                                    Ball BLUE,   Ball BLUE,   Ball YELLOW,  Ball GREEN,   Ball BLACK, Ball GREEN,  Ball BLUE,    Ball RED,    Ball RED,    Ball EMPTY,
                                    Ball BLUE,   Ball BLUE,   Ball YELLOW,  Ball GREEN,   Ball GREEN, Ball GREEN,  Ball BLUE,    Ball RED,    Ball RED,    Ball RED,
                                    Ball EMPTY,  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                    Ball EMPTY,  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                    Ball EMPTY,  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                    Ball EMPTY,  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                    Ball EMPTY,  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY]

test1 :: Board
test1 = MX.fromList theight bwidth [Ball EMPTY,  Ball BLACK,  Ball BLACK,     Ball BLACK,   Ball BLACK, Ball BLACK,  Ball BLACK,  Ball BLACK,     Ball BLACK,   Ball EMPTY,
                                    Ball EMPTY,    Ball BLUE,  Ball BLUE,   Ball EMPTY,   Ball EMPTY ,Ball EMPTY,    Ball BLUE,  Ball BLUE,   Ball EMPTY,   Ball EMPTY,
                                    Ball EMPTY,  Ball EMPTY,  Ball EMPTY,     Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,  Ball EMPTY,     Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY]

test2 :: Board
test2 = MX.fromList theight bwidth [Ball YELLOW,  Ball BLACK,  Ball BLACK,     Ball BLACK,   Ball BLACK, Ball BLACK,  Ball BLACK,  Ball BLACK,     Ball BLACK,   Ball YELLOW,
                                    Ball YELLOW,    Ball BLUE,  Ball BLUE,   Ball YELLOW,   Ball EMPTY ,Ball YELLOW,    Ball BLUE,  Ball BLUE,   Ball YELLOW,   Ball EMPTY,
                                    Ball YELLOW,  Ball YELLOW,  Ball YELLOW,     Ball YELLOW,   Ball YELLOW, Ball YELLOW,  Ball YELLOW,  Ball YELLOW,     Ball YELLOW,   Ball YELLOW,
                                  Ball EMPTY,  Ball BLACK,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY]

test3 :: Board
test3 = init1


test4 :: Board
test4 = MX.fromList theight bwidth [Ball EMPTY,  Ball EMPTY,  Ball EMPTY,     Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,  Ball EMPTY,     Ball EMPTY,   Ball EMPTY,
                                    Ball EMPTY,    Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY ,Ball EMPTY,    Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                    Ball EMPTY,  Ball EMPTY,  Ball EMPTY,     Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,  Ball EMPTY,     Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY,
                                  Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY, Ball EMPTY,  Ball EMPTY,   Ball EMPTY,   Ball EMPTY,   Ball EMPTY]

rvalues :: [Int]
rvalues = map fst $ scanl (\(_, gen) _ -> random gen) (random (mkStdGen 1)) $ repeat ()

randList :: [Int]
randList = map abs (take (bheight*bwidth*1000) rvalues)

randGenBalls :: Int -> Int -> Ball
randGenBalls r c =
  case (r `mod` 2, r > bheight, bwidth - c) of
    (0, _ ,0) -> Ball EMPTY
    (_, True  ,_) -> Ball EMPTY
    _ -> Ball (colors !! ((randList !! ((r-1)*bwidth + c - 1) ) `mod` colorNum ) )

genRandBall :: Int -> Ball
genRandBall n = Ball (colors !! (randList !! (bheight*bwidth + n) `mod` colorNum ) )

findNeighbor :: Pos -> [Pos]
findNeighbor (Pos r c) = if even r then filter validPos pEven else filter validPos pOdd
  where
    pOdd  = [Pos (r-1) c, Pos (r-1) (c-1), Pos r (c+1), Pos r (c-1), Pos (r+1) c, Pos (r+1) (c-1)]
    pEven = [Pos (r-1) c, Pos (r-1) (c+1), Pos r (c+1), Pos r (c-1), Pos (r+1) c, Pos (r+1) (c+1)]

insertBoard :: Pos -> Ball -> Board -> Board
insertBoard (Pos r c) ball board = case MX.safeSet ball (r, c) board of
                                      Just b  -> b
                                      _       -> board

setBoardEmpty :: Pos -> Board -> Board
setBoardEmpty pos = insertBoard pos (Ball EMPTY)

dfsColor' :: Board -> Ball -> Pos -> PosSet -> PosSet
dfsColor' board ball pos set = if S.member pos set then set else newset
  where
    neighbor = filter (\p -> (board ! p) == Just ball ) (findNeighbor pos)
    newset = foldr next (S.insert pos set) neighbor
    next pos' set' = dfsColor' board ball pos' set'

dfsColor :: Board -> Maybe Ball -> Pos -> PosSet
dfsColor board (Just ball) pos = dfsColor' board ball pos S.empty
dfsColor _ Nothing _ = S.empty

dfs' :: Board -> Pos -> PosSet -> PosSet
dfs' board pos set = if S.member pos set then set else newset
  where
    neighbor = filter (\p -> isJust (board ! p) ) (findNeighbor pos)
    newset = foldr next (S.insert pos set) neighbor
    next pos' set' = dfs' board pos' set'

dfs :: Board -> Pos -> PosSet -> (PosSet, [Pos])
dfs board pos set = if S.member pos set then (S.empty, []) else (s, l)
  where
    s = dfs' board pos S.empty
    l' = S.toList s
    l = if any attachWall l' then [] else l'

attachWall :: Pos -> Bool
attachWall (Pos 1 _) = True
attachWall (Pos r 1) = odd r
attachWall (Pos _ c) = c == bwidth

removeFromBoard :: Pos -> Board -> Board
removeFromBoard pos board = if size > 2 then newBoard else board
  where
    dfsSet = dfsColor board (board ! pos) pos
    size = S.size dfsSet
    dfsList = S.toList dfsSet
    newBoard = foldr setBoardEmpty board dfsList

removeDetachBoard :: Board -> Board
removeDetachBoard board = newBoard
  where
    l = filter (\p -> isJust (board ! p)) allPos
    (_, rl) = foldr dfsD (S.empty,[]) l
    dfsD pos (s,l) = slu (dfs board pos s) (s,l)
    slu (s1,l1) (s2,l2) = (S.union s1 s2, l1 ++ l2)
    newBoard = foldr setBoardEmpty board rl

closeTo :: Float -> Int -> Bool
closeTo f i = abs (f - int2Float i) < 0.0001

updateBoard' :: Pos -> Ball -> Board -> (Bool,Board)
updateBoard' pos ball board  = if validPos pos && isNothing (board ! pos)
                                then (True, removeDetachBoard (removeFromBoard pos (insertBoard pos ball board)))
                                else (False, board)

hasNeighbors :: Float -> Float -> Board -> Bool
hasNeighbors r c board = foldr (\p b -> isJust (board ! p) || b) False (findNeighbor (findNearPos r c))

findNearPos :: Float -> Float -> Pos
findNearPos r c = Pos (round r) (find c)
  where
    find c = if even (round r) then floor c else round c

updateBoard :: (Float, Float, Ball) -> Board -> (Bool,Board)
updateBoard (c, r, ball) board
  | closeTo r 1 || hasNeighbors r c board = updateBoard' (fp1) ball board
  | otherwise = (False, board)
  where
    fp1 =  if r - int2Float(floor r) < 0.05 then findNearPos r c else Pos 0 0

isBoardFinished :: Board -> Bool
isBoardFinished board = null l
  where
    l = filter (\p -> isJust (board ! p)) allPos

failBoard :: Board -> Bool
failBoard board = any touchBoardBottom l
  where
    l = filter (\p -> isJust (board ! p)) allPos

touchBoardBottom :: Pos -> Bool
touchBoardBottom (Pos r _) = r == theight

getExistBall :: Board -> Int -> Ball
getExistBall board seed  | not (null ballList) = ballList !! randInt
                          | otherwise           = Ball EMPTY
  where
    randInt = fst $ randomR (0, length ballList - 1) (mkStdGen seed)
    ballList = S.toList (foldr getOneBall S.empty allPos)
      where
        getOneBall pos set = case board ! pos of
                              Just ball -> S.insert ball set
                              Nothing   -> set

-- >>> init1

-- >>> removeFromBoard (Pos 1 1) init1

-- >>> removeFromBoard (Pos 2 1) init1

-- >>> a = (setBoardEmpty (Pos 1 3) (setBoardEmpty (Pos 3 3) (setBoardEmpty (Pos 3 2) (setBoardEmpty (Pos 1 2) (removeDetachBoard (removeFromBoard (Pos 2 1) (setBoard (Pos 2 2) (Ball RED) (removeFromBoard (Pos 2 1) init1))))))))  
-- >>> removeDetachBoard a

-- >>> removeFromBoard (Pos 2 1) init2
