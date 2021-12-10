module Main where 

import System.Exit

import qualified Model.Board 
import Model.Board (Board,
    Ball (..),
    Color (..)
  , Pos (..)
  , ResultB (..)
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
  , testBoard)

main :: IO ()
main = do 
  
  putStrLn "\nRunning my tests... "
  putStrLn "\nTest Board EQ "
  print (Model.Board.init 0 /= Model.Board.init 1)

  putStrLn "\nTEST ATTACH "
  print ((updateBoard (2.1, 4.04, (Ball BLACK )) (Model.Board.init 0)) == (True, testBoard 1 ))

  putStrLn "\nTEST REMOVE ATTACH "
  print ((updateBoard (2.1, 4.04, (Ball YELLOW)) (Model.Board.init 0)) == (True, testBoard 0 ))

  putStrLn "\nTEST EMPTY INSERT "
  print ((updateBoard (5.1, 5.04, (Ball BLACK )) (Model.Board.init 0)) == (False, testBoard 2 ))

  putStrLn "\nTEST REMOVE DETACHED "
  print (updateBoard (1.1, 1.0, (Ball BLACK)) (snd (updateBoard (2.1, 4.04, (Ball YELLOW)) (Model.Board.init 0))) == (True, testBoard 3 ))

  putStrLn "\nDone Testing"
  exitWith ExitSuccess 
  return ()

--- >>>  print (updateBoard (1.1, 1.0, (Ball BLACK)) (snd (updateBoard (2.1, 4.04, (Ball YELLOW)) (Model.Board.init 0))) == (True, testBoard 3)) 
