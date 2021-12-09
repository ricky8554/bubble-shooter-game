module View (view,atrMap) where

import Brick
    ( attrMap,
      on,
      emptyWidget,
      hBox,
      padLeftRight,
      str,
      vBox,
      withBorderStyle,
      AttrMap,
      AttrName,
      Widget, attrName, withAttr, Location (Location), hLimitPercent )
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder, border)

import Brick.Widgets.Border.Style (unicode, unicodeBold)
import Text.Printf (printf)

import Model
import Model.Board ( Pos(Pos, pRow, pCol), (!), Ball(..), Color(..), theight, bheight, bwidth )
import Graphics.Vty hiding (dim)


import System.Random (randomRIO)
import qualified Graphics.Vty as V
import Model.FlyingBall
import Model.Player
import GHC.Float (int2Float, float2Int)
import Brick.Widgets.Core (setAvailableSize, translateBy)
import Brick.Types (Location(loc))

-- sr :: [Integer]
-- sr = [1,2,3,4,5,6,7,8,9]

-- selectRandom :: [a] -> IO a
-- selectRandom xs = do
--   i <- randomRIO (0, length xs - 1)
--   return (sr !! i)

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [view' s]

view' :: PlayState -> Widget String
view' s =
    center $ withBorderStyle unicodeBold $
      -- borderWithLabel (str (header s)) $
        vBox [ mkRow s row | row <- [1..theight] ]

header :: PlayState -> String
header s = printf ""

mkRow :: PlayState -> Int -> Widget n
mkRow s row = case even row || row == theight  of
                True -> hBox [ mkCell s row i | i <- [1..bwidth+1]]
                _ -> hBox [ mkCell s row i | i <- [1..bwidth] ]


mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c = mkCell' s r c



posFB :: PlayState -> Int -> Int -> (Int, Int, Int)
posFB s r c =
  case (flfb == Ball EMPTY,
        inRange x y,
        inRange (x + 1) y,
        inRange x  (y + 1),
        inRange (x + 1) (y + 1)) of
    (True, _, _, _, _) -> (0,0,0)
    (_, True, _, _, _) -> (1,wrap (x - mc), wrap y)
    (_, _, True, _, _) -> (2,wrap (x + 1 - mc), wrap y)
    (_, _, _, True, _) -> (3,wrap (x - mc) , wrap (y + 1))
    (_, _, _, _, True) -> (4,wrap (x + 1 - mc), wrap (y + 1))
    _ -> (0,0,0)
  where
    (FlyingBall x y _ _ flfb) = psFlying s
    lr = int2Float r
    lc = int2Float c - mc
    rr = int2Float r + 1
    rc = int2Float c + 1 - mc
    mc = if even r then 0.5 else 0
    fa a= int2Float (floor a)
    wrap a = float2Int ((a - fa a) * bsf)
    inRange c' r' = (lr <= r' && r' < rr) && (lc <= c' && c' < rc)
-- withCursor :: Widget n -> Widget n
-- withCursor = modifyDefAttr (`withStyle` reverseVideo)

mkCell' :: PlayState -> Int -> Int -> Widget n
-- mkCell' _ r c = center (str (printf "(%d, %d)" r c))
mkCell' s r c
  -- | r == 5 && c == 5 = center (mkFBCell 1 1 1 40 60 (Just (Ball RED)))
  | hasFB > 0 = center (mkFBCell hasFB r c y' x' bmb''')
  | r == theight && c == (bwidth `div` 2 + 1) =  center (mkBlock bmb'')
  | odd r = center (mkBlock bmb)
  | c == 1 || c == (bwidth + 1) =  mkBlock (Just (Ball HALF))
  | otherwise =  center (mkBlock bmb')
  where
    bmb      = psBoard s ! Pos r c
    bmb'     = psBoard s ! Pos r (c-1)
    bmb''    = Just psfb
    bmb'''   = Just flfb
    (hasFB, x', y')   = posFB s r c
    (Player _ _ psfb) = ps s
    (FlyingBall _ _ _ _ flfb) = psFlying s
    

mkFBCell :: Int -> Int -> Int -> Int -> Int -> Maybe Ball -> Widget n
mkFBCell v r c r' c' ball
  | v == 1 = mkfb' mkfb1
  | v == 2 = mkfb' mkfb2
  | v == 3 = mkfb' mkfb3
  | v == 4 = mkfb' mkfb4
  where
    ep  = Nothing
    mkfb' f =  center (vBox [ f y  | y <- [1..bs]])
    mkfb1 y = center (hBox [ mkBlock' (if x > c' && y > r' `div` 2 then ball else ep) | x <- [1..bs]])
    mkfb2 y = center (hBox [ mkBlock' (if x < c' && y > r' `div` 2 then ball else ep) | x <- [1..bs]])
    mkfb3 y = center (hBox [ mkBlock' (if x > c' && y < r' `div` 2 then ball else ep) | x <- [1..bs]])
    mkfb4 y = center (hBox [ mkBlock' (if x < c' && y < r' `div` 2 then ball else ep) | x <- [1..bs]])
mkFBCell _ _ _ _ _ _ = mkBlock (Just (Ball EMPTY))



mkBlock :: Maybe Ball -> Widget n
mkBlock (Just (Ball RED)) = border (withAttr redAtr blockB)
mkBlock (Just (Ball BLUE)) = border (withAttr blueAtr blockB)
mkBlock (Just (Ball YELLOW)) = border (withAttr yellowAtr blockB)
mkBlock (Just (Ball BLACK)) = border (withAttr blackAtr blockB)
mkBlock (Just (Ball GREEN)) = border (withAttr greenAtr blockB)
mkBlock (Just (Ball HALF)) = blockHalf
mkBlock _  = blockB


mkBlock' ::  Maybe Ball -> Widget n
mkBlock'  (Just (Ball RED)) =  (withAttr redAtr blockC)
mkBlock'  (Just (Ball BLUE)) =  (withAttr blueAtr blockC)
mkBlock' (Just (Ball YELLOW)) =  (withAttr yellowAtr blockC)
mkBlock'  (Just (Ball BLACK)) =  (withAttr blackAtr blockC)
mkBlock' (Just (Ball GREEN)) =  (withAttr greenAtr blockC)
mkBlock'  _  =   blockC


bsf :: Float
bsf = 100

bs :: Int
bs = 100
bsh :: Int
bsh = 50

blockB :: Widget n
blockB = block bs bs
blockC :: Widget n
blockC = block 1 1

blockHalf :: Widget n
blockHalf = block bs bsh

block :: Int -> Int -> Widget n
block h w =  vBox (replicate h (str (replicate w ' ')))







redAtr, blueAtr, yellowAtr, blackAtr, greenAtr,testAtr,test1Atr :: AttrName
redAtr    = attrName "redAtr"
blueAtr   = attrName "blueAtr"
yellowAtr = attrName "yellowAtr"
blackAtr  = attrName "blackAtr"
greenAtr  = attrName "greenAtr"
testAtr  = attrName "testAtr"
test1Atr  = attrName "test1Atr"


atrMap :: AttrMap
atrMap = attrMap V.defAttr [
    (redAtr,   V.red  `on` V.red)
   ,(blueAtr,  V.blue `on` V.blue )
   ,(yellowAtr,V.yellow `on` V.yellow )
   ,(blackAtr,  V.white  `on` V.white )
   ,(greenAtr,  V.green  `on` V.green )
   ,(testAtr,  V.cyan `on` V.cyan )
   ,(test1Atr,  V.black  `on` V.black )
  ]