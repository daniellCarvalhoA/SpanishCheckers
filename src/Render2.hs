{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module Render2 where 

import qualified Graphics.Gloss.Data.Color        as C
import qualified Graphics.Gloss.Interface.IO.Game as G
import qualified Data.List.NonEmpty               as N (head) 
import Data.Foldable   (find)
import Data.Maybe      (isJust)
import Data.List.Split (chunksOf) 
import Data.Function   ((&))
import Game 
import Types
import Board
import Data.Bits       (Bits(..))
import Debug.Trace

------------------------------
-- Colors 

nonCellColor :: C.Color 
nonCellColor = C.makeColorI 189 159 128 255 

playingCellColor :: C.Color 
playingCellColor = C.makeColorI 94 68 43 255

whiteColor :: C.Color 
whiteColor = C.makeColorI 226 207 188 255 

kingWhiteColor :: C.Color 
kingWhiteColor = C.dark whiteColor 

blackColor :: C.Color 
blackColor = C.makeColorI 30 17 3 255 

kingBlackColor :: C.Color 
kingBlackColor = C.dark blackColor 

background :: C.Color 
background = nonCellColor

--------------------------------
-- Render Game 

renderMatch :: Match -> G.Picture 
renderMatch (Menu ds) = renderMenu ds 
renderMatch (Match ds cs p) = 
  case p of 
    Play _ (Over SR     res _ board) -> renderGameOver cs ds board res Human
    Play _ (Over (SS _) res _ board) -> renderGameOver cs ds board res Computer
    Play _ (AI _ board _ _         ) -> renderState Nothing      cs ds board
    Play _ (HS board imove         ) -> renderState (Just imove) cs ds board

renderMenu :: Dimensions -> G.Picture 
renderMenu Dimensions{..} = 
  let white = G.color C.white $ G.rectangleSolid (cellsize * 1.5) cellsize 
      black = G.color C.black $ G.rectangleSolid (cellsize * 1.5) cellsize
  in G.translate (- cellsize) 0 
   $ G.pictures [white, G.translate (2 * cellsize) 0 black] 

renderGameOver :: Color -> Dimensions -> Board -> Result -> Turn -> G.Picture
renderGameOver c ds@Dimensions{..} brd r t = 
  G.pictures [ G.translate (-2 * cellsize) (4.25 * cellsize)
             $ G.scale 0.4 0.4 $ G.color C.white $ G.text txt 
             , renderState Nothing c ds brd
             , G.translate (- 6.25  * cellsize) 0 $ G.color C.white $ square cellsize
             , G.translate (- 6.525 * cellsize) (- 0.1 * cellsize)
             $ G.color C.black $ G.scale 0.1 0.1 $ G.text "Menu"
             ] 
  where txt = case r of 
                Loss -> if t == Human then "You Lost" else "You Won"
                Tie  -> "Tie"

renderState :: Maybe IMove -> Color -> Dimensions -> Board -> G.Picture 
renderState ms cr Dimensions{..} board = 
    G.translate (negate $ cellsize * 3.5) (negate $ cellsize * 3.5)
  $ G.pictures
  $ (<> [back, forward])
  $ concatMap addToPicture (zip [0..7] $ chunksOf 8 [0..63])
  where addToPicture = row ms cr cellsize board
        back         = G.translate (9.75 * cellsize) (3.5 * cellsize) 
                     $ goBack G.white cellsize 
        forward      = G.translate (9.75 * cellsize) (2 * cellsize)
                     $ goforward G.white cellsize

row :: Maybe IMove -> Color -> Float -> Board -> (Int,[Int]) -> [G.Picture]
row ms cr n Board{..} (y, xs) = map renderCell xs 
  where
    renderCell x 
      | even $ x + y                  = G.translate x' y' $ noncell n 
      | uppawns   `testBit` realpoint = G.translate x' y' $ pawncell n cr 
      | downpawns `testBit` realpoint = G.translate x' y' $ pawncell n $ otherColor cr 
      | upkings   `testBit` realpoint = G.translate x' y' $ kingcell n cr 
      | downkings `testBit` realpoint = G.translate x' y' $ kingcell n $ otherColor cr 
      | otherwise                     = G.translate x' y' $ emptyCell realpoint n ms 
      where point     = x `div` 2 
            realpoint = point + y `div` 2 
            (x',y')   = (n * fromIntegral (x `mod` 8), n * fromIntegral y)

square :: Float -> G.Picture
square f = G.rectangleSolid f f

circle :: Float -> G.Picture
circle r = G.thickCircle 0 r

pawncell :: Float -> Color -> G.Picture 
pawncell f c  = 
  let circ = circle radius 
              & case c of 
                  White -> G.color whiteColor 
                  Black -> G.color blackColor 
      radius = f * (2 / 3)  
  in G.pictures [G.color playingCellColor (square f), circ]

kingcell :: Float -> Color -> G.Picture 
kingcell f c  = 
  let circ = circle radius 
              & case c of 
                  White -> G.color kingWhiteColor 
                  Black -> G.color kingBlackColor 
      radius = f * (2 / 3)  
  in G.pictures [G.color playingCellColor (square f), circ]

noncell :: Float -> G.Picture
noncell = G.color nonCellColor . square

plainCell :: Float -> G.Picture 
plainCell f = G.color playingCellColor $ square f

emptyCell :: Int -> Float -> Maybe IMove -> G.Picture 
emptyCell i f = \case 
  Nothing        -> plainCell f 
  Just (Start _) -> plainCell f
  Just (MidSimple _ _ xs) -> 
    if isJust $ find ((bit i ==) ) xs 
      then nextCell 
      else plainCell f 
  Just (MidJump _ _ _ _ xs) -> 
    if isJust $ find ((bit i ==) . cell . N.head) xs
      then nextCell 
      else plainCell f 
  where nextCell = G.color (G.light playingCellColor) $ square f 

----------------------------
--  Go back button 

triangle :: C.Color -> Float -> G.Picture
triangle c f = G.color c 
             $ G.translate (thickness / 1.5) (- (thickness / 1.5))
             $ G.rotate 315 
             $ G.polygon [(0,0),(0, - side),(tip / 1.3, (-side) / 2)]
    where
      tip       = sqrt (side ^ 2 - (side / 2) ^ 2)
      thickness = f / 4 - f / 10
      side      = 1.5 * thickness 

goBack :: C.Color -> Float -> G.Picture 
goBack c f = G.pictures [ G.color (C.greyN 0.5) $ square f
                        , G.color c $ G.thickArc 0 315 radius thickness 
                        , triangle c f 
                        ]
  where radius    = f / 4 
        thickness = f / 10 

triangle' :: C.Color -> Float -> G.Picture
triangle' c f = G.color c
              $ G.translate (- thickness / 1.5) (- (thickness / 1.5))
              $ G.rotate 45
              $ G.polygon [(0,0),(0, - side), (- tip / 1.3, (-side) / 2)]
    where
      tip       = sqrt (side ^ 2 - (side / 2) ^ 2)
      thickness = f / 4 - f / 10
      side      = 1.5 * thickness 

goforward  :: C.Color -> Float -> G.Picture 
goforward c f = G.rotate 180 
              $ G.pictures [ G.color (C.greyN 0.5) $ square f 
                           , G.rotate 135 $ G.color c  $ G.thickArc 0 315 radius thickness 
                           , triangle' c f 
                           ] 
  where radius    = f / 4 
        thickness = f / 10 

