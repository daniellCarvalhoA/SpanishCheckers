{-# LANGUAGE RecordWildCards #-}

module Render (renderState, GameState(..), Dimensions(..), buildInitialDim, disp, isEndState) where

import qualified Graphics.Gloss.Interface.IO.Game as G
import qualified Graphics.Gloss.Interface.IO.Display as G
import qualified Graphics.Gloss.Interface.Environment as G
import Data.Bifunctor (bimap)
import Data.Bits ( Bits(testBit) )
import Colors
    ( blackColor,
      kingBlackColor,
      kingWhiteColor,
      nonCellColor,
      playingCellColor,
      whiteColor )
import GoBack ( goBack )
import Types
    ( Color(Black, White),
      Turn(Computer, Human),
      Assoc,
      )
import Game (Game(..), initialGame, isGameOver)
import Board (Board(..), groupAt)

data Dimensions = Dimensions {
    screenDimensions :: (Int, Int)
  , windowDimensions :: (Int, Int)
  , cellDimensions   :: Float
  , translation      :: (Int, Int)
} deriving Show

data GameState = GameState {
    game :: Game
  , dims :: Dimensions
}

isEndState :: GameState -> Bool 
isEndState = isGameOver . game

renderState :: GameState -> G.Picture
renderState GameState{..} =
    G.translate (negate $ cellDimensions dims * (7 / 2)) (negate (cellDimensions dims) * (7 / 2))
  $ G.pictures
  $ (<>[back]) $ concatMap addToPicture (zip [0..7] $ groupAt 8 [0 .. 63])
  where
    addToPicture = row (assoc game) (cellDimensions dims) (board game)
    back = G.translate (9.75 * cellDimensions dims) (3.5 * cellDimensions dims) 
         $ goBack G.white $ cellDimensions dims



row :: Assoc -> Float -> Board -> (Int, [Int]) -> [G.Picture]
row asc n Board{..} (y, xs) = map renderCell xs
    where
        renderCell x
            | even x && even y       = G.translate x' y' $ nonCell n
            | odd x  && odd  y       = G.translate x' y' $ nonCell n
            | ups `testBit` point    = G.translate x' y' $ pawnHumanCell n asc
            | kups `testBit` point   = G.translate x' y' $ kingHumanCell n asc
            | downs `testBit` point  = G.translate x' y' $ pawnComputerCell n asc
            | kdowns `testBit` point = G.translate x' y' $ kingComputerCell n asc
            | otherwise              = G.translate x' y' $ emptyCell n
            where (x', y') = (n * fromIntegral (x `mod` 8), n * fromIntegral  y)
                  point = x `div` 2

square :: Float -> G.Picture
square f = G.rectangleSolid f f

pawnHumanCell :: Float -> Assoc -> G.Picture
pawnHumanCell f asc = G.pictures $ (G.color playingCellColor (square f) :) $ (:[]) $
  case asc of
    (Human, White)    -> G.color whiteColor $ G.thickCircle 0 radius
    (Human, Black)    -> G.color blackColor $ G.thickCircle 0 radius
    (Computer, White) -> G.color blackColor $ G.thickCircle 0 radius
    (Computer, Black) -> G.color whiteColor $ G.thickCircle 0 radius
    where radius = f * (2 / 3)

kingHumanCell :: Float -> Assoc -> G.Picture
kingHumanCell f asc = G.pictures $ (G.color playingCellColor (square f) :) $ (:[]) $
  case asc of
    (Human, White)    -> G.color kingWhiteColor $ G.thickCircle 0 radius
    (Human, Black)    -> G.color kingBlackColor $ G.thickCircle 0 radius
    (Computer, White) -> G.color kingBlackColor $ G.thickCircle 0 radius
    (Computer, Black) -> G.color kingWhiteColor $ G.thickCircle 0 radius
    where radius = f * (2 / 3)

pawnComputerCell :: Float -> Assoc -> G.Picture
pawnComputerCell f asc = G.pictures $ (G.color playingCellColor (square f) :) $ (:[]) $
  case asc of
    (Human, White)    -> G.color blackColor $ G.thickCircle 0 radius
    (Human, Black)    -> G.color whiteColor $ G.thickCircle 0 radius
    (Computer, White) -> G.color whiteColor $ G.thickCircle 0 radius
    (Computer, Black) -> G.color blackColor $ G.thickCircle 0 radius
    where radius = f * (2 / 3)

kingComputerCell :: Float -> Assoc -> G.Picture
kingComputerCell f asc = G.pictures $ (G.color playingCellColor (square f) :) $ (:[]) $
  case asc of
    (Human, White)    -> G.color kingBlackColor $ G.thickCircle 0 radius
    (Human, Black)    -> G.color kingWhiteColor $ G.thickCircle 0 radius
    (Computer, White) -> G.color kingWhiteColor $ G.thickCircle 0 radius
    (Computer, Black) -> G.color kingBlackColor $ G.thickCircle 0 radius
    where radius = f * (2 / 3)

nonCell :: Float -> G.Picture
nonCell f = G.color nonCellColor $ square f

emptyCell :: Float -> G.Picture
emptyCell f = G.color playingCellColor $ square f

buildInitialDim :: IO Dimensions
buildInitialDim = do
    sc <- G.getScreenSize
    let winDim   = bimap width height sc
        transDim = (negate $ fst winDim `div` 2, negate $ snd winDim `div` 2)
        cellDim  = fromIntegral ( 3 * uncurry min winDim) / 4 / 8
    return $ Dimensions sc winDim cellDim transDim

width :: Int -> Int
width w = 2 * w `div` 3

height :: Int -> Int
height h = 2 * h `div` 3

disp :: IO ()
disp = do
    ds@Dimensions{..} <- buildInitialDim
    let center = bimap ((fst screenDimensions `div` 2) + ) ((snd screenDimensions `div` 2) +) translation
        win    = G.InWindow "" windowDimensions center
    G.displayIO win G.black (return $ renderState $ GameState initialGame ds) (const (return ()))
