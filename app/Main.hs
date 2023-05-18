{-# LANGUAGE RecordWildCards  #-}
module Main (main) where


import Graphics.Gloss.Interface.IO.Game
  (Event (EventKey), MouseButton (LeftButton), Key (..), KeyState (..), playIO, Display(..), black)
import Data.Tuple.Extra    (both)
import Data.Foldable.Extra (notNull)
import Types
import Game
import Render ( GameState(..), isEndState , cellDimensions, buildInitialDim, renderState, Dimensions(..))
import AlphaBeta
import MkHumanMove
import AlphaBeta (iterativeDeepening)

data Click = CellClick Int | UndoClick

type CellWidth = Float

filterMouseClick :: CellWidth -> (Float, Float) -> Maybe Click
filterMouseClick cellWidth pointInScreen =
  let maybeCellClick = toBoardNumber <$> playable (both (floor . (/cellWidth)) pointInScreen)
      playable p@(x,y) | x < (-4) || x > 3 || y < (-4) || y > 3 = Nothing
                       | even x && even y = Nothing
                       | odd x  && odd y  = Nothing
                       | otherwise        = Just p
      maybeUndoClick p = (x > 5.75) && (x < 6.75) && (y > (-0.5)) &&  y < 0.5
        where (x,y) = both (/cellWidth) p

  in case maybeCellClick of
       Just x  -> Just $ CellClick x
       Nothing -> if maybeUndoClick pointInScreen then Just UndoClick else Nothing

toBoardNumber :: (Int, Int) -> Int
toBoardNumber (x, y) = (4 + x) `div` 2 + 4 * (4 + y)

-- make GameState a Reader 
eventHandler :: Event -> GameState -> IO GameState
eventHandler _ g | isEndState g = return g
eventHandler (EventKey (MouseButton LeftButton) Up _ point) g = do
  print $ game g
  let click = filterMouseClick (cellDimensions $ dims g ) point
  case fst $ assoc $ game g of
    Human -> print point -- debug
          >> return (case click of
                     Nothing            -> g
                     Just UndoClick
                        | state (game g) == Start && notNull (ghisto (game g)) -> GameState (undo' $ undo' (game g)) (dims g)
                        | otherwise  -> g
                     Just (CellClick i) -> GameState (step j (game g)) (dims g)
                      where j = fromIntegral i
                    )
    Computer -> return g
eventHandler _ g@GameState{..}
  | fst (assoc game) == Computer = GameState <$> iterativeDeepening 10000000 game <*> pure dims
  | otherwise                    = return g


buildInitialState :: IO GameState
buildInitialState = GameState <$> buildInitialGame <*> buildInitialDim

main :: IO ()
main = do
    istate <- buildInitialState
    let Dimensions{..} = dims istate
        xcenter = fst screenDimensions `div` 2 + fst translation
        ycenter = snd screenDimensions `div` 2 + snd translation
        center = (xcenter, ycenter)
        window = InWindow "Spanish Checkers"  windowDimensions center
    playIO window black 1000 istate (return . renderState) eventHandler (const return)


