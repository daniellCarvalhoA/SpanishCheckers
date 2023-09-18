{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RecordWildCards    #-}

module Main (main) where

import Render2 
import Game 
import ParseEvents 
import Types 
import HumanMove
import qualified Graphics.Gloss.Interface.IO.Game as G
import Data.Bits
import AI hiding (Play)
-- import Debug.Trace
-- import Data.Monoid
-- import Data.Maybe
-- import Data.Tuple.Extra (both)

treshold  :: Int
treshold = 5_000_000

eventHandler :: G.Event -> Match -> IO Match 
eventHandler (G.EventKey (G.MouseButton G.LeftButton) G.Down _ point) m@(Menu ds@Dimensions{..}) = 
  case parseColor cellsize point of 
    Nothing    -> return m 
    Just White -> return $ startMatch White ds 
    Just Black -> return $ startMatch Black ds
eventHandler _ m@(Menu _ ) = return m 
eventHandler event m@Match{..} = 
  case play of 
    Play hs g@(AI _ _ _ _) -> do 
      pm <- iterativedeepening treshold g
      return $ case pm of
        Nothing -> m 
        Just mv -> Match dims color $ Play (nforward mv hs) (mkMove g  mv) 
    Play hs g@(HS _ _)     -> do 
      case event of 
        G.EventKey (G.MouseButton G.LeftButton) G.Down _ point -> 
          case parseEvent (cellsize dims) point of 
            Just (CellClick i) -> (return $ Match dims color $ imove (bit i) hs g)
          
            Just UndoClick     -> 
              case hs of 
                ZipVec (Cons _ (Cons _ _)) _  -> 
                  let (newhistory, newgame) = undo (hs, g)
                  in return $ Match dims color $ Play newhistory newgame 
                _                 -> return m 
            Just RedoClick     -> 
              case hs of 
                ZipVec _ (Cons _ (Cons _ _)) -> 
                  let (newhistory, newgame) = redo (hs, g)
                  in return $ Match dims color $ Play newhistory newgame 
                _              -> return m
            _                  -> return m 
        _                      -> return m
    Play hs g@(Over SR _ _ _) -> 
      case event of 
        G.EventKey (G.MouseButton G.LeftButton) G.Down _ point -> do 
          case parseEvent (cellsize dims) point of 
            Just UndoClick     -> 
              case hs of 
                ZipVec (Cons _ (Cons _ _)) _  -> 
                  let (newhistory, newgame) = undo (hs, g)
                  in return $ Match dims color $ Play newhistory newgame 
                _  -> return m 

            Just AgainClick -> return $ Menu dims
            _               -> return m 
        _ -> return m
    Play hs g@(Over (SS _) _ _ _) -> 
      case event of 
        G.EventKey (G.MouseButton G.LeftButton) G.Down _ point -> do 
          case parseEvent (cellsize dims) point of 
            Just UndoClick -> 
              case hs of 
                ZipVec (Cons _ _) _ -> 
                  let (newhistory, newgame) = goback (hs, g)
                  in return $ Match dims color $ Play newhistory newgame 
                _ -> return m 
            Just AgainClick -> return $ Menu dims
            _  -> return m 
        _ -> return m 
                  
main :: IO () 
main = do 
  ds <- getdimensionsfromenv
  let Dimensions{..} = ds
      window         = G.InWindow "SpanishCheckers" windowdimensions translation
  G.playIO window G.black 0 (Menu ds) (return . renderMatch) eventHandler (const return)
