{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module HumanMove (imove)  where

import qualified Data.List.NonEmpty as N (uncons, head, filter, fromList)
import Data.Foldable (find) 
import Data.Word (Word64) 
import Game 
import Types 
import SimpleMoves
import Board
import Data.Bits (Bits(..))
import Debug.Trace 

imove :: Word64 -> History n m -> Game 'Real -> Play 
imove _ hs g@(Over _ _ _ _)  = Play hs g
imove n hs g@(HS board (Start (SHM sms))) = 
  case N.filter ((== n) . fst . snd) sms of 
    [] -> Play hs g
    xs -> Play hs $ HS board (MidSimple sz n $ N.fromList nl)
     where sz = fst $ head xs 
           nl = (snd . snd) <$> xs
imove n hs g@(HS board (Start (JHM sjs))) = 
  case N.filter ((== n) . root) sjs of 
    [] -> Play hs g 
    xs -> Play hs $ HS board (MidJump sz n n mempty pt)
     where sz = siz $ head xs 
           pt = N.fromList $ fmap forest xs
imove n hs g@(HS board (MidSimple sz begin ends)) = 
  case find (n ==) ends of 
    Nothing -> Play hs g 
    Just e  -> 
     let simplem  = (sz, (begin .|.  e))
         newboard = mkSimpleMove simplem board Human 
     in case allcmoves newboard Computer of 
          Nothing -> Play (nforward (Left simplem) hs) $ Over (SS mempty) Loss Computer newboard
          Just mv -> Play (nforward (Left simplem) hs) $ AI Computer newboard mv mempty
imove n hs g@(HS board (MidJump sz begin x c paths)) = 
  case N.filter ((== n) . cell . N.head) paths of 
    [] -> Play hs g 
    xs -> 
      let parts = N.fromList $ N.uncons <$> xs
          point = N.head $ fst <$> parts 
          rest  = snd <$> parts 
      in case sequence rest of 
          Nothing -> 
            let newboard = jump (x .|. cell point) (eat point) board sz 
                mvs      = allcmoves newboard Computer
                newH     = Right (AMove sz (begin .|. cell point) (eat point <> c))
            in case mvs of 
                 Nothing -> Play (nforward newH hs) $ Over (SS mempty) Loss Computer newboard
                 Just ms -> Play (nforward newH hs) $ AI Computer newboard ms mempty
          Just xss -> 
            let newboard = jump (x .|. cell point) (eat point) board sz 
                chr      = c <> eat point 
            in Play hs $ HS newboard (MidJump sz begin (cell point) chr xss)

jump :: Word64 -> Cache -> Board -> Size -> Board
jump path Cache{..} Board{..} = \case 
  Pawn -> 
    let newuppawns   = uppawns `xor` path `xor` (path .&. upperRow)
        newdownpawns = downpawns `xor` pawn 
        newdownkings = downkings `xor` king 
        newupkings   = upkings .|. (path .&. upperRow)
    in Board newuppawns newdownpawns newupkings newdownkings mask 
  King ->
    let newupkings   = upkings `xor` path 
        newdownpawns = downpawns `xor` pawn 
        newdownkings = downkings `xor` king 
    in Board uppawns newdownpawns newupkings newdownkings mask

