{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module MkHumanMove where 
import Data.Word (Word8)
import Game ( Game (..), State (..) )
import qualified HumanMoves    as HM
import qualified ComputerMoves as CM 
import Data.Foldable (find)
import Data.Maybe (isJust)
import Types (Status(..), changeTurn, HumanMove (..), Turn (..), Point (..), Size (..), ComputerMove (ComputerMove), addToCache)
import MkSimpleMove (mkSimpleMove)
import qualified Data.List.NonEmpty as N 
import Board ( Board(..), swap, atUpperRow, atLowerRow )
import Control.Arrow ( ArrowChoice((|||)) )
import Data.Bits (Bits(..))
import Data.Bifunctor (bimap)


step :: Word8 -> Game -> Game 
step n g@Game{..} 
  | status == GameOver    = g 
  | fst assoc == Computer = g 
  | otherwise =
    case state of 
      Start -> 
        case hMoves of 
          HM.SMoves sms -> 
            case find (isJust . find ((n==) . fst) . snd ) sms of 
              Nothing -> g 
              Just (sz, pws) -> Game OnGoing assoc board CM.NoCMoves HM.NoHMoves lhisto ghisto (StepS (sz, n, pws)) 
          HM.JMoves jms -> 
            case find (isJust . find ((n ==) . root) . snd) jms of 
              Nothing -> g 
              Just (sz, pws) -> 
                let mv = (sz, n, n, 0, mempty, forest <$> pws) 
                in Game OnGoing assoc board CM.NoCMoves HM.NoHMoves lhisto ghisto (StepJ mv)
          HM.NoHMoves -> g 
      StepS (sz, x, ss) -> 
        case find ((n==) . snd) ss of 
          Nothing -> g
          Just (_, y) -> 
            let newBoard = mkSimpleMove (fst assoc) board sz (x, y)
                mvs      = CM.allMoves (fst newturn) newBoard 
                newturn  = changeTurn assoc
                st       = case mvs of CM.NoCMoves -> GameOver; _ -> OnGoing 
            in Game st newturn newBoard mvs HM.NoHMoves lhisto (Left (sz,(x,y)) : ghisto) Start 
      StepJ (sz, s, x, p, c, ys) -> 
        let next = N.filter ((n==) . cell . N.head) ys 
        in case next of
          [] -> g 
          xs -> 
            let parts = N.fromList $ N.uncons <$> xs 
                point = N.head $ fst <$> parts 
                rest  = snd  <$> parts 
            in case sequence rest of 
              Nothing -> 
                let newBoard = jump x point board sz $ fst assoc 
                    newturn  = changeTurn assoc 
                    mvs      = CM.allMoves (fst newturn) newBoard 
                    st       = case mvs of CM.NoCMoves -> GameOver; _ -> OnGoing
                    newG     = Right (sz, ComputerMove s (addToCache c $ eat point) (setBit p j) n)
                    j        = fromIntegral $ cell point
                in Game st newturn newBoard mvs HM.NoHMoves lhisto (newG : ghisto) Start
              Just xss -> 
                let newBoard = jump x point board sz $ fst assoc 
                    j        = fromIntegral $ cell point 
                    ch       = addToCache c $ eat point 
                in Game OnGoing assoc newBoard CM.NoCMoves HM.NoHMoves lhisto ghisto (StepJ (sz,s, cell point, setBit p j, ch, xss ))

jump :: Word8 -> Point -> Board -> Size -> Turn -> Board
jump x Point{..} Board{..} sz = \case 
  Human ->    Board (seteat $ swap emptys (x', cell'))
                    (if sz == Pawn && not (atUpperRow cell)
                      then swap ups (cell', x')
                      else if sz == Pawn then clearBit ups x' else ups)
                    ((clearBit downs ||| const downs) eat')
                    (if sz == King 
                      then swap kups (cell', x')
                      else if atUpperRow cell then setBit kups cell' else kups)
                    ((const kdowns ||| clearBit kdowns) eat')
  Computer -> Board (seteat $ swap emptys (x', cell'))
                    ((clearBit ups ||| fromIntegral) eat')
                    (if sz == Pawn && not (atLowerRow cell)
                      then swap downs (cell', x')
                      else if sz == Pawn then clearBit downs x' else downs)
                    ((fromIntegral ||| clearBit kups) eat')
                    (if sz == King 
                      then swap kdowns (cell', x')
                      else if atLowerRow cell then setBit kdowns cell' else kdowns)
  where seteat = (flip setBit ||| flip setBit) eat'
        eat'   = bimap fromIntegral fromIntegral eat
        cell'  = fromIntegral cell
        x'     = fromIntegral x  

                 
