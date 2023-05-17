{-# LANGUAGE RecordWildCards #-}

module AlphaBeta(computer, extendGame, devolveGame, undo) where 

import Types              (Assoc, ComputerMove, Size, Status (..), changeTurn, Turn (..) )
import Board              (Board (..))
import Game               (Game (..), LocalHistory, GlobalHistory, State(..))
import MkJumpMove         (mkJumpMove, rvJumpMove )
import ComputerMoves      (allMoves, CMoves (..))
import MkSimpleMove       (mkSimpleMove, rvSimpleMove)
import Data.Sequence      (Seq(..))
import Data.Word          (Word8)
import Data.List.NonEmpty (NonEmpty (..), fromList)
import Data.Semigroup     (Semigroup(sconcat))
import Data.Bits          (Bits(..))

import qualified HumanMoves         as H        (allMoves, HMoves (NoHMoves))
import qualified Data.List.NonEmpty as N        (reverse, fromList)
import qualified Data.List          as List     (scanl')
import qualified Data.Foldable      as Foldable (toList)


computer :: Int -> Game -> Game 
computer n = backtrack . alphaBeta n

alphaBeta :: Int -> Game -> Game
alphaBeta n game 
  | fst (assoc game) == Computer = snd $ maxValue n minBound maxBound game 
  | otherwise                    = snd $ minValue n minBound maxBound game 
  where 
    maxValue depth a b g@Game{..} 
      | status == GameOver = (minBound,  g)
      | depth == 0         = (utility g, g)
      | otherwise = 
        let nextGames = N.reverse $ extendGame g

            getMinmaxAndAlpha (bestMinmaxSofar, a') game' = 
              let (newMinmax, nextGame) = bestMinmaxSofar `max'` minValue (depth - 1) a' b game' 
              in ((newMinmax, nextGame), a `max` newMinmax)
            
            (best, _ ) = 
              takeFirstorLast (\(v,_) -> fst v >= b) $ 
                scanl' getMinmaxAndAlpha ((minBound, g), a) nextGames 
        in best
    
    minValue depth a b g@Game{..}
      | status == GameOver = (maxBound,  g)
      | depth == 0         = (utility g, g)
      | otherwise = 
        let nextGames = N.reverse $ extendGame g 

            getMinmaxAndBeta (bestMinmaxSofar, b') game' = 
              let (newMinmax, nextGame) = bestMinmaxSofar `min'` maxValue (depth - 1) a b' game' 
              in ((newMinmax, nextGame), b `min` newMinmax)
            
            (best, _) = 
              takeFirstorLast (\(v, _) -> fst v <= a) $ 
                scanl' getMinmaxAndBeta ((maxBound, g), b) nextGames 
        in best 

scanl' :: Foldable t => (a1 -> a2 -> a1) -> a1 -> t a2 -> NonEmpty a1
scanl' f z = fromList . List.scanl' f z . Foldable.toList

utility :: Game -> Int 
utility Game{..} = popCount (downs board .|. kdowns board) - popCount (ups board .|. kups board)

min' :: (Int, Game) -> (Int, Game) -> (Int, Game)
min' (x, g1) (y, g2) | x < y     = (x, g1)
                     | otherwise = (y, g2)

max' :: (Int, Game) -> (Int, Game) -> (Int, Game)
max' (x, g1) (y, g2) | x > y     = (x, g1)
                     | otherwise = (y, g2)
 
takeFirstorLast :: (a -> Bool) -> NonEmpty a -> a 
takeFirstorLast _    (x :| []) = x 
takeFirstorLast cond (x :| xs) = if cond x then x else takeFirstorLast cond (N.fromList xs)

backtrack :: Game -> Game 
backtrack = f . until (single . lhisto) devolveGame
  where 
    f Game{..} = Game status assoc board NoCMoves (H.allMoves (fst assoc) board) Empty (hd lhisto : ghisto) Start
      where hd (x :<| Empty) = x
            hd _             = error "until single makes sure this does not happen "      

single :: LocalHistory -> Bool 
single (_ :<| Empty) = True 
single _             = False 

extendGame :: Game -> NonEmpty Game 
extendGame g@Game{..} = 
  case cMoves of 
    SMoves mvs -> sconcat $ h (extendSimpleGame assoc status board lhisto ghisto) <$> mvs 
    JMoves mvs -> sconcat $ h (extendJumpGame   assoc status board lhisto ghisto) <$> mvs 
    NoCMoves   -> g :| []
  where h f (size, l) = f size <$> l

extendJumpGame :: Assoc -> Status -> Board -> LocalHistory -> GlobalHistory -> Size -> ComputerMove -> Game 
extendJumpGame asc sts brd lhisto ghisto size mv = 
  let newBoard  = mkJumpMove brd size mv (fst asc) 
      asc'      = changeTurn asc 
      newCMoves = allMoves (fst asc') newBoard 
      newStatus = case newCMoves of NoCMoves -> GameOver; _ -> OnGoing
  in if sts == GameOver
       then Game sts asc brd NoCMoves H.NoHMoves lhisto ghisto Start
       else Game newStatus asc' newBoard newCMoves H.NoHMoves (Right (size, mv) :<| lhisto) ghisto Start

extendSimpleGame :: Assoc -> Status -> Board -> LocalHistory -> GlobalHistory -> Size -> (Word8, Word8) -> Game 
extendSimpleGame asc sts brd lhisto ghisto size is = 
  let newBoard  = mkSimpleMove (fst asc) brd size is 
      asc'      = changeTurn asc 
      newCMoves = allMoves (fst asc') newBoard 
      newStatus = case newCMoves of NoCMoves -> GameOver; _ -> OnGoing 
  in if sts == GameOver 
       then Game sts asc brd NoCMoves H.NoHMoves lhisto ghisto Start
       else Game newStatus asc' newBoard newCMoves H.NoHMoves (Left (size, is) :<| lhisto) ghisto Start

devolveGame :: Game -> Game
devolveGame g@Game{..} = 
  case lhisto of 
    Empty    -> g 
    (Left (sz, is) :<| hs) -> 
      let newBoard  = rvSimpleMove (fst assoc) board sz is
          asc'      = changeTurn assoc
          newCMoves = allMoves (fst asc') newBoard 
      in Game OnGoing asc' newBoard newCMoves H.NoHMoves hs ghisto Start
    (Right (sz, mv) :<| hs) -> 
      let newBoard  = rvJumpMove board sz mv (fst assoc)
          asc'      = changeTurn assoc 
          newCMoves = allMoves (fst asc') newBoard 
      in Game OnGoing asc' newBoard newCMoves H.NoHMoves hs ghisto Start

undo :: Game -> Game
undo g@Game{..} = 
  case ghisto of 
    []    -> g 
    (Left (sz, is) : hs) -> 
      let newBoard  = rvSimpleMove (fst assoc) board sz is
          asc'      = changeTurn assoc
          newCMoves = allMoves (fst asc') newBoard 
      in Game OnGoing asc' newBoard newCMoves H.NoHMoves lhisto hs Start
    (Right (sz, mv) : hs) -> 
      let newBoard  = rvJumpMove board sz mv (fst assoc)
          asc'      = changeTurn assoc 
          newCMoves = allMoves (fst asc') newBoard 
      in Game OnGoing asc' newBoard newCMoves H.NoHMoves lhisto hs Start


