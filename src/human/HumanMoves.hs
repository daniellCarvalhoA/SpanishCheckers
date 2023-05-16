{-# LANGUAGE RecordWildCards #-}
module HumanMoves where 
import Types (Size (..), HumanMove, Memoize, Turn (..))
import Data.List.NonEmpty ( NonEmpty (..), (<|) )
import Data.Word (Word8)
import Board
import Data.Bits (Bits(..), countTrailingZeros)
import HumanKingJump (kingJumpMove)
import SimpleMove (simpleKingMove, simplePawnMove)
import HumanPawnJump

type SMovesAt   = (Size, NonEmpty (Word8, Word8))
type HJMovesAt  = (Size, NonEmpty HumanMove)


data HMoves = SMoves (NonEmpty SMovesAt)
            | JMoves (NonEmpty HJMovesAt)
            | NoHMoves
            deriving (Eq, Show)

consSM :: Maybe SMovesAt -> (Memoize, HMoves) -> (Memoize, HMoves)
consSM Nothing (c, mvs)        = (c, mvs)
consSM (Just m) (_, NoHMoves)  = (mempty, SMoves $ m :| [])
consSM (Just m) (c, SMoves ms) = (c, SMoves $ m <| ms)
consSM _        (c, JMoves ms) = (c, JMoves ms)

consJM :: Maybe (Memoize, HJMovesAt) -> (Memoize, HMoves) -> (Memoize, HMoves)
consJM Nothing (c, mvs)  = (c, mvs)
consJM (Just (c, m)) (_, NoHMoves) = (c, JMoves $ m :| [])
consJM (Just (c, m)) (_, SMoves _) = (c, JMoves $ m :| [])
consJM (Just (c, m)) (c', JMoves ms) 
  | c  > c'   = (c, JMoves $ m :| [])
  | c' > c    = (c', JMoves ms )
  | otherwise = (c, JMoves $ m <| ms)

jMove :: (Memoize, [HumanMove]) -> Size -> Maybe (Memoize, HJMovesAt)
jMove (_,[]) _      = Nothing 
jMove (c, m : ms) s = Just (c, (s, m :| ms))

sMove :: [(Word8,Word8)] -> Size -> Maybe SMovesAt 
sMove [] _       = Nothing 
sMove (m : ms) s = Just (s, m :| ms)

allMoves :: Turn -> Board -> HMoves 
allMoves turn b@Board{..} = snd $ 
  go (if turn == Human then popCount ups + popCount kups else popCount downs + popCount kdowns)
     (if turn == Human then countTrailingZeros ups   `min` countTrailingZeros kups 
                       else countTrailingZeros downs `min` countTrailingZeros kdowns)
  where 
    go 0 _ = (mempty, NoHMoves)
    go n i 
      | testBit (if turn == Human then kups else kdowns) i 
          = jMove (kingJumpMove turn b j) King `consJM` (sMove (simpleKingMove j b) King `consSM` go (n- 1) (succ i))
      | testBit (if turn == Human then ups else downs) i 
          = jMove (jumpPawnMove turn j b) Pawn `consJM` (sMove (simplePawnMove j turn b) Pawn `consSM` go (n - 1) (succ i))
      | otherwise
          = go n (succ i) 
      where j = fromIntegral i 