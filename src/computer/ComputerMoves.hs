{-# LANGUAGE RecordWildCards #-}
module ComputerMoves where 

import Data.Word (Word8)
import Types ( ComputerMove, Size (..), Cache, Turn (..) )
import Board ( Board(..) )  
import Data.List.NonEmpty (NonEmpty (..), (<|))
import SimpleMove ( simpleKingMove, simplePawnMove )
import ComputerKingJump ( kingJumpMove )
import ComputerPawnJump ( jumpPawnMove )
import Data.Bits (Bits(..), FiniteBits (countTrailingZeros))

type SMovesAt   = (Size, NonEmpty (Word8, Word8))
type CJMovesAt  = (Size, NonEmpty ComputerMove)


data CMoves = SMoves (NonEmpty SMovesAt)
            | JMoves (NonEmpty CJMovesAt)
            | NoCMoves
            deriving (Eq, Show)
    


consSM :: Maybe SMovesAt -> (Cache, CMoves) -> (Cache, CMoves)
consSM Nothing (c, mvs)        = (c, mvs)
consSM (Just m) (_, NoCMoves)  = (mempty,SMoves $ m :| [])
consSM (Just m) (c, SMoves ms) = (c, SMoves $ m <| ms) 
consSM _        (c, JMoves ms) = (c, JMoves ms)

consJM :: Maybe (Cache, CJMovesAt) -> (Cache, CMoves) -> (Cache, CMoves)
consJM Nothing (c, mvs)            = (c, mvs)
consJM (Just (c, m)) (_, NoCMoves) = (c, JMoves $ m :| [])
consJM (Just (c, m)) (_, SMoves _) = (c, JMoves $ m :| [])
consJM (Just (c, m)) (c', JMoves ms) 
  | c  > c'   = (c, JMoves $ m :| [])
  | c' > c    = (c', JMoves ms )
  | otherwise = (c, JMoves $ m <| ms)

jMove :: (Cache, [ComputerMove]) -> Size -> Maybe (Cache, CJMovesAt)
jMove (_,[]) _      = Nothing 
jMove (c, m : ms) s = Just (c, (s, m :| ms))

sMove :: [(Word8,Word8)] -> Size -> Maybe SMovesAt 
sMove [] _       = Nothing 
sMove (m : ms) s = Just (s, m :| ms)

allMoves :: Turn -> Board -> CMoves 
allMoves turn b@Board{..} = snd $ 
  go (if turn == Human then popCount ups + popCount kups else popCount downs + popCount kdowns)
     (if turn == Human then countTrailingZeros ups   `min` countTrailingZeros kups 
                       else countTrailingZeros downs `min` countTrailingZeros kdowns)
  where 
    go 0 _ = (mempty, NoCMoves)
    go n i 
      | testBit (if turn == Human then kups else kdowns) i 
          = jMove (kingJumpMove turn b j) King `consJM` (sMove (simpleKingMove j b) King `consSM` go (n- 1) (succ i))
      | testBit (if turn == Human then ups else downs) i 
          = jMove (jumpPawnMove turn j b) Pawn `consJM` (sMove (simplePawnMove j turn b) Pawn `consSM` go (n - 1) (succ i))
      | otherwise
          = go n (succ i) 
      where j = fromIntegral i 