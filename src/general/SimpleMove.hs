{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module SimpleMove (
       simplePawnMove
     , simpleKingMove
     ) where

import Game       (Board(..), Turn (..))
import Utils      (rup, lup, rdp, ldp)
import Data.Word  (Word8)
import Data.List  (unfoldr)
import Data.Maybe (catMaybes)
-- import qualified Data.Sequence as S

------------
--- Simple Pawn Move


-- According to measuremebts thsi is worst
-- hsimplemove :: Word8 -> Board -> [(Word8,Word8)]
-- hsimplemove n Board{..} 
--     | notLeft && notRight = []   -- occupied on both sides 
--     | notRight            = [(n,tl)]
--     | notLeft             = [(n,tr)]
--     | otherwise           = [(n,tl),(n,tr)]
--     where notLeft  = not (emptys `testBit` bl) || (d == nofRows || (m == 0 && odd  d))
--           notRight = not (emptys `testBit` br) || (d == nofRows || (m == 3 && even d))
--           r = d `mod` 2
--           (d, m) = n `divMod` cellsPerRow
--           tl     = n + 4 - r
--           tr     = 1 + tl
--           bl     = fromIntegral tl
--           br     = fromIntegral tr


simplePawnMove :: Word8 -> Turn -> Board  -> [(Word8, Word8)]
simplePawnMove n turn board = catMaybes $
    case turn of
        Human    -> [(n,) <$> lup board n, (n,) <$> rup board n]
        Computer -> [(n,) <$> ldp board n, (n,) <$> rdp board n]


-----------
---- Simple King Move 


diagonal :: (Board -> Word8 -> Maybe Word8) -> Word8 -> Board -> [(Word8, Word8)]
diagonal fun i = go i 
    where go m board = 
            unfoldr (\x -> case fun board x of 
                            Just t  -> Just ((i, t), t)
                            Nothing -> Nothing
            ) m

rudiagonal :: Word8 -> Board -> [(Word8, Word8)]
rudiagonal = diagonal rup 

ludiagonal :: Word8 -> Board -> [(Word8, Word8)]
ludiagonal = diagonal lup 

rddiagonal :: Word8 -> Board -> [(Word8, Word8)]
rddiagonal = diagonal rdp 

lddiagonal :: Word8 -> Board -> [(Word8, Word8)]
lddiagonal = diagonal ldp 

simpleKingMove :: Word8 -> Board -> [(Word8, Word8)]
simpleKingMove i board = rudiagonal i board <> (ludiagonal i board <> (rddiagonal i board <> lddiagonal i board))

-- simpleKingMove' :: Word8 -> Board -> S.Seq (Word8, Word8)
-- simpleKingMove' i board = rudiagonal' i board S.>< (ludiagonal' i board S.>< (rddiagonal' i board S.>< lddiagonal' i board))

-- rudiagonal' :: Word8 -> Board -> S.Seq (Word8, Word8)
-- rudiagonal' = diagonal' rup 

-- ludiagonal' :: Word8 -> Board -> S.Seq (Word8, Word8)
-- ludiagonal' = diagonal' lup 

-- rddiagonal' :: Word8 -> Board -> S.Seq (Word8, Word8)
-- rddiagonal' = diagonal' rdp 

-- lddiagonal' :: Word8 -> Board -> S.Seq (Word8, Word8)
-- lddiagonal' = diagonal' ldp 

-- diagonal' :: (Board -> Word8 -> Maybe Word8) -> Word8 -> Board -> S.Seq (Word8, Word8)
-- diagonal' fun i = go i 
--     where go m board = 
--             S.unfoldr (\x -> case fun board x of 
--                             Just t  -> Just ((i, t), t)
--                             Nothing -> Nothing
--             ) m
