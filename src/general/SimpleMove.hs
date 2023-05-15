{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module SimpleMove (simplePawnMove, simpleKingMove) where

import Game (Board(..), Turn (..))
import Data.Word (Word8)
import Data.Bits (Bits(..))
import Data.List (unfoldr)
import Data.Maybe (catMaybes)
-- import qualified Data.Sequence as S


cellsPerRow :: Word8
cellsPerRow = 4

nofCells :: Word8
nofCells = 31 -- counting from 0

nofRows :: Word8
nofRows = 7  -- counting from 0

------------
--- Simple Pawn Move

rup :: Board -> Word8 -> Maybe Word8
rup Board{..} n
    | not $ testBit emptys b     = Nothing
    | d == nofRows || m == 3 && even d = Nothing
    | otherwise                  = Just t
    where r      = d `mod` 2
          (d, m) = n `divMod` cellsPerRow
          t      = n + 5 - r
          b      = fromIntegral t

lup :: Board -> Word8 -> Maybe Word8
lup Board{..} n
    | not $ testBit emptys b    = Nothing
    | d == nofRows || m == 0 && odd d = Nothing
    | otherwise                 = Just t
    where r     = d `mod` 2
          (d,m) = n `divMod` cellsPerRow
          t     = n + 4 - r
          b     = fromIntegral t

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

rdp :: Board -> Word8 -> Maybe Word8
rdp Board{..} n
    | not $ testBit emptys b     = Nothing
    | d == 0 || m == 3 && even d = Nothing
    | otherwise                  = Just $ n - t
    where r      = (nofCells - n) `div` cellsPerRow `mod` 2
          (d, m) =  n `divMod` cellsPerRow
          t      = 4 - r
          b      = fromIntegral $ n - t

ldp :: Board -> Word8 -> Maybe Word8
ldp Board{..} n
    | not $ testBit emptys b = Nothing
    | d == 0 || m == 0 && odd d = Nothing
    | otherwise                 = Just $ n - t
    where r      = (nofCells - n) `div` 4 `mod` 2
          (d, m) = n `divMod` 4
          t      = 5 - r
          b      = fromIntegral $ n - t

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
