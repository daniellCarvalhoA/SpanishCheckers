{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module ComputerPawnJump (jumpPawnMove) where 
import Types (cachePawn, cacheKing, Cache(..), Turn (..), ComputerMove (ComputerMove), consM)
import Board ( Board(..), cellsPerRow, nofCells, nofRows ) 
import Data.Word (Word8)
import Data.Bits (Bits(..))
import Control.Arrow (second)
import Utils (lup, ldp, rup, rdp)

jumpPawnMove :: Turn -> Word8 -> Board -> (Cache, [ComputerMove])
jumpPawnMove turn n board = start n mempty
  where
    start i m = 
      case (leftEat board i turn, rightEat board i turn) of 
        (Nothing,Nothing)           -> mempty
        (Nothing,Just (c,t))        -> consM m i $ go t c 
        (Just (c,t),Nothing)        -> consM m i $ go t c 
        (Just (c1,t1),Just (c2,t2)) -> consM m i (go t1 c1 `append` go t2 c2)

    go i m =  
      case (leftEat board i turn, rightEat board i turn) of 
        (Nothing,Nothing)           -> (m, [ComputerMove 0 m 0 i])
        (Nothing,Just (c,t))        -> consM m i $ go t c 
        (Just (c,t),Nothing)        -> consM m i $ go t c 
        (Just (c1,t1),Just (c2,t2)) -> consM m i (go t1 c1 `append` go t2 c2)

leftUpEat :: Board -> Word8 -> Maybe (Cache, Word8)
leftUpEat Board{..} n 
  | d == nofRows || m == 0 && odd d = Nothing
  | testBit downs  j = Just (cachePawn t, t)
  | testBit kdowns j = Just (cacheKing t, t)
  | otherwise = Nothing
  where r      = d `mod` 2
        (d, m) = n `divMod` cellsPerRow
        t      = n + 4 - r 
        j      = fromIntegral t 

rightUpEat :: Board -> Word8 -> Maybe (Cache, Word8)
rightUpEat Board{..} n 
  | d == nofRows || m == 3 && even d = Nothing
  | testBit downs  j = Just (cachePawn t, t)
  | testBit kdowns j = Just (cacheKing t, t)
  | otherwise = Nothing 
  where r      = d `mod` 2 
        (d, m) = n `divMod` cellsPerRow
        t      = n + 5 - r
        j      = fromIntegral t

leftDownEat :: Board -> Word8 -> Maybe (Cache, Word8)
leftDownEat Board{..} n 
  | d == 0 || m == 0 && odd d = Nothing 
  | testBit ups  j = Just (cachePawn t, t)
  | testBit kups j = Just (cacheKing t, t)
  | otherwise = Nothing 
  where r      = (nofCells - n) `div` cellsPerRow `mod` 2 
        (d, m) = n `divMod` cellsPerRow
        t      = n - (5 - r)
        j      = fromIntegral t 

rightDownEat :: Board -> Word8 -> Maybe (Cache, Word8)
rightDownEat Board{..} n 
  | d == 0 || m == 3 && even d = Nothing 
  | testBit ups  j = Just (cachePawn t, t)
  | testBit kups j = Just (cacheKing t, t)
  | otherwise = Nothing 
  where r      = (nofCells - n) `div` cellsPerRow `mod` 2 
        (d, m) = n `divMod` cellsPerRow
        t      = n - (4 - r)
        j      = fromIntegral t 

leftEat :: Board -> Word8 -> Turn -> Maybe (Cache, Word8)
leftEat board i = \case 
  Human    -> leftUpEat   board i >>= sequence . second (lup board)
  Computer -> leftDownEat board i >>= sequence . second (ldp board)  

rightEat :: Board -> Word8 -> Turn -> Maybe (Cache, Word8)
rightEat board i = \case 
  Human    -> rightUpEat   board i >>= sequence . second (rup board)
  Computer -> rightDownEat board i >>= sequence . second (rdp board)


append :: (Cache, [ComputerMove]) -> (Cache, [ComputerMove]) -> (Cache, [ComputerMove])
append (c1, m1) (c2, m2)
  | c1 > c2   = (c1, m1)
  | c2 > c1   = (c2, m2)
  | otherwise = (c1, m1 <> m2)