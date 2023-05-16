{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase      #-}

module HumanPawnJump (jumpPawnMove )where 

import Types
    ( Turn(..),
      HumanMove(Cons),
      Memoize,
      Path,
      Point(Point),
      Eaten(..),
      mem,
      cons ) 
import Board ( Board(..), cellsPerRow, nofCells, nofRows )
import Data.Word          (Word8)
import Data.Bits          (testBit)
import Data.List.NonEmpty (NonEmpty(..))
import Utils              (ldp, lup, rup, rdp)
import Control.Arrow      (second)

jumpPawnMove :: Turn -> Word8 -> Board -> (Memoize, [HumanMove])
jumpPawnMove turn n board = start n 
  where 
    start i = 
      case (leftEat board i turn, rightEat board i turn) of 
        (Nothing,Nothing)           -> (mempty, [])
        (Nothing,Just (m,t))        -> fmap (Cons i) <$> go t m 
        (Just (m,t),Nothing)        -> fmap (Cons i) <$> go t m
        (Just (m1,t1),Just (m2,t2)) -> fmap (Cons i) <$> (go t1 m1 `append` go t2 m2)  

    go i m = 
      case (leftEat board i turn, rightEat board i turn) of 
        (Nothing,Nothing)           -> (mem m, [Point i m :| [] ])
        (Nothing,Just (m',t))       -> cons m i $ go t m' 
        (Just (m',t),Nothing)       -> cons m i $ go t m'
        (Just (m1,t1),Just (m2,t2)) -> cons m i $ (go t1 m1 `append` go t2 m2)  


append :: (Memoize, [Path]) -> (Memoize, [Path]) -> (Memoize, [Path])
append (m1, p1) (m2, p2)
    | m1 > m2   = (m1, p1)
    | m2 > m1   = (m2, p2)
    | otherwise = (m1, p1 <> p2)

leftEat :: Board -> Word8 -> Turn -> Maybe (Eaten, Word8)
leftEat board i = \case 
    Human    -> leftUpEat   board i >>= (sequence . second (lup board))
    Computer -> leftDownEat board i >>= (sequence . second (ldp board)) 

rightEat :: Board -> Word8 -> Turn -> Maybe (Eaten, Word8)
rightEat board i = \case 
    Human    -> rightUpEat   board i >>= (sequence . second (rup board))
    Computer -> rightDownEat board i >>= (sequence . second (rdp board))

leftUpEat :: Board -> Word8 -> Maybe (Eaten, Word8)
leftUpEat Board{..} n 
    | d == nofRows || m == 0 && odd d = Nothing 
    | testBit downs  b = Just (P t, t) 
    | testBit kdowns b = Just (K t, t)
    | otherwise        = Nothing
    where r      = d `mod` 2 
          (d, m) = n `divMod` cellsPerRow
          t      = n + 4 - r 
          b      = fromIntegral t 

rightUpEat :: Board -> Word8 -> Maybe (Eaten, Word8)
rightUpEat Board{..} n 
    | d == 0 || m == 3 && even d = Nothing 
    | testBit downs b  = Just (P t, t)
    | testBit kdowns b = Just (K t, t) 
    | otherwise        = Nothing
    where r      = d `mod` 2 
          (d, m) = n `divMod` cellsPerRow
          t      = n + 5 -r 
          b      = fromIntegral t

rightDownEat :: Board -> Word8 -> Maybe (Eaten, Word8)
rightDownEat Board{..} n 
    | d == 0 || m == 3 && even d = Nothing 
    | testBit ups  b = Just (P t, t)
    | testBit kups b = Just (K t, t)
    | otherwise      = Nothing
    where r     = (nofCells - n) `div` cellsPerRow `mod` 2 
          (d,m) = n `divMod` cellsPerRow
          t     = n - (4 - r)
          b     = fromIntegral t

leftDownEat :: Board -> Word8 -> Maybe (Eaten, Word8)
leftDownEat Board{..} n 
    | d == 0 || m == 0 && odd d = Nothing
    | testBit ups  b = Just (P t, t)
    | testBit kups b = Just (K t, t)
    | otherwise      = Nothing 
    where r      = (nofCells - n) `div` cellsPerRow `mod` 2 
          (d, m) = n `divMod` cellsPerRow
          t      = n - (5 - r)
          b      = fromIntegral t 