{-# LANGUAGE RecordWildCards #-}


module SimplePawn where

import Game (Board(..), Turn (..))
import Data.Word (Word8)
import Data.Bits (Bits(..))
import Data.Maybe (catMaybes)

cellsPerRow :: Word8
cellsPerRow = 4

nofCells :: Word8
nofCells = 31 -- counting from 0

nofRows :: Word8
nofRows = 7  -- counting from 0

rup :: Board -> Word8 -> Maybe (Word8, Word8)
rup Board{..} n
    | not $ testBit emptys b     = Nothing
    | d == nofRows || m == 3 && even d = Nothing
    | otherwise                  = Just (n,t)
    where r      = d `mod` 2
          (d, m) = n `divMod` cellsPerRow
          t      = n + 5 - r
          b      = fromIntegral t

lup :: Board -> Word8 -> Maybe (Word8, Word8)
lup Board{..} n
    | not $ testBit emptys b    = Nothing
    | d == nofRows || m == 0 && odd d = Nothing
    | otherwise                 = Just (n, t)
    where r     = d `mod` 2
          (d,m) = n `divMod` cellsPerRow
          t     = n + 4 - r
          b     = fromIntegral t


hsimplemove :: Word8 -> Board -> [(Word8,Word8)]
hsimplemove n Board{..} 
    | notLeft && notRight = []   -- occupied on both sides 
    | notRight            = [(n,tl)]
    | notLeft             = [(n,tr)]
    | otherwise           = [(n,tl),(n,tr)]
    where notLeft  = not (emptys `testBit` bl) || (d == nofRows || (m == 0 && odd  d))
          notRight = not (emptys `testBit` br) || (d == nofRows || (m == 3 && even d))
          r = d `mod` 2
          (d, m) = n `divMod` cellsPerRow
          tl     = n + 4 - r
          tr     = 1 + tl
          bl     = fromIntegral tl
          br     = fromIntegral tr

rdp :: Board -> Word8 -> Maybe (Word8, Word8)
rdp Board{..} n
    | not $ testBit emptys b     = Nothing
    | d == 0 || m == 3 && even d = Nothing
    | otherwise                  = Just (n, n - t)
    where r      = (nofCells - n) `div` cellsPerRow `mod` 2
          (d, m) =  n `divMod` cellsPerRow
          t      = 4 - r
          b      = fromIntegral $ n - t

ldp :: Board -> Word8 -> Maybe (Word8, Word8)
ldp Board{..} n
    | not $ testBit emptys b = Nothing
    | d == 0 || m == 0 && odd d = Nothing
    | otherwise                 = Just (n, n - t)
    where r      = (nofCells - n) `div` 4 `mod` 2
          (d, m) = n `divMod` 4
          t      = 5 - r
          b      = fromIntegral $ n - t

simplePawnMove :: Word8 -> Turn -> Board  -> [(Word8, Word8)]
simplePawnMove n turn board = catMaybes $
    case turn of
        Human    -> [lup board n, rup board n]
        Computer -> [ldp board n, rdp board n]

