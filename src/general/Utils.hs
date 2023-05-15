{-# LANGUAGE RecordWildCards #-}

module Utils ( rup, lup, rdp, ldp
             , leftDownAdj, leftUpAdj
             , rightDownAdj, rightUpAdj
             , filterNullMap, concatFilter
             ) where 

import Data.Foldable (foldl')
import Data.Bits     (testBit)
import Data.Word     (Word8)
import Game          (Board(..), nofCells, nofRows, cellsPerRow)

filterNullMap :: Foldable t => (p -> (t a, b, c, d)) -> [p] -> [(t a, b, c, d)]
filterNullMap _  [] = []
filterNullMap f (x : xs)
    | null (first x') = filterNullMap f xs
    | otherwise       = x' : filterNullMap f xs 
    where x' = f x 
          first (y, _, _, _) = y

concatFilter :: (Ord b, Foldable t, Monoid b) =>
                (a -> (b, [c])) -> t a -> (b, [c])
concatFilter f = foldl' h (mempty, [])
    where h (c, mvs) i 
            | c' > c    = (c', mvs')
            | c' < c    = (c , mvs )
            | otherwise = (c, mvs <> mvs') 
            where (c', mvs') = f i

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

rightUpAdj :: Word8 -> Maybe Word8 
rightUpAdj n 
    | d == nofRows || m == 3 && even d = Nothing
    | otherwise                        = Just $ n + 5 - r 
    where r        = d `mod` 2
          (d, m) = n `divMod` cellsPerRow

leftUpAdj :: Word8 -> Maybe Word8
leftUpAdj n 
    | d == nofRows || m == 0 && odd d = Nothing
    | otherwise                       = Just $ n + 4 - r 
    where r      = d `mod` 2 
          (d, m) = n `divMod` cellsPerRow 

rightDownAdj :: Word8 -> Maybe Word8 
rightDownAdj n 
    | d == 0 || m == 3 && even d = Nothing
    | otherwise                 = Just $ n - (4 - r)
    where r      = (nofCells - n) `div` cellsPerRow `mod` 2 
          (d, m) = n `divMod` cellsPerRow

leftDownAdj :: Word8 -> Maybe Word8 
leftDownAdj n 
    | d == 0 || m == 0 && odd d = Nothing
    | otherwise                 = Just $ n - (5 - r)
    where r      = (nofCells - n) `div` cellsPerRow `mod` 2
          (d, m) = n `divMod` cellsPerRow