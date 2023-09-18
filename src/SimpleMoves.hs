{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE LinearTypes            #-}

module SimpleMoves 
    ( allCSimplePawnMoves
    , allHSimplePawnMoves 
    , allCSimpleKingMoves
    , allHSimpleKingMoves
    , allsimplesV
    , mkSimpleMove
    , allsimples
    , allCSimplePawnMoves
    , rvSimpleMove
    ) where

import Data.Word     (Word64)
import Data.Bits     (Bits(..), countTrailingZeros, FiniteBits(..))
import Types         ( Turn(..), Size(..), Direction(..), CSimpleMove, HSimpleMove)
import Board         (Board(..), empties, upperRow, lowerRow) 
import Data.Function ((&))
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV 
import Control.Monad.ST
import Data.STRef
import JumpMoves (decomp')
import Data.Array.Mutable.Linear
---------------------


allsimplesV :: V.Vector v Word64 => Board -> v Word64
allsimplesV board@Board{..} = V.create $ do  
   i <- newSTRef 0 
   v <- MV.new total 
   loop start total i v
    where 
     loop _ 0 _ v = return v
     loop j t i v 
        | testBit wpsl j = do
            n <- readSTRef i 
            MV.write v n (bit j .|. (bit j `shiftR` 4))
            modifySTRef' i (1 +)
            loop (j + 1) (t - 1) i v
        | otherwise = loop (j + 1) t i v  
  
     wpsl  = (uppawns `shiftL` 4) .&. es 
     total = popCount wpsl 
     start = countTrailingZeros wpsl
     es    = empties board 
 

allsimples :: Board -> Turn -> [CSimpleMove]
allsimples b@Board{..} = \case 
  Human    -> simplesUp
  Computer -> simplesDown
  where 
    wpsl = (uppawns `shiftL` 4) .&. es
    wpsr = (uppawns `shiftL` 5) .&. es
    bpsl = (downpawns `shiftR` 5) .&. es
    bpsr = (downpawns `shiftR` 4) .&. es
    es   = empties b

    simplesUp :: [CSimpleMove]
    simplesUp = 
      let start = countTrailingZeros (wpsl .|. wpsr)
          total = popCount wpsl + popCount wpsr
          go _ 0 = []
          go i t =
            if testBit wpsl i 
              then if testBit wpsr i 
                then (Pawn, bi .|. (bi `shiftR` 4)) 
                   : (Pawn, bi .|. (bi `shiftR` 5)) : go (i + 1) (t - 2)
                else (Pawn, bi .|. (bi `shiftR` 4)) : go (i + 1) (t - 1)
              else if testBit wpsr i 
                then (Pawn, bi .|. (bi `shiftR` 5)) : go (i + 1) (t - 1)
                else go (i + 1) t
            where bi = bit i
      in go 0 total
      
    simplesDown :: [CSimpleMove]
    simplesDown = 
      let start = countTrailingZeros (bpsl .|. bpsr) 
          total = popCount bpsl + popCount bpsr 
          go _ 0 = []
          go i t = 
            if testBit bpsl i 
              then if testBit bpsr i 
                then (Pawn, bi .|. (bi `shiftL` 5)) 
                   : (Pawn, bi .|. (bi `shiftL` 4)) : go (i + 1) (t - 2)
                else (Pawn, bi .|. (bi `shiftL` 5)) : go (i + 1) (t - 1)
              else if testBit bpsr i 
                then (Pawn, bi .|. (bi `shiftL` 4)) : go (i + 1) (t - 1)
                else go (i + 1) t
            where bi = bit i

      in go 0 total 

allCSimplePawnMoves :: Board -> Turn ->  [CSimpleMove]
allCSimplePawnMoves b@Board{..} = \case
  Human    -> if uppawns == 0 then [] 
                else simpleWhiteMovesLeft wpsl <> simpleWhiteMovesRight wpsr
  Computer -> if downpawns == 0 then [] 
                else simpleBlackMovesLeft bpsl <> simpleBlackMovesRight bpsr
  where
    wpsl = (uppawns `shiftL` 4) .&. es
    wpsr = (uppawns `shiftL` 5) .&. es
    bpsl = (downpawns `shiftR` 5) .&. es
    bpsr = (downpawns `shiftR` 4) .&. es
    es   = empties b
    simpleWhiteMovesLeft :: Word64 -> [(Size,Word64)]
    simpleWhiteMovesLeft mv =
      let start = countTrailingZeros mv
          total = popCount mv
          go _ 0 = []
          go i t | testBit mv i = (Pawn, bit i .|. (bit i `shiftR` 4)) : go (i + 1) (t - 1)
                 | otherwise    = go (i + 1) t
      in go start total

    simpleWhiteMovesRight :: Word64 -> [(Size,Word64)]
    simpleWhiteMovesRight mv =
      let start = countTrailingZeros mv
          total = popCount mv
          go _ 0 = []
          go i t | testBit mv i = (Pawn, bit i .|. (bit i `shiftR` 5)) : go (i + 1) (t - 1)
                 | otherwise    = go (i + 1) t
      in go start total

    simpleBlackMovesLeft :: Word64 -> [(Size,Word64)]
    simpleBlackMovesLeft mv =
      let start = countTrailingZeros mv
          total = popCount mv
          go _ 0 = []
          go i t | testBit mv i = (Pawn, bit i .|. (bit i `shiftL` 5)) : go (i + 1) (t - 1)
                 | otherwise    = go (i + 1) t
      in go start total

    simpleBlackMovesRight :: Word64 -> [(Size,Word64)]
    simpleBlackMovesRight mv =
      let start = countTrailingZeros mv
          total = popCount mv
          go _ 0 = []
          go i t | testBit mv i = (Pawn, bit i .|. (bit i `shiftL` 4)) : go (i + 1) (t - 1)
                 | otherwise    = go (i + 1) t
      in go start total

---------------------

allHSimplePawnMoves :: Board -> Turn ->  [HSimpleMove]
allHSimplePawnMoves b@Board{..} = \case
  Human    -> simpleWhiteMovesLeft wpsl <> simpleWhiteMovesRight wpsr
  Computer -> simpleBlackMovesLeft bpsl <> simpleBlackMovesRight bpsr
  where
    wpsl = (uppawns `shiftL` 4) .&. empties b
    wpsr = (uppawns `shiftL` 5) .&. empties b
    bpsl = (downpawns `shiftR` 5) .&. empties b
    bpsr = (downpawns `shiftR` 4) .&. empties b
    
    simpleWhiteMovesLeft :: Word64 -> [HSimpleMove]
    simpleWhiteMovesLeft mv =
      let start = countTrailingZeros mv
          total = popCount mv
          go _ 0 = []
          go i t | testBit mv i = (Pawn, (bit i `shiftR` 4, bit i)) : go (i + 1) (t - 1)
                 | otherwise    = go (i + 1) t
      in go start total

    simpleWhiteMovesRight :: Word64 -> [HSimpleMove]
    simpleWhiteMovesRight mv =
      let start = countTrailingZeros mv
          total = popCount mv
          go _ 0 = []
          go i t | testBit mv i = (Pawn, (bit i `shiftR` 5, bit i)) : go (i + 1) (t - 1)
                 | otherwise    = go (i + 1) t
      in go start total

    simpleBlackMovesLeft :: Word64 -> [HSimpleMove]
    simpleBlackMovesLeft mv =
      let start = countTrailingZeros mv
          total = popCount mv
          go _ 0 = []
          go i t | testBit mv i = (Pawn, (bit i `shiftL` 5, bit i)) : go (i + 1) (t - 1)
                 | otherwise    = go (i + 1) t
      in go start total

    simpleBlackMovesRight :: Word64 -> [HSimpleMove]
    simpleBlackMovesRight mv =
      let start = countTrailingZeros mv
          total = popCount mv
          go _ 0 = []
          go i t | testBit mv i = (Pawn, (bit i `shiftL` 4, bit i)) : go (i + 1) (t - 1)
                 | otherwise    = go (i + 1) t
      in go start total

----------------------
 
allCSimpleKingMoves :: Board -> Turn -> [CSimpleMove]
allCSimpleKingMoves b@Board{..} turn = if ks == 0 then [] else ksm
  where
    es  = empties b
    ks  = case turn of Human -> upkings; Computer -> downkings
    ksm = concat
        [ ksmlu ks & kingMovesLeftUp
        , ksmru ks & kingMovesRightUp
        , ksmld ks & kingMovesLeftDown
        , ksmrd ks & kingMovesRightDown
        ]

    ksmlu :: Word64 -> [Word64]
    ksmlu kgs = go $ (kgs `shiftL` 4) .&. es
      where go 0  = []
            go !n = n : go ((n `shiftL` 4) .&. es)

    kingMovesLeftUp :: [Word64] -> [CSimpleMove]
    kingMovesLeftUp []          = []
    kingMovesLeftUp xs@(mv : _) =
      let go !i !m !n !t 
            | t == 0      = []
            | testBit m i = (King, bit i .|. (bit i `shiftR` (n * 4))) : go (i + 1) m n (t - 1)
            | otherwise   = go (i + 1) m n t
          every [] _ _         = []
          every (y : ys) !s !j = go s y j total <> every ys (s + 4) (j + 1)
            where total = popCount y
      in every xs (countTrailingZeros mv) 1

    ksmru :: Word64 -> [Word64]
    ksmru kgs = go $ (kgs `shiftL` 5) .&. es
      where go 0 = []
            go !n = n : go ((n `shiftL` 5) .&. es)

    kingMovesRightUp :: [Word64] -> [CSimpleMove]
    kingMovesRightUp [] = []
    kingMovesRightUp xs@(mv : _) =
      let go !i !m !n !t 
            | t == 0      = []
            | testBit m i = (King, bit i .|. (bit i `shiftR` (n * 5))) : go (i + 1) m n (t - 1)
            | otherwise   = go (i + 1) m n t
          every [] _ _         = []
          every (y : ys) !s !j = go s y j total <> every ys (s + 5) (j + 1)
            where total = popCount y
      in every xs (countTrailingZeros mv) 1

    ksmld :: Word64 -> [Word64]
    ksmld kgs = go $ (kgs `shiftR` 5) .&. es
      where go 0 = []
            go !n = n : go ((n `shiftR` 5) .&. es)

    kingMovesLeftDown :: [Word64] -> [CSimpleMove]
    kingMovesLeftDown [] = []
    kingMovesLeftDown xs@(mv : _) =
      let go !i !m !n !t 
            | t == 0      = []
            | testBit m i = (King, bit i .|. (bit i `shiftL` (n * 5))) : go (i - 1) m n (t - 1)
            | otherwise   = go (i - 1) m n t
          every [] _ _         = []
          every (y : ys) !s !j = go s y j total <> every ys (s + 5) (j + 1)
            where total = popCount y
      in every xs (countLeadingZeros mv) 1

    ksmrd :: Word64 -> [Word64]
    ksmrd kgs = go $ (kgs `shiftR` 4) .&. es
      where go 0 = []
            go !n = n : go ((n `shiftR` 4) .&. es)

    kingMovesRightDown :: [Word64] -> [CSimpleMove]
    kingMovesRightDown [] = []
    kingMovesRightDown xs@(mv : _) =
      let go !i !m !n !t 
            | t == 0      = []
            | testBit m i = (King, bit i .|. (bit i `shiftL` (n * 4))) : go (i - 1) m n (t - 1)
            | otherwise   = go (i - 1) m n t
          every [] _ _         = []
          every (y : ys) !s !j = go s y j total <> every ys (s + 4) (j + 1)
            where total = popCount y
      in every xs (countLeadingZeros mv) 1

----------------------

allHSimpleKingMoves :: Board -> Turn -> [HSimpleMove]
allHSimpleKingMoves b@Board{..} turn = if ks == 0 then [] else ksm
  where
    es  = empties b
    ks  = case turn of Human -> upkings; Computer -> downkings
    ksm = concat
        [ ksmlu ks & kingMovesLeftUp
        , ksmru ks & kingMovesRightUp
        , ksmld ks & kingMovesLeftDown
        , ksmrd ks & kingMovesRightDown
        ]

    ksmlu :: Word64 -> [Word64]
    ksmlu kgs = go $ (kgs `shiftL` 4) .&. es
      where go 0  = []
            go !n = n : go ((n `shiftL` 4) .&. es)

    kingMovesLeftUp :: [Word64] -> [HSimpleMove]
    kingMovesLeftUp []          = []
    kingMovesLeftUp xs@(mv : _) =
      let go !i !m !n !t 
            | t == 0      = []
            | testBit m i = (King, (bit i `shiftR` (n * 4), bit i)) : go (i + 1) m n (t - 1)
            | otherwise   = go (i + 1) m n t
          every [] _ _         = []
          every (y : ys) !s !j = go s y j total <> every ys (s + 4) (j + 1)
            where total = popCount y
      in every xs (countTrailingZeros mv) 1

    ksmru :: Word64 -> [Word64]
    ksmru kgs = go $ (kgs `shiftL` 5) .&. es
      where go 0 = []
            go !n = n : go ((n `shiftL` 5) .&. es)

    kingMovesRightUp :: [Word64] -> [HSimpleMove]
    kingMovesRightUp [] = []
    kingMovesRightUp xs@(mv : _) =
      let go !i !m !n !t 
            | t == 0      = []
            | testBit m i = (King, (bit i `shiftR` (n * 5), bit i)) : go (i + 1) m n (t - 1)
            | otherwise   = go (i + 1) m n t
          every [] _ _         = []
          every (y : ys) !s !j = go s y j total <> every ys (s + 5) (j + 1)
            where total = popCount y
      in every xs (countTrailingZeros mv) 1

    ksmld :: Word64 -> [Word64]
    ksmld kgs = go $ (kgs `shiftR` 5) .&. es
      where go 0 = []
            go !n = n : go ((n `shiftR` 5) .&. es)

    kingMovesLeftDown :: [Word64] -> [HSimpleMove]
    kingMovesLeftDown [] = []
    kingMovesLeftDown xs@(mv : _) =
      let go !i !m !n !t 
            | t == 0      = []
            | testBit m i = (King, (bit i `shiftL` (n * 5), bit i)) : go (i - 1) m n (t - 1)
            | otherwise   = go (i - 1) m n t
          every [] _ _         = []
          every (y : ys) !s !j = go s y j total <> every ys (s + 5) (j + 1)
            where total = popCount y
      in every xs (countLeadingZeros mv) 1

    ksmrd :: Word64 -> [Word64]
    ksmrd kgs = go $ (kgs `shiftR` 4) .&. es
      where go 0 = []
            go !n = n : go ((n `shiftR` 4) .&. es)

    kingMovesRightDown :: [Word64] -> [HSimpleMove]
    kingMovesRightDown [] = []
    kingMovesRightDown xs@(mv : _) =
      let go !i !m !n !t 
            | t == 0      = []
            | testBit m i = (King, (bit i `shiftL` (n * 4), bit i)) : go (i - 1) m n (t - 1)
            | otherwise   = go (i - 1) m n t
          every [] _ _         = []
          every (y : ys) !s !j = go s y j total <> every ys (s + 4) (j + 1)
            where total = popCount y
      in every xs (countLeadingZeros mv) 1



--------------------------
-- Make Simple Moves

mkSimpleMove :: CSimpleMove -> Board -> Turn -> Board
mkSimpleMove (size, w64) Board{..} = \case
  Human    ->
    case size of 
      Pawn -> 
        let newuppawns = uppawns `xor` w64 `xor` (w64 .&. upperRow) 
            newupkings = upkings .|. (w64 .&. upperRow)
        in Board newuppawns downpawns newupkings downkings mask 
      King -> 
        let newupkings = upkings `xor` w64 
        in Board uppawns downpawns newupkings downkings mask 
  Computer ->
    case size of 
      Pawn -> 
        let newdownpawns = downpawns `xor` w64 `xor` (w64 .&. lowerRow)
            newdownkings = downkings .|. (w64 .&. lowerRow)
        in Board uppawns newdownpawns upkings newdownkings mask 
      King -> 
        let newdownkings = downkings `xor` w64 
        in Board uppawns downpawns upkings newdownkings mask 

rvSimpleMove :: CSimpleMove -> Board -> Turn -> Board
rvSimpleMove (size, w64) Board{..} = \case
  Computer ->
    case size of 
      Pawn -> 
        let newuppawns = uppawns `xor` w64 `xor` (w64 .&. upperRow)
            newupkings = upkings `xor` (w64 .&. upperRow)
        in Board newuppawns downpawns newupkings downkings mask 
      King ->
        let newupkings = upkings `xor` w64 
        in  Board uppawns downpawns newupkings downkings mask
  Human    ->
    case size of 
      Pawn ->
        let newdownpawns = downpawns `xor` w64 `xor` (w64 .&. lowerRow)
            newdownkings = downkings `xor` (w64 .&. lowerRow)
        in Board uppawns newdownpawns upkings newdownkings mask 
      King -> 
        let newdownkings = downkings `xor` w64
        in Board uppawns downpawns upkings newdownkings mask

