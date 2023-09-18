module Main (main) where

import Criterion.Main   (nf, bench, bgroup, whnf, defaultMain )
import Game             
import qualified AI     as F
import Types
import Game 
import Data.Bits
import Data.Word
import Board
import SimpleMoves
import JumpMoves
import Data.Vector
-- jm = ComputerMove {start = 1, cache = Cache {pawn = 2113536, king = 64}, end = 24}

-- jm2 = B.AMove {B.path = 134217730, B.cache = B.Cache {B.pawn = 8421376, B.king = 64}}

main :: IO ()
main = defaultMain 
    [
      bgroup "alphabeta" 
        [ 
          bench "v1" $ whnf (allCSimplePawnMoves  initialboard) Human
        , bench "v1" $ whnf (allsimples           initialboard) Human
        ]
    ]





