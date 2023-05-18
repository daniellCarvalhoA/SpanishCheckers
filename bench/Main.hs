module Main where 


import Criterion.Main  
-- import Game 
import ComputerKingJump
import Types 
import Board

main :: IO ()
main = defaultMain [

    bgroup "simplepawnmove" [ 
                              bench "kj0" $ whnf (kingJumpMove Human testBoard3) 18 
                            -- , bench "kj1" $ whnf (kingJumpMove' Human testBoard3) 18
                            ]
                   ]





