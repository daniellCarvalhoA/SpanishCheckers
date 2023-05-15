module Main where 


import Criterion.Main  
import SimpleMove
import Game 

main :: IO ()
main = defaultMain [

    bgroup "simplepawnmove" [ 
                              bench "spm0" $ whnf (simpleKingMove 10 ) initialboard 
                            ]
                   ]





