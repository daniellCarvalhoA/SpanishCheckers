module Main where 


import Criterion.Main  
import SimplePawn
import Game 


main :: IO ()
main = defaultMain [

    bgroup "simplepawnmove" [ 
                              bench "hpm0" $ whnf (hsimplemove 10 ) initialboard 
                            , bench "spm0" $ whnf (simplePawnMove 10 Human) initialboard 
                            ]
                   ]





