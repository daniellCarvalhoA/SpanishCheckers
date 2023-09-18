{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Scan where

import qualified Control.Foldl as F
import qualified Control.Scanl as L 

import Control.Monad.State.Strict
import Data.Monoid
import Types 
import Game
import Data.Int

average :: F.Fold Double Double
average = (/) <$> F.sum <*> F.genericLength

statistics :: Num b => (a -> Bool) -> F.Fold a (b, Maybe a, Maybe Int, Maybe a) 
statistics p = (,,,) <$> F.genericLength <*> F.find p <*> F.findIndex p <*> F.last

data Status = Status {
    minmax  :: !Int8
  , alpbet  :: !Int8
  , curfmv  :: !FirstMove
  } 

minValue :: Game 'Simulated -> (Int8, FirstMove)
minValue = undefined

getMinAndAlph :: Int8 -> Game 'Simulated -> State Status (Int8, FirstMove)
getMinAndAlph a game = do 
  Status{..} <- get
  let (nm, fm) = minValue game 
  if minmax >= nm 
    then do
      put $ Status minmax (a `max` alpbet) curfmv
      return (minmax, curfmv) 
    else do 
      put $ Status nm (a `max` nm) fm 
      return (nm, fm)
