{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE BangPatterns         #-}

module AI where 

import Data.Ord           (Down(..))
-- import Data.Maybe         (fromJust)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Bits          (Bits(..))
import Game               (Game(..), 
                           GameType(..),
                           SG(..),
                           FirstMove,
                           getturn,
                           allcmoves, 
                           allhmoves, 
                          ) 
import Types
import Data.List (scanl')
import Board
import Data.Monoid        (First(..))
import SimpleMoves 
import JumpMoves 
import Control.Concurrent (threadDelay) 
import Control.Concurrent.Async (wait, withAsync, race_) 
import Data.IORef 
import Data.List.NonEmpty.Extra (sortOn)
import Data.Int (Int8)
import qualified Data.List.NonEmpty as N 
import qualified Data.Foldable as Foldable
import Control.Arrow
import qualified Control.Scanl as S 
import qualified Control.Monad.State.Strict as ST

mkMove :: Game 'Simulated -> Move -> Game 'Real
mkMove (Over _ res turn board) _ = Over SR res turn board
mkMove (AI turn board _ _) (Left sm) = 
  let newboard = mkSimpleMove sm board turn
  in case allhmoves newboard of 
       Nothing -> Over SR Loss Human newboard
       Just mv -> HS newboard $ Start mv 
mkMove (AI turn board _ _) (Right jm) = 
  let newboard = mkJumpMove board jm turn
  in case allhmoves newboard of 
       Nothing -> Over SR Loss Human newboard
       Just mv -> HS newboard $ Start mv 

nextSimpleGame :: Board -> FirstMove -> Turn -> CSimpleMove -> Game 'Simulated 
nextSimpleGame board firstmove turn simplemove = 
  let newboard = mkSimpleMove simplemove board turn 
      newmoves = allcmoves newboard newturn 
      newturn  = changeturn turn 
      nextmove = First $ Just $ Left simplemove
  in case newmoves of 
    Nothing -> Over (SS $ firstmove <> nextmove) Loss newturn newboard 
    Just mv -> AI newturn newboard mv (firstmove <> nextmove)
                    
nextJumpGame :: Board -> FirstMove -> Turn -> AMove -> Game 'Simulated 
nextJumpGame board firstmove turn jumpmove = 
  let newboard = mkJumpMove board jumpmove turn 
      newmoves = allcmoves newboard newturn 
      newturn  = changeturn turn 
      nextmove = First $ Just $ Right jumpmove 
  in case newmoves of 
    Nothing -> Over (SS $ firstmove <> nextmove) Loss newturn newboard 
    Just mv -> AI newturn newboard mv (firstmove <> nextmove)

extendGames :: Game 'Simulated -> NonEmpty (Game 'Simulated)
extendGames o@(Over _ _ _ _)  = o :| []
extendGames (AI turn board moves firstmove) = 
  case moves of 
    SCM simplemoves -> nextSimpleGame board firstmove turn <$> simplemoves 
    JCM jumpmoves   -> nextJumpGame   board firstmove turn <$> jumpmoves

------------------------------------
--  Alphabeta 
--
alphabeta' :: Int -> Game 'Simulated -> (Int8,FirstMove)
alphabeta' n game = maxValue n minBound maxBound game 
  where 
    maxValue :: Int -> Int8 -> Int8 -> Game 'Simulated -> (Int8, FirstMove)
    maxValue _ _ _ (Over (SS fm) result _ _) = 
      case result of 
        Loss -> (minBound, fm)
        Tie  -> (0       , fm)
    maxValue depth a b g@(AI _ board _ fm) = 
      case depth of 
        0 -> (utility' board, fm)
        _ -> 
          let nextGames = N.reverse $ extendGames g 
              getMinmaxAndAlpha (bestMinmaxSofar, a') g' = 
                let (newMinmax, firstmove) = bestMinmaxSofar `max'` minValue (depth - 1) a' b g'
                in  ((newMinmax, firstmove), a `max` newMinmax)
              (best, _) = 
                takeFirstorLast (\(v,_) -> fst v >= b) $ 
                  scanl'' getMinmaxAndAlpha ((minBound, fm), a) nextGames 
          in best
    minValue :: Int -> Int8 -> Int8 -> Game 'Simulated -> (Int8, FirstMove)
    minValue _ _ _ (Over (SS fm) result _ _) = 
      case result of 
        Loss -> (maxBound, fm)
        Tie  -> (0       , fm)
    minValue depth a b g@(AI _ board _ fm) = 
      case depth of 
        0 -> (utility' board, fm)
        _ -> 
          let nextGames = N.reverse $ extendGames g 
              getMinmaxAndBeta (bestMinmaxSofar, b') g' = 
                let (newMinmax, firstMove) = bestMinmaxSofar `min'` maxValue (depth - 1) a b' g'
                in  ((newMinmax, firstMove), b `min` newMinmax)
              (best, _) = 
                takeFirstorLast (\(v, _) -> fst v <= a) $ 
                  scanl'' getMinmaxAndBeta ((maxBound, fm), b) nextGames
          in best 

data Status = Status {
    minmax :: !Int8 
  , alpbet :: !Int8 
  , curfmv :: !FirstMove
  } 
------------------------------------------------------------
--
alphabeta'' :: Int -> Game 'Simulated -> (Int8,FirstMove)
alphabeta'' n game = maxValue n minBound maxBound game 
  where 
    maxValue :: Int -> Int8 -> Int8 -> Game 'Simulated -> (Int8, FirstMove)
    maxValue _ _ _ (Over (SS fm) result _ _) = 
      case result of 
        Loss -> ( minBound, fm)
        Tie  -> ( 0       , fm)
    maxValue depth a b g@(AI _ board _ fm) = 
      case depth of 
        0 -> (utility' board, fm)
        _ -> 
          let nextGames = extendGames g 
              getMinMaxAlpha :: Game 'Simulated -> ST.State Status (Int8, FirstMove)
              getMinMaxAlpha game = do 
                Status{..} <- ST.get 
                let (nm, fm') = minValue (depth - 1) alpbet b game 
                if minmax >= nm 
                  then do 
                    ST.put $ Status minmax (a `max` alpbet) curfmv 
                    return (minmax, curfmv) 
                  else do 
                    ST.put $ Status nm (a `max` nm) fm'
                    return (nm, fm')  
              initial = Status minBound a fm
              scan    = S.Scan getMinMaxAlpha initial
              best    = takeFirstorLast (\(v,_) -> v >= b) $ S.scan scan nextGames 
          in best

    minValue :: Int -> Int8 -> Int8 -> Game 'Simulated -> (Int8, FirstMove)
    minValue _ _ _ (Over (SS fm) result _ _) = 
      case result of 
        Loss -> (maxBound, fm)
        Tie  -> (0       , fm)
    minValue depth a b g@(AI _ board _ fm) = 
      case depth of 
        0 -> (utility' board, fm)
        _ -> 
          let nextGames = N.reverse $ extendGames g 
              getMinMaxBeta :: Game 'Simulated -> ST.State Status (Int8, FirstMove)
              getMinMaxBeta game = do 
                Status{..} <- ST.get
                let (mm, fm') = maxValue (depth - 1) a alpbet game 
                if minmax <= mm
                  then do 
                    ST.put $ Status minmax (b `min` alpbet) curfmv 
                    return (minmax, curfmv)
                  else do 
                    ST.put $ Status mm (b `min` mm) fm'
                    return (mm, fm') 

              initial = Status maxBound b fm 
              scan    = S.Scan getMinMaxBeta initial 
              best    = takeFirstorLast (\(v, _) -> v <= a) $ S.scan scan nextGames
          
          in best 

utility' :: Board -> Int8
utility' Board{..} = fromIntegral $ 
                    (popCount $ downpawns .|. downkings) 
                  - (popCount $ uppawns   .|. upkings)

scanl'' :: Foldable t => (a1 -> a2 -> a1) -> a1 -> t a2 -> NonEmpty a1 
scanl'' f z = N.fromList . scanl f z . Foldable.toList 

min' :: Ord m => (m, a) -> (m, a) -> (m, a) 
min' (x, g1) (y, g2) | x <= y     = (x, g1)
                     | otherwise = (y, g2) 

max' :: Ord m => (m, a) -> (m, a) -> (m, a) 
max' (x, g1) (y, g2) | x >= y    = (x, g1) 
                     | otherwise = (y, g2) 

takeFirstorLast :: (a -> Bool) -> NonEmpty a -> a 
takeFirstorLast _    (x :| []) = x 
takeFirstorLast cond (x :| xs) = if cond x then x else takeFirstorLast cond (N.fromList xs)



-------
newtype Fix f = Fix (f (Fix f))

deriving instance Show (f (Fix f)) => Show (Fix f)

data GameF a r = Value a | Play Turn (NonEmpty r) 
  deriving Functor 

type GameTree a = Fix (GameF a)

cata :: (GameF a r -> r) -> GameTree a -> r 
cata f (Fix g) = f $ fmap (cata f) g

class Lattice l where
  inf, sup :: l -> l -> l 

newtype Order a = Order { unOrder :: a }
  deriving (Eq,Ord)

instance Ord a => Lattice (Order a) where
  inf = min
  sup = max

gminimax :: Lattice l => (a -> l) -> GameTree a -> l 
gminimax leaf = cata minimaxF 
  where
    minimaxF (Value x)   = leaf x 
    minimaxF (Play p xs) = foldr1 (lopti p) xs 

lopti :: Lattice l => Turn -> l -> l -> l 
lopti Human    = sup 
lopti Computer = inf

minimax :: Ord a => GameTree a -> a
minimax = unOrder . gminimax Order

type Interval a = (WithBot a, WithTop a) 

data WithBot a = Bot | NoBot a deriving (Eq, Ord)
data WithTop a = NoTop a | Top deriving (Eq, Ord)

newtype Pruning a = Pruning { unPruning :: Interval a -> a }

instance Ord a => Lattice (Pruning a) where
  inf l r = Pruning (\(alpha,beta) -> 
    let v1 = unPruning l (alpha, beta)
    in if NoBot v1 <= alpha 
        then v1 
        else min v1 $ unPruning r (alpha, min (NoTop v1) beta)
                    )
  sup l r = Pruning (\(alpha,beta) -> 
    let v1 = unPruning l (alpha,beta)
    in if beta <= NoTop v1 
        then v1 
        else max v1 $ unPruning r (max (NoBot v1) alpha, beta)
                    )

alphabeta :: Ord a => GameTree a -> a 
alphabeta = runPruning . gminimax constPruning 

constPruning :: a -> Pruning a 
constPruning = Pruning . const 

runPruning :: Pruning a -> a 
runPruning f = unPruning f (Bot, Top)

data ValueAndMove = ValueAndMove { 
      value ::  !Int8 
    , fmove ::  !FirstMove
    } deriving Show 

instance Eq ValueAndMove where 
  sm1 == sm2 = (value sm1) == (value sm2)

instance Ord ValueAndMove where
  sm1 <=  sm2 = value sm1 <= value sm2

buildGameTree :: Int -> Game 'Simulated -> GameTree ValueAndMove 
buildGameTree _ (Over (SS firstmove) result turn _) = 
  case result of 
    Tie  -> Fix $ Value $ ValueAndMove 0 firstmove
    Loss -> Fix $ Value 
                $ ValueAndMove (if turn == Computer then maxBound else minBound) firstmove
buildGameTree 0 (AI _ board _ firstmove) = 
            Fix $ Value $ ValueAndMove (utility board) firstmove
buildGameTree n g@(AI turn _ _ _) = 
            Fix $ Play turn 
                $ fmap (buildGameTree (n - 1)) 
                $ sortOn (Down . score)  
                $ extendGames g

score :: Game 'Simulated -> Int8
score (Over _ _ _ _ ) = 0
score (AI Human _ mvs _) =
  case mvs of
    SCM _                -> 0
    JCM (AMove{..} :| _) -> negate
                          $ fromIntegral 
                          $ 5 * popCount (king cache) + popCount (pawn cache)
score (AI Computer _ mvs _) =
  case mvs of
    SCM _                -> 0
    JCM (AMove{..} :| _) -> fromIntegral 
                          $ 5 * popCount (king cache) + popCount (pawn cache)

utility :: Board -> Int8 
utility Board{..} = fromIntegral  
                  $ popCount uppawns   + 5 * popCount upkings
                  - popCount downpawns - 5 * popCount downkings 
                  
computer :: Int -> Game 'Simulated -> ValueAndMove
computer n = alphabeta . buildGameTree n

iterativedeepening :: Int -> Game 'Simulated -> IO (Maybe Move)
iterativedeepening _ (Over _ _ _ _) = return Nothing 
iterativedeepening i g@(AI _ _ cm _) 
  | single cm = return $ Just $ head' $ cm 
  | otherwise = do 
    ior <- newIORef Nothing
    withAsync (iterative 5 30 g ior) $ \a1 -> 
      withAsync (threadDelay i) $ \a2 -> 
        race_ (wait a1) (wait a2)
    readIORef ior 

single :: CMoves -> Bool 
single (SCM (_ :| [])) = True 
single (JCM (_ :| [])) = True
single _             = False 

head' :: CMoves -> Move
head' (SCM (mv :| _)) = Left mv 
head' (JCM (mv :| _)) = Right mv

iterative :: Int -> Int -> Game 'Simulated  -> IORef (Maybe Move) -> IO () 
iterative _ _ (Over _ _ _ _) _ = return ()
iterative i e g irg 
  | i > e = return ()
  | otherwise = do 
    print i
    let ValueAndMove{..} = computer i g 
    print value
    modifyIORef' irg (const $ getFirst fmove) 
    if value == minBound 
      then return ()
      else iterative (i + 1) e g irg






-- aimove :: Int -> Game 'Simulated

-- mkMove :: Game 'Simulated -> Move -> Game 'Simulated
-- mkMove o@(Over _ _ _ _)  = const o
-- mkMove (AI turn board _ _) = \case
-- Left mv -> AI newturn newboard newmoves mempty
-- where newturn  = changeturn turn
-- newboard = mkSimpleMove mv board turn
-- newmoves = allcmoves newboard newturn






