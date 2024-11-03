{-# LANGUAGE OverloadedStrings #-}

module Maverick
  ( printEquityTable,
    printEquityTable',
  )
where

import Control.Monad.Trans.RWS.CPS (RWST, ask, get, put, runRWST, tell)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Mutable (IOVector)
import Data.Vector.Mutable qualified as VM
import Maverick.Score (score)
import Maverick.Types
import Numeric (showFFloat)
import System.Random.MWC (GenIO, UniformRange (uniformRM), createSystemRandom)
import Prelude hiding (ask, get, put)

(<>^) :: (Monoid m, Applicative f) => f m -> f m -> f m
(<>^) = liftA2 (<>)
{-# INLINE (<>^) #-}

hands :: V.Vector (V.Vector Card)
hands =
  V.fromList $
    fmap
      (V.fromList . fmap (uncurry card))
      [ [(Ace, Spade), (Ace, Heart)],
        [(King, Spade), (King, Heart)],
        [(Queen, Spade), (Queen, Heart)],
        [(Jack, Spade), (Jack, Heart)],
        [(Ten, Spade), (Ten, Heart)],
        [(Ace, Spade), (King, Spade)],
        [(Ace, Spade), (King, Heart)],
        [(Two, Spade), (Two, Heart)],
        [(Two, Spade), (Seven, Heart)]
      ]

deck :: V.Vector Hand
deck = V.fromList $ do
  suit <- universe
  rank <- universe
  pure (hand $ card rank suit)

type EquityIO = RWST (GenIO, IOVector Hand) (Sum Double) Int IO

drawCommunity :: EquityIO Hand
drawCommunity = do
  (gen, d) <- ask
  i <- get
  modify' (+ 5)
  for_ [i .. i + 4] $ \k ->
    VM.unsafeSwap d k =<< uniformRM (k, VM.length d - 1) gen
  h <-
    VM.unsafeRead d i
      <>^ VM.unsafeRead d (i + 1)
      <>^ VM.unsafeRead d (i + 2)
      <>^ VM.unsafeRead d (i + 3)
      <>^ VM.unsafeRead d (i + 4)
  h `seq` pure h

drawHand :: Hand -> EquityIO Score
drawHand c = do
  (gen, d) <- ask
  i <- get
  modify' (+ 2)
  VM.unsafeSwap d i =<< uniformRM (i, VM.length d - 1) gen
  VM.unsafeSwap d (i + 1) =<< uniformRM (i + 1, VM.length d - 1) gen
  score <$!> (pure c <>^ VM.unsafeRead d i <>^ VM.unsafeRead d (i + 1))

playHand :: Hand -> Int -> EquityIO ()
playHand p n = do
  put 0
  community <- drawCommunity
  ss <- V.replicateM n $ drawHand community
  let !s = score $ community <> p
      best = max s (V.maximum ss)
  let x =
        if s == best
          then 1 / (1 + countEqual best ss)
          else 0
  x `seq` tell (Sum x)
  where
    countEqual x = V.foldl' (\a y -> if x == y then a + 1 else a) 0

calcEquity :: GenIO -> V.Vector Card -> Int -> IO Double
calcEquity gen cs n = do
  let hs = hand <$> cs
  deck' <- V.thaw $ V.filter (`notElem` hs) deck
  let p = V.foldMap' hand cs
      m = 100000
  (_, _, s) <- runRWST (replicateM_ m $ playHand p n) (gen, deck') 0
  pure $ getSum s / fromIntegral m

printEquityTable' :: IO ()
printEquityTable' = do
  putStrLn "maverick"
  gen <- createSystemRandom
  for_ hands $ \pocket -> do
    es <- traverse (calcEquity gen pocket) [1 .. 9]
    putText (T.intercalate " " (V.toList $ render <$> pocket))
    putText " | "
    putTextLn (T.intercalate " " (toText . flip (showFFloat (Just 2)) "" <$> es))

printEquityTable :: IO ()
printEquityTable = do
  putStrLn "maverick"
  gen <- createSystemRandom
  let c2s = (`card` Heart) <$> universe
  putTextLn (T.intercalate "   " $ "   |" : (render <$> c2s))
  for_ universe $ \r1 -> do
    let c1 = card r1 Spade
    es <- forM c2s $ \c2 -> do
      let pocket = V.fromList [c1, c2]
      calcEquity gen pocket 7
    putText (render c1)
    putText " | "
    putTextLn (T.intercalate " " (toText . flip (showFFloat (Just 2)) "" <$> es))
