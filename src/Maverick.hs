{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Maverick (printEquityTable) where

import Data.Text qualified as T
import Data.Vector qualified as V
import Maverick.Score (score)
import Maverick.Types
import Numeric (showFFloat)
import System.Random.MWC (GenIO, createSystemRandom)
import System.Random.MWC.Distributions (uniformShuffle)

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

deck :: V.Vector Card
deck = V.fromList $ do
  suit <- universe
  rank <- universe
  pure (card rank suit)

type EquityIO = StateT (V.Vector Card) (ReaderT GenIO IO)

deal :: Int -> EquityIO (V.Vector Card)
deal n = do
  h <- V.unsafeTake n <$> get
  modify' (V.unsafeDrop n)
  pure h

shuffle :: EquityIO ()
shuffle = do
  gen <- ask
  d <- get
  d' <- uniformShuffle d gen
  put d'

playHand :: Hand -> Int -> EquityIO Double
playHand p n = do
  shuffle
  ps <- V.foldMap' hand <<$>> V.replicateM n (deal 2)
  cs <- deal 5
  let community = V.foldMap' hand cs
  let s = score $ community <> p
      ss = score . (community <>) <$> ps
      best = max s (V.maximum ss)
  if s == best
    then pure (1 / (1 + countEqual best ss))
    else pure 0
  where
    countEqual x = getSum . V.foldMap' (\y -> if x == y then 1 else 0)

calcEquity :: V.Vector Card -> Int -> ReaderT GenIO IO Double
calcEquity cs n = do
  let deck' = V.filter (`notElem` cs) deck
      p = V.foldMap' hand cs
      m = 10000
  (/ fromIntegral m) . getSum
    <$> timesM m (Sum <$> evaluatingStateT deck' (playHand p n))

timesM :: (Monoid m, Monad f) => Int -> f m -> f m
timesM cnt0 x = loop cnt0 mempty
  where
    loop !cnt !acc
      | cnt <= 0 = pure acc
      | otherwise = (acc <>) <$> x >>= loop (cnt - 1)

printEquityTable :: IO ()
printEquityTable = do
  putStrLn "maverick"
  gen <- createSystemRandom
  usingReaderT gen $ do
    for_ hands $ \pocket -> do
      es <- traverse (calcEquity pocket) [1 .. 9]
      putText (T.intercalate " " (V.toList $ render <$> pocket))
      putText " | "
      putTextLn (T.intercalate " " (toText . (flip (showFFloat (Just 2)) "") <$> es))
