{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Maverick (printEquityTable) where

import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Maverick.Score (score)
import Maverick.Types
import Numeric (showFFloat)
import Relude.Extra.Foldable (average)
import Relude.Extra.Foldable1 (Foldable1 (maximum1))
import System.Random.MWC (GenIO, createSystemRandom)
import System.Random.MWC.Distributions (uniformShuffle)

hands :: [[Card]]
hands =
  fmap
    (fmap (uncurry card))
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

deck :: Vector Card
deck = V.fromList $ do
  suit <- universe
  rank <- universe
  pure (card rank suit)

type EquityIO = StateT (Vector Card) (ReaderT GenIO IO)

deal :: Int -> EquityIO (Vector Card)
deal n = do
  v <- get
  let (h, t) = V.splitAt n v
  put t
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
  ps <- foldMap' hand <<$>> replicateM n (deal 2)
  cs <- deal 5
  let community = foldMap' hand cs
  let hs = (community <>) <$> (p :| ps)
      ss@(s :| _) = score <$> hs
      best = maximum1 ss
  if s == best
    then pure (1 / countEqual best ss)
    else pure 0
  where
    countEqual x = getSum . foldMap' (\y -> if x == y then 1 else 0)

calcEquity :: [Card] -> Int -> ReaderT GenIO IO Double
calcEquity cs n = do
  let deck' = V.filter (`notElem` cs) deck
      p = foldMap' hand cs
  fromMaybe 0 . average
    <$> replicateM 10000 (evaluatingStateT deck' $ playHand p n)

printEquityTable :: IO ()
printEquityTable = do
  putStrLn "maverick"
  gen <- createSystemRandom
  usingReaderT gen $ do
    for_ hands $ \pocket -> do
      es <- traverse (calcEquity pocket) [1 .. 9]
      putText (T.intercalate " " (render <$> pocket))
      putText " | "
      putTextLn (T.intercalate " " (toText . (flip (showFFloat (Just 2)) "") <$> es))
