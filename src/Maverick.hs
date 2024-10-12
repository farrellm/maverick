{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Maverick (printEquityTable) where

import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Mutable (IOVector)
import Data.Vector.Mutable qualified as VM
import Maverick.Score (score)
import Maverick.Types
import Numeric (showFFloat)
import System.Random.MWC (GenIO, UniformRange (uniformRM), createSystemRandom)

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

type EquityIO = StateT Int (ReaderT (GenIO, IOVector Hand) IO)

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

playHand :: Hand -> Int -> EquityIO Double
playHand p n = do
  community <- drawCommunity
  ss <- V.replicateM n $ drawHand community
  let !s = score $ community <> p
      best = max s (V.maximum ss)
  let x =
        if s == best
          then 1 / (1 + countEqual best ss)
          else 0
  x `seq` pure x
  where
    countEqual x = getSum . V.foldMap' (\y -> if x == y then 1 else 0)

calcEquity :: V.Vector Card -> Int -> ReaderT GenIO IO Double
calcEquity cs n = do
  gen <- ask
  let hs = hand <$> cs
  deck' <- V.thaw $ V.filter (`notElem` hs) deck
  let p = V.foldMap' hand cs
      m = 10000
  liftIO $
    (/ fromIntegral m) . getSum
      <$> timesM
        m
        (Sum <$> (usingReaderT (gen, deck') . evaluatingStateT 0 $ playHand p n))

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
