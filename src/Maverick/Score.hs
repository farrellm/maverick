{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Maverick.Score (score) where

import Data.Bits
  ( Bits
      ( clearBit,
        popCount,
        shiftR,
        (.&.)
      ),
    FiniteBits (countTrailingZeros),
  )
import Maverick.Types

data Dups = Dups
  { q :: !(Maybe Rank),
    t :: ![Rank],
    p :: ![Rank]
  }
  deriving (Show)

score :: Hand -> Score
score h =
  let rs = rankSet h.handSuitSet
      rs' = rs `shiftR` 1
      ds = countDups h
      ms = findStraight rs
      mf = findFlush h
   in case (mf, ms, ds) of
        (Just f, _, _)
          | Just s <- findStraight f -> StraightFlush s
        (_, _, Dups {q = Just r}) ->
          Quad r (clearTo 1 $ rs' `clearBit` fromEnum r)
        (_, _, Dups {t = r : _, p = s : _}) -> FullHouse r s
        (_, _, Dups {t = r : s : _b}) -> FullHouse r s
        (Just f, _, _) -> Flush (clearTo 5 $ f `shiftR` 1)
        (_, Just r, _) -> Straight r
        (_, _, Dups {t = r : _}) ->
          Trip r (clearTo 2 $ rs' `clearBit` fromEnum r)
        (_, _, Dups {p = r : s : _}) ->
          TwoPair r s
            . clearTo 1
            $ rs' `clearBit` fromEnum r `clearBit` fromEnum s
        (_, _, Dups {p = r : _}) ->
          OnePair r (clearTo 3 $ rs' `clearBit` fromEnum r)
        _ -> High (clearTo 5 rs')

straightMask :: Word
straightMask = 0b11111

findFlush :: Hand -> Maybe RankSet
findFlush h =
  (`getSuitRankSet` h.handSuitSet)
    <$> find (\s -> getSuitCount' s h.handCount >= 5) universe

findStraight :: RankSet -> Maybe Int
findStraight (RankSet s) =
  find (\r -> (s `shiftR` r) .&. straightMask == straightMask) [9, 8 .. 0]

countDups :: Hand -> Dups
countDups h =
  foldl' go (Dups Nothing [] []) universe
  where
    go :: Dups -> Rank -> Dups
    go d r = case getRankCount r (comboRankCount h.handCount) of
      4 -> d {q = Just r}
      3 -> d {t = r : t d}
      2 -> d {p = r : p d}
      _ -> d

clearLSB :: (FiniteBits b) => b -> b
clearLSB b = clearBit b (countTrailingZeros b)
{-# INLINE clearLSB #-}

clearTo :: (FiniteBits a) => Int -> a -> a
clearTo n b = foldl' (\x _ -> clearLSB x) b [n + 1 .. popCount b]
{-# INLINE clearTo #-}
