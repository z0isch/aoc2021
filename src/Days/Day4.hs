{-# LANGUAGE TupleSections #-}

module Days.Day4
  ( part1,
    part2,
    test1,
  )
where

import qualified Data.List as L
import qualified Data.List.Split as L
import Data.Semigroup (Sum (Sum, getSum))
import RIO
import RIO.Lens (_1)
import RIO.List (headMaybe)
import qualified RIO.Map as Map
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T

test1 = parseInput "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7\n"

mark :: Int -> Map Int (Bool, Int, Int) -> Map Int (Bool, Int, Int)
mark = Map.adjust (\(_, x, y) -> (True, x, y))

wins :: Map Int (Bool, Int, Int) -> Bool
wins = L.or . zipWith runLine allLines . L.repeat . Map.elems
  where
    runLine :: ((Int, Int) -> Bool) -> [(Bool, Int, Int)] -> Bool
    runLine f = L.all (\(t, x, y) -> if f (x, y) then t else True)
    allLines = map horizontal [0 .. 4] <> map vertical [0 .. 4]
    horizontal h (_, y) = h == y
    vertical v (x, _) = v == x

score :: Int -> Map Int (Bool, Int, Int) -> Int
score n = (* n) . getSum . Map.foldMapWithKey (\n' (t, _, _) -> if t then mempty else Sum n')

runUntil :: (c -> Bool) -> (c -> a -> c) -> c -> [a] -> c
runUntil f g m = L.head . L.dropWhile f . L.scanl' g m

-- >>> solve1 test1
-- Just 4512
solve1 (nums, cards) = calcBoard $ runUntil (L.null . view _1) runNumbers (Nothing, -1, cards) nums
  where
    calcBoard (winners, n, _) = fmap (score n) winners
    runNumbers (_, _, cards) n = (headMaybe $ filter wins cards', n, cards')
      where
        cards' = map (mark n) cards

-- >>> solve2 test1
-- 1924
solve2 (nums, cards) = calcBoard $ runUntil dropCond runNumbers (0 :: Int, mempty, cards) nums
  where
    dropCond (_, winners, _) = (length cards /=) $ length winners
    calcBoard (_, winners, _) =
      let (_, n, cards) = L.head $ L.sortOn (Down . view _1) $ Map.elems winners
       in score n cards
    runNumbers (r, winners, cards) n = (r + 1, winners', cards')
      where
        newWinners = Map.fromList $ catMaybes $ zipWith (\(i :: Int) cs -> if wins cs then Just (i, (r, n, cs)) else Nothing) [0 ..] cards'
        winners' = winners `Map.union` newWinners
        cards' = map (mark n) cards

parseInput :: Text -> ([Int], [Map Int (Bool, Int, Int)])
parseInput i = (nums, map (Map.fromList . concat . zipWith (\y -> zipWith (\x c -> (c, (False, x, y))) [0 ..]) [0 ..]) cards)
  where
    nums = readInt $ T.splitOn "," ns
    cards = L.init $ map (map (readInt . T.splitOn " ")) $ L.splitOn [""] cs
    readInt = mapMaybe ((readMaybe @Int) . T.unpack)
    (ns : _ : cs) = T.splitOn "\n" i

-- >>> part1
-- Just 41668
part1 = solve1 . parseInput <$> readFileUtf8 "./input/day4.txt"

-- >>> part2
-- 10478
part2 = solve2 . parseInput <$> readFileUtf8 "./input/day4.txt"
