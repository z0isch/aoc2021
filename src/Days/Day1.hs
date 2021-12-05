module Days.Day1
  ( part1,
    part2,
    test1,
  )
where

import Data.List (tails)
import RIO
import qualified RIO.Text as T

test1 :: [Int]
test1 = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

-- >>> solve1 test1
-- 7
solve1 :: [Int] -> Int
solve1 = length . filter increases . tails
  where
    increases (x : y : _) = y > x
    increases _ = False

solve2 :: [Int] -> Int
solve2 = solve1 . go
  where
    go (x : xs@(y : z : _)) = x + y + z : go xs
    go _ = []

parseInput :: IO [Int]
parseInput =
  mapMaybe readMaybe . lines . T.unpack <$> readFileUtf8 "./input/day1.txt"

-- >>> part1
-- 1713

part1 :: IO Int
part1 = solve1 <$> parseInput

-- >>> part2
-- 1734

part2 :: IO Int
part2 = solve2 <$> parseInput
