module Days.Day7
  ( part1,
    part2,
    test1,
  )
where

import Data.Foldable (Foldable (minimum))
import Data.List (maximum)
import RIO
import qualified RIO.Text as T

--- >>> test1
-- [16,1,2,0,4,2,7,1,2,14]
test1 = parseInput "16,1,2,0,4,2,7,1,2,14"

crabWalk f xs = minimum $ map (\i -> sum $ map (\x -> f $ abs (i - x)) xs) [minimum xs .. maximum xs]

-- >>> solve1 test1
-- 37
solve1 = crabWalk id

-- >>> solve2 test1
-- 168
solve2 = crabWalk $ \d -> d * (d + 1) `div` 2

parseInput :: Text -> [Int]
parseInput x = fromMaybe [] $ readMaybe $ T.unpack $ "[" <> x <> "]"

-- >>> part1
-- 328262
part1 = solve1 . parseInput <$> readFileUtf8 "./input/day7.txt"

-- >>> part2
-- 90040997
part2 = solve2 . parseInput <$> readFileUtf8 "./input/day7.txt"
