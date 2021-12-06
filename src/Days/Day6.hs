module Days.Day6
  ( part1,
    part2,
    test1,
  )
where

import qualified Data.IntMap.Monoidal.Strict as MonoidalIntMap
import qualified Data.List as L
import Data.Semigroup (Sum (Sum))
import RIO
import RIO.Lens (_2)
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T
import Util (histogram)

--- >>> test1
-- [3,4,3,1,2]
test1 :: [Int]
test1 = parseInput "3,4,3,1,2"

runFish :: Int -> [Int] -> Sum Integer
runFish days = fold . (L.!! days) . L.iterate fishAlg . mkMap
  where
    mkMap = MonoidalIntMap.fromList . fmap (over _2 Sum) . histogram
    fishAlg = MonoidalIntMap.foldMapWithKey goFish
    goFish 0 x = MonoidalIntMap.fromList [(6, x), (8, x)]
    goFish i x = MonoidalIntMap.singleton (i -1) x

-- >>> solve1 test1
-- Sum {getSum = 5934}
solve1 :: [Int] -> Sum Integer
solve1 = runFish 80

-- >>> solve2 256 test1
-- Sum {getSum = 26984457539}
solve2 :: [Int] -> Sum Integer
solve2 = runFish 256

parseInput :: Text -> [Int]
parseInput = mapMaybe (readMaybe @Int . T.unpack) . T.splitOn ","

-- >>> part1
-- Sum {getSum = 375482}
part1 :: MonadIO f => f (Sum Integer)
part1 = solve1 . parseInput <$> readFileUtf8 "./input/day6.txt"

-- >>> part2
-- Sum {getSum = 1689540415957}
part2 :: MonadIO f => f (Sum Integer)
part2 = solve2 . parseInput <$> readFileUtf8 "./input/day6.txt"
