{-# LANGUAGE TupleSections #-}

module Days.DayX
  ( part1,
    part2,
    test1,
  )
where

import Data.Foldable (maximum, minimum)
import qualified Data.List as L
import RIO
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T
import Util (histogram)

--- >>> L.sort $ concatMap mkLine test1
-- [(0,9),(0,9), (1,9),(1,9), (2,9),(2,9),(3,4),(3,4),(3,9),(4,4),(4,9),(5,4),(5,9),(6,4),(7,0),(7,1),(7,2),(7,3),(7,4),(7,4),(8,4),(9,4)]
test1 = parseInput "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"

mkLine :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
mkLine ((x1, y1), (x2, y2))
  | y1 == y2 =
    let start = minimum [x1, x2]
        end = maximum [x1, x2]
     in map (,y1) [start .. end]
  | x1 == x2 =
    let start = minimum [y1, y2]
        end = maximum [y1, y2]
     in map (x1,) [start .. end]
  | otherwise = []

genSolve f = length . filter (\(_, x) -> x > 1) . histogram . concatMap f

-- >>> solve1 test1
-- 5
solve1 = genSolve mkLine

mkLine2 :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
mkLine2 cs@((x1, y1), (x2, y2))
  | y1 == y2 || x1 == x2 = mkLine cs
  | otherwise = (x2, y2) : inBetweenPts
  where
    inBetweenPts = takeWhile notThere $ L.iterate goDir (x1, y1)
    notThere (xN, yN) = xN /= x2 && yN /= y2
    goDir (xN, yN) = (xN + dir xN x2, yN + dir yN y2)
    dir z1 z2
      | z1 > z2 = -1
      | otherwise = 1

-- >>> solve2 test1
-- 12
solve2 = genSolve mkLine2

parseInput = mapMaybe ((\[c1, c2] -> (,) <$> parseCoord c1 <*> parseCoord c2) . T.splitOn " -> ") . T.lines
  where
    parseCoord = (\[x, y] -> (,) <$> readInt x <*> readInt y) . T.splitOn ","
    readInt :: Text -> Maybe Int
    readInt = readMaybe . T.unpack

-- >>> part1
-- 4873
part1 = solve1 . parseInput <$> readFileUtf8 "./input/day5.txt"

-- >>> part2
-- 19472
part2 = solve2 . parseInput <$> readFileUtf8 "./input/day5.txt"
