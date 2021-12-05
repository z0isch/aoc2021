module Days.Day3
  ( part1,
    part2,
    test1,
  )
where

import Data.Char (digitToInt)
import Data.Semigroup (Sum (Sum))
import RIO
import qualified RIO.List.Partial as L
import qualified RIO.Text as T

--- >>> test1
-- ["011110011100","010001010101","111111110000","011101100011","000111100100"]
test1 :: [Text]
test1 = parseInput "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"

-- >>> solve1 test1
-- 198
solve1 :: [Text] -> Int
solve1 i = gammma * epsilon
  where
    transposed = T.transpose i
    gammma = toDec $ fmap (foo (>)) transposed
    epsilon = toDec $ fmap (foo (<)) transposed
    foo f xs = if s `f` (l `div` 2) then '1' else '0'
      where
        l = T.length xs
        Sum s = fromMaybe 0 $ foldMap (fmap Sum . readMaybe . pure) $ T.unpack xs

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- >>> oxygen test1
-- 23
oxygen xs = toDec $ T.unpack $ L.head $ foldl' (foo True) xs [0 .. length xs]

-- >>> co2 test1
-- 10
co2 xs = toDec $ T.unpack $ L.head $ foldl' (foo False) xs [0 .. length xs]

foo :: Bool -> [Text] -> Int -> [Text]
foo _ [] _ = []
foo _ [x] _ = [x]
foo f xs i = filter (\x -> T.index x i == keeper) xs
  where
    transposed = T.transpose xs
    checking = transposed L.!! i
    keeper = if t then '1' else '0'
      where
        t = if f then s >= (l - s) else s < (l - s)
        l = T.length checking
        Sum s = fromMaybe 0 $ foldMap (fmap Sum . readMaybe . pure) $ T.unpack checking

-- >>> solve2 test1
solve2 xs = oxygen xs * co2 xs

parseInput = T.lines

-- >>> part1
-- 3148794
part1 = solve1 . parseInput <$> readFileUtf8 "./input/day3.txt"

-- >>> part2
-- 2795310
part2 = solve2 . parseInput <$> readFileUtf8 "./input/day3.txt"
