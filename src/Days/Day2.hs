module Days.Day2
  ( part1
  , part2
  , test1
  ) where

import           RIO

import qualified RIO.Text                      as T

data Dir = F | U | D
    deriving stock Show

test1 :: [(Dir, Integer)]
test1 = parseInput "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

-- >>> solve1 test1
-- 150
solve1 :: [(Dir, Integer)] -> Integer
solve1 = uncurry (*) . foldl' goDir (0, 0)

goDir :: (Integer, Integer) -> (Dir, Integer) -> (Integer, Integer)
goDir (hor, depth) (dir, m) = case dir of
  F -> (hor + m, depth)
  U -> (hor, depth - m)
  D -> (hor, depth + m)

-- >>> solve2 test1
-- 900
solve2 :: [(Dir, Integer)] -> Integer
solve2 = (\(x, y, _) -> x * y) . foldl' goAim (0, 0, 0)

goAim
  :: (Integer, Integer, Integer)
  -> (Dir, Integer)
  -> (Integer, Integer, Integer)
goAim (hor, depth, aim) (dir, m) = case dir of
  F -> (hor + m, depth + (aim * m), aim)
  U -> (hor, depth, aim - m)
  D -> (hor, depth, aim + m)

parseInput :: Text -> [(Dir, Integer)]
parseInput = mapMaybe (fromWords . T.words) . T.lines
 where
  fromWords [d, x] = (,) <$> parseDir d <*> readMaybe (T.unpack x)
  fromWords _      = Nothing
  parseDir "forward" = Just F
  parseDir "up"      = Just U
  parseDir "down"    = Just D
  parseDir _         = Nothing

-- >>> part1
-- 1499229
part1 :: IO Integer
part1 = solve1 . parseInput <$> readFileUtf8 "./input/day2.txt"

-- >>> part2
-- 1340836560
part2 :: IO Integer
part2 = solve2 . parseInput <$> readFileUtf8 "./input/day2.txt"
