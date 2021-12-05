module Days.DayX
  ( part1,
    part2,
    test1,
  )
where

import RIO

--- >>> test1
test1 = parseInput ""

-- >>> solve1 test1
solve1 = undefined

-- >>> solve2 test1
solve2 = undefined

parseInput = undefined

-- >>> part1
part1 = solve1 . parseInput <$> readFileUtf8 "./input/dayx.txt"

-- >>> part2
part2 = solve2 . parseInput <$> readFileUtf8 "./input/dayx.txt"
