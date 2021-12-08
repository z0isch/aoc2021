{-# LANGUAGE TupleSections #-}

module Days.Day8
  ( part1,
    part2,
    test1,
  )
where

import qualified Data.List as L
import Data.Monoid (Alt (Alt, getAlt))
import RIO
import qualified RIO.HashMap as HashMap
import qualified RIO.HashMap.Partial as HashMap
import qualified RIO.HashSet as HashSet
import RIO.Lens (_1)
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T

--- >>> test1
--- >>> fmap (HashMap.fromList . zip "abc") $ L.permutations "abc"
-- [(["be","cfbegad","cbdgef","fgaecd","cgeb","fdcge","agebfd","fecdb","fabcd","edb"],["fdgacbe","cefdb","cefbgd","gcbe"]),(["edbfga","begcd","cbg","gc","gcadebf","fbgde","acbgfd","abcde","gfcbed","gfec"],["fcgedb","cgb","dgebacf","gc"]),(["fgaebd","cg","bdaec","gdafb","agbcfd","gdcbef","bgcad","gfac","gcb","cdgabef"],["cg","cg","fdcagb","cbg"]),(["fbegcd","cbd","adcefb","dageb","afcb","bc","aefdc","ecdab","fgdeca","fcdbega"],["efabcd","cedba","gadfec","cb"]),(["aecbfdg","fbg","gf","bafeg","dbefa","fcge","gcbea","fcaegb","dgceab","fcbdga"],["gecf","egdcabf","bgf","bfgea"]),(["fgeab","ca","afcebg","bdacfeg","cfaedg","gcfdb","baec","bfadeg","bafgc","acf"],["gebdcfa","ecba","ca","fadegcb"]),(["dbcfg","fgd","bdegcaf","fgec","aegbdf","ecdfab","fbedc","dacgb","gdcebf","gf"],["cefg","dcbef","fcge","gbcadfe"]),(["bdfegc","cbegaf","gecbf","dfcage","bdacg","ed","bedf","ced","adcbefg","gebcd"],["ed","bcgafe","cdgba","cbgef"]),(["egadfb","cdbfeg","cegd","fecab","cgb","gbdefca","cg","fgcdab","egfdb","bfceg"],["gbdfcae","bgc","cg","cgb"]),(["gcafb","gcf","dcaebfg","ecagb","gf","abcdeg","gaef","cafbge","fdbac","fegbdc"],["fgae","cfgab","fg","bagce"])]
-- [fromList [('a','a'),('b','b'),('c','c')],fromList [('a','b'),('b','a'),('c','c')],fromList [('a','c'),('b','b'),('c','a')],fromList [('a','b'),('b','c'),('c','a')],fromList [('a','c'),('b','a'),('c','b')],fromList [('a','a'),('b','c'),('c','b')]]
test1 :: [([String], [String])]
test1 = parseInput "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"

-- >>> solve1 test1
-- 26
solve1 = sum . fmap (length . filter isUniqueLetter . snd)
  where
    isUniqueLetter = (`elem` [2, 3, 4, 7]) . L.length

--- >>> doPerm "abcdefg" ["acedgfb","cdfbe","gcdfa","fbcad","dab","cefabd","cdfgeb","eafb","cagedb","ab"]
-- Nothing
doPerm :: String -> [String] -> Maybe (HashMap (HashSet Char) Int)
doPerm p = fmap (HashMap.fromList . map (over _1 HashSet.fromList)) . traverse (\ws -> (ws,) <$> lookupWire (map (wireLookup HashMap.!) ws))
  where
    lookupWire = (`HashMap.lookup` wires) . HashSet.fromList
    wireLookup = HashMap.fromList $ zip "abcdefg" p

-- >>> findPerm ["acedgfb","cdfbe","gcdfa","fbcad","dab","cefabd","cdfgeb","eafb","cagedb","ab"]
-- fromList [(fromList "abd",7),(fromList "abcdefg",8),(fromList "abef",4),(fromList "ab",1),(fromList "abcdeg",0),(fromList "bcdefg",6),(fromList "abcdef",9),(fromList "acdfg",2),(fromList "abcdf",3),(fromList "bcdef",5)]
findPerm :: [String] -> HashMap (HashSet Char) Int
findPerm ws = fold $ getAlt $ foldMap (\p -> Alt $ doPerm p ws) $ L.permutations "abcdefg"

-- >>> number (["acedgfb","cdfbe","gcdfa","fbcad","dab","cefabd","cdfgeb","eafb","cagedb","ab"],["cdfeb","fcadb","cdfeb","cdbaf"])
-- 5353
number :: ([String], [String]) -> Int
number (i, o) = fromMaybe (0 :: Int) $ readMaybe $ concatMap show $ mapMaybe ((`HashMap.lookup` wireLookup) . HashSet.fromList) o
  where
    wireLookup = findPerm i

wires :: HashMap (HashSet Char) Int
wires =
  HashMap.fromList
    [ (HashSet.fromList "abcefg", 0),
      (HashSet.fromList "cf", 1),
      (HashSet.fromList "acdeg", 2),
      (HashSet.fromList "acdfg", 3),
      (HashSet.fromList "bcdf", 4),
      (HashSet.fromList "abdfg", 5),
      (HashSet.fromList "abdfeg", 6),
      (HashSet.fromList "acf", 7),
      (HashSet.fromList "abcdefg", 8),
      (HashSet.fromList "abcdfg", 9)
    ]

-- >>> solve2 test1
-- 61229
solve2 :: [([String], [String])] -> Int
solve2 = sum . map number

parseInput :: Text -> [([String], [String])]
parseInput = fmap (bimap L.words L.words . (\[x, y] -> (T.unpack x, T.unpack y)) . T.splitOn " | ") . T.lines

-- >>> part1
-- 421
part1 = solve1 . parseInput <$> readFileUtf8 "./input/day8.txt"

-- >>> part2
-- 986163
part2 = solve2 . parseInput <$> readFileUtf8 "./input/day8.txt"
