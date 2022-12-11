module Lib () where
import Data.List
import Data.List.Split
import Data.Set (Set, elemAt, intersection, fromList, toList)
import qualified Data.Set as Set

someFunc :: IO ()
someFunc = day2part1 


day1part1 :: IO () 
day1part1 = do 
    input <- readFile "input/day1.txt"
    let d =  map sum (map inputParser1 (splitOn "\n\n" input))
    let x = sort d
    print (last x)

day1part2 :: IO () 
day1part2 = do
    input <- readFile "input/day1.txt"
    let d =  map sum (map inputParser1 (splitOn "\n\n" input))
    let x = sort d
    let top3 = take 3 (reverse x)
    print (sum top3)

inputParser1 :: String -> [Int]
inputParser1 = map read . words

day2part1 :: IO ()
day2part1 = do
    input <- readFile "input/day2.txt"
    let d = map inputParser2 (splitOn "\n" input)
    print (sum (map day2part1helper d))


day2part1helper :: (String, String) -> Int
day2part1helper ("A","X") = 1 + 3
day2part1helper ("A","Y") = 2 + 6  
day2part1helper ("A","Z") = 3 + 0
day2part1helper ("B","X") = 1 + 0
day2part1helper ("B","Y") = 2 + 3
day2part1helper ("B","Z") = 3 + 6
day2part1helper ("C","X") = 1 + 6
day2part1helper ("C","Y") = 2 + 0
day2part1helper ("C","Z") = 3 + 3


day2part2 :: IO ()
day2part2 = do
    input <- readFile "input/day2.txt"
    let d = map inputParser2 (splitOn "\n" input)
    print (sum (map day2part2helper d))


day2part2helper :: (String, String) -> Int
day2part2helper ("A","X") = 3 + 0
day2part2helper ("A","Y") = 1 + 3  
day2part2helper ("A","Z") = 2 + 6
day2part2helper ("B","X") = 1 + 0
day2part2helper ("B","Y") = 2 + 3
day2part2helper ("B","Z") = 3 + 6
day2part2helper ("C","X") = 2 + 0
day2part2helper ("C","Y") = 3 + 3
day2part2helper ("C","Z") = 1 + 6

inputParser2 :: String -> (String, String)
inputParser2 d = do
    let ar = splitOn " " d
    (ar !! 0, ar !! 1)









day3part1 :: IO () 
day3part1 = do
    input <- readFile "input/day3.txt"
    let d = map inputParser3 (splitOn "\n" input)
    let t = map helper3 d
    print (sum t)

day3part2 :: IO () 
day3part2 = do
    input <- readFile "input/day3.txt"
    let d = splitOn "\n" input
    let t = helper3chunks d
    print t

helper3chunks :: [String] -> Int
helper3chunks [] = 0
helper3chunks (a:b:c:xs) = priority ((intersect (intersect a b) c) !! 0) + helper3chunks xs

priority :: Char -> Int 
priority 'a' = 1
priority 'b' = 2
priority 'c' = 3
priority 'd' = 4
priority 'e' = 5
priority 'f' = 6
priority 'g' = 7
priority 'h' = 8
priority 'i' = 9
priority 'j' = 10
priority 'k' = 11
priority 'l' = 12
priority 'm' = 13
priority 'n' = 14
priority 'o' = 15
priority 'p' = 16
priority 'q' = 17
priority 'r' = 18
priority 's' = 19
priority 't' = 20
priority 'u' = 21
priority 'v' = 22
priority 'w' = 23
priority 'x' = 24
priority 'y' = 25
priority 'z' = 26
priority 'A' = 27
priority 'B' = 28
priority 'C' = 29
priority 'D' = 30
priority 'E' = 31
priority 'F' = 32
priority 'G' = 33
priority 'H' = 34
priority 'I' = 35
priority 'J' = 36
priority 'K' = 37
priority 'L' = 38
priority 'M' = 39
priority 'N' = 40
priority 'O' = 41
priority 'P' = 42
priority 'Q' = 43
priority 'R' = 44
priority 'S' = 45
priority 'T' = 46
priority 'U' = 47
priority 'V' = 48
priority 'W' = 49
priority 'X' = 50
priority 'Y' = 51
priority 'Z' = 52

inputParser3 :: String -> ([Char], [Char])
inputParser3 d = Data.List.splitAt (length d `div` 2) d

helper3 :: ([Char], [Char]) -> Int
helper3 (a, b) = priority ((intersect a b) !! 0)



day4part1 :: IO () 
day4part1 = do
    input <- readFile "input/day4.txt"
    let d = map inputParser4 (splitOn "\n" input)
    print (sum (map day4helper1 d))


day4helper1 :: ((Int,Int), (Int,Int)) -> Int
day4helper1 ((a,b), (c,d)) = do
    -- if interval 1 is in interval 2 or interval 2 is in interval 1
    if (a >= c && b <= d) || (c >= a && d <= b) then 1 else 0


day4part2 :: IO () 
day4part2 = do
    input <- readFile "input/day4.txt"
    let d = map inputParser4 (splitOn "\n" input)
    print (sum (map day4helper2 d))


-- 444

day4helper2 :: ((Int,Int), (Int,Int)) -> Int
day4helper2 ((a,b), (c,d)) = do
    let x = [a..b]
    let y = [c..d]
    let z = intersect x y
    if length z == 0 then 0 else 1

inputParser4 :: String -> ((Int,Int), (Int,Int))
inputParser4 d = do
    let ar1 = splitOn "," d
    let ar2 = splitOn "-" (ar1 !! 0)
    let ar3 = splitOn "-" (ar1 !! 1)
    ((read (ar2 !! 0), read (ar2 !! 1)), (read (ar3 !! 0), read (ar3 !! 1)))

    -- 1-2,3-4
    -- split by ','         [1-2, 3-4]
    -- map & split by '-'   [[1, 2], [3, 4]]