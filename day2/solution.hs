import Data.Char
import Data.String.Utils

parse :: String -> [[Int]]
parse = map (map read) . map words . lines . (replace "\t" " ")

part1 :: [[Int]] -> Int
part1 xs = sum . map rowValue $ xs
  where rowValue row = (maximum row) - (minimum row)

part2 :: [[Int]] -> Int
part2 xs = sum . map (findValue . head) . map (filter predicate) . map divisors $ xs
  where
    predicate (n, divs) = any (\x -> n `mod` x == 0) divs
    findValue (n, divs) = (div n) . head . filter (\x -> n `mod` x == 0) $ divs

divisors :: [Int] -> [(Int, [Int])]
divisors xs = map (\n -> (n, others n)) xs
  where
    others n = filter (/= n) xs

main = do
  content <- readFile "input.txt"
  let input = parse content
  putStrLn $ "part1: " ++ show (part1 input)
  putStrLn $ "part2: " ++ show (part2 input)

test :: [[Int]]
test = [[5, 9, 2, 8], [9, 4, 7, 3], [3, 8, 6, 5]]
