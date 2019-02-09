import Data.Char

circularPairs :: Int -> [a] -> [(a, a)]
circularPairs offset xs = zip xs $ drop offset (xs ++ xs)

part1 :: [Int] -> Int
part1 xs = foldl (\sum (a, b) -> if a == b then sum + a else sum) 0 . circularPairs 1 $ xs

part2 :: [Int] -> Int
part2 xs = foldl (\sum (a, b) -> if a == b then sum + a else sum) 0 . circularPairs offset $ xs
  where
    offset = length xs `div` 2

main = do
  content <- readFile "input.txt"
  let input = map digitToInt . filter isDigit $ content

  putStrLn $ "part1: " ++ (show $ part1 input)
  putStrLn $ "part2: " ++ (show $ part2 input)
