import Data.String.Utils
import Data.List

isValid :: String -> Bool
isValid passphrase = (nub ws) == ws
  where ws = words passphrase

isValid2 :: String -> Bool
isValid2 passphrase = (nub ws) == ws
  where ws = map sort . words $ passphrase

part1 :: [String] -> Int
part1 = length . filter isValid

part2 :: [String] -> Int
part2 = length . filter isValid2

main = do
  content <- readFile "input.txt"
  let input = lines . strip $ content
  putStrLn $ "Part1: " ++ (show $ part1 input)
  putStrLn $ "Part2: " ++ (show $ part2 input)
