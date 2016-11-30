import Prelude

lineSound :: Int -> [Char]
lineSound x
      | words == 1 = "blip"
      | words == 2 = "east"
      | words == 3 = "cp"
      | words == 4 = "sf"
      | words == 5 = "dr2"
      | words == 6 = "gretsch"
      | words == 0 = "cc"
  where words = x `mod` 7

brackets :: [Char] -> [Char]
brackets x = "[" ++ x ++ "]"

wordPattern :: Int -> Int -> [Char]
wordPattern x y = brackets $ lineSound x ++ "*" ++ show y

fullLine :: [Int] -> [Char]
fullLine y =
  brackets $ (unwords $ map (wordPattern $ length y) y)

poem :: [[Int]] -> [Char]
poem x = "d1 $ slow " ++ show (length x + 1) ++ " $ sound \" " ++ (brackets $ (unwords $ map fullLine x) ++ brackets "~")
