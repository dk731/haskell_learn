removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number!"
lucky x = "Not so lucky number :("

factorial :: (Integral x) => x -> x
factorial 0 = 1
factorial x = x * factorial (x - 1)

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "Underweight!"
  | bmi <= 25.0 = "OK!"
  | bmi <= 30.0 = "Kinda not OK but still OK"
  | otherwise = "You are really fat..."
  where
    bmi = weight / height ^ 2

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ 2

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "WTF?!"
maximum' [x] = x
maximum' (x : xs)
  | x > maxTail = x
  | otherwise = maxTail
  where
    maxTail = maximum' xs

replicate' :: (Integral b) => a -> b -> [a]
replicate' val 1 = [val]
replicate' val n = val : replicate' val (n - 1)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []