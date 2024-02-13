import Data.List

-- разбиение списка
partitionN :: [a] -> Int -> [[a]]
partitionN xs n
  | n <= 0 = error "Number of partitions should be greater than 0"
  | length xs < n = error "Number of partitions should be less than the length of the list"
  | otherwise =
      let len = (length xs) `div` n
          extra = length xs `mod` n
       in helper xs n len extra

helper :: [a] -> Int -> Int -> Int -> [[a]]
helper _ 0 _ _ = []
helper xs n len extra =
  let part = if extra > 0 then len + 1 else len
   in take part xs : helper (drop part xs) (n - 1) len (max 0 (extra - 1))

-- пифагоровы тройки
findPythagoreanTriple :: Int -> Int
findPythagoreanTriple sum = head [a * b * c | a <- [1 .. sum], b <- [a .. sum], let c = sum - a - b, a * a + b * b == c * c]

-- список подсписков
powerSublists :: Integer -> Integer -> [[Integer]]
powerSublists numElements maxNumber = [[n ^ k | k <- [1 .. numElements]] | n <- [1 .. maxNumber]]

-- транспонирование матрицы
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose x = (map head x) : Main.transpose (map tail x)

--скобки

isValidParentheses :: String -> Bool
isValidParentheses s = null $ foldl' f [] s
  where
    f [] ')' = "Unmatched closing parenthesis"
    f (x : xs) ')' = xs
    f xs '(' = '(' : xs
    f xs _ = xs

checkFile :: FilePath -> IO ()
checkFile filename = do
  content <- readFile filename
  if isValidParentheses content
    then putStrLn "Brackets are properly matched"
    else putStrLn "Incorrect bracket matching"

main :: IO ()
main = do
  print (partitionN [1, 2, 3, 4, 5, 6, 7] 3)
  print (findPythagoreanTriple 1000)
  print (powerSublists 2 4)
  print (Main.transpose [[1, 2, 3], [4, 5, 6]])
  checkFile "input.txt"
  