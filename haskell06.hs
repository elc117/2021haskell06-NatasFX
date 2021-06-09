-- Prática 06 de Haskell
-- Nome: Natã Schmitt


ends :: [Int] -> [Int]
ends [] = []
ends [f] = [f]
ends (f:l) = [f,last l]


deduzame :: [Integer] -> [Integer]
deduzame = map (2 *)


deduzame2 :: [Integer] -> [Integer]
deduzame2 [] = []
deduzame2 (x:xs)
  | x > 2 = x : deduzame2 xs
  | otherwise = deduzame2 xs


geraTabela :: Int -> [(Int,Int)]
geraTabela n
  | n > 1 = (n,n^2) : geraTabela (n-1)
  | otherwise = [(1,1)]


contido :: String -> Char -> Bool
contido (h:t) c
  | c == h = True
  | null t = False
  | otherwise = contido t c

translate :: [(Float,Float)] -> [(Float,Float)]
translate (h:t)
  | not $ null t = (fst h + 2, snd h + 2) : translate t
  | otherwise = [(fst h + 2, snd h + 2)]


countLongs :: [String] -> Int
countLongs (h:l)
  | null l = fromEnum $ length h > 5
  | otherwise = fromEnum (length h > 5) + countLongs l


onlyLongs :: [String] -> [String]
onlyLongs [] = []
onlyLongs (h:l)
  | length h > 5 = h : onlyLongs l
  | not $ null l = onlyLongs l
  