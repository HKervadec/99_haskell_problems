import Data.List (group)
import Control.Arrow
import Control.Category
import Prelude hiding (id,(.))

main = do{prob11;
          prob12;
          prob13;
          prob14;
          prob15;
          prob16;
          prob17;
          prob18;
          prob19}

prob11 = do{putStrLn "Problem 11";
            putStrLn . show $ encodeModified "aaaabccaadeeee";
            putStrLn . show $ encodeModified' "aaaabccaadeeee"}

encode :: Eq a => [a] -> [(Int, a)]
encode = map (length &&& head) . group

data Encoding a = Single a | Multiple Int a deriving (Show)

encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified xs = map (uncurry fn) (encode xs)
    where
        fn 1 x = Single x
        fn n x = Multiple n x

encodeModified' xs = [y | x <- group xs, let y = if (length x) == 1 then Single (head x) else Multiple (length x ) (head x)]


prob12 = do{putStrLn "Problem 12";
            putStrLn . show $ decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e'];
            putStrLn . show $ decodeModified' [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']}

decodeModified :: Eq a => [Encoding a] -> [a]
decodeModified = concatMap fn
    where
        fn (Single x) = [x]
        fn (Multiple n x) = replicate n x

to_tuple :: Encoding a -> (Int, a)
to_tuple (Single x) = (1, x)
to_tuple (Multiple n x) = (n, x)

decodeModified' = concatMap (uncurry replicate . to_tuple)

decode :: Eq a => [(Int, a)] -> [a]
decode = foldr f []
    where
        f (1, x) r = x:r
        f (n, x) r = x:f (n-1, x) r


prob13 = do{putStrLn "Problem 13";
            putStrLn . show $ encodeDirect "aaaabccaadeeee"}

to_encoding :: Eq a => Int -> a -> Encoding a
to_encoding 1 x = Single x
to_encoding n x = Multiple n x

encodeDirect :: Eq a => [a] -> [Encoding a]
encodeDirect (x:xs) = fn 1 x xs
    where
        fn n x [] = [to_encoding n x]
        fn n x (y:xs)
            | x == y = fn (n+1) x xs
            | otherwise = (to_encoding n x):fn 1 y xs


prob14 = do{putStrLn "Problem 14";
            putStrLn . show $ dupli [1, 2, 3];
            putStrLn . show $ dupli' [1, 2, 3];
            putStrLn . show $ dupli'' [1, 2, 3];
            putStrLn . show $ dupli''' [1, 2, 3]}

dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

dupli' = foldr (\x acc -> x:x:acc) []

dupli'' [] = []
dupli'' (x:xs) = x:x:dupli xs

dupli''' = foldr (\x -> (x:) . (x:)) []


prob15 = do{putStrLn "Problem 15";
            putStrLn . show $ repli "abc" 3;
            putStrLn . show $ repli' "abc" 3;
            putStrLn . show $ repli'' "abc" 3;
            putStrLn . show $ repli''' "abc" 3}

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

repli' [] _ = []
repli' (x:xs) n = replicate n x ++ repli' xs n

repli'' = flip $ concatMap . replicate

repli''' xs n = foldr (\x acc -> (replicate n x)++acc) [] xs


prob16 = do{putStrLn "Problem 16";
            putStrLn . show $ dropEvery "abcdefghik" 3;
            putStrLn . show $ dropEvery' "abcdefghik" 3;
            putStrLn . show $ dropEvery'' "abcdefghik" 3}

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = fn xs n n
    where
        fn [] _ _ = []
        fn (_:xs) n 1 = fn xs n n
        fn (x:xs) n m = x : fn xs n (m-1)

dropEvery' xs n = map fst $ filter ((/=n) . snd) (zip xs (cycle [1..n]))

dropEvery'' xs n = [x | (x,i) <- zip xs (cycle [1..n]), i /= n]

prob17 = do{putStrLn "Problem 17";
            putStrLn . show $ split "abcdefghik" 3;
            putStrLn . show $ split' "abcdefghik" 3}

split :: [a] -> Int -> ([a],[a])
split = fn []
    where
        fn xs [] _ = (reverse xs,[])
        fn xs ys 0 = (reverse xs,ys)
        fn xs (y:ys) n = fn (y:xs) ys (n-1)

split' xs n = foldr (\(x,i) (xs,ys) -> if i <= n then (x:xs,ys) else (xs,x:ys)) ([],[]) (zip xs [1..])


prob18 = do{putStrLn "Problem 18";
            putStrLn . show $ slice ['a','b','c','d','e','f','g','h','i','k'] 3 7;
            putStrLn . show $ slice' ['a','b','c','d','e','f','g','h','i','k'] 3 7;
            putStrLn . show $ slice'' ['a','b','c','d','e','f','g','h','i','k'] 3 7;
            putStrLn . show $ slice''' ['a','b','c','d','e','f','g','h','i','k'] 3 7;
            putStrLn . show $ slice'''' ['a','b','c','d','e','f','g','h','i','k'] 3 7;
            putStrLn . show $ slice''''' ['a','b','c','d','e','f','g','h','i','k'] 3 7}

slice :: [a] -> Int -> Int -> [a]
slice xs a b = take (b-a+1) $ drop (a-1) xs

slice' :: [a] -> Int -> Int -> [a]
slice' [] _ _ = []
slice' (x:xs) a b
    | b == 1 = [x]
    | a <= 1 = x:rest
    | otherwise = rest
    where
        rest = slice' xs (a-1) (b-1)

slice'' xs a b = foldr (\(x,i) acc -> if (a <= i) && (i <= b) then x:acc else acc) [] (zip xs [1..])

slice''' xs a b = foldr (\(x,i) acc -> if a <= i then x:acc else acc) [] (zip xs [1..b])

slice'''' xs a b = [x | (x,i) <- zip xs [1..b], a <= i]

slice''''' xs a b = map fst $ filter ((a<=) . snd) (zip xs [1..b])


prob19 = do{putStrLn "Problem 19";
            putStrLn . show $ rotate ['a','b','c','d','e','f','g','h'] 3;
            putStrLn . show $ rotate ['a','b','c','d','e','f','g','h'] (-2)}

rotate :: [a] -> Int -> [a]
rotate xs n
    | n >= 0 = (uncurry . flip) (++) (split xs n)
    | otherwise = rotate xs (length xs + n)