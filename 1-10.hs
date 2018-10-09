import Data.List (group)
import Control.Arrow
import Control.Category
import Prelude hiding (id,(.))

main = do {prob1;
           prob2;
           prob3;
           prob4;
           prob5;
           prob6;
           prob7;
           prob8;
           prob9;
           prob10}


prob1 = do {putStrLn "Problem 1";
            putStrLn . show $ myLast [1..1000000];
            putStrLn . show $ myLast' [1..1000000];
            putStrLn . show $ myLast'' [1..1000000];
            putStrLn . show $ last [1..1000000]}

myLast :: [a] -> a
myLast [] = error "Last on en empty list"
myLast [e] = e
myLast (_:xs) = myLast xs

myLast' = head . reverse

myLast'' = foldr1 (flip const)


prob2 = do{putStrLn "Problem 2";
           putStrLn . show $ myButLast [1..1000000];
           putStrLn . show $ myButLast' [1..1000000];
           putStrLn . show $ myButLast'' [1..1000000];
           putStrLn . show $ myButLast''' [1..1000000]}

myButLast :: [a] -> a
myButLast [] = error "Not enough elements"
myButLast [_] = error "Not enough elements"
myButLast (e:_:[]) = e
myButLast (_:xs) = myButLast xs

myButLast' = (!!1) . reverse

myButLast'' = last . init

myButLast''' = head . tail . reverse


prob3 = do {putStrLn "Problem 3";
            putStrLn . show $ element_at [1..1000000] 4;
            putStrLn . show $ element_at' [1..1000000] 4;
            putStrLn . show $ element_at'' [1..1000000] 4}

element_at :: [a] -> Int -> a
element_at [] _ = error "Empty list or not enough elements"
element_at (x:_) 1 = x
element_at (_:xs) n
    | n > 1 = element_at xs (n - 1)
    | otherwise = error "Invalid index value"

element_at' l n = last (take n l)

element_at'' l n = head $ drop (n-1) l


prob4 = do{putStrLn "Problem 4";
           putStrLn . show $ myLength [1..1000000];
           putStrLn . show $ myLength' [1..1000000];
           putStrLn . show $ myLength'' [1..1000000];
           putStrLn . show $ myLength''' [1..1000000];
           putStrLn . show $ myLength'''' [1..1000000];
           putStrLn . show $ myLength''''' [1..1000000]}

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength' = sum . map (\_ -> 1)

myLength'' = foldl (\n _ -> n+1) 0

myLength''' = foldr (\_ n -> n+1) 0

myLength'''' = foldl (const . (+1)) 0

myLength''''' = fst . last . zip [1..]


prob5 = do{putStrLn "Problem 5";
           putStrLn . show $ (head . myReverse) [1..1000000];
           putStrLn . show $ (head . myReverse') [1..1000000];
           putStrLn . show $ (head . myReverse'') [1..1000000];
           putStrLn . show $ (head . myReverse''') [1..1000000]}

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myReverse' = myReverse_accum []
    where
        myReverse_accum acc [] = acc
        myReverse_accum acc (x:xs) = myReverse_accum (x:acc) xs

myReverse'' = foldl (\acc e -> e:acc) []

myReverse''' = foldl (flip (:)) []


prob6 = do{putStrLn "Problem 6";
           putStrLn . show $ isPalindrome [1..1000000];
           putStrLn . show $ isPalindrome [1,2,3,2,1];
           putStrLn . show $ isPalindrome' [1..1000000];
           putStrLn . show $ isPalindrome' [1,2,3,2,1];
           putStrLn . show $ isPalindrome'' [1..1000000];
           putStrLn . show $ isPalindrome'' [1,2,3,2,1];
           putStrLn . show $ isPalindrome''' [1..1000000];
           putStrLn . show $ isPalindrome''' [1,2,3,2,1];
           putStrLn . show $ isPalindrome'''' [1..1000000];
           putStrLn . show $ isPalindrome'''' [1,2,3,2,1]}

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = (head xs) == (last xs) && (isPalindrome $ init . tail $ xs)

isPalindrome' xs = xs == (reverse xs)

isPalindrome'' xs = foldl (\acc (a,b) -> acc && a == b) True (zip xs (reverse xs))

isPalindrome''' xs = foldl (&&) True $ zipWith (==) xs (reverse xs)

isPalindrome'''' xs = and $ zipWith (==) xs (reverse xs)


prob7 = do{putStrLn "Problem 7";
           putStrLn . show $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]);
           putStrLn . show $ flatten' (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]);
           putStrLn . show $ flatten'' (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])}

data NestedList a = Elem a | List [NestedList a] deriving (Show)

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

flatten' (Elem x) = [x]
flatten' (List xs) = concatMap flatten xs

flatten'' = reverse . rec []
    where
        rec acc (List []) = acc
        rec acc (Elem x) = x:acc
        rec acc (List (x:xs)) = rec (rec acc x) (List xs)


prob8 = do{putStrLn "Problem 8";
           putStrLn . show $ compress "aaaabccaadeeee";
           putStrLn . show $ compress' "aaaabccaadeeee";
           putStrLn . show $ compress'' "aaaabccaadeeee";
           putStrLn . show $ compress''' "aaaabccaadeeee"}

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
    | x == (head xs) = compress xs
    | otherwise = x : compress xs

compress' :: (Eq a) => [a] -> [a]
compress' = reverse . foldl eqHead []
    where
        eqHead [] e = [e]
        eqHead acc e = if e == head(acc) then acc else e:acc

compress'' :: (Eq a) => [a] -> [a]
compress'' = foldr eqHead []
    where
        eqHead e [] = [e]
        eqHead e acc = if e == head(acc) then acc else e:acc

compress''' = map head . group


prob9 = do{putStrLn "Problem 9";
           putStrLn . show $ pack "aaaabccaadeeee";
           putStrLn . show $ pack' "aaaabccaadeeee";
           putStrLn . show $ pack'' "aaaabccaadeeee";
           putStrLn . show $ pack''' "aaaabccaadeeee"}


pack :: Eq a => [a] -> [[a]]
pack (x:xs) = reverse $ ppack  xs [[x]]

ppack :: Eq a => [a] -> [[a]] -> [[a]]
ppack [] acc = acc
ppack (x:xs) (y:ys)
  | x == head(y) = ppack xs ((x:y):ys)
  | otherwise = ppack xs ([x]:y:ys)


pack' :: Eq a => [a] -> [[a]]
pack' xs = foldr fn [] xs
  where
    fn x [] = [[x]]
    fn x (y:ys)
      | x == head(y) = ((x:y):ys)
      | otherwise = ([x]:y:ys)

pack'' :: Eq a => [a] -> [[a]]
pack'' [] = []
pack'' (x:xs) = let (first,rest) = span (==x) (x:xs)
                in first:(pack'' rest)

pack''' :: Eq a => [a] -> [[a]]
pack''' [] = []
pack''' (x:xs) = (x:takeWhile (==x) xs):(pack''' $ dropWhile (==x) xs)

prob10 = do{putStrLn "Problem 10";
            putStrLn . show $ encode "aaaabccaadeeee";
            putStrLn . show $ encode' "aaaabccaadeeee";
            putStrLn . show $ encode'' "aaaabccaadeeee"}

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\l -> (length l, head l)) . group

encode' xs = [(length l, head l) | l <- group xs]

encode'' :: Eq a => [a] -> [(Int, a)]
encode'' = map (length &&& head) . group