main = do {prob1;
           prob2;
           prob3}


prob1 = do {putStrLn "Problem 1";
            putStrLn.show $ myLast [1,2,3];
            putStrLn.show $ myLast [1..20];
            putStrLn.show $ myLast' [1,2,3];
            putStrLn.show $ myLast' [1];
            putStrLn.show $ myLast'' [1,2,3];
            putStrLn.show $ last [1,2,3];
            putStrLn.show $ last [1]}

myLast :: [a] -> a
myLast [] = error "Last on en empty list"
myLast [e] = e
myLast (_:xs) = myLast xs

myLast' :: [a] -> a
myLast' = head . reverse

myLast'' :: [a] -> a
myLast'' = foldr1 (flip const)


prob2 = do{putStrLn "Problem 2";
           putStrLn.show $ myButLast [1..20];
           putStrLn.show $ myButLast' [1..20];
           putStrLn.show $ myButLast'' [1..20];
           putStrLn.show $ myButLast''' [1..20]}

myButLast :: [a] -> a
myButLast [] = error "Not enough elements"
myButLast [_] = error "Not enough elements"
myButLast (e:_:[]) = e
myButLast (_:xs) = myButLast xs

myButLast' :: [a] -> a
myButLast' = (!!1) . reverse

myButLast'' :: [a] -> a
myButLast'' = last . init

myButLast''' = head . tail . reverse


prob3 = do {putStrLn "Problem 3";
            putStrLn.show $ element_at [1..20] 4;
            putStrLn.show $ element_at' [1..20] 4;
            putStrLn.show $ element_at'' [1..20] 4}

element_at :: [a] -> Int -> a
element_at [] _ = error "Empty list or not enough elements"
element_at (x:_) 1 = x
element_at (_:xs) n
    | n > 1 = element_at xs (n - 1)
    | otherwise = error "Invalid index value"

element_at' l n = last (take n l)

element_at'' l n = head $ drop (n-1) l