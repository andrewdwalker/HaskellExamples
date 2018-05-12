--import Data.List(genericTake)

-- MacClaurin series for Sin(x) is f(0) + f'(0)(x)/1! + f''(0)(x)^2 / 2!  + ... + f^n (0) x^n / n!
-- or in the case of Sin(x):  0 + 1x + 0 + -1x^3 / 3! + 0 + 1 x^5 / 5! + 0 + -1x^7/7! ...
-- See Wikipedia https://en.wikipedia.org/wiki/Taylor_series

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  

-- return list of alterating 0 and 1
ymod2 :: Integral a => a -> [a]
ymod2 n = map (\y->(y `mod` 2)) [0..n]    



powersMac :: (Integral a) => a -> a
powersMac n
    | n == 0 = 0
    | n == 1 = 1
    | ( (n `mod` 2) == 0 ) = 0
    | otherwise =  ( (-1) * powersMac (n-2) )

-- return list of 0, 1, 0, -1, 0, 1, 0, -1 ...
powersMacList :: (Integral a) => a -> [a]
powersMacList n = map (\y->(powersMac y)) [0..n]

-- return list of x^0, x^1, x^2, ... x^n
powersOfx :: (Enum b, Floating b) =>  b -> b -> [b]
powersOfx n x = map (\y->(x**y)) [0..n]

-- return list of 0, 1, 0, -1, 0, ... to n 
firstLst :: Integral a => a -> [a]
firstLst  n = zipWith (*) (ymod2 n) (powersMacList n)

-- return list of 0x, 1x, 0x^2, -1x^3, ... to n
secondLst :: (Integral a, Enum a, Enum b, Floating b) => a -> b -> [b]
secondLst n x = zipWith (*) (map fromIntegral(firstLst n)) (powersOfx ( fromIntegral n) x)

-- return list of 0, 1x, 0 , -1x^3 / 3! , 0 , 1 x^5 / 5! , 0 ... to n
thirdLst :: (Integral a, Enum a, Enum b, Floating b) => a -> b -> [b]
thirdLst n x = zipWith (/) (secondLst n x) (map fromIntegral ( (map(\y->(factorial y) ) [0..n])) )

-- sum the list to get result
macResult :: (Integral a, Enum a, Enum b, Floating b) => a -> b -> b
macResult n x = sum (thirdLst n x)

maclaurin :: (Integral a, Floating b) => a -> b -> b
maclaurin  n x = sum (zipWith (*) (map(\y->(x^^y))[0..n])(zipWith (/) (map fromIntegral (powersMacList n)) (map fromIntegral (map(\y->(factorial y) )[0..n]) )) )
--maclaurin x n = zipWith (*) map(\y->(x^^y))[0..n] (zipWith (/) ( zipWith (*) (ymod2 n) (powersMacList n) ) map(\y->(factorial y) ) [0..n] )
     
testLst :: (Integral a, Floating b, Enum b) => a -> b -> [b]    
testLst n x = zipWith (*) (map fromIntegral (ymod2 n)) (powersOfx (fromIntegral n) x)
