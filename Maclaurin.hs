--import Data.List(genericTake)

-- MacClaurin series for Sin(x) is f(0) + f'(0)(x)/1! + f''(0)(x)^2 / 2!  + ... + f^n (0) x^n / n!
-- or in the case of Sin(x):  0 + 1x + 0 + -1x^3 / 3! + 0 + 1 x^5 / 5! + 0 + -1x^7/7! ...
-- See Wikipedia https://en.wikipedia.org/wiki/Taylor_series


-- return list of alterating 0 and 1
-- not needed for maclaurin or macResult
ymod2 :: Integral a =>  [a]
ymod2  = map (\y->(y `mod` 2)) [0..]    

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  

powersMac :: (Integral a) => a -> a
powersMac n
    | n == 0 = 0
    | n == 1 = 1
    | ( (n `mod` 2) == 0 ) = 0
    | otherwise =  ( (-1) * powersMac (n-2) )

-- return list of 0, 1, 0, -1, 0, 1, 0, -1 ...
powersMacList :: (Integral a) => [a]
powersMacList  = map (\y->(powersMac y)) [0..]


-- return list of x^0, x^1, x^2, ... x^n
powersOfx :: (Enum b, Floating b) =>  b -> [b]
powersOfx  x = map (\y->(x^^y)) [0..]

-- return list of 0, 1, 0, -1, 0, ... to n 
firstLst :: Integral a =>  [a]
firstLst   = zipWith (*) (ymod2 ) (powersMacList)

-- return list of 0x, 1x, 0x^2, -1x^3, ... to n
secondLst :: (Enum b, Floating b) =>  b -> [b]
secondLst  x = zipWith (*) (map fromIntegral(powersMacList )) (powersOfx  x)

-- return list of 0, 1x, 0 , -1x^3 / 3! , 0 , 1 x^5 / 5! , 0 ... to n
thirdLst :: ( Enum b, Floating b) =>  b -> [b]
thirdLst  x = zipWith (/) (secondLst  x) (map fromIntegral ( (map(\y->(factorial y) ) [0..])) )

-- sum the list to get result
macResult :: (  Enum b, Floating b) =>  Int -> b -> b
macResult  n x = sum (take n (thirdLst  x))

maclaurin :: (  Floating b) => Int-> b -> b
maclaurin  n x = sum ( take n (zipWith (*) (map(\y->(x^^y))[0..])(zipWith (/) (map fromIntegral (powersMacList )) (map fromIntegral (map(\y->(factorial y) )[0..]) )) ))
    

