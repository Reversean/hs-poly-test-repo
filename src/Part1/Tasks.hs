module Part1.Tasks where

twoPi :: Double
twoPi = 2 * pi

taylorSeriesLength :: Int
taylorSeriesLength = 10

shortMonths :: [Integer]
shortMonths = [4, 6, 9, 11]

normalizeAngle :: Double -> Double -> Double
normalizeAngle x center = x - twoPi * (fromInteger . floor $ (x + pi - center) / twoPi)

taylorSeries :: (Double -> Int -> Double) -> Double -> Double
taylorSeries term x = sum $ term x `map` [0 .. taylorSeriesLength]

divideOnFactorial :: Double -> Int -> Double
divideOnFactorial x 0 = x
divideOnFactorial x 1 = x
divideOnFactorial x n = (x / fromIntegral n) `divideOnFactorial` (n - 1)

sinTerm :: Double -> Int -> Double
sinTerm x n = ((-1) ^ n * normalizeAngle x pi ^ (2 * n + 1)) `divideOnFactorial` (2 * n + 1)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin = taylorSeries sinTerm

cosTerm :: Double -> Int -> Double
cosTerm x n = ((-1) ^ n * normalizeAngle x 0 ^ (2 * n)) `divideOnFactorial` (2 * n)

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos = taylorSeries cosTerm

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD x 0 = abs x
myGCD 0 y = abs y
myGCD x y = myGCD y $ x `mod` y

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
  | day <= 0 || month <= 0 || year <= 0 = False
  | month == 2 = day <= 28 || day <= 29 && isLeap year
  | month `elem` shortMonths = day <= 30
  | otherwise = day <= 31 && month <= 12
  where
    isLeap y = y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x n = product $ replicate n' x
  where
    n' = fromIntegral n

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime = foo $ 2 : 3 : scanl (+) 5 (cycle [2, 4])
  where
    foo [] _ = False
    foo (h : t) x
      | h * h > x = True
      | x `mod` h == 0 = False
      | otherwise = foo t x

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = abs (sum1 - sum2) / 2
  where
    rotate (h : t) = t ++ [h]
    rotate [] = []
    xList = map fst points
    yList = map snd points
    rotatedYList = rotate yList
    rotatedXList = rotate xList
    sum1 = sum $ zipWith (*) xList rotatedYList
    sum2 = sum $ zipWith (*) rotatedXList yList

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
  | a <= 0 || b <= 0 || c <= 0 || a + b <= c || b + c <= a || a + c <= b = -1
  | pythagorasA == 0 || pythagorasB == 0 || pythagorasC == 0 = 2
  | pythagorasA < 0 || pythagorasB < 0 || pythagorasC < 0 = 0
  | otherwise = 1
  where
    square x = x * x
    pythagoras leg1 leg2 hyp = square leg1 + square leg2 - square hyp
    pythagorasA = pythagoras b c a
    pythagorasB = pythagoras a c b
    pythagorasC = pythagoras a b c
