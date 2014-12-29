import Data.Ratio

sequence1 :: Num a => (a -> a) -> a -> [a]
sequence1 f seed = seed : sequence1 f (f seed)

sequence2 :: Num a => (a -> a -> a) -> a -> a -> [a]
sequence2 f seed1 seed2 = seed1 : sequence2 f seed2 (f seed1 seed2) 

geometricImp :: Num a => a -> a -> [a] 
geometricImp seed ratio = sequence1 (\x -> x * ratio) seed

arithmeticImp:: Num a => a -> a -> [a]
arithmeticImp seed difference = sequence1 (\x -> x + difference) seed

fibonacciImp :: Num a => a -> a -> [a]
fibonacciImp seed1 seed2 = sequence2 (\x y -> x + y) seed1 seed2

pellImp :: Num a => a -> a -> [a]
pellImp seed1 seed2 = sequence2 (\x y -> x + 2 * y) seed1 seed2

arithmetic :: [Integer]
arithmetic = arithmeticImp 1 1

geometric :: [Integer]
geometric = geometricImp 1 2

harmonic :: [Rational]
harmonic = map (\x -> 1 % x) arithmetic 

fibonacci :: [Integer]
fibonacci = fibonacciImp 0 1

pell :: [Integer]
pell = pellImp 0 1

type Range = (Int, Int)

sequenceInRange :: Int -> Int -> [a] -> [a]
sequenceInRange a b
  | a <= b    = sequenceInRangeImp (a, b)
  | otherwise = sequenceInRangeImp (b, a)

sequenceInRangeImp :: Range -> [a] -> [a]
sequenceInRangeImp (smaller, larger) = (drop smaller).(take larger)


-- subsequence ::
