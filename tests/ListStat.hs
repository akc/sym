-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--

module ListStat where

import Data.List

-- shadow :: Ord a => [a] -> [[a]]
-- shadow w = nubsort . map normalize $ ptDeletions w
--     where
--       w' = sort w
--       normalize u = [ w'!!i | i <- st u ]
--       nubsort = map head . group . sort
--       ptDeletions [] = []
--       ptDeletions (x:xt) = xt : map (x:) (ptDeletions xt)

-- coshadow :: Integral a => [a] -> [[Int]]
-- coshadow w =
--     nub . sort . map (map (+1) . st) $ [0..length w] >>= \i ->
--         ptExtensions (fromIntegral i + 0.5 :: Double) (map fromIntegral w)
--     where
--       ptExtensions n [] = [[n]]
--       ptExtensions n xs@(x:xt) = (n:xs) : map (x:) (ptExtensions n xt)

-- The list of indices of components in a permutation
components :: Ord a => [a] -> [a]
components w = lMaxima w `cap` rMinima (bubble w)
    where
      -- The bubble-sort operator
      bubble :: Ord a => [a] -> [a]
      bubble = bub []
          where
            bub xs []       = reverse xs
            bub [] (y:ys)   = bub [y] ys
            bub (x:xs) (y:ys)
                | x < y     = bub (y:x:xs) ys
                | otherwise = bub (x:y:xs) ys

-- Like Data.List.intersect, but by assuming that the lists are sorted
-- uses a faster algorithm
cap :: Ord a => [a] -> [a] -> [a]
cap [] _  = []
cap _  [] = []
cap xs@(x:xt) ys@(y:yt) =
    case compare x y of
      EQ -> x : cap xt yt
      LT -> cap xt ys
      GT -> cap xs yt

-- The list of indices of skew components in a permutation
skewComponents :: [Int] -> [Int]
skewComponents w = components $ map (\x -> length w - x - 1) w

binomial :: Integral a => a -> a -> a
binomial n k = fromIntegral $ product [n', n'-1 .. n'-k'+1] `div` product [1..k']
    where
      n' = toInteger n
      k' = toInteger k

kSubsequences :: Int -> [a] -> [[a]]
kSubsequences 0 _      = [[]]
kSubsequences _ []     = []
kSubsequences k (x:xs) = map (x:) (kSubsequences (k-1) xs) ++ kSubsequences k xs

copies :: [Int] -> [Int] -> [[Int]]
copies p w = [ is | js <- u, let (is, q) = unzip (f js (zip [0..] w)), st q == p ]
    where
      k = length p
      n = length w
      u = kSubsequences k [0..n-1]
      f s@(j:t) ((i,x):v) = if i == j then (i,x) : f t v else f s v
      f _       _         = []

-- the group theoretical inverse of w
inverse :: (Ord a) => [a] -> [Int]
inverse w = map snd . sort $ zip w [0..]

-- the standardization of w
st :: (Ord a) => [a] -> [Int]
st = inverse . inverse

ascents, descents :: (Ord a) => [a] -> [(a, a)]
ascents  w = filter (uncurry (<)) $ zip w (tail w)
descents w = filter (uncurry (>)) $ zip w (tail w)

peaks, valleys, doubleAscents, doubleDescents :: Ord a => [a] -> [(a, a, a)]
peaks          w = [ v | v@(x,y,z) <- zip3 w (tail w) (tail (tail w)), x < y, y > z ]
valleys        w = [ v | v@(x,y,z) <- zip3 w (tail w) (tail (tail w)), x > y, y < z ]
doubleAscents  w = [ v | v@(x,y,z) <- zip3 w (tail w) (tail (tail w)), x < y, y < z ]
doubleDescents w = [ v | v@(x,y,z) <- zip3 w (tail w) (tail (tail w)), x > y, y > z ]

inversions :: (Ord a) => [a] -> [(a, a)]
inversions w = init (tails w) >>= \(x:xs) -> [ (x,y) | y<-xs, x > y ]

records :: (a -> a -> Bool) -> [a] -> [a]
records _ []     = []
records f (x:xs) = records' [x] xs where
    records' recs       []     = recs
    records' recs@(r:_) (y:ys) = records' (if f r y then y:recs else recs) ys

lMinima, lMaxima, rMinima, rMaxima :: (Ord a) => [a] -> [a]
lMinima = reverse . records (>)
lMaxima = reverse . records (<)
rMinima = records (>) . reverse
rMaxima = records (<) . reverse

excedances :: [Int] -> [Int]
excedances xs =
    map fst . filter (\(i,a)->i <  fromIntegral a) $ zip [0::Int ..] xs

fixedpoints :: [Int] -> [Int]
fixedpoints xs =
    map fst . filter (\(i,a)->i == fromIntegral a) $ zip [0::Int ..] xs

exc, fp :: [Int] -> Int
exc = length . excedances
fp  = length . fixedpoints

cyc :: [Int] -> Int
cyc w = length $ orbits (w!!) w
    where
      orbit f x = y : takeWhile (/=y) ys where (y:ys) = iterate f x
      orbits _ [] = []
      orbits f (x:xs) = ys : orbits f (xs\\ys) where ys = orbit f x

runs :: Ord a => (a -> a -> Bool) -> [a] -> [a] -> [[a]]
runs _ [] []     = []
runs _ rs []     = [rs]
runs f [] (x:xs) = runs f [x] xs
runs f u@(r:_) (x:xs)
    | f r x      = runs f (x:u) xs
    | otherwise  = u : runs f [x] xs

decruns :: Ord a => [a] -> [[a]]
decruns = runs (>) []

incruns :: Ord a => [a] -> [[a]]
incruns = runs (<) []

ldr, rdr, lir, rir :: (Ord a) => [a] -> Int

ldr [] = 0
ldr xs = length . head $ decruns xs

rdr [] = 0
rdr xs = length . last $ decruns xs

lir [] = 0
lir xs = length . head $ incruns xs

rir [] = 0
rir xs = length . last $ incruns xs

des, asc, inv, lmin, lmax, rmin, rmax, peak, vall :: [Int] -> Int
dasc, ddes, maj, comaj, ep, dim, comp, scomp, asc0, des0 :: [Int] -> Int
-- shad :: [Int] -> Int

-- rank a la Elizalde
ep = fst . last . filter (\(k,ys) -> all (k<=) ys) . zip [0..] . inits

dim   w = maximum $ 0 : [ i | (i,x) <- zip [0..] w, i /= x ]
maj   w = sum [ i | (i,x,y) <- zip3 [1..] w (tail w), x > y ]
comaj w = sum [ n-i | (i,x,y) <- zip3 [1..] w (tail w), x > y ] where n = length w
asc0  w = sum [ 1 | (x,y) <- ascents  w, y-x == 1 ]
des0  w = sum [ 1 | (x,y) <- descents w, x-y == 1 ]

asc   = length . ascents
des   = length . descents
inv   = length . inversions
lmin  = length . lMinima
lmax  = length . lMaxima
rmin  = length . rMinima
rmax  = length . rMaxima
peak  = length . peaks
vall  = length . valleys
dasc  = length . doubleAscents
ddes  = length . doubleDescents
-- shad  = length . shadow
comp  = length . components
scomp = length . skewComponents
