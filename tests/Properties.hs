-- |
-- Copyright   : (c) Anders Claesson 2012
-- License     : BSD-style
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>

import Data.List
import Data.Monoid
import Control.Monad
import qualified Math.Sym as Sym
import qualified Math.Sym.D8 as D8
import qualified Math.Sym.Stat as S
import qualified Math.Sym.Internal as I
import qualified Data.Vector.Storable as SV
import Test.QuickCheck

check :: Testable prop => prop -> IO ()
check = quickCheck

---------------------------------------------------------------------------------
-- Generators
---------------------------------------------------------------------------------

rank :: Int -> Gen Integer
rank n = choose (0, product [1..fromIntegral n] - 1)

lenRank :: Gen (Int, Integer)
lenRank = sized $ \m -> do
            n <- choose (0, m)
            r <- rank n
            return (n, r)

lenRank2 :: Gen (Int, Integer, Integer)
lenRank2 = do (n, r1) <- lenRank
              r2 <- rank n
              return (n, r1, r2)

lenRank3 :: Gen (Int, Integer, Integer, Integer)
lenRank3 = do (n, r1, r2) <- lenRank2
              r3 <- rank n
              return (n, r1, r2, r3)

-- The sub-permutation determined by a set of indices.
subperm :: Sym.Set -> Sym.StPerm -> Sym.StPerm
subperm m w = Sym.fromVector . I.st $ SV.map ((SV.!) (Sym.toVector w)) m

subperms :: Int -> Sym.StPerm -> [Sym.StPerm]
subperms k w = [ subperm m w | m <- Sym.subsets (Sym.size w) k ]

instance Arbitrary Sym.StPerm where
    arbitrary = uncurry Sym.unrankStPerm `liftM` lenRank
    shrink w = nub $ [0 .. Sym.size w - 1] >>= \k -> subperms k w

perm :: Gen [Int]
perm = liftM (\w -> w `Sym.act` [1..Sym.size w]) arbitrary

perm2 :: Gen (Sym.StPerm, [Int])
perm2 = do (n,r1,r2) <- lenRank2
           let u = Sym.unrankStPerm n r1
           let v = Sym.unrankStPerm n r2
           return (u, v `Sym.act` [1..n])

perm3 :: Gen (Sym.StPerm, Sym.StPerm, [Int])
perm3 = do (n,r1,r2,r3) <- lenRank3
           let u = Sym.unrankStPerm n r1
           let v = Sym.unrankStPerm n r2
           let w = Sym.unrankStPerm n r3
           return (u, v, w `Sym.act` [1..n])

newtype Symmetry = Symmetry (Sym.StPerm -> Sym.StPerm, String)

d8Symmetries :: [Symmetry]
d8Symmetries = [ Symmetry (D8.r0, "r0")
               , Symmetry (D8.r1, "r1")
               , Symmetry (D8.r2, "r2")
               , Symmetry (D8.r3, "r3")
               , Symmetry (D8.s0, "s0")
               , Symmetry (D8.s1, "s1")
               , Symmetry (D8.s2, "s2")
               , Symmetry (D8.s3, "s3")
               ]

instance Show Symmetry where
    show (Symmetry (_,s)) = s

instance Arbitrary Symmetry where
    arbitrary = liftM (d8Symmetries !!) $ choose (0, length d8Symmetries - 1)


---------------------------------------------------------------------------------
-- Properties for Math.Sym
---------------------------------------------------------------------------------

prop_monoid_mempty1 w = mempty <> w == (w :: Sym.StPerm)
prop_monoid_mempty2 w = w <> mempty == (w :: Sym.StPerm)
prop_monoid_associative u v w = u <> (v <> w) == (u <> v) <> (w :: Sym.StPerm)

newtype S = S {unS :: Sym.StPerm} deriving (Eq, Show)

instance Arbitrary S where
    arbitrary = liftM S arbitrary

prop_monoid_mempty1_S w = mempty <> w == (w :: S)
prop_monoid_mempty2_S w = w <> mempty == (w :: S)
prop_monoid_associative_S u v w = u <> (v <> w) == (u <> v) <> (w :: S)

instance Monoid S where
    mempty = S $ Sym.fromVector SV.empty
    mappend u v = S $ (Sym./-/) (unS u) (unS v)

prop_unrankStPerm_distinct =
    forAll lenRank $ \(n, r) ->
        let w = Sym.toList (Sym.unrankStPerm n r) in nub w == w

prop_unrankStPerm_injective =
    forAll lenRank2 $ \(n, r1, r2) ->
        (Sym.unrankStPerm n r1 :: Sym.StPerm) /= Sym.unrankStPerm n r2 || r1 == r2

prop_sym = and [ sort (Sym.sym n) == sort (sym' n) | n<-[0..6] ]
    where
      sym' n = map Sym.fromList $ Data.List.permutations [0..fromIntegral n - 1]

prop_perm =
    and [ sort (Sym.perms n) == sort (permutations [1..n]) | n<-[0..6::Int] ]

prop_st =
    forAll perm2 $ \(u,v) -> Sym.st (u `Sym.act` v) == u `Sym.act` Sym.st v

prop_act_def =
    forAll perm2 $ \(u,v) -> u `Sym.act` v == map (v!!) (Sym.toList u)

prop_act_id =
    forAll perm2 $ \(u,v) -> Sym.neutralize u `Sym.act` v == v

prop_act_associative =
    forAll perm3 $ \(u,v,w) -> (u `Sym.act` v) `Sym.act` w == u `Sym.act` (v `Sym.act` w)

prop_size =
    forAll perm $ \v -> Sym.size v == Sym.size (Sym.st v)

prop_neutralize =
    forAll perm2 $ \(u,v) -> Sym.neutralize u == Sym.inverse (Sym.st u) `Sym.act` u

prop_inverse =
    forAll perm $ \v -> Sym.inverse v == Sym.inverse (Sym.st v) `Sym.act` Sym.neutralize v

prop_ordiso1 =
    forAll perm2 $ \(u,v) -> u `Sym.ordiso` v == (u == Sym.st v)

prop_ordiso2 =
    forAll perm2 $ \(u,v) -> u `Sym.ordiso` v == (Sym.inverse u `Sym.act` v == Sym.neutralize v)

shadow :: Ord a => [a] -> [[a]]
shadow w = nubSort . map normalize $ ptDeletions w
    where
      w' = sort w
      normalize u = [ w'!!i | i <- st u ]
      nubSort = map head . group . sort
      ptDeletions [] = []
      ptDeletions xs@(x:xt) = xt : map (x:) (ptDeletions xt)

prop_shadow = forAll (resize 30 perm) $ \w -> Sym.shadow w == shadow w

coshadow :: (Enum a, Ord a) => [a] -> [[a]]
coshadow w = sort $ ptExtensions (succ $ maximum (toEnum 0 : w)) w
    where
      ptExtensions n [] = [[n]]
      ptExtensions n xs@(x:xt) = (n:xs) : map (x:) (ptExtensions n xt)

prop_coshadow = forAll (resize 50 perm) $ \w -> Sym.coshadow w == coshadow w

prop_record f g =
    forAll perm $ \w -> SV.fromList (recordIndices w) == f w
        where
          recordIndices w = [ head $ elemIndices x w | x <- g w ]

prop_lMaxima = prop_record Sym.lMaxima lMaxima

prop_lMinima = prop_record Sym.lMinima lMinima

prop_rMaxima = prop_record Sym.rMaxima rMaxima

prop_rMinima = prop_record Sym.rMinima rMinima

prop_lMaxima_card =
    forAll perm $ \w -> S.lmax w == SV.length (Sym.lMaxima w)

prop_lMinima_card =
    forAll perm $ \w -> S.lmin w == SV.length (Sym.lMinima w)

prop_rMaxima_card =
    forAll perm $ \w -> S.rmax w == SV.length (Sym.rMaxima w)

prop_rMinima_card =
    forAll perm $ \w -> S.rmin w == SV.length (Sym.rMinima w)

-- The list of indices of components in a permutation
components w = lMaxima w `cap` rMinima (bubble w)

-- The list of indices of skew components in a permutation
skewComponents w = components $ map (\x -> length w - x - 1) w

prop_components =
    forAll perm $ \w -> components (st w) == SV.toList (Sym.components w)

prop_skewComponents =
    forAll perm $ \w -> skewComponents (st w) == SV.toList (Sym.skewComponents w)

segments :: [a] -> [[a]]
segments [] = [[]]
segments (x:xs) = segments xs ++ map (x:) (inits xs)

nonEmptySegments :: [a] -> [[a]]
nonEmptySegments = drop 1 . segments

properSegments :: [a] -> [[a]]
properSegments xs = [ ys | ys@(_:_:_) <- init $ segments xs ]

properIntervals :: Ord a => [a] -> [[a]]
properIntervals xs = [ ys | ys <- yss, sort ys `elem` zss ]
    where
      yss = properSegments xs
      zss = properSegments $ sort xs

simple :: Ord a => [a] -> Bool
simple = null . properIntervals

prop_simple = forAll (resize 40 perm) $ \w -> Sym.simple w == simple w

prop_unrankPerm =
    forAll perm $ \w ->
        let n = length w
        in forAll (choose (0, product [1..fromIntegral n - 1])) $ \r ->
            Sym.st (Sym.unrankPerm n r :: [Int]) == Sym.unrankStPerm n r

prop_stackSort = forAll perm $ \v -> Sym.stackSort v == stack v

prop_stackSort_231 =
    forAll perm $ \v ->
        (Sym.stackSort v == Sym.neutralize v) == (v `Sym.avoids` [Sym.st "231"])

prop_bubbleSort = forAll perm $ \v -> Sym.bubbleSort v == bubble v

prop_bubbleSort_231_321 =
    forAll perm $ \v ->
        (Sym.bubbleSort v == Sym.neutralize v) == (v `Sym.avoids` [Sym.st "231", Sym.st "321"])

prop_subperm_copies p =
    forAll (resize 21 perm) $ \w -> and [ subperm m (Sym.st w) == p | m <- Sym.copiesOf p w ]

prop_copies =
    forAll (resize  6 arbitrary) $ \p ->
    forAll (resize 12 perm)      $ \w ->
        sort (Sym.copiesOf p w) == sort (map I.fromList $ copies (Sym.toList p) w)

prop_copies_self =
    forAll perm $ \v -> Sym.copiesOf (Sym.st v) v == [SV.fromList [0 .. length v - 1]]

prop_copies_d8 (Symmetry (f,_)) =
    forAll (resize  6 arbitrary) $ \p ->
    forAll (resize 20 perm)      $ \w ->
        let p' = f p
            w' = Sym.generalize f w
        in length (Sym.copiesOf p w) == length (Sym.copiesOf p' w')

prop_avoiders_avoid =
    forAll (resize 20 arbitrary) $ \ws ->
    forAll (resize  6 arbitrary) $ \ps ->
        all (`Sym.avoids` ps) $ Sym.avoiders ps (ws :: [Sym.StPerm])

prop_avoiders_idempotent =
    forAll (resize 18 arbitrary) $ \vs ->
    forAll (resize  5 arbitrary) $ \ps ->
        let ws = Sym.avoiders ps (vs :: [Sym.StPerm]) in ws == Sym.avoiders ps ws

prop_avoiders_d8 (Symmetry (f,_)) =
    forAll (choose (0, 5))      $ \n ->
    forAll (resize 5 arbitrary) $ \p ->
        let ws = Sym.sym n in sort (map f $ Sym.avoiders [p] ws) == sort (Sym.avoiders [f p] ws)

prop_avoiders_d8' (Symmetry (f,_)) =
    forAll (choose (0, 5))      $ \n ->
    forAll (resize 5 arbitrary) $ \ps ->
        let ws = Sym.sym n in sort (map f $ Sym.avoiders ps ws) == sort (Sym.avoiders (map f ps) (map f ws))

prop_avoiders_d8'' (Symmetry (f,_)) =
    forAll (resize 18 arbitrary) $ \ws ->
    forAll (resize  5 arbitrary) $ \ps ->
        sort (map f $ Sym.avoiders ps ws) == sort (Sym.avoiders (map f ps) (map f ws :: [Sym.StPerm]))

prop_av_cardinality =
    forAll (resize 3 arbitrary) $ \p ->
        let spec = [ length $ Sym.av [p] n | n<-[0..6] ]
        in case Sym.size p of
             0 -> spec == [0,0,0,0,0,0,0]
             1 -> spec == [1,0,0,0,0,0,0]
             2 -> spec == [1,1,1,1,1,1,1]
             3 -> spec == [1,1,2,5,14,42,132]
             _ -> True

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

prop_subsets1 =
    forAll (choose (0,14)) $ \n ->
    forAll (choose (0,14)) $ \k ->
        sort (kSubsequences k [0..n-1]) == sort (map SV.toList $ Sym.subsets n k)

prop_subsets2 =
    forAll (choose (0,35)) $ \n ->
    forAll (choose (0,3))  $ \k ->
        sort (kSubsequences k [0..n-1]) == sort (map SV.toList $ Sym.subsets n k)

prop_subsets_singleton =
    forAll (choose (0,500)) $ \n ->
        let [v] = Sym.subsets n n in SV.toList v == [0..n-1]

prop_subsets_cardinality1 =
    forAll (choose (0,20)) $ \n ->
    forAll (choose (0,20)) $ \k ->
        length (Sym.subsets n k) == binomial n k

prop_subsets_cardinality2 =
    forAll (choose (0,20)) $ \n ->
    forAll (choose (0,20)) $ \k ->
        let cs = map SV.length (Sym.subsets n k) in ((k > n) && null cs) || ([k] == nub cs)

testsPerm =
    [ ("monoid/mempty/1",                check prop_monoid_mempty1)
    , ("monoid/mempty/2",                check prop_monoid_mempty2)
    , ("monoid/mempty/associative",      check prop_monoid_associative)
    , ("monoid/mempty/1/skew",           check prop_monoid_mempty1_S)
    , ("monoid/mempty/2/skew",           check prop_monoid_mempty2_S)
    , ("monoid/mempty/associative/skew", check prop_monoid_associative_S)
    , ("unrankStPerm/distinct",          check prop_unrankStPerm_distinct)
    , ("unrankStPerm/injective",         check prop_unrankStPerm_injective)
    , ("sym",                            check prop_sym)
    , ("perm",                           check prop_perm)
    , ("st",                             check prop_st)
    , ("act/def",                        check prop_act_def)
    , ("act/id",                         check prop_act_id)
    , ("act/associative",                check prop_act_associative)
    , ("size",                           check prop_size)
    , ("neutralize",                     check prop_neutralize)
    , ("inverse",                        check prop_inverse)
    , ("ordiso/1",                       check prop_ordiso1)
    , ("ordiso/2",                       check prop_ordiso2)
    , ("shadow",                         check prop_shadow)
    , ("coshadow",                       check prop_coshadow)
    , ("simple",                         check prop_simple)
    , ("lMaxima",                        check prop_lMaxima)
    , ("lMinima",                        check prop_lMinima)
    , ("rMaxima",                        check prop_rMaxima)
    , ("rMinima",                        check prop_rMinima)
    , ("lMaxima/card",                   check prop_lMaxima_card)
    , ("lMinima/card",                   check prop_lMinima_card)
    , ("rMaxima/card",                   check prop_rMaxima_card)
    , ("rMinima/card",                   check prop_rMinima_card)
    , ("components",                     check prop_components)
    , ("skewComponents",                 check prop_skewComponents)
    , ("unrankPerm",                     check prop_unrankPerm)
    , ("stackSort",                      check prop_stackSort)
    , ("stackSort/231",                  check prop_stackSort_231)
    , ("bubbleSort",                     check prop_bubbleSort)
    , ("bubbleSort/231&321",             check prop_bubbleSort_231_321)
    , ("subperm/copies",                 check prop_subperm_copies)
    , ("copies",                         check prop_copies)
    , ("copies/self",                    check prop_copies_self)
    , ("copies/D8",                      check prop_copies_d8)
    , ("avoiders/avoid",                 check prop_avoiders_avoid)
    , ("avoiders/idempotent",            check prop_avoiders_idempotent)
    , ("avoiders/D8/0",                  check prop_avoiders_d8)
    , ("avoiders/D8/1",                  check prop_avoiders_d8')
    , ("avoiders/D8/2",                  check prop_avoiders_d8'')
    , ("av/cardinality",                 check prop_av_cardinality)
    , ("subsets/1",                      check prop_subsets1)
    , ("subsets/2",                      check prop_subsets2)
    , ("subsets/singleton",              check prop_subsets_singleton)
    , ("subsets/cardinality/1",          check prop_subsets_cardinality1)
    , ("subsets/cardinality/2",          check prop_subsets_cardinality2)
    ]

---------------------------------------------------------------------------------
-- Properties for Math.Sym.D8
---------------------------------------------------------------------------------

prop_D8_orbit fs w = all (`elem` orbD8) $ D8.orbit (map fn fs) w
    where
      orbD8 = D8.orbit D8.d8 w
      fn (Symmetry (f,_)) = f

prop_D8_reverse w    = I.reverse    (Sym.toVector w) == Sym.toVector (D8.reverse w)
prop_D8_complement w = I.complement (Sym.toVector w) == Sym.toVector (D8.complement w)
prop_D8_inverse w    = I.inverse    (Sym.toVector w) == Sym.toVector (D8.inverse w)
prop_D8_rotate w     = I.rotate     (Sym.toVector w) == Sym.toVector (D8.rotate w)

testsD8 =
    [ ("D8/orbit",       check prop_D8_orbit)
    , ("D8/reverse",     check prop_D8_reverse)
    , ("D8/complement",  check prop_D8_complement)
    , ("D8/inverse",     check prop_D8_inverse)
    , ("D8/rotate",      check prop_D8_rotate)
    ]

---------------------------------------------------------------------------------
-- Properties for Math.Sym.Stat
---------------------------------------------------------------------------------

-- the group theoretical inverse of w
inverse :: (Ord a) => [a] -> [Int]
inverse w = map snd . sort $ zip w [0..]

-- the standardization of w
st :: (Ord a) => [a] -> [Int]
st = inverse . inverse

ascents, descents :: (Ord a) => [a] -> [(a, a)]
ascents  w = filter (uncurry (<)) $ zip w (tail w)
descents w = filter (uncurry (>)) $ zip w (tail w)

peaks          w = [ v | v@(x,y,z) <- zip3 w (tail w) (tail (tail w)), x < y, y > z ]
valleys        w = [ v | v@(x,y,z) <- zip3 w (tail w) (tail (tail w)), x > y, y < z ]
doubleAscents  w = [ v | v@(x,y,z) <- zip3 w (tail w) (tail (tail w)), x < y, y < z ]
doubleDescents w = [ v | v@(x,y,z) <- zip3 w (tail w) (tail (tail w)), x > y, y > z ]

inversions :: (Ord a) => [a] -> [(a, a)]
inversions w = init (tails w) >>= \(x:xs) -> [ (x,y) | y<-xs, x > y ]

records :: (a -> a -> Bool) -> [a] -> [a]
records f []     = []
records f (x:xs) = records' f [x] xs where
    records' f recs       []     = recs
    records' f recs@(r:_) (x:xs) = records' f (if f r x then x:recs else recs) xs

lMinima, lMaxima, rMinima, rMaxima :: (Ord a) => [a] -> [a]

lMinima = reverse . records (>)
lMaxima = reverse . records (<)
rMinima = records (>) . reverse
rMaxima = records (<) . reverse

excedances  xs = map fst . filter (\(i,a)->i <  fromIntegral a) $ zip [0..] xs
fixedpoints xs = map fst . filter (\(i,a)->i == fromIntegral a) $ zip [0..] xs

orbit :: Eq a => (a -> a) -> a -> [a]
orbit f x = y:takeWhile (/=y) ys where (y:ys) = iterate f x

orbits :: Eq a => (a -> a) -> [a] -> [[a]]
orbits f [] = []
orbits f (x:xs) = ys:orbits f (xs\\ys) where ys = orbit f x

exc, fp :: [Int] -> Int
exc = length . excedances . st
fp  = length . fixedpoints . st

cyc :: [Int] -> Int
cyc w = let v = st w in length $ orbits (v!!) v

runs :: Ord a => (a -> a -> Bool) -> [a] -> [a] -> [[a]]
runs _ [] [] = []
runs _ rs [] = [rs]
runs f [] (x:xs) = runs f [x] xs
runs f u@(r:_) v@(x:xs) | f r x = runs f (x:u) xs
                        | otherwise = u : runs f [x] xs

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

-- The stack-sort operator
stack [] = []
stack xs = stack left ++ stack right ++ [n]
    where
      (left, n:right) = span ( < maximum xs) xs

-- The bubble-sort operator; i.e. one pass of the classical bubble
-- sort algorithm
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
cap [] ys = []
cap xs [] = []
cap xs@(x:xt) ys@(y:yt) = case compare x y of
                            EQ -> x : cap xt yt
                            LT -> cap xt ys
                            GT -> cap xs yt

-- The number of components in a permutation
comp = length . components

-- The number of skew components in a permutation
scomp = length . skewComponents

-- rank a la Elizalde
ep = fst . last . filter (\(k,ys) -> all (k<=) ys) . zip [0..] . inits . st

des, asc, inv, lmin, lmax, rmin, rmax, peak, vall :: [Int] -> Int
dasc, ddes, maj, comp, ep, dim :: [Int] -> Int

dim   w = maximum $ 0 : [ i | (i,x) <- zip [0..] (st w), i /= x ]
maj   w = sum [ i | (i,x,y) <- zip3 [1..] w (tail w), x > y ]
comaj w = sum [ n-i | (i,x,y) <- zip3 [1..] w (tail w), x > y ] where n = length w
asc0  w = sum [ 1 | (x,y) <- ascents  $ st w, y-x == 1 ]
des0  w = sum [ 1 | (x,y) <- descents $ st w, x-y == 1 ]

asc  = length . ascents
des  = length . descents
inv  = length . inversions
lmin = length . lMinima
lmax = length . lMaxima
rmin = length . rMinima
rmax = length . rMaxima
peak = length . peaks
vall = length . valleys
dasc = length . doubleAscents
ddes = length . doubleDescents

prop_asc   = forAll perm $ \w -> asc   w == S.asc   w
prop_des   = forAll perm $ \w -> des   w == S.des   w
prop_exc   = forAll perm $ \w -> exc   w == S.exc   w
prop_fp    = forAll perm $ \w -> fp    w == S.fp    w
prop_cyc   = forAll perm $ \w -> cyc   w == S.cyc   w
prop_inv   = forAll perm $ \w -> inv   w == S.inv   w
prop_maj   = forAll perm $ \w -> maj   w == S.maj   w
prop_comaj = forAll perm $ \w -> comaj w == S.comaj w
prop_lmin  = forAll perm $ \w -> lmin  w == S.lmin  w
prop_lmax  = forAll perm $ \w -> lmax  w == S.lmax  w
prop_rmin  = forAll perm $ \w -> rmin  w == S.rmin  w
prop_rmax  = forAll perm $ \w -> rmax  w == S.rmax  w
prop_head  = forAll perm $ \w -> not (null w) ==> head w == 1 + S.head w
prop_last  = forAll perm $ \w -> not (null w) ==> last w == 1 + S.last w
prop_peak  = forAll perm $ \w -> peak  w == S.peak  w
prop_vall  = forAll perm $ \w -> vall  w == S.vall  w
prop_dasc  = forAll perm $ \w -> dasc  w == S.dasc  w
prop_ddes  = forAll perm $ \w -> ddes  w == S.ddes  w
prop_ep    = forAll perm $ \w -> ep    w == S.ep    w
prop_lir   = forAll perm $ \w -> lir   w == S.lir   w
prop_ldr   = forAll perm $ \w -> ldr   w == S.ldr   w
prop_rir   = forAll perm $ \w -> rir   w == S.rir   w
prop_rdr   = forAll perm $ \w -> rdr   w == S.rdr   w
prop_comp  = forAll perm $ \w -> comp  w == S.comp  w
prop_scomp = forAll perm $ \w -> scomp w == S.scomp w
prop_dim   = forAll perm $ \w -> dim   w == S.dim   w
prop_asc0  = forAll perm $ \w -> asc0  w == S.asc0  w
prop_des0  = forAll perm $ \w -> des0  w == S.des0  w

prop_inv_21 = forAll perm $ \w -> S.inv w == length (Sym.copiesOf (Sym.st "21") w)

testsStat =
    [ ("asc",          check prop_asc)
    , ("des",          check prop_des)
    , ("exc",          check prop_exc)
    , ("fp",           check prop_fp)
    , ("cyc",          check prop_cyc)
    , ("inv",          check prop_inv)
    , ("maj",          check prop_maj)
    , ("comaj",        check prop_comaj)
    , ("lmin",         check prop_lmin)
    , ("lmax",         check prop_lmax)
    , ("rmin",         check prop_rmin)
    , ("rmax",         check prop_rmax)
    , ("head",         check prop_head)
    , ("last",         check prop_last)
    , ("peak",         check prop_peak)
    , ("vall",         check prop_vall)
    , ("dasc",         check prop_dasc)
    , ("ddes",         check prop_ddes)
    , ("ep",           check prop_ep)
    , ("lir",          check prop_lir)
    , ("ldr",          check prop_ldr)
    , ("rir",          check prop_rir)
    , ("rdr",          check prop_rdr)
    , ("comp",         check prop_comp)
    , ("scomp",        check prop_scomp)
    , ("dim",          check prop_dim)
    , ("asc0",         check prop_asc0)
    , ("des0",         check prop_des0)
    , ("inv/21",       check prop_inv_21)
    ]

---------------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------------

tests = testsPerm ++ testsD8 ++ testsStat

main = mapM_ (\(name, t) -> putStr (name ++ ":\t") >> t) tests
