-- |
-- Copyright   : (c) Anders Claesson 2012
-- License     : BSD-style
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>

import Data.Ord
import Data.List
import Data.Monoid
import Data.Function
import Control.Monad
import qualified Math.Sym as Sym
import qualified Math.Sym.D8 as D8
import qualified Math.Sym.Stat as S
import qualified Math.Sym.Class as C
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
lenRank2 = do
  (n, r1) <- lenRank
  r2 <- rank n
  return (n, r1, r2)

lenRank3 :: Gen (Int, Integer, Integer, Integer)
lenRank3 = do
  (n, r1, r2) <- lenRank2
  r3 <- rank n
  return (n, r1, r2, r3)

-- The sub-permutation determined by a set of indices.
subperm :: Sym.Set -> Sym.StPerm -> Sym.StPerm
subperm m w = Sym.fromVector . I.st $ SV.map ((SV.!) (Sym.toVector w)) m

subperms :: Int -> Sym.StPerm -> [Sym.StPerm]
subperms k w = [ subperm m w | m <- Sym.subsets (Sym.size w) k ]

instance Arbitrary Sym.StPerm where
    arbitrary = uncurry Sym.unrankPerm `liftM` lenRank
    shrink w = nub $ [0 .. Sym.size w - 1] >>= \k -> subperms k w

perm :: Gen [Int]
perm = liftM (\w -> w `Sym.act` [1..Sym.size w]) arbitrary

perm2 :: Gen (Sym.StPerm, [Int])
perm2 = do
  (n,r1,r2) <- lenRank2
  let u = Sym.unrankPerm n r1
  let v = Sym.unrankPerm n r2
  return (u, v `Sym.act` [1..n])

perm3 :: Gen (Sym.StPerm, Sym.StPerm, [Int])
perm3 = do
  (n,r1,r2,r3) <- lenRank3
  let u = Sym.unrankPerm n r1
  let v = Sym.unrankPerm n r2
  let w = Sym.unrankPerm n r3
  return (u, v, w `Sym.act` [1..n])

stPermsOfEqualLength :: Gen [Sym.StPerm]
stPermsOfEqualLength = sized $ \m -> do
  n  <- choose (0,m)
  k  <- choose (0,m^2)
  rs <- replicateM k $ rank n
  return $ nub $ map (Sym.unrankPerm n) rs

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

neutralize :: Sym.Perm a => a -> a
neutralize = Sym.idperm . Sym.size

forAllPermEq f g = forAll perm $ \w -> f w == g w

prop_unrankPerm_distinct =
    forAll lenRank $ \(n, r) ->
        let w = Sym.toList (Sym.unrankPerm n r) in nub w == w

prop_unrankPerm_injective =
    forAll lenRank2 $ \(n, r1, r2) ->
        (Sym.unrankPerm n r1 :: Sym.StPerm) /= Sym.unrankPerm n r2 || r1 == r2

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
    forAll perm2 $ \(u,v) -> neutralize u `Sym.act` v == v

prop_act_associative =
    forAll perm3 $ \(u,v,w) -> (u `Sym.act` v) `Sym.act` w == u `Sym.act` (v `Sym.act` w)

prop_size = Sym.size `forAllPermEq` (Sym.size . Sym.st)

prop_neutralize = neutralize `forAllPermEq` (\u -> Sym.inverse (Sym.st u) `Sym.act` u)

prop_inverse =
    forAllPermEq Sym.inverse $ \v -> Sym.inverse (Sym.st v) `Sym.act` neutralize v

prop_ordiso1 =
    forAll perm2 $ \(u,v) -> u `Sym.ordiso` v == (u == Sym.st v)

prop_ordiso2 =
    forAll perm2 $ \(u,v) ->
        u `Sym.ordiso` v == (Sym.inverse u `Sym.act` v == neutralize v)

shadow :: Ord a => [a] -> [[a]]
shadow w = nubsort . map normalize $ ptDeletions w
    where
      w' = sort w
      normalize u = [ w'!!i | i <- st u ]
      nubsort = map head . group . sort
      ptDeletions [] = []
      ptDeletions xs@(x:xt) = xt : map (x:) (ptDeletions xt)

prop_shadow = forAll (resize 30 perm) $ \w -> Sym.shadow [w] == shadow w

prop_downset_shadow =
    forAll (resize 10 perm) $ \w ->
        [ v | v <- Sym.downset [w], 1 + length v == length w ] == Sym.shadow [w]

prop_downset_orderideal =
    forAll (resize 9 perm) $ \w -> null [ v | v <- Sym.downset [w]
                                        , w `Sym.avoids` [Sym.st v]
                                        ]

coshadow :: (Enum a, Ord a) => [a] -> [[a]]
coshadow w = sort $ ptExtensions (succ $ maximum (toEnum 0 : w)) w
    where
      ptExtensions n [] = [[n]]
      ptExtensions n xs@(x:xt) = (n:xs) : map (x:) (ptExtensions n xt)

prop_coshadow = forAll (resize 50 perm) $ \w -> Sym.coshadow [w] == coshadow w

recordIndicesAgree f g =
    forAll perm $ \w -> SV.fromList (recordIndices w) == f w
        where
          recordIndices w = [ head $ elemIndices x w | x <- g w ]

prop_lMaxima = recordIndicesAgree Sym.lMaxima lMaxima
prop_lMinima = recordIndicesAgree Sym.lMinima lMinima
prop_rMaxima = recordIndicesAgree Sym.rMaxima rMaxima
prop_rMinima = recordIndicesAgree Sym.rMinima rMinima

prop_lMaxima_card = S.lmax `forAllPermEq` (SV.length . Sym.lMaxima)
prop_lMinima_card = S.lmin `forAllPermEq` (SV.length . Sym.lMinima)
prop_rMaxima_card = S.rmax `forAllPermEq` (SV.length . Sym.rMaxima)
prop_rMinima_card = S.rmin `forAllPermEq` (SV.length . Sym.rMinima)

-- The list of indices of components in a permutation
components w = lMaxima w `cap` rMinima (bubble w)

-- The list of indices of skew components in a permutation
skewComponents w = components $ map (\x -> length w - x - 1) w

prop_components = (components . st) `forAllPermEq` (SV.toList . Sym.components)

prop_skewComponents = (skewComponents . st) `forAllPermEq` (SV.toList . Sym.skewComponents)

prop_dsum = forAll perm $ \u ->
            forAll perm $ \v -> (Sym.\+\) u v == Sym.inflate "12" [u,v]

prop_ssum = forAll perm $ \u ->
            forAll perm $ \v -> (Sym./-/) u v == Sym.inflate "21" [u,v]

inflate :: [Int] -> [[Int]] -> [Int]
inflate w vs = concat . map snd $ sort [ (i, map (+c) u) | (i, c, u) <- zip3 w' cs us ]
    where
      (_, w',us) = unzip3 . sort $ zip3 w [0..] vs
      cs = scanl (\i u -> i + length u) 0 us

prop_inflate =
    forAll perm $ \u0 ->
    forAll perm $ \u1 ->
    forAll perm $ \u2 ->
    forAll perm $ \u3 ->
        let us = [u0, u1, u2, u3]
        in and [ inflate w us == Sym.inflate w us | w <- permutations [1..4] ]

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

prop_stackSort = Sym.stackSort `forAllPermEq` stack

prop_stackSort_231 =
  (\v -> Sym.stackSort v == neutralize v) `forAllPermEq` (`Sym.avoids` [Sym.st "231"])

prop_bubbleSort = Sym.bubbleSort `forAllPermEq` bubble

prop_bubbleSort_231_321 = forAllPermEq f g
    where f v = Sym.bubbleSort v == neutralize v
          g v = v `Sym.avoids` [Sym.st "231", Sym.st "321"]

prop_subperm_copies p =
    forAll (resize 21 perm) $ \w ->
        and [ subperm m (Sym.st w) == p | m <- Sym.copiesOf p w ]

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
            w' = Sym.generalize f w :: [Int]
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
        let ws = Sym.sym n
        in sort (map f $ Sym.avoiders [p] ws) == sort (Sym.avoiders [f p] ws)

prop_avoiders_d8' (Symmetry (f,_)) =
    forAll (choose (0, 5))      $ \n ->
    forAll (resize 5 arbitrary) $ \ps ->
        let ws = Sym.sym n
        in sort (map f $ Sym.avoiders ps ws) == sort (Sym.avoiders (map f ps) (map f ws))

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
    forAll (choose (0,13)) $ \n ->
    forAll (choose (0,13)) $ \k ->
        sort (kSubsequences k [0..n-1]) == sort (map SV.toList $ Sym.subsets n k)

prop_subsets2 =
    forAll (choose (0,33)) $ \n ->
    forAll (choose (0,3))  $ \k ->
        sort (kSubsequences k [0..n-1]) == sort (map SV.toList $ Sym.subsets n k)

prop_subsets_singleton =
    forAll (choose (0,500)) $ \n ->
        let [v] = Sym.subsets n n in SV.toList v == [0..n-1]

prop_subsets_cardinality1 =
    forAll (choose (0,16)) $ \n ->
    forAll (choose (0,16)) $ \k ->
        length (Sym.subsets n k) == binomial n k

prop_subsets_cardinality2 =
    forAll (choose (0,16)) $ \n ->
    forAll (choose (0,16)) $ \k ->
        let cs = map SV.length (Sym.subsets n k)
        in ((k > n) && null cs) || ([k] == nub cs)

testsPerm =
    [ ("monoid/mempty/1",                check prop_monoid_mempty1)
    , ("monoid/mempty/2",                check prop_monoid_mempty2)
    , ("monoid/mempty/associative",      check prop_monoid_associative)
    , ("monoid/mempty/1/skew",           check prop_monoid_mempty1_S)
    , ("monoid/mempty/2/skew",           check prop_monoid_mempty2_S)
    , ("monoid/mempty/associative/skew", check prop_monoid_associative_S)
    , ("unrankPerm/distinct",            check prop_unrankPerm_distinct)
    , ("unrankPerm/injective",           check prop_unrankPerm_injective)
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
    , ("downset/shadow",                 check prop_downset_shadow)
    , ("downset/orderideal",             check prop_downset_orderideal)
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
    , ("dsum",                           check prop_dsum)
    , ("ssum",                           check prop_ssum)
    , ("inflate",                        check prop_inflate)
    , ("skewComponents",                 check prop_skewComponents)
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

fn (Symmetry (f,_)) = f

prop_D8_orbit fs w = all (`elem` orbD8) $ D8.orbit (map fn fs) w
    where
      orbD8 = D8.orbit D8.d8 (w :: Sym.StPerm)

symmetriesAgrees f g = (f . Sym.toVector) `forAllPermEq` (Sym.toVector . g)

prop_D8_reverse    = symmetriesAgrees I.reverse    D8.reverse
prop_D8_complement = symmetriesAgrees I.complement D8.complement
prop_D8_inverse    = symmetriesAgrees I.inverse    D8.inverse
prop_D8_rotate     = symmetriesAgrees I.rotate     D8.rotate

-- Auxilary function that partitions a list xs with respect to the
-- equivalence induced by a function f; i.e. x ~ y iff f x == f y.
-- The time complexity is the same as for sorting, O(n log n).
eqClasses :: Ord a => (b -> a) -> [b] -> [[b]]
eqClasses f xs = (map . map) snd . group' $ sort' [ (f x, x) | x <- xs ]
    where
      group' = groupBy ((==) `on` fst)
      sort' = sortBy $ comparing fst

symmetryClasses :: (Ord a, Sym.Perm a) => [a -> a] -> [a] -> [[a]]
symmetryClasses fs xs = sort . map sort $ eqClasses (D8.orbit fs) xs

symmetryClassesByGroup fs =
    forAll (resize 10 stPermsOfEqualLength) $ \ws ->
        symmetryClasses fs ws == D8.symmetryClasses fs ws

prop_symmetryClasses_d8     = symmetryClassesByGroup D8.d8
prop_symmetryClasses_klein4 = symmetryClassesByGroup D8.klein4
prop_symmetryClasses_ei     = symmetryClassesByGroup [D8.id, D8.inverse]
prop_symmetryClasses_er     = symmetryClassesByGroup [D8.id, D8.reverse]
prop_symmetryClasses_ec     = symmetryClassesByGroup [D8.id, D8.complement]

testsD8 =
    [ ("D8/orbit",                   check prop_D8_orbit)
    , ("D8/reverse",                 check prop_D8_reverse)
    , ("D8/complement",              check prop_D8_complement)
    , ("D8/inverse",                 check prop_D8_inverse)
    , ("D8/rotate",                  check prop_D8_rotate)
    , ("D8/symmetryClasses/ei",      check prop_symmetryClasses_ei)
    , ("D8/symmetryClasses/er",      check prop_symmetryClasses_er)
    , ("D8/symmetryClasses/ec",      check prop_symmetryClasses_ec)
    , ("D8/symmetryClasses/d8",      check prop_symmetryClasses_d8)
    , ("D8/symmetryClasses/klein4",  check prop_symmetryClasses_klein4)
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
shad = length . shadow

prop_asc    = forAllPermEq asc   S.asc
prop_des    = forAllPermEq des   S.des
prop_exc    = forAllPermEq exc   S.exc
prop_fp     = forAllPermEq fp    S.fp
prop_cyc    = forAllPermEq cyc   S.cyc
prop_inv    = forAllPermEq inv   S.inv
prop_maj    = forAllPermEq maj   S.maj
prop_comaj  = forAllPermEq comaj S.comaj
prop_lmin   = forAllPermEq lmin  S.lmin
prop_lmax   = forAllPermEq lmax  S.lmax
prop_rmin   = forAllPermEq rmin  S.rmin
prop_rmax   = forAllPermEq rmax  S.rmax
prop_head   = forAll perm $ \w -> not (null w) ==> head w == 1 + S.head w
prop_last   = forAll perm $ \w -> not (null w) ==> last w == 1 + S.last w
prop_peak   = forAllPermEq peak  S.peak
prop_vall   = forAllPermEq vall  S.vall
prop_dasc   = forAllPermEq dasc  S.dasc
prop_ddes   = forAllPermEq ddes  S.ddes
prop_ep     = forAllPermEq ep    S.ep
prop_lir    = forAllPermEq lir   S.lir
prop_ldr    = forAllPermEq ldr   S.ldr
prop_rir    = forAllPermEq rir   S.rir
prop_rdr    = forAllPermEq rdr   S.rdr
prop_comp   = forAllPermEq comp  S.comp
prop_scomp  = forAllPermEq scomp S.scomp
prop_dim    = forAllPermEq dim   S.dim
prop_asc0   = forAllPermEq asc0  S.asc0
prop_des0   = forAllPermEq des0  S.des0
prop_shad   = forAllPermEq shad  S.shad
prop_inv_21 = forAll (resize 30 perm) $ \w ->
              S.inv w == (length . Sym.copiesOf (Sym.st "21")) w

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
    , ("shad",         check prop_shad)
    , ("inv/21",       check prop_inv_21)
    ]

---------------------------------------------------------------------------------
-- Properties for Math.Sym.Class
---------------------------------------------------------------------------------

agreesWithBasis bs cls m =
    and [ sort (Sym.av (map Sym.st bs) n) == sort (cls n) | n<-[0..m] ]

prop_av231      = agreesWithBasis ["231"]          C.av231      7
prop_vee        = agreesWithBasis ["132", "231"]   C.vee        7
prop_wedge      = agreesWithBasis ["213", "312"]   C.wedge      7
prop_gt         = agreesWithBasis ["132", "312"]   C.gt         7
prop_lt         = agreesWithBasis ["213", "231"]   C.lt         7
prop_separables = agreesWithBasis ["2413", "3142"] C.separables 7

testsClass =
    [ ("av231",        check prop_av231)
    , ("vee",          check prop_vee)
    , ("wedge",        check prop_wedge)
    , ("gt",           check prop_gt)
    , ("lt",           check prop_lt)
    , ("separables",   check prop_separables)
    ]

---------------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------------

tests = testsPerm ++ testsD8 ++ testsStat ++ testsClass

runTests = mapM_ (\(name, t) -> putStr (name ++ ":\t") >> t)

main = runTests tests
