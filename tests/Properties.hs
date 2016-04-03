{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Copyright   : Anders Claesson 2013-2016
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--

import qualified ListStat as LS
import Sym.Perm.Meta hiding (choose)
import qualified Sym.Perm.SSYT as Y
import qualified Sym.Perm.Pattern as P
import qualified Sym.Perm.Stat as S
import qualified Sym.Perm.D8 as D8
import Sym.Permgram (Permgram)
import qualified Sym.Permgram as PG
import Data.List
import Data.Hashable
import Control.Monad
import Test.QuickCheck

checkN :: Testable prop => Int -> prop -> IO Result
checkN n = quickCheckWithResult stdArgs {maxSuccess = n}

check :: Testable prop => prop -> IO ()
check p = checkN 1000 p >> return ()

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

lenRank :: Gen (Int, Integer)
lenRank = sized $ \m -> do
            n <- choose (0, m) -- length
            r <- choose (0, product [1..fromIntegral n] - 1) -- rank
            return (n, r)

instance Arbitrary Perm where
    arbitrary = uncurry unrank `liftM` lenRank

instance Arbitrary a => Arbitrary (Permgram a) where
    arbitrary = do
      (n, r) <- lenRank
      xs <- vector n
      return $ PG.permgram (unrank n r) xs

newtype GPerm = GPerm [(Int, Int)] deriving (Eq, Show)

instance Arbitrary GPerm where
    arbitrary = sized $ \m -> do
                  n  <- choose (0, m)
                  xs <- vector n
                  ys <- vector n
                  return . GPerm . sort $ zip (map abs xs) (map abs ys)

newtype PFun = PFun (Int -> Permgram Int)

instance Show PFun where
    show (PFun f) = "[" ++ images  ++ ", ..]"
        where
          images = intercalate ", " $ map (show . f) [0..2]

instance Arbitrary PFun where
    arbitrary = sized $ \m -> do
      salt <- arbitrary
      ws <- listOf1 arbitrary
      return . PFun $ \x -> cycle ws !! (hashWithSalt salt x `mod` (m+1))

--------------------------------------------------------------------------------
-- Properties for Data.Perm
--------------------------------------------------------------------------------

prop_length xs = size (fromList xs) == length (xs :: [Int])

prop_unrankRankId =
    forAll (resize 18 arbitrary) $ \w ->
        unrank (size w) (rank w) == w

prop_rankUnrankId =
    forAll (resize 18 lenRank) $ \(n,r) ->
        rank (unrank n r) == r

st :: Ord a => [a] -> [Int]
st = sti . sti where sti w = map snd . sort $ zip w [0::Int ..]

prop_mkPermToListSt xs = toList (mkPerm xs) == st (xs :: [Int])

prop_unsafeAt w = map (w `unsafeAt`) [0 .. size w - 1] == toList w

prop_perms = and [ sort (permutations [0..n-1]) == sort (map toList (perms n))
                   | n <- [0..7]
                 ]

prop_coeff_des =
    forAll (resize 7 arbitrary) $ \w ->
        let n = size w
        in n > 0 ==> fromEnum (w `at` 0 > w `at` (n-1))*(-1)^n == P.coeff S.des w

prop_coeff_lmax =
    forAll (resize 7 arbitrary) $ \w ->
        let n = size w
        in n > 0 ==> fromEnum (w `at` (n-1) == 0) * (-1)^(n-1) == P.coeff S.lmax w

-- prop_monoid_mempty1 w = mempty <> w == (w :: StPerm)
-- prop_monoid_mempty2 w = w <> mempty == (w :: StPerm)
-- prop_monoid_associative u v w = u <> (v <> w) == (u <> v) <> (w :: StPerm)

-- newtype S = S {unS :: StPerm} deriving (Eq, Show)

-- instance Arbitrary S where
--     arbitrary = liftM S arbitrary

-- instance Monoid S where
--     mempty = S $ Sym.fromVector SV.empty
--     mappend u v = S $ (Sym.\-\) (unS u) (unS v)

-- prop_monoid_mempty1_S w = mempty <> w == (w :: S)
-- prop_monoid_mempty2_S w = w <> mempty == (w :: S)
-- prop_monoid_associative_S u v w = u <> (v <> w) == (u <> v) <> (w :: S)

testsDataPerm =
    [ ("Data.Perm/length",                check prop_length)
    , ("Data.Perm/unrankRankId",          check prop_unrankRankId)
    , ("Data.Perm/rankUnrankId",          check prop_rankUnrankId)
    , ("Data.Perm/mkPermToListSt",        check prop_mkPermToListSt)
    , ("Data.Perm/unsafeAt",              check prop_unsafeAt)
    , ("Data.Perm/perms",                 check prop_perms)
    , ("Data.Perm/coeff/des",             check prop_coeff_des)
    , ("Data.Perm/coeff/lmax",            check prop_coeff_lmax)
    ]

--------------------------------------------------------------------------------
-- Properties for Data.Permgram
--------------------------------------------------------------------------------

prop_monadLeftId (PFun f) x = (return x >>= f) == f x

prop_monadRightId w = (w >>= return) == (w :: Permgram Int)

prop_monadAssociativity =
    forAll (resize 7 arbitrary) $ \(PFun f) ->
    forAll (resize 7 arbitrary) $ \(PFun g) ->
    forAll (resize 9 arbitrary) $ \w ->
        ((w >>= f) >>= g) == (w >>= (\x -> f x >>= g))

testsDataPermgram =
    [ ("Data.Permgram/Monad/LeftId",         check prop_monadLeftId)
    , ("Data.Permgram/Monad/RightId",        check prop_monadRightId)
    , ("Data.Permgram/Monad/Associativity",  check prop_monadAssociativity)
    ]

--------------------------------------------------------------------------------
-- Properties for Data.SSYT
--------------------------------------------------------------------------------

swap (x,y) = (y,x)

inverseGeneralizedPerm = sort . map swap

prop_RSK (GPerm w) = w == Y.toGeneralizedPerm (Y.fromGeneralizedPerm w)

prop_RSKInverse (GPerm w) = (p, q) == (s, r)
    where
      Y.SSYTPair p q = Y.fromGeneralizedPerm w
      Y.SSYTPair r s = Y.fromGeneralizedPerm $ inverseGeneralizedPerm w

prop_RS w = w == Y.toPerm (Y.fromPerm w)

prop_RSInverse w = (p, q) == (s, r)
    where
      Y.SSYTPair p q = Y.fromPerm w
      Y.SSYTPair r s = Y.fromPerm $ D8.inverse w

prop_RSfp w = S.fp v == length (filter odd (Y.shape (transpose q)))
    where
      Y.SSYTPair p _ = Y.fromPerm w
      v = Y.toPerm $ Y.SSYTPair p p -- Kludge to get an involution
      Y.SSYTPair _ q = Y.fromPerm v

testsDataSSYT =
    [ ("Data.SSYT/RSK/Bijection",    check prop_RSK)
    , ("Data.SSYT/RSK/Inverse",      check prop_RSKInverse)
    , ("Data.SSYT/RS/Bijection",     check prop_RS)
    , ("Data.SSYT/RS/Inverse",       check prop_RSInverse)
    , ("Data.SSYT/RS/fp",            check prop_RSfp)
    ]

--------------------------------------------------------------------------------
-- Properties for Math.Perm
--------------------------------------------------------------------------------

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

isSimple :: Ord a => [a] -> Bool
isSimple = null . properIntervals

prop_simple =
    forAll (resize 20 arbitrary) $ \w ->
        simple w == isSimple (toList w)

testsMathPerm =
    [ ("Math.Perm/simple",    check prop_simple)
    ]

--------------------------------------------------------------------------------
-- Properties for Math.Perm.Pattern
--------------------------------------------------------------------------------

prop_copiesSelf v = P.copiesOf v v == [fromList [0 .. size v - 1]]

prop_avoidersAvoid =
    forAll (resize 20 arbitrary) $ \ws ->
    forAll (resize  6 arbitrary) $ \ps ->
        all (`P.avoidsAll` ps) $ P.avoiders ps ws

prop_avoidersIdempotent =
    forAll (resize 18 arbitrary) $ \vs ->
    forAll (resize  5 arbitrary) $ \ps ->
        let ws = P.avoiders ps vs in ws == P.avoiders ps ws

prop_avoidersSpectrum =
    forAll (resize 3 arbitrary) $ \p ->
        let spec = [ length $ P.avoiders [p] (perms n) | n<-[0..6] ]
        in case size p of
             0 -> spec == [0,0,0,0,0,0,0]
             1 -> spec == [1,0,0,0,0,0,0]
             2 -> spec == [1,1,1,1,1,1,1]
             3 -> spec == [1,1,2,5,14,42,132]
             _ -> False

prop_minimaSmallest =
    forAll (resize 14 arbitrary) $ \ws ->
        let vs = P.minima ws
        in and [ not (w `P.avoidsAll` vs) | w <- ws ]

prop_minimaAntichain =
    forAll (resize 14 arbitrary) $ \ws ->
        let vs = P.minima ws in and [ v `P.avoidsAll` (vs \\ [v]) | v <- vs ]

prop_maximaAntichain =
    forAll (resize 12 arbitrary) $ \ws ->
        let vs = P.maxima ws
        in and [ v `P.avoidsAll` (vs \\ [v]) | v <- vs ]

testsMathPermPattern =
    [ ("Math.Perm.Pattern/copies/self",           check prop_copiesSelf)
    , ("Math.Perm.Pattern/avoiders/avoid",        check prop_avoidersAvoid)
    , ("Math.Perm.Pattern/avoiders/idempotent",   check prop_avoidersIdempotent)
    , ("Math.Perm.Pattern/avoiders/spectrum",     check prop_avoidersSpectrum)
    , ("Math.Perm.Pattern/minima/smallest",       check prop_minimaSmallest)
    , ("Math.Perm.Pattern/minima/antichain",      check prop_minimaAntichain)
    , ("Math.Perm.Pattern/maxima/antichain",      check prop_maximaAntichain)
    ]

--------------------------------------------------------------------------------
-- Properties for Math.Perm.D8
--------------------------------------------------------------------------------

prop_reverseDef u =
    let v = D8.reverse u
        n = size u
    in n > 0 ==> forAll (choose (0,n-1)) $ \i -> v `at` i == u `at` (n-1-i)

prop_reverseInvolutive w = D8.reverse (D8.reverse w) == w

prop_complementDef u =
    let v = D8.complement u
        n = size u
    in n > 0 ==> forAll (choose (0,n-1)) $ \i -> v `at` i == n - 1 - u `at` i

prop_complementInvolutive w = D8.complement (D8.complement w) == w

prop_inverseDef u =
    let v = D8.inverse u
        n = size u
    in n > 0 ==> forAll (choose (0,n-1)) $ \i -> v `at` (u `at` i) == i

prop_inverseInvolutive w = D8.inverse (D8.inverse w) == w

testsMathPermD8 =
    [ ("Math.Perm.D8/reverse/def",           check prop_reverseDef)
    , ("Math.Perm.D8/reverse/involutive",    check prop_reverseInvolutive)
    , ("Math.Perm.D8/complement/def",        check prop_complementDef)
    , ("Math.Perm.D8/complement/involutive", check prop_complementInvolutive)
    , ("Math.Perm.D8/inverse/def",           check prop_inverseDef)
    , ("Math.Perm.D8/inverse/involutive",    check prop_inverseInvolutive)
    ]

--------------------------------------------------------------------------------
-- Properties for Math.Perm.Group
--------------------------------------------------------------------------------

prop_act_id v =
    idperm (size v) `act` v == v

prop_act_associative u v w =
    (u `compose` v) `act` w == u `act` (v `act` w)

prop_compose_id_left v =
    idperm (size v) `compose` v == v

prop_compose_id_right v =
    v `compose` idperm (size v) == v

prop_compose_associative u v w =
    (u `compose` v) `compose` w == u `compose` (v `compose` w)

testsMathPermGroup =
    [ ("Math.Perm.Group/act/id",               check prop_act_id)
    , ("Math.Perm.Group/act/associative",      check prop_act_associative)
    , ("Math.Perm.Group/compose/id/left",      check prop_compose_id_left)
    , ("Math.Perm.Group/compose/id/right",     check prop_compose_id_right)
    , ("Math.Perm.Group/compose/associative",  check prop_compose_associative)
    ]

--------------------------------------------------------------------------------
-- Properties for Math.Perm.Bijection
--------------------------------------------------------------------------------

p123 = fromList [0,1,2]
p132 = fromList [0,2,1]

prop_simionSchmidt_avoid =
    forAll (resize 12 arbitrary) $ \w ->
        w `P.avoids` p123 ==> simionSchmidt w `P.avoids` p132

prop_simionSchmidt_avoid' =
    forAll (resize 12 arbitrary) $ \w ->
        w `P.avoids` p132 ==> simionSchmidt' w `P.avoids` p123

prop_simionSchmidt_id =
    forAll (resize 12 arbitrary) $ \w ->
        w `P.avoids` p123 ==> simionSchmidt' (simionSchmidt w) == w

prop_simionSchmidt_id' =
    forAll (resize 12 arbitrary) $ \w ->
        w `P.avoids` p132 ==> simionSchmidt (simionSchmidt' w) == w

testsMathPermBijection =
    [ ("Math.Perm.Bijection/simionSchmidt/avoid",  check prop_simionSchmidt_avoid)
    , ("Math.Perm.Bijection/simionSchmidt'/avoid", check prop_simionSchmidt_avoid')
    , ("Math.Perm.Bijection/simionSchmidt/id",     check prop_simionSchmidt_id)
    , ("Math.Perm.Bijection/simionSchmidt'/id",    check prop_simionSchmidt_id')
    ]

--------------------------------------------------------------------------------
-- Properties for Math.Perm.Sort
--------------------------------------------------------------------------------

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

prop_stackDef  w = stack  (toList w) == toList (stackSort  w)
prop_bubbleDef w = bubble (toList w) == toList (bubbleSort w)

prop_stackAv w =
    let basis = [fromList [1,2,0]]
    in (stackSort w == idperm (size w)) == w `P.avoidsAll` basis

prop_bubbleAv w =
    let basis = [fromList [1,2,0], fromList [2,1,0]]
    in (bubbleSort w == idperm (size w)) == w `P.avoidsAll` basis

testsMathPermSort =
    [ ("Math.Perm.Sort/stack/def",    check prop_stackDef)
    , ("Math.Perm.Sort/bubble/def",   check prop_bubbleDef)
    , ("Math.Perm.Sort/stack/Av",     check prop_stackAv)
    , ("Math.Perm.Sort/bubble/Av",    check prop_bubbleAv)
    ]

--------------------------------------------------------------------------------
-- Properties for Math.Perm.Stat
--------------------------------------------------------------------------------

statEq f g w = f w == g (toList w)

prop_asc    = statEq S.asc   LS.asc
prop_des    = statEq S.des   LS.des
prop_exc    = statEq S.exc   LS.exc
prop_fp     = statEq S.fp    LS.fp
prop_cyc    = statEq S.cyc   LS.cyc
prop_inv    = statEq S.inv   LS.inv
prop_maj    = statEq S.maj   LS.maj
prop_comaj  = statEq S.comaj LS.comaj
prop_lmin   = statEq S.lmin  LS.lmin
prop_lmax   = statEq S.lmax  LS.lmax
prop_rmin   = statEq S.rmin  LS.rmin
prop_rmax   = statEq S.rmax  LS.rmax
prop_head w = size w > 0 ==> statEq S.head head w
prop_last w = size w > 0 ==> statEq S.last last w
prop_peak   = statEq S.peak  LS.peak
prop_vall   = statEq S.vall  LS.vall
prop_dasc   = statEq S.dasc  LS.dasc
prop_ddes   = statEq S.ddes  LS.ddes
prop_ep     = statEq S.ep    LS.ep
prop_lir    = statEq S.lir   LS.lir
prop_ldr    = statEq S.ldr   LS.ldr
prop_rir    = statEq S.rir   LS.rir
prop_rdr    = statEq S.rdr   LS.rdr
prop_comp   = statEq S.comp  LS.comp
prop_scomp  = statEq S.scomp LS.scomp
prop_dim    = statEq S.dim   LS.dim
prop_asc0   = statEq S.asc0  LS.asc0
prop_des0   = statEq S.des0  LS.des0
prop_inv_21 = forAll (resize 17 arbitrary) $ \w ->
                 let stat21 = length . P.copiesOf (fromList [1,0])
                 in S.inv w == stat21 w
prop_lis_r w = S.lis w == S.lds (D8.reverse w)
prop_lis_c w = S.lis w == S.lds (D8.complement w)
prop_lis_i w = S.lis w == S.lis (D8.inverse w)
prop_lds_i w = S.lds w == S.lds (D8.inverse w)
prop_ErdosSzekeres w = n > 0 ==> S.lis w > m || S.lds w > m
    where
      n = size w
      m = floor . sqrt $ fromIntegral (n-1)

-- prop_shad   = forAllPermEq  (shad  . ints)  S.shad


testsMathPermStat =
    [ ("Math.Perm.Stat/asc",        check prop_asc)
    , ("Math.Perm.Stat/des",        check prop_des)
    , ("Math.Perm.Stat/exc",        check prop_exc)
    , ("Math.Perm.Stat/fp",         check prop_fp)
    , ("Math.Perm.Stat/cyc",        check prop_cyc)
    , ("Math.Perm.Stat/inv",        check prop_inv)
    , ("Math.Perm.Stat/maj",        check prop_maj)
    , ("Math.Perm.Stat/comaj",      check prop_comaj)
    , ("Math.Perm.Stat/lmin",       check prop_lmin)
    , ("Math.Perm.Stat/lmax",       check prop_lmax)
    , ("Math.Perm.Stat/rmin",       check prop_rmin)
    , ("Math.Perm.Stat/rmax",       check prop_rmax)
    , ("Math.Perm.Stat/head",       check prop_head)
    , ("Math.Perm.Stat/last",       check prop_last)
    , ("Math.Perm.Stat/peak",       check prop_peak)
    , ("Math.Perm.Stat/vall",       check prop_vall)
    , ("Math.Perm.Stat/dasc",       check prop_dasc)
    , ("Math.Perm.Stat/ddes",       check prop_ddes)
    , ("Math.Perm.Stat/ep",         check prop_ep)
    , ("Math.Perm.Stat/lir",        check prop_lir)
    , ("Math.Perm.Stat/ldr",        check prop_ldr)
    , ("Math.Perm.Stat/rir",        check prop_rir)
    , ("Math.Perm.Stat/rdr",        check prop_rdr)
    , ("Math.Perm.Stat/comp",       check prop_comp)
    , ("Math.Perm.Stat/scomp",      check prop_scomp)
    , ("Math.Perm.Stat/dim",        check prop_dim)
    , ("Math.Perm.Stat/asc0",       check prop_asc0)
    , ("Math.Perm.Stat/des0",       check prop_des0)
    , ("Math.Perm.Stat/inv/21",     check prop_inv_21)
    , ("Math.Perm.Stat/lis/r",      check prop_lis_r)
    , ("Math.Perm.Stat/lis/c",      check prop_lis_c)
    , ("Math.Perm.Stat/lis/i",      check prop_lis_i)
    , ("Math.Perm.Stat/lds/i",      check prop_lds_i)
    , ("Math.Perm.Stat/Erdös-Szekeres", check prop_ErdosSzekeres)
--     , ("shad",       check prop_shad)
    ]

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

tests = concat [
          testsDataPerm
        , testsDataPermgram
        , testsDataSSYT
        , testsMathPerm
        , testsMathPermPattern
        , testsMathPermD8
        , testsMathPermGroup
        , testsMathPermBijection
        , testsMathPermSort
        , testsMathPermStat
        ]

runTests = mapM_ (\(name, t) -> putStr (name ++ ":\t") >> t)

main = runTests tests
