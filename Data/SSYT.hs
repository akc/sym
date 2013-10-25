{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- |
-- Copyright   : Anders Claesson 2013
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
--
-- Data types for Semistandard Young Tableaux (SSYT) and functions for
-- converting between (generalized) permutataions and SSYT. In other
-- words, this module implements the Robinson-Schensted-Knuth (RSK)
-- correspondence.

module Data.SSYT
    (
      GeneralizedPerm
    , Entry
    , SSYT
    , SSYTPair (..)
    , Shape (..)
    , empty
    , null
    , display
    , fromPerm
    , fromGeneralizedPerm
    , toPerm
    , toGeneralizedPerm
    ) where

import Prelude    hiding (null)
import Data.List  hiding (null)
import Data.Perm

type Row = Int

-- | An entry is a non-negative integer
type Entry = Int

-- | A /Generalized Permutation/ is a lexicographically sorted list of
-- pairs of non-negative integers.
type GeneralizedPerm = [(Int, Int)]

-- | A /Semistandard Young Tableau (SSYT)/: the entries weakly increase
-- along each row and strictly increase down each column.
type SSYT = [[Entry]]

-- | A pair of Semistandard Young Tableaux.
data SSYTPair = SSYTPair { insertionTableau :: SSYT
                         , recordingTableau :: SSYT
                         } deriving Eq

class Shape a where
    shape :: a -> [Int]

instance Shape SSYT where
    shape = map length

instance Shape SSYTPair where
    shape = shape . recordingTableau

-- | A pair of empty Young tableaux.
empty :: SSYTPair
empty = SSYTPair [] []

-- | Check if a given pair of Young tableaux are empty.
null :: SSYTPair -> Bool
null pq = pq == empty

instance Show SSYTPair where
    show (SSYTPair p q) = unwords ["SSYTPair", show p, show q]

-- | Produce a string for pretty printing SSYT pairs.
display :: SSYTPair -> String
display pq@(SSYTPair p q)
    | null pq   = "[] []"
    | otherwise = intercalate "\n" $ zipWith (++) (pad p') q'
       where
         p'@(r:_) = map show p
         q'       = map show q
         pad      = map $ \s -> take (1+length r) (s ++ repeat ' ')

-- Inserts Entry into a given Tableau returning the resulting Tableau
-- and the row where the Tableau was extended with a new box.
insertP :: SSYT -> Entry -> (SSYT, Row)
insertP []     k = ([[k]], 1)
insertP (r:rs) k =
    let (smaller, larger) = span (<=k) r
    in case larger of
         []   -> ((r++[k]):rs, 1)
         c:cs -> let (rs', i) = insertP rs c
                 in ((smaller ++ k:cs) : rs', i+1)

-- Given (i,j), inserts j at the end of row i in the given Tableau.
insertQ :: SSYT -> Row -> Entry -> SSYT
insertQ []     _ j = [[j]]
insertQ (r:rs) 1 j = (r ++ [j]) : rs
insertQ (r:rs) i j = r : insertQ rs (i-1) j

-- Given (i,j) and pair of tableaux (p,q) of the same shape, inserts i
-- into p and j into q so that the resulting pair of tableaux (p',q')
-- still have the same shape.
insertPQ :: SSYTPair -> (Entry, Entry) -> SSYTPair
insertPQ (SSYTPair p q) (i,j) =
    let (p',k) = insertP p j in SSYTPair p' (insertQ q k i)

trim :: SSYT -> SSYT
trim = takeWhile (/=[])

-- The inverse of insertP
removeP :: SSYT -> Row -> (SSYT, Entry)
removeP p k = (trim $ reverse vs ++ [init t] ++ p2, e)
    where
      (p1, p2) = splitAt (k+1) p
      (t : ts) = reverse p1 -- t is the k-th row (counting from 0)
      (vs,  e) = unbump (last t) ts
      unbump x [] = ([], x)
      unbump x (r:rs) =
          let (r1, r2) = span (<x) r
              (us,  y) = unbump (last r1) rs
          in ((init r1 ++ x:r2) : us, y)

-- The inverse of insertQ
removeQ :: SSYT -> (SSYT, Row, Entry)
removeQ q = (trim q', k, e)
    where
      -- The last element and the length of a given row:
      f = foldl (\(_,n) x -> (x,n+1)) (0,0) :: [Int] -> (Int, Int)
      -- Equal elements of Q are inserted left-to-right, allowing us to
      -- know which element, e, was the last to be inserted:
      ((e, _), k) = maximum $ zip (map f q) [0..]
      -- Remove e from Q:
      q' = [ if i == k then init r else r | (r,i) <- zip q [0..] ]

-- The inverse of insertPQ
removePQ :: SSYTPair -> (SSYTPair, (Entry, Entry))
removePQ (SSYTPair p q) = (SSYTPair p' q', (e1, e2))
    where
      (q', k, e1) = removeQ q
      (p', e2)    = removeP p k

-- | The Robinson-Schensted-Knuth (RSK) algorithm.
fromGeneralizedPerm :: GeneralizedPerm -> SSYTPair
fromGeneralizedPerm = foldl insertPQ empty

-- | The Robinson-Schensted algorithm.
fromPerm :: Perm -> SSYTPair
fromPerm = fromGeneralizedPerm . zip [0..] . toList

-- | The inverse of the Robinson-Schensted-Knuth algorithm.
toGeneralizedPerm :: SSYTPair -> GeneralizedPerm
toGeneralizedPerm = go []
    where
      go ijs pq | null pq   = ijs
                | otherwise = let (rs,ij) = removePQ pq in go (ij:ijs) rs

-- | The inverse of the Robinson-Schensted algorithm.
toPerm :: SSYTPair -> Perm
toPerm = fromList . map snd . toGeneralizedPerm
