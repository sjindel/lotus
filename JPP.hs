{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
             GeneralizedNewtypeDeriving, DeriveFoldable, DeriveTraversable,
             DeriveFunctor, ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.JPP
-- Copyright   :  Samir Jindel (c) 2013
--
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  sjindel@sjindel.org
-- Stability   :  experimental
-- Portability :  no
--
-- Pretty print module based on a new algorithm for formatting the document
-- abstraction of Daan Leijen and Philip Wadler. The document abstraction (but
-- not this algorithm) is described in:
--
-- @
--      \"A prettier printer\"
--      Draft paper, April 1997, revised March 1998.
--      <http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf>
-- @
--
-- * The 'Doc' abstraction
--
-- The 'Doc' data structure is an abstraction between ASTs and plain text that
-- separates the problem of translating nodes in an AST into pieces of text (a
-- local operation) from formatting these text pieces on a page (a global
-- operation). In particular, this library allows an AST to be pretty printed in
-- two phases:
--
--   1) The AST is translated into a 'Doc', using the cost system to encode
--   style preferences in the object language, such as at which positions it is
--   prefereable to insert linebreaks, how the indentation is affected by
--   different syntactic structures, etc.
--
--   2) There may be many ways to format a particular 'Doc', each with an
--   associated cost. The algorithm implemented here formats a 'Doc' into a
--   piece of text with minimal global cost.
--
-- The algebric properties of the 'Doc' data structure are central to this
-- algorithm for rendering them. In particular, if 'c' is a semiring and 't' a
-- monoid, 'Doc c t' is a semiring with additive operator '+', multiplicative
-- operator '*', additive identity '0', and multiplicative identity '1':
--
--   - '0' is a document that can not be rendered
--   - '1' is a document corresponding to \epsilon
--   - 'x' + 'y' is a document that may be rendered either as 'x' or as 'y'.
--   - 'x' * 'y' i sa document rendered as 'x' concatenated with 'y'.
--
-- It is easy to see that the semiring axioms hold on this structure.
-- Consequently, this library exports an instance of 'Num (Doc c t)'.
--
-- Several other primitives are required to represent most programming
-- languages; combinators encapsulating them are described (with examples) in
-- the combinators section below.

-- TODO(sjindel): Add documentation everywhere.
-- TODO(sjindel): Use hash-consing of the 'Doc's to improve memoization reuse.

module Text.PrettyPrint.JPP
  ( -- Classes
    Long(..)
  , TextLike(..)
    -- Types
  , Doc
  , DocCost(..)
    -- Combinators
  , align
  , column
  , cost
  , tag
  , flatAlt
  , flatten
  , hfill
  , line
  , nest
  , nesting
  , pagewidth
  , ref
  , sep
  , text
    -- Rendering/displaying functions
  , display
  , render
  , sprint
  ) where

import Control.Monad.ST (ST)
import qualified Control.Monad.ST as ST
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import Data.Monoid
import Data.STRef (STRef)
import qualified Data.STRef as SR
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (Traversable)
import qualified Data.Traversable as Tr

------------------------------------------------------------

class Long t where
  size :: t -> Int

class Monoid t => TextLike t where
  newline :: t
  space :: t

data Doc c t
  = Fail
  | Empty
  | Line
  | Cost c
  | Text Int t
  | Nest Int
  | Choice (Doc c t) (Doc c t)
  | Cat (Doc c t) (Doc c t)
  | FlatAlt (Doc c t) (Doc c t)
  | Ref Int
  | Tag (Doc c t) (Int -> Doc c t)
  | Column (Int -> Doc c t)
  | Nesting (Int -> Doc c t)
  | PageWidth (Int -> Doc c t)
  deriving (Functor)

data SimpleDoc c t
  = SFail
  | SEmpty
  | SCost c
  | SText t
  | SLine Int
  | SCat (SimpleDoc c t) (SimpleDoc c t)
  deriving (Show, Eq, Functor, Foldable, Traversable)

data CachedDoc s c t
  = CFail (DocCache s c t)
  | CEmpty (DocCache s c t)
  | CLine (DocCache s c t)
  | CCost (DocCache s c t) c
  | CText (DocCache s c t) Int t
  | CNest (DocCache s c t) Int
  | CChoice (DocCache s c t) (CachedDoc s c t) (CachedDoc s c t)
  | CCat (DocCache s c t) (CachedDoc s c t) (CachedDoc s c t)
  | CFlatAlt (DocCache s c t) (CachedDoc s c t) (CachedDoc s c t)
  | CRef (DocCache s c t) Int
  | CTag (DocCache s c t) (CachedDoc s c t) (Int -> ST s (CachedDoc s c t))
  | CColumn (DocCache s c t) (Int -> ST s (CachedDoc s c t))
  | CNesting (DocCache s c t) (Int -> ST s (CachedDoc s c t))
  | CPageWidth (DocCache s c t) (Int -> ST s (CachedDoc s c t))

-- (over 80c?, total newlines, user cost)
newtype DocCost c = DC { getDC :: (Bool, Int, c) }
                    deriving (Eq, Ord, Show)

data LayoutResult c t
  = Good [(Int, Int, (DocCost c, SimpleDoc (DocCost c) t))]
  | Bad [(Int, Int, (DocCost c, SimpleDoc (DocCost c) t))]

type DocCache s c t = STRef s (HashMap (Int, Int) (LayoutResult c t))
type DocIndex s c t = STRef s (Int, IntMap (CachedDoc s c t))

------------------------------------------------------------

instance Long [a] where
  size = length

instance Monoid (SimpleDoc c t) where
  mempty = SEmpty
  mappend = SCat

-- | 'Doc's form a /rig/, a ring without negation*. For convenience, we define a
-- partial instance of the 'Num' class instead of using a more precise but
-- tedious algebra package. 'c' should form an ordered rig and 't' should form a
-- monoid for the rig properties to hold for this instance, but the superclasses
-- are not added for convenience.
--
-- * /It's not quite a rig because we choose the left of two cost-equivalent
-- witnesses in the 'Choice' operator. But it is a rig w.r.t. the optimization
-- problem alone (finding the cost of the least expensive formatting). A purist
-- could change the witness representation to unordered sets of witnesses./
instance Num (Doc c t) where
  (+) = Choice
  (*) = Cat

  abs = id
  signum _ = 1

  fromInteger 0 = Fail
  fromInteger 1 = Empty
  fromInteger _ = error "'Doc's only form a rig."

instance (Num c, Ord c, Bounded c, Monoid w) => Num (DocCost c, w) where
  (c1, w1) + (c2, w2) = if c1 <= c2 then (c1, w1) else (c2, w2)
  (DC (a, b, c), w1) * (DC (x, y, z), w2) =
    (DC (a || x, b + y, c + z), w1 <> w2)

  abs = error "no abs for 'DocCost' with witness"
  signum = error "no signum for 'DocCost' with witness"

  fromInteger 0 = (DC (maxBound, maxBound, maxBound),
                   error "'Fail' can not be rendered.")
  fromInteger 1 = (DC (False, 0, 0), mempty)
  fromInteger _ = error $ "'fromInteger' only works for 0 and 1 for 'DocCost' "
                  ++ "with witness"

instance TextLike String where
  newline = "\n"
  space = " "

instance TextLike Text where
  newline = T.singleton '\n'
  space = T.singleton ' '

---------------------------------------------
-- * Combinators

cost :: c -> Doc c t
cost = Cost

line :: Doc c t
line = Line

nest :: Int -> Doc c t -> Doc c t
nest n d = Nest n * d * Nest (-n)

flatAlt :: Doc c t -> Doc c t -> Doc c t
flatAlt = FlatAlt

tag :: Doc c t -> (Int -> Doc c t) -> Doc c t
tag = Tag

align :: Doc c t -> Doc c t
align d = Tag d $ \r -> Column $ \c -> Nesting $ \n -> nest (c - n) (Ref r)

column :: (Int -> Doc c t) -> Doc c t
column = Column

nesting :: (Int -> Doc c t) -> Doc c t
nesting = Nesting

pagewidth :: (Int -> Doc c t) -> Doc c t
pagewidth = PageWidth

ref :: Int -> Doc c t
ref = Ref

sep :: a -> [a] -> [a]
sep _ [] = []
sep _ [x] = [x]
sep a (x:xs) = x:a:(sep a xs)

text :: Long t => t -> Doc c t
text t = Text (size t) t

hfill :: Monoid t => Doc c t
hfill = column $ \c -> pagewidth $ \p -> Text (p - c) mempty

-- | For efficiency, 'flatten' assumes that for any document 'Choice x y',
-- 'flatten x' = 'flatten y' (equality under rendering).
flatten :: Doc c t -> Doc c t
flatten Line = Fail
flatten (Nest i) = Nest i
flatten (Choice x _) = flatten x
flatten (Cat x y) = Cat (flatten x) (flatten y)
flatten (FlatAlt _ y) = flatten y
flatten (Column k) = Column (flatten.k)
flatten (Nesting k) = Nesting (flatten.k)
flatten (PageWidth k) = PageWidth (flatten.k)
flatten x = x

---------------------------------------------
-- * Rendering

layoutRaw :: (Num c, Ord c, Bounded c) => Int -> (Int, Int) -> CachedDoc s c t
             -> DocIndex s c t -> ST s (LayoutResult c t)
layoutRaw w (n, k) d idx =
  case d of
    CFail _ -> return $ Bad []
    CEmpty _ -> return $ Good [(n, k, 1)]
    CLine _ ->
      if k > w then return $ Bad [(n, n, (DC (True, 1, 0), SLine n))]
      else return $ Good [(n, n, (DC (False, 1, 0), SLine n))]
    CCost _ c -> return $ Good [(n, k, (DC (k > w, 0, c), SEmpty))]
    CText _ l t ->
      return $ Good [(n, k + l, (DC (k + l > w, 0, 0), SText t))]
    CNest _ i -> do t <- initialize Empty; layout w (n + i, k) t idx
    CChoice _ x y -> do xr <- layout w (n, k) x idx
                        yr <- layout w (n, k) y idx
                        return (addr xr yr)
    CCat _ x y ->
      do xr <- layout w (n, k) x idx
         layouts <- mapM trv (maph xr)
         return $ foldr addr (Bad []) layouts
         where
           trv (n', k', (c, s)) =
             do yr <- layout w (n', k') y idx
                if c < DC (True, 0, 0)
                  then case yr of
                    Good yrh ->
                      return $ Good $ map (\(n, k, x) -> (n, k, (c, s) * x)) yrh
                    Bad yrh ->
                      return $ Bad $ map (\(n, k, x) -> (n, k, (c, s) * x)) yrh
                  else return $ Bad
                       $ map (\(n, k, x) -> (n, k, x * (c, s))) $ maph yr
    CFlatAlt _ x _ -> layout w (n, k) x idx
    CRef _ r -> do (_, i) <- SR.readSTRef idx; layout w (n, k) (i I.! r) idx
    CTag _ e f -> do (r, i) <- SR.readSTRef idx
                     SR.writeSTRef idx (r + 1, I.insert r e i)
                     a <- f r
                     layout w (n, k) a idx
    CColumn _ f -> do x <- f k; layout w (n, k) x idx
    CNesting _ f -> do x <- f n; layout w (n, k) x idx
    CPageWidth _ f -> do x <- f w; layout w (n, k) x idx
  where
    addr (Good x) (Good y) = Good $ addl x y
    addr (Bad  _) (Good y) = Good y
    addr (Good x) (Bad  _) = Good x
    addr (Bad  x) (Bad  y) = Bad $ addl x y
    addl ((n1, k1, c1):x) ((n2, k2, c2):y) =
      case compare (n1, k1) (n2, k2) of
        LT -> (n1, k1, c1):(addl x ((n2, k2, c2):y))
        GT -> (n2, k2, c2):(addl ((n1, k1, c1):x) y)
        EQ -> (n1, k1, c1 + c2):(addl x y)
    addl [] y = y
    addl x [] = x

layout :: (Num c, Ord c, Bounded c) => Int -> (Int, Int) -> CachedDoc s c t
          -> DocIndex s c t -> ST s (LayoutResult c t)
layout w (n, k) d i =
  do c <- SR.readSTRef $ cache d
     case H.lookup (n, k) c of
       Just r -> return r -- trace "memo" $ return r
       Nothing -> -- trace "nomemo" $
                  do r <- layoutRaw w (n, k) d i
                     SR.writeSTRef (cache d) $ H.insert (n, k) r c
                     return r

display :: TextLike t => SimpleDoc c t -> t
display SFail = error "Can not display @SFail@."
display SEmpty = mempty
display (SCost _) = mempty
display (SText t) = t
display (SLine i) = newline <> mconcat (take i (repeat space))
display (SCat x y) = display x <> display y

maph :: LayoutResult c t -> [(Int, Int, (DocCost c, SimpleDoc (DocCost c) t))]
maph (Good x) = x
maph (Bad x) = x

maybeline :: Doc c t
maybeline = Column $ \k -> Nesting $ \n -> if (k == n) then 0 else Line

render :: (Num c, Ord c, Bounded c) => Int -> Doc c t
          -> (DocCost c, SimpleDoc (DocCost c) t)
render w x = ST.runST $
  do xs <- initialize (x * maybeline)
     i <- SR.newSTRef (0, I.empty)
     r <- layout w (0, 0) xs i
     return $ sum $ map (\(_, _, x) -> x) $ maph r

sprint :: (Num c, Ord c, Bounded c, TextLike t) => Int -> Doc c t -> t
sprint w x = display $ snd $ render w x

---------------------------------------------
-- * Boilerplate

initialize :: Doc c t -> ST s (CachedDoc s c t)
initialize Fail =
  do s <- SR.newSTRef H.empty
     return (CFail s)
initialize Empty =
  do s <- SR.newSTRef H.empty
     return (CEmpty s)
initialize Line =
  do s <- SR.newSTRef H.empty
     return (CLine s)
initialize (Cost c) =
  do s <- SR.newSTRef H.empty
     return (CCost s c)
initialize (Text w t) =
  do s <- SR.newSTRef H.empty
     return (CText s w t)
initialize (Nest i) =
  do s <- SR.newSTRef H.empty
     return (CNest s i )
initialize (Choice x y) =
  do s <- SR.newSTRef H.empty
     x' <- initialize x
     y' <- initialize y
     return (CChoice s x' y')
initialize (Cat x y) =
  do s <- SR.newSTRef H.empty
     x' <- initialize x
     y' <- initialize y
     return (CCat s x' y')
initialize (FlatAlt x y) =
  do s <- SR.newSTRef H.empty
     x' <- initialize x
     y' <- initialize y
     return (CFlatAlt s x' y')
initialize (Ref r) =
  do s <- SR.newSTRef H.empty
     return (CRef s r)
initialize (Tag d f) =
  do s <- SR.newSTRef H.empty
     d' <- initialize d
     return (CTag s d' (initialize.f))
initialize (Column f) =
  do s <- SR.newSTRef H.empty
     return (CColumn s (initialize.f))
initialize (Nesting f) =
  do s <- SR.newSTRef H.empty
     return (CNesting s (initialize.f))
initialize (PageWidth f) =
  do s <- SR.newSTRef H.empty
     return (CPageWidth s (initialize.f))

cache :: CachedDoc s c t -> DocCache s c t
cache (CFail c) = c
cache (CEmpty c) = c
cache (CLine c) = c
cache (CCost c _) = c
cache (CText c _ _) = c
cache (CNest c _) = c
cache (CChoice c _ _) = c
cache (CCat c _ _) = c
cache (CFlatAlt c _ _) = c
cache (CRef c _) = c
cache (CTag c _ _) = c
cache (CColumn c _) = c
cache (CNesting c _) = c
cache (CPageWidth c _) = c
