{-#LANGUAGE BangPatterns, RankNTypes #-}

module Control.FoldM (
    -- * Fold Types
      Fold(..)
    , FoldM(..)

    -- * Folding
    , fold
    , foldM
    , scan

    -- * Stock Pure Folds
    , Control.FoldM.mconcat
    , Control.FoldM.foldMap
    , head
    , last
    , lastDef
    , lastN
    , null
    , length
    , and
    , or
    , any
    , all
    , sum
    , product
    , maximum
    , minimum
    , maximumBy
    , minimumBy
    , elem
    , notElem
    , find
    , index
    , elemIndex
    , findIndex
--    , count

    -- * Stock Impure Folds
    , random
    , randomN
    , Control.Foldl.mapM_
    , sink
    
    -- * Generic Folds
    , genericLength
    , genericIndex

    -- * Container folds
    , list
    , revList
    -- , nub
    -- , eqNub
    -- , set
    , vector

    -- * Utilities
    -- $utilities
    -- , purely
    -- , purely_
    , impurely
    , impurely_
    -- , generalize
    -- , simplify
    , hoists
    -- , duplicateM
    , premap
    -- , premapM
    , HandlerM
    , handles
    -- , handlesM
    , folded

    -- * IO fripperies
    , toHandle
    , stdout
    , stderr
    , toHandleLn
    , stdoutLn
    , stderrLn
    
    -- * Re-exports
    -- $reexports
    , module Control.Monad.Primitive
    , module Data.Foldable
    , module Data.Vector.Generic
       ) where

import Control.Foldl (FoldM(..),Fold(..), HandlerM
                     , foldM, sink, mapM_, vector, impurely
                     , impurely_, hoists, random, randomN
                     , folded)
import qualified Control.Foldl as L
import Data.Foldable (Foldable)
import Control.Monad.Primitive (PrimMonad, RealWorld)
import Data.Vector.Generic (Vector, Mutable)
import Data.Functor.Identity (Identity(..))
import Prelude hiding (
    head, last, null, length, any, all, and, or,
    maximum, minimum, elem, notElem, sum, product,
    mapM_)
import qualified Data.Foldable as F
import qualified Data.IOData as IOData
import Control.Monad.IO.Class
import qualified System.IO as IO
import Data.Monoid (Monoid(..))

fold :: (Foldable f) => FoldM Identity a b -> f a -> b
fold f = runIdentity . L.foldM f
{-#INLINE fold #-}

-- | Convert a pure strict left 'Fold' into a scan
scan :: FoldM Identity a b -> [a] -> [b]
scan f = L.scan (L.simplify f)
{-# INLINE scan #-}

-- | Fold all values within a container using 'mappend' and 'mempty'
mconcat :: (Monad m, Monoid a) => FoldM m a a
mconcat = L.generalize L.mconcat
{-# INLINE mconcat #-}

-- | Convert a \"@foldMap@\" to a 'Fold'
foldMap :: (Monad m, Monoid w) => (a -> w) -> (w -> b) -> FoldM m a b
foldMap to done = L.generalize (L.foldMap to done)

-- | Fold all values into a list
list :: Monad m => FoldM m a [a]
list = L.generalize L.list
{-# INLINE list #-}

{-| Get the first character of a text stream or return 'Nothing' if the stream
    is empty
-}
head :: Monad m => FoldM m a (Maybe a)
head =  L.generalize L.head
{-# INLINE head #-}

{-| Get the last character of a text stream or return 'Nothing' if the text
    stream is empty
-}
last :: Monad m => FoldM m a (Maybe a)
last = L.generalize L.last
{-# INLINE last #-}


{-| Get the last element of a container or return a default value if the container
    is empty
-}
lastDef :: Monad m => a -> FoldM m a a
lastDef a = L.generalize (L.lastDef a)
{-# INLINE lastDef #-}

{-| Return the last N elements
-}
lastN :: Monad m => Int -> FoldM m a [a]
lastN n = L.generalize (L.lastN n)
{-# INLINE lastN #-}

-- | Returns 'True' if the text stream is empty, 'False' otherwise
null :: Monad m => FoldM m a Bool
null = L.generalize L.null
{-# INLINE null #-}

-- | Return the length of the text stream in characters
length :: (Monad m) => FoldM m a Int
length = L.generalize L.length
{-# INLINE length #-}

{-| @(all predicate)@ returns 'True' if all characters satisfy the predicate,
    'False' otherwise
-}
all ::  Monad m => (a -> Bool) -> FoldM m a Bool
all thus = L.generalize (L.all thus)
{-| @(any predicate)@ returns 'True' if any character satisfies the predicate,
    'False' otherwise
-}
any ::  Monad m => (a -> Bool) -> FoldM m a Bool
any thus = L.generalize (L.any thus)
{-# INLINE any #-}

-- | Returns 'True' if all elements are 'True', 'False' otherwise
and :: Monad m => FoldM m Bool Bool
and = L.generalize L.and
{-# INLINE and #-}

-- | Returns 'True' if any element is 'True', 'False' otherwise
or :: Monad m => FoldM m Bool Bool
or = L.generalize L.or
{-# INLINE or #-}

-- | Calculate the sum of all the elements
sum :: (Monad m, Num n) => FoldM m n n
sum = L.generalize L.sum
{-#INLINE sum #-}

-- | Calculate the product of all the elements
product :: (Monad m, Num n) => FoldM m n n
product = L.generalize L.product
{-#INLINE product #-}

-- | Computes the maximum character
maximum :: (Monad m, Ord a) => FoldM m a (Maybe a)
maximum = L.generalize L.maximum
{-# INLINE maximum #-}

{-| Computes the maximum element with respect to the given comparison
    function
-}
maximumBy :: Monad m  => (a -> a -> Ordering) -> FoldM m a (Maybe a)
maximumBy cmp = L.generalize (L.maximumBy cmp)
{-# INLINABLE maximumBy #-}

-- | Computes the minimum 
minimum :: (Monad m, Ord a) => FoldM m a (Maybe a)
minimum = L.generalize L.minimum
{-# INLINE minimum #-}

{-| Computes the minimum element with respect to the given comparison
    function
-}
minimumBy :: Monad m => (a -> a -> Ordering) -> FoldM m a (Maybe a)
minimumBy = L.generalize . L.minimumBy
{-# INLINE minimumBy #-}

{-| @(elem c)@ returns 'True' if the text stream has a character equal to @c@,
    'False' otherwise
-}
elem :: (Monad m, Eq a) => a -> FoldM m a Bool
elem = L.generalize . L.elem
{-# INLINE elem #-}

{-| @(notElem c)@ returns 'False' if the text stream has a character equal to
    @c@, 'True' otherwise
-}
notElem :: (Monad m, Eq a) => a -> FoldM m a Bool
notElem = L.generalize . L.notElem
{-# INLINABLE notElem #-}

{-| @(find predicate)@ returns the first character that satisfies the predicate
    or 'Nothing' if no character satisfies the predicate
-}
find :: Monad m => (a -> Bool) -> FoldM m a (Maybe a)
find thus = L.generalize (L.find thus)
{-# INLINABLE find #-}

{-| @(index n)@ returns the @n@th character of the text stream, or 'Nothing' if
    the stream has an insufficient number of characters
-}

index :: Monad m => Int -> FoldM m a (Maybe a)
index = L.generalize . L.index
{-#INLINE index #-}


{-| @(elemIndex c)@ returns the index of the first character that equals @c@,
    or 'Nothing' if no character matches
-}
elemIndex :: (Monad m, Eq a) => a -> FoldM m a (Maybe Int)
elemIndex c = L.generalize (L.elemIndex c)
{-# INLINE elemIndex #-}

{-| @(findIndex predicate)@ returns the index of the first character that
    satisfies the predicate, or 'Nothing' if no character satisfies the
    predicate
-}
findIndex :: Monad m =>  (a -> Bool) -> FoldM m a (Maybe Int)
findIndex thus = L.generalize (L.findIndex thus)
{-# INLINE findIndex #-}

-- -- | @(count c)@ returns the number of times @c@ appears
-- count :: (Monad m, Eq a) => a -> FoldM m a Int
-- count = L.generalize . L.count
-- {-# INLINE count #-}


-- | Like 'length', except with a more general 'Num' return value
genericLength :: (Monad m, Num b) => FoldM m a b
genericLength = L.generalize L.genericLength
{-# INLINE genericLength #-}

-- | Like 'index', except with a more general 'Integral' argument
genericIndex :: (Monad m, Integral i) => i -> FoldM m a (Maybe a)
genericIndex = L.generalize . L.genericIndex
{-# INLINE genericIndex #-}

premap :: (a -> b) -> FoldM m b r -> FoldM m a r
premap = L.premapM
{-#INLINE premap #-}

revList :: Monad m => FoldM m a [a]
revList = L.generalize L.revList

handles :: Monad m => HandlerM m a b -> FoldM m b r -> FoldM m a r
handles = L.handlesM 
{-#INLINE handles #-}

-- simplified IO nonsense

toHandle :: (IOData.IOData a, MonadIO m) => IO.Handle -> FoldM m a ()
toHandle h = FoldM (\() bs -> IOData.hPut h bs) (return ()) return 
{-#INLINE toHandle #-}

stdout :: (IOData.IOData a, MonadIO m) => FoldM m a ()
stdout = toHandle IO.stdout
{-#INLINE stdout #-}

stderr :: (IOData.IOData a, MonadIO m) => FoldM m a ()
stderr = toHandle IO.stderr
{-#INLINE stderr #-}

toHandleLn :: (IOData.IOData a, MonadIO m) => IO.Handle -> FoldM m a ()
toHandleLn h = FoldM (\() bs -> IOData.hPutStrLn h bs) (return ()) return
{-#INLINE toHandleLn #-}

stdoutLn :: (IOData.IOData a, MonadIO m) => FoldM m a ()
stdoutLn = toHandleLn IO.stdout
{-#INLINE stdoutLn #-}

stderrLn :: (IOData.IOData a, MonadIO m) => FoldM m a ()
stderrLn = toHandleLn IO.stderr
{-#INLINE stderrLn #-}
