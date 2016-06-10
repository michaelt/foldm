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
    , count

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

import Control.Foldl (FoldM(..),Fold(..), EndoM(..), HandlerM
                     , foldM, sink, mapM_, vector, impurely
                     , impurely_, hoists, random, randomN
                     , folded)
import qualified Control.Foldl as L
import Data.Foldable (Foldable)
import Control.Monad.Primitive (PrimMonad, RealWorld)
import Data.Vector.Generic (Vector, Mutable)
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import Data.Functor.Identity (Identity(..))
import Prelude hiding (
    head, last, null, length, any, all, and, or,
    maximum, minimum, elem, notElem, sum, product,
    mapM_)
import qualified Data.Foldable as F
import qualified Data.IOData as IOData
import Control.Monad.IO.Class
import qualified System.IO as IO
import qualified GHC.IO.Exception as Exc

fold :: (Foldable f) => FoldM Identity a b -> f a -> b
fold f = runIdentity . L.foldM f
{-#INLINE fold #-}

-- | Convert a pure strict left 'Fold' into a scan
scan :: FoldM Identity a b -> [a] -> [b]
scan f = L.scan (L.simplify f)
{-# INLINE scan #-}

-- | Fold all values within a container using 'mappend' and 'mempty'
mconcat :: (Monad m, Monoid a) => FoldM m a a
mconcat = FoldM (\a b -> return $! mappend a b) (return $! mempty) return
{-# INLINE mconcat #-}

-- | Convert a \"@foldMap@\" to a 'Fold'
foldMap :: (Monad m, Monoid w) => (a -> w) -> (w -> b) -> FoldM m a b
foldMap to done = FoldM (\x a -> return $! mappend x (to a)) (return $! mempty) (return . done)
{-# INLINE foldMap #-}

-- | Fold all values into a list
list :: Monad m => FoldM m a [a]
list = FoldM (\diff a -> return (diff . (a:))) (return id) (\diff -> return (diff []))
{-# INLINE list #-}

data Option a = Some !a | None deriving (Show)

optionToMaybe :: Monad m => Option a -> m (Maybe a)
optionToMaybe None  = return Nothing
optionToMaybe (Some a) = return (Just a)
{-#INLINE optionToMaybe #-}

{-| Get the first character of a text stream or return 'Nothing' if the stream
    is empty
-}
head :: Monad m => FoldM m a (Maybe a)
head =  FoldM step (return None) optionToMaybe
  where
    step o a = case o of
      None -> return $! Some a
      Some a' -> return $! Some a'
{-# INLINE head #-}

{-| Get the last character of a text stream or return 'Nothing' if the text
    stream is empty
-}
last :: Monad m => FoldM m a (Maybe a)
last = FoldM step (return None) optionToMaybe
  where
    step !_ a = return (Some a)
{-# INLINE last #-}


{-| Get the last element of a container or return a default value if the container
    is empty
-}
lastDef :: Monad m => a -> FoldM m a a
lastDef a = FoldM (\_ a' -> return $! a') (return $! a) return
{-# INLINE lastDef #-}

{-| Return the last N elements
-}
lastN :: Monad m => Int -> FoldM m a [a]
lastN n = FoldM step (return $! Seq.empty) (return . F.toList)
  where
    step s a = return $! if Seq.length s < n then s |> a else Seq.drop 1 s |> a
{-# INLINE lastN #-}

-- | Returns 'True' if the text stream is empty, 'False' otherwise
null :: Monad m => FoldM m a Bool
null = FoldM (\_ _ -> return False) (return True) return
{-# INLINE null #-}

-- | Return the length of the text stream in characters
length :: (Monad m) => FoldM m a Int
length = FoldM (\n _ -> return $! n + 1) (return 0) return
{-# INLINE length #-}

{-| @(all predicate)@ returns 'True' if all characters satisfy the predicate,
    'False' otherwise
-}
all ::  Monad m => (a -> Bool) -> FoldM m a Bool
all thus = FoldM step (return True) return
 where 
   step b a = return $! thus a `and_` b
{-# INLINE all #-}

{-| @(any predicate)@ returns 'True' if any character satisfies the predicate,
    'False' otherwise
-}
any ::  Monad m => (a -> Bool) -> FoldM m a Bool
any thus = FoldM step (return False) return
 where 
   step b a = return $! thus a `or_` b
{-# INLINE any #-}

or_ :: Bool -> Bool -> Bool
or_ !x !y = x || y   -- improves time for `any`
{-#INLINE or_ #-}

and_ :: Bool -> Bool -> Bool
and_ !x !y = x || y   -- improves time for `any`
{-#INLINE and_ #-}

-- | Returns 'True' if all elements are 'True', 'False' otherwise
and :: Monad m => FoldM m Bool Bool
and = FoldM (\a b -> return $! a && b) (return True) return
{-# INLINE and #-}

-- | Returns 'True' if any element is 'True', 'False' otherwise
or :: Monad m => FoldM m Bool Bool
or = FoldM (\a b -> return $! a `or_` b) (return False) return
{-# INLINE or #-}

-- | Calculate the sum of all the elements
sum :: (Monad m, Num n) => FoldM m n n
sum = FoldM (\a -> return . (a +)) (return 0) return
{-#INLINE sum #-}

-- | Calculate the product of all the elements
product :: (Monad m, Num n) => FoldM m n n
product = FoldM (\a b -> return $! a * b) (return 1) return
{-#INLINE product #-}

-- | Computes the maximum character
maximum :: (Monad m, Ord a) => FoldM m a (Maybe a)
maximum = FoldM step (return None) optionToMaybe
  where
    step o a = return (Some (case o of
      None -> a
      Some b -> max a b))
{-# INLINE maximum #-}

{-| Computes the maximum element with respect to the given comparison
    function
-}
maximumBy :: Monad m  => (a -> a -> Ordering) -> FoldM m a (Maybe a)
maximumBy cmp = FoldM step (return None) optionToMaybe 
  where
    step None y     = return (Some y)
    step (Some x) y = case cmp x y of
        GT -> return (Some x)
        _  -> return (Some y)
    {-#INLINE step #-}
{-# INLINABLE maximumBy #-}


-- | Computes the minimum 
minimum :: (Monad m, Ord a) => FoldM m a (Maybe a)
minimum = FoldM step_ (return None) optionToMaybe 
  where
  step_ o a = case o of 
    None -> return (Some a)
    Some b -> case min a b of c -> return (Some c)
  {-#INLINE step_ #-}
{-# INLINE minimum #-}



{-| Computes the minimum element with respect to the given comparison
    function
-}
minimumBy :: Monad m => (a -> a -> Ordering) -> FoldM m a (Maybe a)
minimumBy cmp = FoldM step (return None) optionToMaybe 
  where
    step None y     = return (Some y)
    step (Some x) y = case cmp x y of
        GT -> return (Some y)
        _  -> return (Some x)
    {-#INLINE step #-}
{-# INLINABLE minimumBy #-}

{-| @(elem c)@ returns 'True' if the text stream has a character equal to @c@,
    'False' otherwise
-}
elem :: (Monad m, Eq a) => a -> FoldM m a Bool
elem c = any (c ==)
{-# INLINE elem #-}

{-| @(notElem c)@ returns 'False' if the text stream has a character equal to
    @c@, 'True' otherwise
-}
notElem :: (Monad m, Eq a) => a -> FoldM m a Bool
notElem c = all (c /=)
{-# INLINABLE notElem #-}

{-| @(find predicate)@ returns the first character that satisfies the predicate
    or 'Nothing' if no character satisfies the predicate
-}
find :: Monad m => (a -> Bool) -> FoldM m a (Maybe a)
find thus = FoldM step (return None) optionToMaybe
  where
    step o !a = case o of
      None -> if thus a then return (Some a) else return None
      x    -> return x
{-# INLINABLE find #-}

{-| @(index n)@ returns the @n@th character of the text stream, or 'Nothing' if
    the stream has an insufficient number of characters
-}

index :: Monad m => Int -> FoldM m a (Maybe a)
index n = FoldM step (return (NotYet n)) done  where
  step s a = return $ case s of
    NotYet 0 -> Yet a
    NotYet m -> NotYet (m-1)
    Yet b    -> Yet b
  done (NotYet _ ) = return Nothing
  done (Yet a)     = return (Just a)

{-#INLINE index #-}

data IndexSt a = NotYet {-#UNPACK#-} !Int | Yet !a
indexToMaybe :: Monad m => IndexSt a -> m (Maybe a)
indexToMaybe c = case c of
  NotYet _ -> return Nothing
  Yet a    -> return (Just a)

{-| @(elemIndex c)@ returns the index of the first character that equals @c@,
    or 'Nothing' if no character matches
-}
elemIndex :: (Monad m, Eq a) => a -> FoldM m a (Maybe Int)
elemIndex c = findIndex (c ==)
{-# INLINABLE elemIndex #-}

{-| @(findIndex predicate)@ returns the index of the first character that
    satisfies the predicate, or 'Nothing' if no character satisfies the
    predicate
-}
findIndex :: Monad m =>  (a -> Bool) -> FoldM m a (Maybe Int)
findIndex thus = FoldM step (return (NotYet 0)) indexToMaybe
  where
    step x a = return $! case x of
        NotYet n -> if thus a then Yet n else NotYet (n+1)
        _       -> x
{-# INLINABLE findIndex #-}

-- | @(count c)@ returns the number of times @c@ appears
count :: (Monad m, Eq a) => a -> FoldM m a Int
count c = FoldM step (return 0) return
  where
    step n a = if c == a then return $! n + 1 else return n
{-# INLINE count #-}


-- | Like 'length', except with a more general 'Num' return value
genericLength :: (Monad m, Num b) => FoldM m a b
genericLength = FoldM (\n _ -> return $!(n + 1)) (return 0) return
{-# INLINE genericLength #-}

data Or a b = InL !a | InR !b
-- | Like 'index', except with a more general 'Integral' argument
genericIndex :: (Monad m, Integral i) => i -> FoldM m a (Maybe a)
genericIndex i = FoldM step (return (InL 0)) done
  where
    step x a = return $! case x of
        InL  j -> if i == j then InR a else InL (j + 1)
        _      -> x
    done x = return (case x of
        InL  _ -> Nothing
        InR a -> Just a)
{-# INLINE genericIndex #-}

premap :: (a -> b) -> FoldM m b r -> FoldM m a r
premap = L.premapM

revList :: Monad m => FoldM m a [a]
revList = FoldM (\xs x -> return (x:xs)) (return []) return


-- newtype Update m a b = Update {runUpdate :: a -> m a}
-- instance Monad m => Monoid (Update m a b) where
--   mempty = Update return
--   {-#INLINE mempty #-}
--   mappend (Update o) (Update u) = Update (o >=> u)
--   {-#INLINE mappend #-}
--
-- instance Monad m => Functor (Update m a) where
--   fmap _ (Update g) = Update g
--   {-#INLINE fmap #-}
--
-- instance Monad m => Applicative (Update m a) where
--    pure _ = Update return
--    {-#INLINE pure #-}
--    Update ff <*> Update fx = Update (ff >=> fx)
--    {-#INLINE (<*>) #-}
--
-- instance Contravariant (Update m a) where
--   contramap _ (Update done) = Update done
--
-- type Handler m a b = forall x . (b -> Update m x b) -> a -> Update m x a

handles :: Monad m => HandlerM m a b -> FoldM m b r -> FoldM m a r
handles = L.handlesM 
{-#INLINE handles #-}

-- simplified IO nonsense

toHandle :: (IOData.IOData a, MonadIO m) => IO.Handle -> FoldM m a ()
toHandle h = FoldM (\() bs -> IOData.hPut h bs) (return ()) return 

stdout :: (IOData.IOData a, MonadIO m) => FoldM m a ()
stdout = toHandle IO.stdout

stderr :: (IOData.IOData a, MonadIO m) => FoldM m a ()
stderr = toHandle IO.stderr

toHandleLn :: (IOData.IOData a, MonadIO m) => IO.Handle -> FoldM m a ()
toHandleLn h = FoldM (\() bs -> IOData.hPutStrLn h bs) (return ()) return

stdoutLn :: (IOData.IOData a, MonadIO m) => FoldM m a ()
stdoutLn = toHandleLn IO.stdout

stderrLn :: (IOData.IOData a, MonadIO m) => FoldM m a ()
stderrLn = toHandleLn IO.stderr
