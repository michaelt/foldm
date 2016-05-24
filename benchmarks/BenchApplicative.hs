{-#LANGUAGE BangPatterns #-}
module Main where
import Control.Foldl (Fold(..),FoldM(..))
import qualified Control.Foldl as L
import qualified Control.FoldM as M
import Control.Applicative
import Pipes
import qualified Pipes.Prelude as P
import qualified Data.Vector.Unboxed as V
-- import Streaming
-- import qualified Streaming.Prelude as S
import Criterion.Main
import Data.Functor.Identity
-- import Data.Foldable (find)
-- import Data.List (elemIndex)

main :: IO ()
main = do
  let size :: Int
      size = 10

      vfold :: Fold Int a -> V.Vector Int -> a
      vfold = \(Fold step begin done) v -> done (V.foldl step begin v)
      {-#INLINE vfold #-}
      
      vfoldM :: FoldM Identity Int a -> V.Vector Int -> a
      vfoldM = \(FoldM step begin done) vec -> runIdentity ( 
          begin >>= flip (V.foldM step) vec >>= done )
      {-#INLINE vfoldM #-}


      lfoldM :: FoldM Identity Int a -> [Int] -> a
      lfoldM  = \f p -> runIdentity (L.foldM f p)
      {-#INLINE lfoldM #-}
      
      pfold :: Fold Int a -> Producer Int Identity () -> a
      pfold = \f p -> runIdentity (L.purely P.fold f p)
      {-#INLINE pfold #-}
      
      pfoldM :: FoldM Identity Int a -> Producer Int Identity () -> a
      pfoldM = \f p -> runIdentity (L.impurely P.foldM f p)
      {-#INLINE pfoldM #-}
      
      p /\ q = p (whnf q (replicate size (1::Int)))
      {-#INLINE (/\) #-}

      p \/ q = p (whnf q (V.replicate size (1::Int)))
      {-#INLINE (\/) #-}


      p >>> q = p (whnf q (each (replicate size 1) :: Producer Int Identity ()))
      {-#INLINE (>>>) #-}
                        
                        
      onefold, twofolds, threefolds :: Fold Int Int
      onefold = L.sum
      {-#INLINE onefold #-}
      twofolds = liftA2 (+) onefold onefold
      {-#INLINE twofolds #-}
      threefolds = liftA2 (+) onefold twofolds
   --   threefolds = liftA3 (\x y z -> x + y + z) onefold onefold onefold
      {-#INLINE threefolds #-}
      
      onefoldM, twofoldMs, threefoldMs :: Monad m => FoldM m Int Int
      onefoldM =  M.sum
      {-#INLINE onefoldM #-}
      twofoldMs = liftA2 (+) onefoldM onefoldM
      {-#INLINE twofoldMs #-}
      threefoldMs = liftA2 (+) onefoldM twofoldMs
    --  threefoldMs = liftA3 (\x y z -> x + y + z) onefoldM onefoldM onefoldM
      {-#INLINE threefoldMs #-}
      
      vector = V.replicate size (1::Int)
      list   = replicate size (1::Int)

  defaultMain 
   [bgroup "unfusable"
     [  bgroup "pure"  [
            bgroup "vector" [ bench "onefold" $ whnf (flip vfold vector) onefold
                            , bench "twofolds" $ whnf (flip vfold vector) twofolds
                            , bench "threefolds" $ whnf (flip vfold vector) threefolds
                            ]
           , bgroup "list" [ bench "onefold" $ whnf (flip L.fold list) onefold
                            , bench "twofolds" $ whnf (flip L.fold list) twofolds
                            , bench "threefolds" $ whnf (flip L.fold list) threefolds
                            ]
                            ]
      , bgroup "impure" [
            bgroup "vector" [ bench "onefold" $ whnf (flip vfoldM vector) onefoldM
                            , bench "twofolds" $ whnf (flip vfoldM vector) twofoldMs
                            , bench "threefolds" $ whnf (flip vfoldM vector) threefoldMs
                            ]
           , bgroup "list" [ bench "onefold" $ whnf (flip M.fold list) onefoldM
                          , bench "twofolds" $ whnf (flip M.fold list) twofoldMs
                            , bench "threefolds" $ whnf (flip M.fold list) threefoldMs
                            ]

                       ]
       ]
   , bgroup "fusable" 
        [bgroup "list"
             [bgroup "pure" [ bench "onefold" /\ L.fold onefold
                            ,  bench "twofolds" /\ L.fold twofolds
                            ,  bench "threefolds" /\ L.fold threefolds
                            ]
             , bgroup "impure" 
                            [ bench "onefold" /\ lfoldM onefoldM
                            ,  bench "twofolds" /\ lfoldM twofoldMs
                            ,  bench "threefolds" /\ lfoldM threefoldMs
                            ]
             ]
        , bgroup "vector"
             [bgroup "pure" [ bench "onefold" \/ vfold onefold
                            ,  bench "twofolds" \/ vfold twofolds
                            ,  bench "threefolds" \/ vfold threefolds
                            ]
             , bgroup "impure" 
                            [ bench "onefold" \/ vfoldM onefoldM
                            ,  bench "twofolds" \/ vfoldM twofoldMs
                            ,  bench "threefolds" \/ vfoldM threefoldMs
                            ]
     
             ]
      
      ]
      , bgroup "pipes" 
        [bgroup "pure" [ bench "onefold" >>> pfold onefold
                       ,  bench "twofolds" >>> pfold twofolds
                       ,  bench "threefolds" >>> pfold threefolds
                       ]
        , bgroup "impure" 
                       [ bench "onefold" >>> pfoldM onefoldM
                       ,  bench "twofolds" >>> pfoldM twofoldMs
                       ,  bench "threefolds" >>> pfoldM threefoldMs
                       ]

        ]
   ]
   --
   -- pfold :: Fold Int a -> Producer Int Identity () -> a
   -- pfold = \f p -> runIdentity (L.purely P.fold f p)
   -- {-#INLINE pfold #-}
   --
   -- pfoldM :: FoldM Identity Int a -> Producer Int Identity () -> a
   -- pfoldM = \f p -> runIdentity (L.impurely P.foldM f p)
   -- {-#INLINE pfoldM #-}
   --
   -- sfold :: Fold Int a -> Stream (Of Int)  Identity () -> (Of a ())
   -- sfold = \f p -> runIdentity (L.purely S.fold f p)
   -- {-#INLINE sfold #-}
   --
   -- sfoldM :: FoldM Identity Int a -> Stream (Of Int)  Identity () -> (Of a ())
   -- sfoldM = \f p -> runIdentity (L.impurely S.foldM f p)
   -- {-#INLINE sfoldM #-}