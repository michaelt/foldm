{-#LANGUAGE BangPatterns #-}
module Main where
import Control.Foldl (Fold(..),FoldM(..))
import qualified Control.Foldl as L
import qualified Control.FoldM as M
import Pipes
import qualified Pipes.Prelude as P
import qualified Data.Vector.Unboxed as V
-- import qualified Data.Vector.Generic as VG
-- import qualified Data.Vector.Fusion.Bundle as VB
import Streaming
import qualified Streaming.Prelude as S
import Criterion.Main
import Data.Functor.Identity
import Data.Foldable (find)
import Data.List (elemIndex)

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
        
      pfold :: Fold Int a -> Producer Int Identity () -> a
      pfold = \f p -> runIdentity (L.purely P.fold f p)
      {-#INLINE pfold #-}
      
      pfoldM :: FoldM Identity Int a -> Producer Int Identity () -> a
      pfoldM = \f p -> runIdentity (L.impurely P.foldM f p)
      {-#INLINE pfoldM #-}
      
      sfold :: Fold Int a -> Stream (Of Int)  Identity () -> (Of a ())
      sfold = \f p -> runIdentity (L.purely S.fold f p)
      {-#INLINE sfold #-}

      sfoldM :: FoldM Identity Int a -> Stream (Of Int)  Identity () -> (Of a ())
      sfoldM = \f p -> runIdentity (L.impurely S.foldM f p)
      {-#INLINE sfoldM #-}

      lfoldm :: FoldM Identity Int a -> [Int] -> a
      lfoldm  = \f p -> runIdentity (L.foldM f p)
      {-#INLINE lfoldm #-}
      
      p /// q = p (whnf q (replicate size (1::Int)))
      {-#INLINE (///) #-}
      
      p //// q = p (whnf q (V.replicate size (1::Int)))
      {-#INLINE (////) #-}
      
      p /\ q = p (whnf q (each (replicate size (1::Int)):: Producer Int Identity ()))
--      p /\ q = p (whnf q (P.replicateM size (Identity (1::Int))))
      {-#INLINE (/\) #-}

      p \/ q = p (whnf q (S.each (replicate size (1::Int)):: Stream (Of Int) Identity ()))
--      p /\ q = p (whnf q (P.replicateM size (Identity (1::Int))))
      {-#INLINE (\/) #-}

  defaultMain
   [ bgroup "list"
       [ bgroup "sum" [bench "prelude" /// sum 
                       , bench "pure" /// L.fold L.sum 
                       , bench "impure" /// M.fold M.sum
                       , bench "generalize" /// lfoldm (L.generalize L.sum)
                       ]
       , bgroup "product" [bench "prelude" /// product
                       , bench "pure" /// L.fold L.product 
                       , bench "impure" /// M.fold M.product
                       , bench "generalize" /// lfoldm (L.generalize L.product)
                       ]
       , bgroup "null" [bench "prelude" /// null 
                       , bench "pure" /// L.fold L.null 
                       , bench "impure" /// M.fold M.null 
                       , bench "generalize" /// lfoldm (L.generalize L.null)
                       ]
       , bgroup "length" [bench "prelude" /// length
                       , bench "pure" /// L.fold L.length
                       , bench "impure" /// M.fold M.length
                       , bench "generalize"  /// lfoldm (L.generalize L.length)
                       ]
       , bgroup "minimum" [bench "prelude" /// minimum
                       , bench "pure" /// L.fold L.minimum
                       , bench "impure" /// M.fold M.minimum
                       , bench "generalize" /// lfoldm (L.generalize L.minimum)
                       ]
       , bgroup "maximum" [bench "prelude" /// maximum
                       , bench "pure" /// L.fold L.maximum
                       , bench "impure" /// M.fold M.maximum
                       , bench "generalize" /// lfoldm (L.generalize L.maximum)
                       ]
       , bgroup "any" [bench "prelude" /// any even
                       , bench "pure" /// L.fold (L.any even)
                       , bench "impure" /// M.fold (M.any even)
                       , bench "generalize" /// lfoldm (L.generalize (L.any even))
                       ]
       , bgroup "elem" [bench "prelude" /// elem 12
                       , bench "pure" /// L.fold (L.elem 12)
                       , bench "impure" /// M.fold (M.elem 12)
                       , bench "generalize" /// lfoldm (L.generalize (L.elem 12))
                       ]
       , bgroup "find" [bench "prelude" /// find even
                       , bench "pure" /// L.fold (L.find even)
                       , bench "impure" /// M.fold (M.find even)
                       , bench "generalize" /// lfoldm (L.generalize (L.find even))
                       ]
       , bgroup "index" [bench "prelude" /// (!! (size-1))
                       , bench "pure" /// L.fold (L.index (size-1))
                       , bench "impure" /// M.fold (M.index (size-1))
                       , bench "generalize" /// lfoldm (L.generalize (L.index (size-1)))
                       ]
       , bgroup "elemIndex" [bench "prelude" /// elemIndex (size-1)
                        , bench "pure" /// L.fold (L.elemIndex (size-1))
                        , bench "impure" /// M.fold (M.elemIndex (size-1))
                        , bench "generalize" /// lfoldm (L.generalize (L.elemIndex (size-1)))
                        ]
       ]
   ,  bgroup "vector"  
       [ bgroup "sum" 
                       [bench "prelude" //// V.sum 
                       , bench "pure" //// vfold L.sum
                       , bench "impure" //// vfoldM M.sum
                       , bench "generalize" //// vfoldM (L.generalize L.sum)
                       ]
       , bgroup "product" [bench "prelude" //// V.product 
                       , bench "pure" //// vfold L.product
                       , bench "impure" //// vfoldM M.product
                       , bench "generalize" //// vfoldM (L.generalize L.product)
                       ]
       , bgroup "null" [bench "prelude" //// V.null 
                       , bench "pure" //// vfold L.null
                       , bench "impure" //// vfoldM M.null
                       , bench "generalize" //// vfoldM (L.generalize L.null)
                       ]
       , bgroup "length" [bench "prelude" //// V.length
                       , bench "pure" //// vfold L.length
                       , bench "impure" //// vfoldM M.length
                       , bench "generalize" //// vfoldM (L.generalize L.length)
                       ]
       , bgroup "minimum" [bench "prelude" //// V.minimum 
                       , bench "pure" //// vfold L.minimum
                       , bench "impure" //// vfoldM M.minimum
                       , bench "generalize" //// vfoldM (L.generalize L.minimum)
                       ]
       , bgroup "any" [bench "prelude" //// V.all even
                       , bench "pure" //// vfold (L.all even)
                       , bench "impure" //// vfoldM (M.all even)
                       , bench "generalize" //// vfoldM (L.generalize (L.all even))
                       ]
       , bgroup "elem" [bench "prelude" //// V.elem 12
                       , bench "pure" //// vfold (L.elem 12)
                       , bench "impure" //// vfoldM (M.elem 12)
                       , bench "generalize" //// vfoldM (L.generalize (L.elem 12))
                       ]
       , bgroup "find" [bench "prelude" //// V.find even
                       , bench "pure" //// vfold (L.find even)
                       , bench "impure" //// vfoldM (M.find even)
                       , bench "generalize" //// vfoldM (L.generalize (L.find even))
                       ]
       , bgroup "index" [bench "prelude" //// (V.! (size-1))
                       , bench "pure" //// vfold (L.index (size-1))
                       , bench "impure" //// vfoldM (M.index (size-1))
                       , bench "generalize" //// vfoldM (L.generalize (L.index (size-1)))
                       ]
       , bgroup "elemIndex" [bench "prelude" //// V.elemIndex (size-1)
                        , bench "pure" //// vfold (L.elemIndex (size-1))
                        , bench "impure" //// vfoldM (M.elemIndex (size-1))
                        , bench "generalize" //// vfoldM (L.generalize (L.elemIndex (size-1)))
                        ]
       ]
   ,  bgroup "pipes"  
       [ bgroup "sum" 
                       [bench "prelude" /\ P.sum 
                       , bench "pure" /\ pfold L.sum
                       , bench "impure" /\ pfoldM M.sum
                       , bench "generalize" /\ pfoldM (L.generalize L.sum)
                       ]
       , bgroup "product" [bench "prelude" /\ P.product 
                       , bench "pure" /\ pfold L.product
                       , bench "impure" /\ pfoldM M.product
                       , bench "generalize" /\ pfoldM (L.generalize L.product)
                       ]
       , bgroup "null" [bench "prelude" /\ P.null 
                       , bench "pure" /\ pfold L.null
                       , bench "impure" /\ pfoldM M.null
                       , bench "generalize" /\ pfoldM (L.generalize L.null)
                       ]
       , bgroup "length" [bench "prelude" /\ P.length
                       , bench "pure" /\ pfold L.length
                       , bench "impure" /\ pfoldM M.length
                       , bench "generalize" /\ pfoldM (L.generalize L.length)
                       ]
       , bgroup "minimum" [bench "prelude" /\ P.minimum
                       , bench "pure" /\ pfold L.minimum
                       , bench "impure" /\ pfoldM M.minimum
                       , bench "generalize" /\ pfoldM (L.generalize L.minimum)
                       ]
       ]
--
    , bgroup "streaming"
      [ bgroup "sum"
                    [bench "prelude" \/ S.sum
                    , bench "pure" \/ sfold L.sum
                    , bench "impure" \/ sfoldM M.sum
                    , bench "generalize" \/ sfoldM (L.generalize L.sum)
                    ]
     , bgroup "product" [bench "prelude" \/ S.product
                    , bench "pure" \/ sfold L.product
                    , bench "impure" \/ sfoldM M.product
                    , bench "generalize" \/ sfoldM (L.generalize L.product)
                    ]
    , bgroup "length" [bench "prelude" \/ S.length
                    , bench "pure" \/ sfold L.length
                    , bench "impure" \/ sfoldM M.length
                    , bench "generalize" \/ sfoldM (L.generalize L.length)
                    ]
    , bgroup "minimum" [bench "prelude" \/ S.minimum
                    , bench "pure" \/ sfold L.minimum
                    , bench "impure" \/ sfoldM M.minimum
                    , bench "generalize" \/ sfoldM (L.generalize L.minimum)
                    ]
    ]
   ]


