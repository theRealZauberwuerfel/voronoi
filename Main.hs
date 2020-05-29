{-# LANGUAGE BangPatterns #-}

{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified System.IO as IO
import System.Random

import Prelude hiding (min, lookup, traverse)

import qualified Data.Array.Repa as Repa
import           Data.Array.Repa.IO.BMP
import           Data.Word

type RGB = (Word8, Word8, Word8)

main :: IO ()
main = do
  str <- IO.getLine
  let
    nsites = read str :: Int
    ctable = genColorTable nsites
  voro <- Repa.computeP 
        $ colorize ctable (voronoi nsites 1024) :: IO (Repa.Array Repa.U Repa.DIM2 RGB)
  writeImageToBMP "out.bmp" voro

sqDistance :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
{-# INLINE sqDistance #-}
sqDistance !x1 !y1 !x2 !y2 = ((x1-x2)^2) + ((y1-y2)^2)

centers :: Int -> Int
        -> Repa.Array Repa.U Repa.DIM2 Word32
centers nCenters nCells =
    Repa.fromListUnboxed (Repa.Z Repa.:. nCenters Repa.:. 2) $ take (2*nCenters) $ randomRs (0, fromIntegral nCells) (mkStdGen 1)

applyReduce2 arr f =
    Repa.traverse arr (\(i  Repa.:. j) -> i) $ \lookup (Repa.Z Repa.:. i) ->
        f (lookup (Repa.Z Repa.:.i Repa.:.0)) (lookup (Repa.Z Repa.:. i Repa.:. 1))

minimize1D array = Repa.foldS f h t
  where
    indexed array' = Repa.traverse array' id (\src idx@(Repa.Z Repa.:. i) -> (src idx, (fromIntegral i)))
    (Repa.Z  Repa.:. n) = Repa.extent array
    iarr = indexed array
    h = iarr Repa.! (Repa.Z Repa.:. 0)
    t = Repa.extract (Repa.Z Repa.:. 1) (Repa.Z Repa.:. (n-1)) iarr

    f min@(!valMin, !_) x@(!val, !_) | val < valMin = x
                                     | otherwise = min

voronoi :: Int
        -> Int
        -> Repa.Array Repa.D Repa.DIM2 Word32
voronoi nCenters nCells =
    let
      {-# INLINE cellReducer #-}
      cellReducer = applyReduce2 (centers nCenters nCells)
      {-# INLINE nearestCenterIndex #-}
      nearestCenterIndex = snd . (Repa.! Repa.Z) . minimize1D
    in
      Repa.fromFunction (Repa.Z Repa.:. nCells Repa.:. nCells :: Repa.DIM2) $ \ (Repa.Z Repa.:. i Repa.:. j) ->
          nearestCenterIndex $ cellReducer (sqDistance (fromIntegral i) (fromIntegral j))

genColorTable :: Int -> Repa.Array Repa.U Repa.DIM1 (Word8, Word8, Word8)
genColorTable n = Repa.fromListUnboxed (Repa.Z  Repa.:. n) $ zip3 l1 l2 l3
    where
      randomPoints = randomRs (0,255) (mkStdGen 1)
      (l1, rest1) = splitAt n randomPoints
      (l2, rest2) = splitAt n rest1
      l3 = take n rest2

colorize :: Repa.Array Repa.U Repa.DIM1 RGB
         -> Repa.Array Repa.D Repa.DIM2 Word32
         -> Repa.Array Repa.D Repa.DIM2 RGB
colorize ctable = Repa.map $ \x ->
    ctable Repa.! (Repa.Z Repa.:. fromIntegral x)
