{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Quasicrystals
--
-- Maintainer  :  peddie@alum.mit.edu
-- Stability   :  experimental
-- Portability :  GHC
--
-- This code is based on Keegan McAllister's code from the blog post
-- http://mainisusuallyafunction.blogspot.com/2011/10/quasicrystals-as-sums-of-waves-in-plane.html.
-- This version is somewhat simplified and uses a different function
-- to combine waves (a cosine rather than a hard zigzag).
--
-- I was trying to learn about Repa and how to make code fast.  This
-- code turns out to take roughly 1.8 times as long as a similarly
-- idiomatic single-threaded C program, but it regains parity with
-- +RTS -N on my (hyperthreaded) quad-core laptop.  The main takeaway
-- from the C experiment is that realtime animations are probably out
-- of the question (until a GPU backend materializes for DPH/Repa).
-- SIMD code would probably help both C and Haskell, but this might
-- happen automatically in GHC 7.8.  Interestingly, enabling
-- -ffast-math in the C program cuts its runtime by a factor of about
-- 5, but I couldn't figure out how to pass the equivalent flag to the
-- LLVM backend.
--
-- Sometimes the C code takes ~13 or 14 seconds to run the calculation
-- loop.  I haven't figured out what causes this!  It's 4 AM.
--
-- Compilation command:
--
--   ghc --make -Odph -rtsopts -threaded -funbox-strict-fields          \
--   -fno-liberate-case -funfolding-use-threshold1000                   \
--   -funfolding-keeness-factor1000 -optlo-O3 -fllvm qc.hs
--
-- Run command:
--
--   ./qc +RTS -N -s
--
-- Beware -- it tries to create PNG output files in the current
-- directory, but if one of these files already exists when it tries
-- to write the output, DevIL will cause it to exit immediately.
--
-- Animation generation command (assuming you rendered the full
-- 1920x1920 image originally and want to crop):
--
--   mencoder -ovc x264 -x264encopts                                           \
--   threads=auto:subq=5:8x8dct:frameref=2:bframes=3:b_pyramid=normal:weight_b \
--   -of lavf -vf crop=1920:1200 mf://*.png -ofps 25 -o waves.mkv
--
-- You can turn it into a gif as well, but beware -- it may be giant
-- and slow to display.  Consider cropping or scaling to 480x300 instead.
--
--   mplayer -vo gif89a:fps=22:output=waves_tmp.gif -ao null -vf        \
--   crop=960:600 waves.mkv
--
--   convert -delay 17 waves_tmp.gif waves.gif
-----------------------------------------------------------------------------

import Data.Array.Repa ( Array, DIM2, DIM3, Z(..), (:.)(..))
import Data.Array.Repa.Repr.ForeignPtr ()
import qualified Data.Array.Repa          as R
import qualified Data.Array.Repa.IO.DevIL as D
import Data.Array.Repa.Unsafe (unsafeTraverse)

import Text.Printf
import Data.Word  ( Word8   )
import Data.Fixed ( divMod' )

-- TODO(MP): Take in command-line args.
-- import System.Environment (getArgs)
-- import System.Console.GetOpt
-- import Data.Maybe (fromMaybe)

{-# INLINE angles #-}
-- | Subdivide the top semicircle into n directions.
angles :: Int -> [Float]
angles n = take n $ enumFromThen 0 (pi / fromIntegral n)

{-# INLINE sumWaves #-}
-- | Combine a list of wave functions into a single wave function.
-- The unused argument is the number of waves.
sumWaves :: Float -> [DIM2 -> Float] -> DIM2 -> Float
sumWaves _ !waves = (\x -> (cos (pi*x) + 1) / 2) . sum . sequence waves

{-# INLINE dm1 #-}
-- | Here's a helper function for computing the hard zigzag.
dm1 :: Float -> (Int, Float)
dm1 x = (dd, x - fromIntegral dd)
  where dd = floor (toRational x)

{-# INLINE sumWaves' #-}
-- | This version of sumWaves does a nonlinear (hard) zigzag wrap,
-- which looks brighter and more interesting but takes about 3 times
-- as much time.
sumWaves' :: Float -> [DIM2 -> Float] -> DIM2 -> Float
sumWaves' _ !waves = wrap . sum . sequence waves
   where
     wrap m = case dm1 m of
         (k, v) | odd k     -> 1-v
                | otherwise -> v

{-# INLINE wave #-}
-- | Compute the value of a single wave (specified by frequency, phase
-- and angle within the plane) at a point in the image.
wave :: Float -> Float -> Float -> DIM2 -> Float
wave freq phase planeAngle (Z :. x' :. y') = (/2) $
     cos (freq * (cx + sy) + phase) + 1
     where
       x = x' - (fst imageSize `div` 2)
       y = y' - (snd imageSize `div` 2)
       cx = cos planeAngle * fromIntegral x
       sy = sin planeAngle * fromIntegral y

{-# INLINE quasicrystal #-}
-- | Compute a single pixel value.
quasicrystal :: Int -> Float -> [Float] -> DIM2 -> Float
quasicrystal n freq !phases !point = sumWaves (fromIntegral n) ws point
  where
   ws = zipWith (wave freq) phases $ angles n

-- | Create a delayed Repa array describing the entire image in
-- floating-point.
mkArray :: Int -> Float -> [Float] -> (Int, Int) -> Array R.D DIM2 Float
mkArray n freq phases (x, y) = R.fromFunction (Z :. x :. y) $
           quasicrystal n freq phases

-- | Convert a floating-point image to a 32-bit RGBA image.
toImage :: Array R.D DIM2 Float -> Array R.D DIM3 Word8
toImage !arr = unsafeTraverse arr8 (:. 4) chans where
    arr8 = R.map (floor . (*255) . min 1 . max 0) arr
    chans _ (Z :. _ :. _ :. 3) = 255  -- alpha channel
    chans a (Z :. x :. y :. _) = a (Z :. x :. y)

-- | Image dimensions, reversed from the customary.  This is global
-- for now to simplify passing this data through the chain all the way
-- to `wave`.
imageSize :: (Int, Int)
imageSize = (1200, 1920)

-- | 3, 2, 1, let's jam!
main :: IO ()
main = do
  -- TODO(MP): Take in at least nwave and frequency on the command
  -- line.  If animated, also take in a specification for phase
  -- progression.
  let nwave = 11
      frequency = 1.0 / 3.0 :: Float
      phases = [0,0.1..]
      arr = mkArray nwave frequency (take nwave $ cycle phases) imageSize
  putStrLn $
   printf "Rendering %dx%d pixel image with %d waves and frequency %f pixels."
              (snd imageSize)
              (fst imageSize)
              nwave
              frequency
  -- TODO(MP): Loop through phases!
  pic <- R.computeP $ toImage arr
  D.runIL $ D.writeImage "out.png" $ D.RGBA pic
