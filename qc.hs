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
-- and slow to display.  Consider cropping (-vf crop=w:h) or scaling
-- (-vf scale=w:h) to 480x300 instead.
--
--   mplayer -vo gif89a:fps=22:output=waves_tmp.gif -ao null -vf        \
--   crop=960:600 -speed 22 waves.mkv
--
--   convert -delay 9 waves_tmp.gif waves.gif
-----------------------------------------------------------------------------

import Data.Array.Repa ( Array, DIM2, DIM3, Z(..), (:.)(..))
import Data.Array.Repa.Repr.ForeignPtr ()
import qualified Data.Array.Repa          as R
import qualified Data.Array.Repa.IO.DevIL as D
import Data.Array.Repa.Unsafe (unsafeTraverse)

import System.Environment (getArgs)
import System.Console.GetOpt
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Text.Printf
import Data.Word  (Word8)

{-# INLINE angles #-}
-- | Subdivide the top semicircle into n directions.
angles :: Int -> [Float]
angles n = take n $ enumFromThen 0 (pi / fromIntegral n)

{-# INLINE sumWaves #-}
-- | Combine a list of wave functions into a single wave function.
-- The unused argument is the number of waves.
sumWaves :: Float -> [DIM2 -> Float] -> DIM2 -> Float
sumWaves _ !waves = (\x -> (cos (pi*x) + 1) / 2) . sum . sequence waves

{-# INLINE wave #-}
-- | Compute the value of a single wave (specified by frequency, phase
-- and angle within the plane) at a point in the image.
wave :: (Int, Int) -> Float -> Float -> Float -> DIM2 -> Float
wave imageSize freq phase planeAngle (Z :. x' :. y') = (/2) $
     cos (freq * (cx + sy) + phase) + 1
     where
       x = x' - (fst imageSize `div` 2)
       y = y' - (snd imageSize `div` 2)
       cx = cos planeAngle * fromIntegral x
       sy = sin planeAngle * fromIntegral y

{-# INLINE quasicrystal #-}
-- | Compute a single pixel value.
quasicrystal :: (Int, Int) -> Int -> Float -> [Float] -> DIM2 -> Float
quasicrystal sz n freq !phases !point = sumWaves (fromIntegral n) ws point
  where
   ws = zipWith (wave sz freq) phases $ angles n

-- | Create a delayed Repa array describing the entire image in
-- floating-point.
mkArray :: Int -> Float -> [Float] -> (Int, Int) -> Array R.D DIM2 Float
mkArray n freq phases (x, y) = R.fromFunction (Z :. x :. y) $
           quasicrystal (x, y) n freq phases

-- | Convert a floating-point image to a 32-bit RGBA image.
toImage :: Array R.D DIM2 Float -> Array R.D DIM3 Word8
toImage !arr = unsafeTraverse arr8 (:. 4) chans where
    arr8 = R.map (floor . (*255) . min 1 . max 0) arr
    chans _ (Z :. _ :. _ :. 3) = 255  -- alpha channel
    chans a (Z :. x :. y :. _) = a (Z :. x :. y)

-- | Standardized (zero-padded) filename construction
mkFilename :: String -> Int -> String
mkFilename dir stepnum = printf "%s/out_%.5d.png" dir stepnum

-- | Render a single still image
doStep :: Options -> Int -> IO ()
doStep opts stepn = do
      let nwave = optWaveCount opts
          frequency = optFrequency opts
          phase0 = replicate nwave 0
          phases = zipWith (+) phase0 $
                   map (* fromIntegral stepn) $
                   cycle $ optPhaseDeltas opts
          imageSize = optImageSize opts
          dirout = optOutputDir opts
          arr = mkArray nwave frequency (take nwave $ cycle phases) imageSize
      pic <- R.computeP $ toImage arr
      D.runIL $ D.writeImage (mkFilename dirout stepn) $ D.RGBA pic

-- | 3, 2, 1, let's jam!
main :: IO ()
main = do
  (opts, _) <- getArgs >>= compilerOpts
  let nwave = optWaveCount opts
      frequency = optFrequency opts
      imageSize = optImageSize opts
      finalAngle = optAngle opts
      usefulStep = minimum $ filter (>0) $ optPhaseDeltas opts
      steps = [1..ceiling (finalAngle / usefulStep)]
  putStrLn $
   printf "Rendering %dx%d pixels for %d steps with %d waves and frequency %f pixels."
              (snd imageSize)
              (fst imageSize)
              (last steps)
              nwave
              frequency
  mapM_ (doStep opts) steps

data Options = Options
     { optWaveCount         :: Int
     , optFrequency         :: Float
     , optPhaseDeltas       :: [Float]
     , optAngle             :: Float
     , optOutputDir         :: String
     , optImageSize         :: (Int, Int)
     } deriving Show

defaultOptions     :: Options
defaultOptions      = Options
   { optWaveCount   = 7
   , optFrequency   = 0.4
   , optPhaseDeltas = [0.03]
   , optAngle       = 2*pi
   , optOutputDir   = "."
   , optImageSize   = (1200, 1920)
   }

withDefault :: Show a => String -> (Options -> a) -> String
withDefault str def = str ++ " (" ++ show (def defaultOptions) ++ ")"

splitOn :: String -> String -> [String]
splitOn p s = case dropWhile (`elem` p) s of
                "" -> []
                s' -> w : splitOn p s''
                       where (w, s'') = break (`elem` p) s'

parseImageSize :: String -> Maybe (Int, Int)
parseImageSize szstr = case splitOn ",x" szstr of
                         [w, h] -> Just (read h, read w)
                         _      -> Nothing

parseDeltas :: String -> Maybe [Float]
parseDeltas dstr = case splitOn "," dstr of
                     [] -> Nothing
                     xs -> Just $ map read xs

options :: [OptDescr (Options -> Options)]
options =
 [ Option "cn"     ["wave-count"]
     (ReqArg (\c opts -> opts {optWaveCount = read c}) "COUNT") $
     withDefault "number of waves to combine" optWaveCount
 , Option "p"      ["period"]
     (ReqArg (\p opts -> opts {optFrequency = 1/read p}) "PERIOD") $
     withDefault "wave period in pixels" optFrequency
 , Option "f"      ["frequency"]
     (ReqArg (\f opts -> opts {optFrequency = read f}) "FREQUENCY") $
     withDefault "wave frequency in waves per pixel" optFrequency
 , Option "d"      ["phase-deltas"]
     (ReqArg (\d opts -> opts {optPhaseDeltas =
         fromMaybe (optPhaseDeltas defaultOptions) $ parseDeltas d})
         "d1,d2,...") $
     withDefault "a list of phase deltas per animation step" optPhaseDeltas
 , Option "a"      ["angle"]
     (ReqArg (\a opts -> opts {optAngle = read a}) "ANGLE") $
     withDefault "total phase angle to simulate" optAngle
 , Option "o"      ["output-dir"]
     (ReqArg (\o opts -> opts {optOutputDir = o}) "DIR") $
     withDefault "directory into which to output rendered frames" optOutputDir
 , Option "s"      ["size"]
     (ReqArg (\s opts -> opts {optImageSize =
         fromMaybe (optImageSize defaultOptions) $ parseImageSize s})
         "<width>x<height>") $
     withDefault "output image size in pixels" (swap . optImageSize)
 ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: qc [options]"
