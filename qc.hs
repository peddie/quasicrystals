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
