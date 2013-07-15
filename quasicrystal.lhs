Here is a simplified (but complete!) program, which renders a single still image.

> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE BangPatterns #-}

> import Data.Array.Repa ( Array, DIM2, DIM3, Z(..), (:.)(..))
> import Data.Array.Repa.Repr.ForeignPtr ()
> import qualified Data.Array.Repa          as R
> import qualified Data.Array.Repa.IO.DevIL as D
> import Data.Array.Repa.Unsafe (unsafeTraverse)
>
> import Text.Printf
> import Data.Word  ( Word8   )
> import Data.Fixed ( divMod' )
> import System.Environment (getArgs)
> import System.Console.GetOpt
> import Data.Maybe (fromMaybe)

For clarity, we define a few type synonyms:

> type R     = Float
> type R2    = (R, R)
> type Angle = R

We'll convert pixel indices to coordinates in the real plane, with
origin at the image center. We have to decide how many pixels to draw,
and how much of the plane to show.

> pixels :: Int
> pixels = 1920

> scale :: R
> scale = 512

Repa's array indices are "snoc lists" of the form (Z :. x :. y). By
contrast, our planar coordinates are conventional tuples.

> {-# INLINE point #-}
> point :: R -> DIM2 -> R2
> point !scl = \(Z :. x :. y) -> (adj x, adj y) where
>     adj n = scl * ((2 * fromIntegral n / denom) - 1)
>     denom = fromIntegral pixels - 1

A single wave is a cosine depending on x and y coordinates in some
proportion, determined by the wave's orientation angle.

To combine several functions, we sum their outputs, and wrap to
produce a result between 0 and 1. As n increases, (wrap n) will rise
to 1, fall back to 0, rise again, and so on. sequence converts a list
of functions to a function returning a list, using the monad instance
for ((->) r).

> {-# INLINE combine #-}
> combine :: [R2 -> R] -> (R2 -> R)
> combine !xs = wrap . sum . sequence xs where
>     wrap n = case ((divMod' n 1) :: (Int, R)) of
>         (k, v) | odd k     -> 1-v
>                | otherwise -> v

To draw the quasicrystal, we combine waves at 7 angles evenly spaced
between 0 and π.

> {-# INLINE angles #-}
> angles :: Int -> [Angle]
> angles !n = take n $ enumFromThen 0 (pi / fromIntegral n)

We convert an array of floating-point values to an image in two
steps. First, we map floats in [0,1] to bytes in [0,255]. Then we copy
this to every color channel. The result is a 3-dimensional array,
indexed by (row, column, channel). repa-devil takes such an array and
outputs a PNG image file.

> toImage :: Array R.D DIM2 R -> Array R.D DIM3 Word8
> toImage !arr = unsafeTraverse arr8 (:. 4) chans where
>     arr8 = R.map (floor . (*255) . min 1 . max 0) arr
>     chans _ (Z :. _ :. _ :. 3) = 255  -- alpha channel
>     chans a (Z :. x :. y :. _) = a (Z :. x :. y)

> {-# INLINE wave' #-}
> wave' :: R -> Angle -> R2 -> R
> wave' !off !th !t = f t where
>     (cth, sth) = (cos th, sin th)
>     f (x,y) = (cos (cth*x + sth*y) + off) / 2

quasicrystal' :: Float -> Int -> DIM2 -> R
quasicrystal' !offset !count = combine (map (wave' offset) (angles count)) . point scale

> {-# INLINE quasicrystal'' #-}
> quasicrystal'' :: [Float] -> Int -> DIM2 -> R
> quasicrystal'' !offsets !count !sh =
>       combine (zipWith wave' (cycle offsets) (angles count)) . point scale $ sh

> step :: Float
> step = 0.3

> doStep :: Int -> Float -> IO ()
> doStep nw offset = do
>      let arr = R.fromFunction (Z :. pixels :. pixels) $ quasicrystal'' [0, 0, 0, offset, 0, 0, 0] nw
>      pic <- R.computeP $ toImage arr
>      D.runIL $ D.writeImage (mkFilename offset) (D.RGBA pic)
>      putStrLn $ show this ++ " of " ++ show final ++ " complete."
>               where
>                 this, final :: Int
>                 this = round $ offset * 1/step
>                 final = round $ 2 * pi / step

> mkFilename :: Float -> String
> mkFilename number = "out_" ++ printf "%.5d" stepnum ++ ".png"
>   where
>     stepnum :: Int
>     stepnum = round $ number * 1/step

data Flag
     = Verbose  | Version
     | Input String | Output String | LibDir String
       deriving Show

options :: [OptDescr Flag]
options =
 [ Option ['v']     ["verbose"] (NoArg Verbose)       "chatty output on stderr"
 , Option ['V','?'] ["version"] (NoArg Version)       "show version number"
 , Option ['o']     ["output"]  (OptArg outp "FILE")  "output FILE"
 , Option ['c']     []          (OptArg inp  "FILE")  "input FILE"
 , Option ['L']     ["libdir"]  (ReqArg LibDir "DIR") "library directory"
 ]

inp,outp :: Maybe String -> Flag
outp = Output . fromMaybe "stdout"
inp  = Input  . fromMaybe "stdin"

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: ic [OPTION...] files..."


> main :: IO ()
> main = do
>      args <- getArgs
>      nw <- case args of
>                    [] -> putStrLn "Defaulting to 7 waves!" >> return 7
>                    (x:_) -> return $ read x
>      mapM_ (\x -> doStep nw x) [step, step * 2 .. 2 * pi]




NOTE: Seems like some animations online shift the phase of a subset of
waves.  Consider exploring.  Additional options: add colors!  Bring
out wave count, scale, offset size and subset of waves to offset on
command line, and maybe wrap with a little shell script for making
videos.  Also allow different periods for different waves.
