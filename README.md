Quasicrystals from plane waves
=========

This code is based on code from [Keegan McAllister's
blog](http://mainisusuallyafunction.blogspot.com/2011/10/quasicrystals-as-sums-of-waves-in-plane.html).
This version is somewhat simplified, more flexible and uses a faster
function to combine waves (a cosine rather than a hard zigzag).

I was trying to learn about Repa and how to make code fast.  This code
turns out to take roughly 1.8 times as long as a similarly idiomatic
single-threaded C program, but it regains parity with `+RTS -N` on my
(hyperthreaded) quad-core laptop.  The main takeaway from the C
experiment is that realtime animations are probably out of the
question (until a GPU backend materializes for DPH/Repa).  SIMD code
would probably help both C and Haskell, but this might happen
automatically in GHC 7.8.  Interestingly, enabling `-ffast-math` in
the C program cuts its runtime by a factor of about 5, but I couldn't
figure out how to pass the equivalent flag to the LLVM backend.

Compile
--------

    ghc --make -Odph -rtsopts -threaded -funbox-strict-fields          \
    -fno-liberate-case -funfolding-use-threshold1000                   \
    -funfolding-keeness-factor1000 -optlo-O3 -fllvm qc.hs

Run
-------

Just go (takes a while):

    ./qc +RTS -N

Learn about more options:

    ./qc --help

Beware -- it tries to create PNG output files in the current
directory, but if one of these files already exists when it tries
to write the output, DevIL will cause it to exit immediately.

Generate a movie from animation frames
-------

    mencoder -ovc x264 -x264encopts                                           \
    threads=auto:subq=5:8x8dct:frameref=2:bframes=3:b_pyramid=normal:weight_b \
    -of lavf mf://*.png -ofps 25 -o waves.mkv

Generate a gif from animation frames
-------

You can turn it into a gif as well, but beware -- it may be giant and
slow to display.  Consider cropping (-vf crop=w:h) or scaling (-vf
scale=w:h) to 480x300 or so instead.  Make a gif with mplayer:

    mplayer -vo gif89a:fps=22:output=waves_tmp.gif -ao null -vf        \
    crop=960:600 -speed 22 waves.mkv

Fix the delays with imagemagick:

    convert -delay 9 waves_tmp.gif waves.gif

Clean up:

    rm waves_tmp.gif

Build and run the C program for comparison
-------

    gcc -std=gnu99 -Wall -Wextra -lm -lrt -lfreeimage -O3         \
       -ftree-vectorize -march=native -o quasi quasicrystal.c
    time ./quasi

You can also follow the comments in `quasicrystal.c` to compare
images.
