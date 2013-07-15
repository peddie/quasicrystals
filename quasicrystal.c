/* Copyright 2013 Matthew Peddie <peddie@alum.mit.edu>
 *
 * See README.md for more comments and explanation.
 *
 */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#include <FreeImage.h>

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

typedef struct greyscale_image {
  int width, height;
  float *v;
} greyscale_image;

static greyscale_image *
alloc_img(int width, int height) {
  if (width < 1 || height < 1)
    return NULL;
  greyscale_image *img;
  if ((img = malloc(sizeof(greyscale_image))) == NULL) {
    fprintf(stderr, "imgload:  error allocating %d bytes for an image!\n",
            sizeof(greyscale_image));
    return NULL;
  }
  img->width = width;
  img->height = height;
  const int imgsize = width * height * sizeof(typeof(img->v));
  if ((img->v = malloc(imgsize)) == NULL) {
    fprintf(stderr, "imgload:  error allocating %d bytes for image data!\n",
            imgsize);
    return NULL;
  }
  return img;
}

static int
write_img(char *name, const greyscale_image *img) {
  FIBITMAP *image;
  RGBQUAD aPixel;
  int i,j;

  image = FreeImage_Allocate(img->width, img->height, 24, 0, 0, 0);
  if(!image) {
    perror("FreeImage_Allocate");
    return -1;
  }
  for(i = 0; i < img->height; i++) {
    for(j = 0; j < img->width; j++) {
      float v = img->v[i*img->width + j];
      if (v > 1.0) v = 1.0;
      else if (v < 0) v = 0.0;
      v *= 255.0;

      aPixel.rgbRed = (unsigned char) v;
      aPixel.rgbGreen = (unsigned char) v;
      aPixel.rgbBlue = (unsigned char) v;

      FreeImage_SetPixelColor(image, j, i, &aPixel);
    }
  }
  if(!FreeImage_Save(FIF_JPEG, image, name, 0)) {
    perror("FreeImage_Save");
  }
  FreeImage_Unload(image);

  return 0;
}


static inline float
wave(float freq, float phase, float plane_angle, int x, int y) {
  const float cx = cos(plane_angle) * x,
      sy = sin(plane_angle) * y;
  return (cos(freq * (cx + sy) + phase) + 1) / 2;
}

int
main(void) {
  const unsigned int pixels = 1920, piyels = 1920;
  const unsigned int nwave = 7;

  const float freq = 1.0/5.0;
  const float phases[] = {0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0};

  const float angles[] = {0, M_PI/nwave, 2*M_PI/nwave, 3*M_PI/nwave,
                          4*M_PI/nwave, 5*M_PI/nwave, 6*M_PI/nwave,
                          7*M_PI/nwave, 8*M_PI/nwave, 9*M_PI/nwave};

  /* Uncomment this line (and its companions below) to actually render
   * the image! */
  /* greyscale_image *out = alloc_img(pixels, piyels); */

  /* A dummy buffer for outputting 32-bit RGBA fixed-point image
   * data. */
  uint32_t *img = NULL;
  if ((img = malloc(pixels * piyels * sizeof(*img))) == NULL)
    return 1;

  struct timespec before, after;
  clock_gettime(CLOCK_REALTIME, &before);

  for (unsigned int x = 0; x < pixels; x++)
    for (unsigned int y = 0; y < piyels; y++) {
      const unsigned int idx = pixels * x + y;
      /* Uncomment me and my undef buddy to output the real image! */
/* #define p out->v[idx] */
#ifndef p
      float p;
#endif  /* p */
      p = 0;
      for (unsigned int w = 0; w < nwave; w++)
        p += wave(freq, phases[w], angles[w], x - pixels / 2, y - piyels / 2);
      p = (cos(M_PI * p) + 1) / 2;
      const uint8_t tmp = (uint8_t) (255 * MIN(1, MAX(p, 0)));
/* #undef p */
      img[idx] = (0xff << 24) | tmp << 16 | tmp << 8 | tmp;
    }

  clock_gettime(CLOCK_REALTIME, &after);

  printf("elapsed time: %lf seconds.\n",
         after.tv_sec - before.tv_sec +
         (after.tv_nsec - before.tv_nsec) * 1e-9);

  /* Make sure GCC doesn't cheat and not calculate anything! */
  printf("First element of img: 0x%08X\n", img[0]);

  /* Uncomment me to output the real image! */
  /* write_img("c_out.png", out); */

  /* Uncomment everything here to output some data for comparison with
   * the qc.hs module. */

  /* printf("phase\tangle\n"); */
  /* for (unsigned int i = 0; i < nwave; i++) */
  /*   printf("%f\t%f\n", phases[i], angles[i]); */
  /* printf("\n"); */

  /* #define testwrite(call) printf("\t" #call " = %f\n", call) */

  /* printf("Tests:\n"); */
  /* testwrite(wave(0.2, 0, 0, 0, 0)); */
  /* testwrite(wave(0.2, 0, 0, 2, 2)); */
  /* testwrite(wave(0.2, 22, 22, 2, 2)); */
  /* testwrite(wave(0.2, 22, 22, 22, 22)); */
  /* testwrite(wave(0.2, 0.2, 0.2, 500, 600)); */

  return 0;
}
