#include <stdio.h>
#include <stdlib.h>
#include <err.h>
#include "gif_lib.h"

int main(int argc, char *argv[]) {
    if (argc < 2)
        errx(1, "Usage: %s FILE", argv[0]);

    int gif_err = 0;

    fprintf(stderr, "Opening file: %s\n", argv[1]);

    GifFileType *gif = DGifOpenFileName(argv[1], &gif_err);
    if (!gif)
        errx(1, "DGifOpenFileName: %s\n", GifErrorString(gif_err));

    fprintf(stderr, "Loading file\n");
    if (DGifSlurp(gif) == GIF_ERROR)
        errx(1, "DGifSlurp: %s\n", GifErrorString(gif->Error));

    GifWord width = gif->SWidth;
    GifWord height = gif->SHeight;
    printf("Dimensions: %i x %i px\n", width, height);
    GifWord color_resolution = gif->SColorResolution;
    printf("Color Resolution: %i bits\n", color_resolution);
    int frame_count = gif->ImageCount;
    printf("Frame Count: %i\n", frame_count);

    fprintf(stderr, "Closing file\n");
    if (DGifCloseFile(gif, &gif_err) == GIF_ERROR)
        errx(1, "DGifCloseFile: %s\n", GifErrorString(gif_err));

    exit(0);
}
