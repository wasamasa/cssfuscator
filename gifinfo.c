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
    GifWord background_color = gif->SBackGroundColor;
    printf("Background Color: %i\n", background_color);
    int frame_count = gif->ImageCount;
    printf("Frame Count: %i\n", frame_count);

    ColorMapObject *global_color_map = gif->SColorMap;
    if (!global_color_map)
        printf("Global Color Map: absent\n");
    else {
        printf("Global Color Map:\n");
        int global_color_count = global_color_map->ColorCount;
        printf("  Count: %i\n", global_color_count);
        int global_color_depth = global_color_map->BitsPerPixel;
        printf("  Depth: %i\n", global_color_depth);
        bool global_color_sorted = global_color_map->SortFlag;
        printf("  Sorted: %s\n", global_color_sorted ? "Yes" : "No");
        GifColorType *global_colors = global_color_map->Colors;
        printf("  Colors:\n");
        for (int i=0; i<global_color_count; ++i) {
            GifColorType color = global_colors[i];
            GifByteType r = color.Red;
            GifByteType g = color.Green;
            GifByteType b = color.Blue;
            printf("    Color %i: #%02x%02x%02x\n", i, r, g, b);
        }
    }

    fprintf(stderr, "Closing file\n");
    if (DGifCloseFile(gif, &gif_err) == GIF_ERROR)
        errx(1, "DGifCloseFile: %s\n", GifErrorString(gif_err));

    exit(0);
}
