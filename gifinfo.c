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

    printf("Dimensions: %i x %i px\n", gif->SWidth, gif->SHeight);
    printf("Color Resolution: %i bits\n", gif->SColorResolution);
    printf("Background Color: %i\n", gif->SBackGroundColor);
    printf("Frame Count: %i\n", gif->ImageCount);

    ColorMapObject *global_color_map = gif->SColorMap;
    if (!global_color_map)
        printf("Global Color Map: absent\n");
    else {
        printf("Global Color Map:\n");
        printf("  Count: %i\n", global_color_map->ColorCount);
        printf("  Depth: %i\n", global_color_map->BitsPerPixel);
        printf("  Sorted: %s\n", global_color_map->SortFlag ? "Yes" : "No");
        printf("  Colors:\n");

        int global_color_map_count = global_color_map->ColorCount;
        for (int i = 0; i < global_color_map_count; ++i) {
            GifColorType color = global_color_map->Colors[i];
            printf("    Color %i: #%02x%02x%02x\n", i,
                   color.Red, color.Green, color.Blue);
        }
    }

    fprintf(stderr, "Closing file\n");
    if (DGifCloseFile(gif, &gif_err) == GIF_ERROR)
        errx(1, "DGifCloseFile: %s\n", GifErrorString(gif_err));

    exit(0);
}
