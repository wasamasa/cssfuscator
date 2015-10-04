#include <stdio.h>
#include <stdlib.h>
#include <err.h>
#include "gif_lib.h"

int main(int argc, char *argv[]) {
    if (argc < 2)
        errx(1, "Usage: %s FILE", argv[0]);

    int gif_err = 0;

    GifFileType *gif = DGifOpenFileName(argv[1], &gif_err);
    if (!gif)
        errx(1, "DGifOpenFileName: %s\n", GifErrorString(gif_err));

    if (DGifSlurp(gif) == GIF_ERROR)
        errx(1, "DGifSlurp: %s\n", GifErrorString(gif->Error));

    printf("Dimensions: %i x %i px\n", gif->SWidth, gif->SHeight);
    printf("Color Resolution: %i bits\n", gif->SColorResolution);
    printf("Background Color: %i\n", gif->SBackGroundColor);
    int frame_count = gif->ImageCount;
    printf("Frame Count: %i\n", frame_count);

    ColorMapObject *global_color_map = gif->SColorMap;
    if (!global_color_map)
        printf("Global Color Map: absent\n");
    else {
        printf("Global Color Map:\n");
        int global_color_map_count = global_color_map->ColorCount;
        printf("  Count: %i\n", global_color_map_count);
        printf("  Depth: %i\n", global_color_map->BitsPerPixel);
        printf("  Colors:\n");

        for (int i = 0; i < global_color_map_count; ++i) {
            GifColorType color = global_color_map->Colors[i];
            printf("    Color %i: #%02x%02x%02x\n", i,
                   color.Red, color.Green, color.Blue);
        }
    }

    SavedImage *frames = gif->SavedImages;
    printf("Frames:\n");
    for (int i = 0; i < frame_count; ++i) {
        SavedImage frame = frames[i];
        printf("  Frame %i:\n", i);
        GifImageDesc frame_desc = frame.ImageDesc;
        printf("    Frame Interlaced: %s\n", frame_desc.Interlace ? "Yes" : "No");
        printf("    Frame Left: %i\n", frame_desc.Left);
        printf("    Frame Top: %i\n", frame_desc.Top);
        GifWord frame_width = frame_desc.Width;
        printf("    Frame Width: %i\n", frame_width);
        GifWord frame_height = frame_desc.Height;
        printf("    Frame Height: %i\n", frame_height);
        ColorMapObject *local_color_map = frame_desc.ColorMap;
        if (!local_color_map)
            printf("    Local Color Map: absent\n");
        else {
            printf("    Local Color Map:\n");
            int local_color_map_count = local_color_map->ColorCount;
            printf("      Count: %i\n", local_color_map_count);
            printf("      Depth: %i\n", local_color_map->BitsPerPixel);
            printf("      Colors:\n");

            for (int i = 0; i < local_color_map_count; ++i) {
                GifColorType color = local_color_map->Colors[i];
                printf("        Color %i: #%02x%02x%02x\n", i,
                       color.Red, color.Green, color.Blue);
            }
        }

        printf("    Image Data:\n");
        GifByteType *frame_bytes = frame.RasterBits;
        for (int i = 0; i < frame_height; ++i) {
            printf("      ");
            for (int j = 0; j < frame_width; ++j)
                printf("%02x", frame_bytes[i*frame_width+j]);
            printf("\n");
        }

        int frame_extension_block_count = frame.ExtensionBlockCount;
        if (frame_extension_block_count > 0) {
            printf("    Frame Extension Blocks:\n");
            for (int i = 0; i < frame_extension_block_count; ++i) {
                ExtensionBlock extension_block = frame.ExtensionBlocks[i];
                printf("      Extension Block %i:\n", i);
                char* function;
                switch (extension_block.Function) {
                    case CONTINUE_EXT_FUNC_CODE:
                        function = "Continuation Block";
                        break;
                    case COMMENT_EXT_FUNC_CODE:
                        function = "Comment Block";
                        break;
                    case GRAPHICS_EXT_FUNC_CODE:
                        function = "Graphics Control Block";
                        break;
                    case PLAINTEXT_EXT_FUNC_CODE:
                        function = "Plain Text Block";
                        break;
                    case APPLICATION_EXT_FUNC_CODE:
                        function = "Application Block";
                        break;
                }
                printf("        Function: %s\n", function);

                printf("        Extension Block Data:");
                int extension_block_byte_count = extension_block.ByteCount;
                for (int j = 0; j < extension_block_byte_count; ++j)
                    printf("%02x", extension_block.Bytes[j]);
                printf("\n");
            }
        }
    }

    int extension_block_count = gif->ExtensionBlockCount;
    if (extension_block_count > 0) {
        printf("  Extension Blocks:\n");
        for (int i = 0; i< extension_block_count; ++i) {
            ExtensionBlock extension_block = gif->ExtensionBlocks[i];
            printf("    Extension Block %i:\n", i);
            char* function;
            switch (extension_block.Function) {
                case CONTINUE_EXT_FUNC_CODE:
                    function = "Continuation Block";
                    break;
                case COMMENT_EXT_FUNC_CODE:
                    function = "Comment Block";
                    break;
                case GRAPHICS_EXT_FUNC_CODE:
                    function = "Graphics Control Block";
                    break;
                case PLAINTEXT_EXT_FUNC_CODE:
                    function = "Plain Text Block";
                    break;
                case APPLICATION_EXT_FUNC_CODE:
                    function = "Application Block";
                    break;
            }
            printf("        Function: %s\n", function);

            printf("        Extension Block Data:");
            int extension_block_byte_count = extension_block.ByteCount;
            for (int j = 0; j < extension_block_byte_count; ++j)
                printf("%02x", extension_block.Bytes[j]);
            printf("\n");
        }
    }

    if (DGifCloseFile(gif, &gif_err) == GIF_ERROR)
        errx(1, "DGifCloseFile: %s\n", GifErrorString(gif_err));

    exit(0);
}
