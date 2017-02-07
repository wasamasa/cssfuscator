CSSfuscator
===========

About
-----

``cssfuscator`` transforms an input image into CSS (and a bit of
HTML5).  See `my blog post on it
<http://emacsninja.com/posts/cssfuscator.html>`_ for more details.

Usage
-----

``-i, --input``
    Input image file name.  Required argument.

``-o, --output``
    Output file name.  Required argument.

``-u, --unit``
    Unit used for coordinates.  Default: "px".  See `MDN
    <https://developer.mozilla.org/en-US/docs/Web/CSS/length>`_ for
    recognized units.  Use "em" for scalable output.

``-s, --scale``
    Scaling factor for coordinates and pixels.  Default: 1.  Factors
    may be floating point numbers.

``--html-title``
    HTML title as used in generated HTML output.  Default: "Image".

``--css-id``
    CSS ID as used in generated HTML and CSS output.  Default:
    "image".

``-O, --optimize``
    Enable basic optimizations.  This attempts representing
    coordinates in integer instead of floating point and hex color
    codes in the ``#XYZ`` instead of ``#XXYYZZ`` form.

``--stylesheet-name``
    Stylesheet name.  Default: "style.css".  Only meaningful in
    combination with ``--stylesheet=split``.

``--stylesheet``
    Mode of stylesheet generation.  Default: "embed".  Allowed values:
    "embed" (embed CSS in a HTML output file), "split" (generate both
    a CSS and HTML file, with ``stylesheet-name`` specifying the file
    name of the CSS file), "only" (generate only a CSS file).  The
    ``--output`` option specifies the respective file names.

``--animate``
    Attempt converting an animated GIF into a CSS animation.

``-h, --help``
    Display the abbreviated usage.

Internals
---------

It's possible to arrange square blocks of a fixed size in any color by
using the ``box-shadow`` property repeatedly on a ``<div>``.  This
allows one to display any bitmap image.

Limitations
-----------

The resulting images are huge (preliminary tests show factors between
20x and 2000x for the generated CSS).  The ``--optimize`` switch
attempts to lessen the impact, but doesn't really help.  Consider
hosting the CSS in a separate file for caching and serving it in
`compressed
<http://nginx.org/en/docs/http/ngx_http_gzip_module.html>`_ form.

Transparency support is very basic.  A fully transparent pixel is
simply omitted, for anything else the transparency is simply ignored.

Animated GIF support is rather basic at the moment and will hopefully
allow for more customization soon.
