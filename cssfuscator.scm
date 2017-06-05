(use (only data-structures string-intersperse)
     (only ports with-output-to-port with-output-to-string)
     (only getopt-long getopt-long usage)
     (only imlib2 image-load image-width image-height image-destroy image-pixel/rgba)
     (only srfi-1 every fold any for-each)
     (rename format (format cl-format))
     giflib giflib-imlib2)

(define (hex-shorten r g b)
  (let ((r* (quotient r 16))
        (g* (quotient g 16))
        (b* (quotient b 16)))
    (if (and (= r* (modulo r 16))
             (= g* (modulo g 16))
             (= b* (modulo b 16)))
        (format "#~x~x~x" r* g* b*)
        #f)))

(define (hex-code-at image x y optimize?)
  (receive (r g b a) (image-pixel/rgba image x y)
    ;; is the pixel fully opaque?
    (if (= a 255)
        (let ((hex-code (cl-format #f "#~2,'0x~2,'0x~2,'0x" r g b)))
          (if optimize?
              (or (hex-shorten r g b) hex-code)
              hex-code))
        #f)))

(define (float->equivalent n)
  (if (= n (floor n))
      (inexact->exact n)
      n))

(define (css-spec-at x y unit scale hex-code optimize?)
  (let* ((scale (if optimize? (float->equivalent scale) scale))
         (x (if optimize? (float->equivalent x) x))
         (y (if optimize? (float->equivalent y) y)))
    (format "~a~a ~a~a ~a"
            (add1 (* x scale)) unit
            (add1 (* y scale)) unit
            hex-code)))

(define (image->string image width height unit scale optimize?)
  (let ((data '()))
    (do ((y 0 (add1 y)))
        ((= y height))
      (do ((x 0 (add1 x)))
          ((= x width))
        (let ((hex-code (hex-code-at image x y optimize?)))
          (when hex-code
            (set! data (cons (css-spec-at x y unit scale hex-code optimize?)
                             data))))))
    (string-intersperse (reverse data) ",")))

(define (transform-image path unit scale optimize?)
  (let* ((image (image-load path))
         (width (image-width image))
         (height (image-height image))
         (data (image->string image width height unit scale optimize?)))
    (image-destroy image)
    data))

(define (error-message message . args)
  (with-output-to-port (current-error-port)
    (lambda () (apply printf message args)))
  (exit 1))

(define opts
  '((input "input file"
           (single-char #\i)
           (value (required "FILE")))
    (output "output file"
            (single-char #\o)
            (value (required "FILE")))
    (unit "unit (default: \"px\")"
          (single-char #\u)
          (value (optional "UNIT")))
    (scale "scaling factor (default: 1)"
           (single-char #\s)
           (value (optional "N")))
    (html-title "HTML title (default: \"Image\")"
                (value (optional "TITLE")))
    (css-id "CSS ID (default: \"image\")"
            (value (optional "ID")))
    (optimize "enable optimizations"
              (single-char #\O)
              (required #f))
    (stylesheet-name "stylesheet name (default: \"style.css\")"
                     (value (optional "NAME")))
    (stylesheet "stylesheet type (default: embed, options: embed, split, only)"
                (value (optional "TYPE")))
    (animate "enable animation mode"
             (required #f))
    (help "display usage"
          (single-char #\h))))

(define (gif-animated? gif)
  (any (lambda (frame) (alist-ref 'loop (frame-metadata frame)))
       (gif-frames gif)))

(define (gif-delays gif)
  (let ((delays (map (lambda (frame) (alist-ref 'delay (frame-metadata frame)))
                     (gif-frames gif))))
    (when (not (every identity delays))
      (error-message "Malformed GIF: missing delay(s)\n"))
    delays))

(define (gif-length gif)
  (fold + 0 (gif-delays gif)))

(define (gif-timings gif)
  (let* ((delays (gif-delays gif))
         (sum (fold + 0 delays)))
    (let loop ((time 0)
               (delays delays)
               (acc '()))
      (if (null? delays)
          (reverse acc)
          (loop (+ time (car delays))
                (cdr delays)
                (cons (/ time sum) acc))))))

(define (gif->css gif timings unit scale optimize?)
  (with-output-to-string
    (lambda ()
      (for-each
       (lambda (timing image)
         (cl-format #t "~,1f%{box-shadow:" (* timing 100))
         (let ((width (image-width image))
               (height (image-height image)))
           (display (image->string image width height unit scale optimize?)))
         (image-destroy image)
         (display "}"))
       timings (gif->imlib2-images gif)))))

(define (save-css output css html-title css-id stylesheet-name stylesheet)
  (with-output-to-file output
    (lambda ()
      (cond
       ((eq? stylesheet 'embed)
        (display (format "<!DOCTYPE html><html><head><title>~a</title><style type=\"text/css\">~a</style></head><body><div id=\"~a\"></div></body></html>"
                         html-title css css-id)))
       ((eq? stylesheet 'split)
        (display (format "<!DOCTYPE html><html><head><title>~a</title><link href=\"~a\" rel=\"stylesheet\" type=\"text/css\" /></head><body><div id=\"~a\"></div></body></html>"
                         html-title stylesheet-name css-id)))
       ((eq? stylesheet 'only)
        (display css)))))
  (when (eq? stylesheet 'split)
    (with-output-to-file stylesheet-name
      (lambda () (display css)))))

(define (process-gif input output unit scale html-title css-id
                     optimize? stylesheet-name stylesheet)
  (let ((gif (open-gif input)))
    (slurp-gif gif)
    (let ((length (/ (gif-length gif) 100))
          (loop? (gif-animated? gif)))
      (when (not loop?)
        (error-message "Couldn't find animation metadata\n"))
      (let* ((frames (gif-frames gif))
             (loop-count (if (number? loop?) loop? 'infinite))
             (timings (gif-timings gif))
             (css (format "#~a{width:~a~a;height:~a~a;margin:-~a~a;animation-iteration-count:~a;animation-timing-function:step-end;animation-duration:~as;animation-name:~a}@keyframes ~a {~a}"
                          css-id scale unit scale unit scale unit
                          loop-count length css-id css-id
                          (gif->css gif timings unit scale optimize?))))
        (save-css output css html-title css-id stylesheet-name stylesheet)))))

(define (process-image input output unit scale html-title css-id
                       optimize? stylesheet-name stylesheet)
  (let ((css (format "#~a{width:~a~a;height:~a~a;margin:-~a~a;box-shadow:~a}"
                     css-id scale unit scale unit scale unit
                     (transform-image input unit scale optimize?))))
    (save-css output css html-title css-id stylesheet-name stylesheet)))

(define (main)
  (let* ((options
          (condition-case
           (getopt-long (command-line-arguments) opts)
           (e (exn)
              (error-message "Error: ~a: ~a\nUsage: ~a [options]\n\n~a"
                             ((condition-property-accessor 'exn 'message) e)
                             ((condition-property-accessor 'exn 'arguments) e)
                             (car (argv))
                             (usage opts)))))

         (input-file (alist-ref 'input options))
         (output-file (alist-ref 'output options))
         (unit (or (alist-ref 'unit options) "px"))
         (scale (let ((option (alist-ref 'scale options)))
                  (if option
                      (string->number option)
                      1.0)))
         (html-title (or (alist-ref 'html-title options) "Image"))
         (css-id (or (alist-ref 'css-id options) "image"))
         (optimize? (alist-ref 'optimize options))
         (stylesheet-name (or (alist-ref 'stylesheet-name options) "style.css"))
         (stylesheet (let ((stylesheet (alist-ref 'stylesheet options)))
                       (if stylesheet
                           (if (member stylesheet '("embed" "split" "only"))
                               (string->symbol stylesheet)
                               (error-message "Invalid stylesheet option\n"))
                           'embed)))
         (animate? (alist-ref 'animate options))
         (help? (alist-ref 'help options)))
    (when help?
      (display (usage opts))
      (exit 0))
    (when (not input-file)
      (error-message "No input file specified\n"))
    (when (not output-file)
      (error-message "No output file specified\n"))
    ((if animate?
         process-gif
         process-image)
     input-file output-file unit scale html-title css-id
     optimize? stylesheet-name stylesheet)))

(main)
