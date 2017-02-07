(use (only data-structures string-intersperse)
     (only ports with-output-to-port with-output-to-string)
     (only args args:make-option args:parse args:usage args:width)
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

(define (error-message message)
  (with-output-to-port (current-error-port)
    (lambda () (print message)))
  (exit 1))

(define (usage #!optional help?)
  (args:width 30)
  (print (format "Usage: ~a [options]\n\n~a"
                 (car (argv)) (args:usage opts)))
  (if help?
      (exit 0)
      (exit 1)))

;; TODO: use getopt-long
(define opts
  (list (args:make-option (i input) (required: "FILE") "input file")
        (args:make-option (o output) (required: "FILE") "output file")
        (args:make-option (u unit) #:optional "unit (default: \"px\")")
        (args:make-option (s scale) #:optional "scaling factor (default: 1)")
        (args:make-option (html-title) #:optional "HTML title (default: \"Image\")")
        (args:make-option (css-id) #:optional "CSS ID (default: \"image\")")
        (args:make-option (O optimize) #:none "enable optimizations")
        (args:make-option (stylesheet-name) #:optional "stylesheet name (default: \"style.css\")")
        (args:make-option (stylesheet) (optional: "TYPE") "stylesheet type (default: embed, options: embed, split, only)")
        (args:make-option (animate) #:none "enable animation mode")
        (args:make-option (h help) #:none "display usage"
                          ;; is the usage invoked from help?
                          (usage (equal? name "help")))))

(define (gif-animated? gif)
  (any (lambda (frame) (alist-ref 'loop (frame-metadata frame)))
       (gif-frames gif)))

(define (gif-delays gif)
  (let ((delays (map (lambda (frame) (alist-ref 'delay (frame-metadata frame)))
                     (gif-frames gif))))
    (when (not (every identity delays))
      (error-message "Malformed GIF: missing delay(s)"))
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
        (error-message "Couldn't find animation metadata"))
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
  (receive (options operands)
      (args:parse (command-line-arguments) opts)
    (let ((input-file (alist-ref 'input options))
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
                                (error-message "Invalid stylesheet option"))
                            'embed)))
          (animate? (alist-ref 'animate options)))
      (unless input-file
        (error-message "No input file specified"))
      (unless output-file
        (error-message "No output file specified"))
      ((if animate?
           process-gif
           process-image)
       input-file output-file unit scale html-title css-id
       optimize? stylesheet-name stylesheet))))

(main)
