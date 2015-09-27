(use (only data-structures string-intersperse)
     (only ports with-output-to-port)
     (only args args:make-option args:parse args:usage)
     (only imlib2 image-load image-width image-height image-destroy image-pixel/rgba)
     (rename format (format cl-format)))
;; TODO use scss and sxml-serializer egg?

;; TODO animated GIF support? would probably require wrapping giflib...
;; TODO optimizations (like, turning #ffffff into #fff or 1.0 into 1)?
;; TODO allow options for splitting out css or pretty-printing or naming

(define (hex-code-at image x y)
  (receive (r g b a) (image-pixel/rgba image x y)
    ;; is the pixel fully opaque?
    (if (= a 255)
        (cl-format #f "#~2,'0x~2,'0x~2,'0x" r g b)
        #f)))

(define (image->string image width height unit scale)
  (let ((data '()))
    (do ((y 0 (add1 y)))
        ((= y height))
      (do ((x 0 (add1 x)))
          ((= x width))
        (let ((hex-code (hex-code-at image x y)))
          (when hex-code
            (set! data (cons (format "~a~a ~a~a ~a"
                                     (add1 (* x scale)) unit
                                     (add1 (* y scale)) unit
                                     hex-code)
                             data))))))
    (string-intersperse (reverse data) ",")))

(define (transform-image image-path unit scale)
  (let* ((image (image-load image-path))
         (width (image-width image))
         (height (image-height image))
         (data (image->string image width height unit scale)))
    (image-destroy image)
    data))

(define (error-message message)
  (with-output-to-port (current-error-port)
    (lambda () (print message)))
  (exit 1))

(define (usage #!optional help?)
  (print (format "Usage: ~a [options]\n\n~a"
                 (car (argv)) (args:usage opts)))
  (if help?
      (exit 0)
      (exit 1)))

(define opts
  (list (args:make-option (i input) (required: "FILE") "input file")
        (args:make-option (o output) (required: "FILE") "output file")
        (args:make-option (u unit) #:optional "unit (default: px)")
        (args:make-option (s scale) #:optional "scaling factor (default: 1)")
        (args:make-option (h help) #:none "Display usage"
                          ;; is the usage invoked from help?
                          (usage (string=? name "help")))))

(define (process-image input output unit scale)
  (with-output-to-file output
    (lambda ()
      ;; FIXME offer option for altering image offset
      (display (format "<!DOCTYPE html><html><head><title>Image</title><style type=\"text/css\">#image{width:~a~a;height:~a~a;box-shadow:"
                       scale unit scale unit))
      (display (transform-image input unit scale))
      (display "}</style></head><body><div id=\"image\"></div></body></html>"))))

(define (main)
  (receive (options operands)
      (args:parse (command-line-arguments) opts)
    (let ((input-file (alist-ref 'input options))
          (output-file (alist-ref 'output options))
          (unit (or (alist-ref 'unit options) "px"))
          (scale (let ((option (alist-ref 'scale options)))
                   (if option
                       (string->number option)
                       1.0))))
      (unless input-file
        (error-message "No input file specified"))
      (unless output-file
        (error-message "No output file specified"))
      (process-image input-file output-file unit scale))))

(main)
