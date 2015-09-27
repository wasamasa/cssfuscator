(use data-structures ports args imlib2 format)
;; TODO use scss and sxml-serializer egg?

;; TODO animated GIF support? would probably require wrapping giflib...
;; TODO optimizations (like, turning #ffffff into #fff)?
;; TODO allow options for splitting out css or pretty-printing or naming

(define (hex-code-at image x y)
  (receive (r g b a) (image-pixel/rgba image x y)
    ;; is the pixel fully opaque?
    (if (= a 255)
        (format #f "#~2,'0x~2,'0x~2,'0x" r g b)
        #f)))

(define (image->string image width height unit)
  (let ((data '()))
    (do ((y 0 (add1 y)))
        ((= y height))
      (do ((x 0 (add1 x)))
          ((= x width))
        (let ((hex-code (hex-code-at image x y)))
          (when hex-code
            ;; FIXME don't hardcode coordinates
            ;; FIXME don't hardcode scaling factor
            (set! data (cons (format #f "~d~a ~d~a ~a"
                                     (add1 x) unit
                                     (add1 y) unit
                                     hex-code)
                             data))))))
    (string-intersperse (reverse data) ",")))

(define (transform image-path unit)
  (let* ((image (image-load image-path))
         (width (image-width image))
         (height (image-height image))
         (data (image->string image width height unit)))
    (image-destroy image)
    data))

(define (error-message message)
  (with-output-to-port (current-error-port)
    (lambda () (print message)))
  (exit 1))

(define (usage #!optional help?)
  (print (format #f "Usage: ~a [options]\n\n~a"
                 (car (argv)) (args:usage opts)))
  (if help?
      (exit 0)
      (exit 1)))

(define opts
  (list (args:make-option (i input) (required: "FILE") "input file")
        (args:make-option (o output) (required: "FILE") "output file")
        (args:make-option (u unit) #:optional "unit (default: px)")
        (args:make-option (h help) #:none "Display usage"
                          ;; is the usage invoked from help?
                          (usage (string=? name "help")))))

(define (process-image input output unit)
  (with-output-to-file output
    (lambda ()
      (display (format #f "<!DOCTYPE html><html><head><title>Image</title><style type=\"text/css\">#image{width:1~a;height:1~a;box-shadow:"
                       unit unit))
      (display (transform input unit))
      (display "}</style></head><body><div id=\"image\"></div></body></html>"))))

(define (main)
  (receive (options operands)
      (args:parse (command-line-arguments) opts)
    (let ((input-file (alist-ref 'input options))
          (output-file (alist-ref 'output options))
          (unit (or (alist-ref 'unit options) "px")))
      (unless input-file
        (error-message "No input file specified"))
      (unless output-file
        (error-message "No output file specified"))
      (process-image input-file output-file unit))))

(main)
