(use data-structures ports args imlib2 format)
;; TODO use scss and sxml-serializer egg?

;; TODO animated GIF support? would probably require wrapping giflib...
;; TODO optimizations (like, turning #ffffff into #fff)?
;; TODO allow options for splitting out css or pretty-printing or naming

(define (hex-code-at image x y)
  (receive (r g b a)
      (image-pixel/rgba image x y)
    ;; is the pixel fully opaque?
    (if (= a 255)
        ;; FIXME introduce an offset
        ;; FIXME don't hardcode coordinates
        ;; FIXME don't hardcode scaling factor
        (format #f "~dpx ~dpx #~2,'0x~2,'0x~2,'0x" x y r g b)
        #f)))

(define (image->string image width height)
  (let ((data '()))
    (do ((y 0 (add1 y)))
        ((= y height))
      (do ((x 0 (add1 x)))
          ((= x width))
        (let ((hex-code (hex-code-at image x y)))
          (when hex-code
            (set! data (cons hex-code data))))))
    (string-intersperse (reverse data) ",")))

(define (transform image-path)
  (let* ((image (image-load image-path))
         (width (image-width image))
         (height (image-height image))
         (data (image->string image width height)))
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
        (args:make-option (h help) #:none "Display usage"
                          ;; is the usage invoked from help?
                          (usage (string=? name "help")))))

(define (main)
  (receive (options operands)
      (args:parse (command-line-arguments) opts)
    (let ((input-file (alist-ref 'input options))
          (output-file (alist-ref 'output options)))
      (unless input-file
        (error-message "No input file specified"))
      (unless output-file
        (error-message "No output file specified"))
      (with-output-to-file output-file
        (lambda ()
          ;; FIXME don't hardcode units
          (display "<!DOCTYPE html><html><head><title>Image</title><style type=\"text/css\">#image{width:1px;height:1px;box-shadow:")
          (display (transform input-file))
          (display "}</style></head><body><div id=\"image\"></div></body></html>"))))))

(main)
