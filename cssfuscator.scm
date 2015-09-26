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
        ;; FIXME don't hardcode coordinates
        ;; FIXME don't hardcode scaling factor
        (format #f "~dpx ~dpx #~2,'0x~2,'0x~2,'0x" x y r g b)
        #f)))

(define (image->string image width height)
  (let y-loop ((y 0) (acc '()))
    (if (< y height)
        (let x-loop ((x 0) (acc acc))
          (if (< x width)
              (let ((hex-code (hex-code-at image x y)))
                (if hex-code
                    (x-loop (add1 x) (cons hex-code acc))
                    (x-loop (add1 x) acc)))
              (y-loop (add1 y) acc)))
        (string-intersperse (reverse acc) ","))))

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

;; it's pretty weird how unrecognized options use the procedure from
;; the help action as these should have different exit codes...
(define (usage)
  (print (format "Usage: ~a [options] [files]\n\n~a"
                 (car (argv)) (args:usage opts)))
  (exit 0))

(define opts
  (list (args:make-option (i input) (required: "FILE") "input file")
        (args:make-option (o output) (required: "FILE") "output file")
        (args:make-option (h help) #:none "Display usage" (usage))))

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
