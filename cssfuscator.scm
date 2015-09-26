(use data-structures ports args imlib2 format)
;; TODO use scss and sxml-serializer egg?

;; TODO animated GIF support? would probably require wrapping giflib...
;; TODO optimizations (like, turning #ffffff into #fff)?
;; TODO allow options for splitting out css or pretty-printing or naming

(define (transform image-path)
  (let* ((image (image-load image-path))
         (width (image-width image))
         (height (image-height image)))
    ;; NOTE one way to fix the trailing comma issue would be
    ;; accumulating strings into a list and concatenating it
    (let y-loop ((y 0))
      (when (< y height)
        (let x-loop ((x 0))
          (when (< x width)
            (receive (r g b a) (image-pixel/rgba image x y)
              ;; is the pixel fully opaque?
              (when (= a 255)
                  ;; FIXME don't hardcode coordinates
                  ;; FIXME don't hardcode scaling factor
                  ;; FIXME Firefox doesn't permit a final trailing
                  ;; comma, either semicolon or nothing are OK
                  (display (format #f "~dpx ~dpx #~2,'0x,~2,'0x,~2,'0x"
                                   x y r g b))))
            (x-loop (add1 x))))
        (y-loop (add1 y))))
    (image-destroy image)))

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
          (transform input-file)
          (display "}</style></head><body><div id=\"image\"></div></body></html>"))))))

(main)
