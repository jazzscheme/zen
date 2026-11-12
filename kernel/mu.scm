#!gsc -:dq-

(define zen:source
  (path-directory (path-strip-trailing-directory-separator (path-directory (path-normalize (car (command-line)))))))

(define (zen:load-source path)
  (load (string-append zen:source path)))

(zen:load-source "kernel/build")
