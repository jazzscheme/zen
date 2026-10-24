#!gsc -:dq-

(define zen:source
  (path-directory (path-strip-trailing-directory-separator (path-directory (path-normalize (car (command-line)))))))

(load (string-append zen:source "kernel/build"))
