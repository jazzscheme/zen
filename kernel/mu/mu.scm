;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Mu
;;;


(let ((config (car (command-args))))
  (let ((runtime (string-append "build/" config "/zen.o1")))
    (pp (list 'load-runtime runtime))
    (load runtime)))
