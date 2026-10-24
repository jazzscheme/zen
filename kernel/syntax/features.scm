;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Features
;;;


(declare
  (block)
  (standard-bindings)
  (extended-bindings)
  (not safe))


(zen:verbose-kernel 'kernel.features)


(define-macro (zen:install-features)
  (let ((features `(zen Zen ZEN zenscheme ZenScheme ZENSCHEME ,zen:kernel-system ,zen:kernel-platform ,zen:kernel-compiler ,zen:kernel-processor ,zen:kernel-windowing ,zen:kernel-safety ,@zen:kernel-features ,@(if zen:kernel-track-memory? '(track) '()))))
    (for-each (lambda (feature)
                (if feature
                    (^#cond-expand-features (append (^#cond-expand-features) (list feature)))))
              features)
    `(for-each (lambda (feature)
                 (if feature
                     (^#cond-expand-features (append (^#cond-expand-features) (list feature)))))
               ',features)))


(zen:install-features)
