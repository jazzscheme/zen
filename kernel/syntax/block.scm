;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Block Macro
;;;


(declare
  (block)
  (standard-bindings)
  (extended-bindings)
  (not safe))


(zen:verbose-kernel 'kernel.block)


(define zen:testing?
  (make-parameter #f))

(define zen:testing
  (make-parameter #f))


(define zen:generate-symbol-for
  (make-parameter #f))

(define zen:generate-symbol-context
  (make-parameter #f))


(define (zen:source-code expr)
  (if (^#source? expr)
      (^#source-code expr)
    expr))


(zen:define-syntax block
  (lambda (form-src)
    (let ((name (zen:source-code (cadr (zen:source-code form-src))))
          (body (cddr (zen:source-code form-src))))
      (zen:generate-symbol-context name)
      `(begin
         (zen:kernel-declares)
         (zen:verbose-kernel ',name)
         ,@body))))
