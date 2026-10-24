;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Scheme Syntax
;;;


(module scheme.syntax scheme


(export declaration-unit
        declaration-path
        declaration-locator
        optional
        let-optionals*)

(import (scheme.kernel)
        (scheme.syntax-rules (phase syntax)))


(native private zen:error)
(native private zen:unspecified)


;;;
;;;; Declaration
;;;


(define-syntax declaration-unit
  (lambda (form-src usage-environment macro-environment)
    (sourcify-deep-if
      `(quote ,(car (get-declaration-path (get-declaration-toplevel (current-declaration)))))
      form-src)))


(define-syntax declaration-path
  (lambda (form-src usage-environment macro-environment)
    (sourcify-deep-if
      `(quote ,(get-declaration-path (current-declaration)))
      form-src)))


(define-syntax declaration-locator
  (lambda (form-src usage-environment macro-environment)
    (sourcify-deep-if
      `(quote ,(apply compose-reference (get-declaration-path (current-declaration))))
      form-src)))


;;;
;;;; Optionals
;;;


(define-syntax optional
  (syntax-rules ()
    ((optional rest default-exp)
     (let ((maybe-arg rest))
       (cond ((null? maybe-arg) default-exp)
             ((null? (cdr maybe-arg)) (car maybe-arg))
             (else (error "too many optional arguments" maybe-arg)))))
    ((optional rest default-exp validator)
     (optional rest default-exp))))


(define-syntax really-let-optionals*
  (syntax-rules ()
    ;; Standard case. Do the first var/default and recurse.
    ((really-let-optionals* args ((var1 default1 typecheck1 ...) etc ...)
       body1 ...)
     (call-with-values (lambda () (if (null? args)
                                      (values default1 '())
                                      (values (car args) (cdr args))))
                       (lambda (var1 rest)
                         (really-let-optionals* rest (etc ...)
                           body1 ...))))

    ;; Single rest arg -- bind to the remaining rest values.
    ((really-let-optionals* args (rest) body1 ...)
     (let ((rest args)) body1 ...))

    ;; No more vars. Make sure there are no unaccounted-for values, and do the body.
    ((really-let-optionals* args () body1 ...)
     (if (null? args) (begin body1 ...)
         (error "Too many optional arguments." args)))))

(define-syntax let-optionals*
  (syntax-rules ()
    ((let-optionals* args vars&defaults body1 ...)
     (let ((rest args))
       (really-let-optionals* rest vars&defaults body1 ...))))))
