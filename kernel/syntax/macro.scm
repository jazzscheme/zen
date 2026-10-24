;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Macros
;;;


(declare
  (block)
  (standard-bindings)
  (extended-bindings)
  (not safe))


(define zen:Macros
  (make-table test: eq?))


(define (zen:register-macro name macro)
  (table-set! zen:Macros name macro))


(define (zen:get-macro name)
  (table-ref zen:Macros name #f))


(define (zen:need-macro name)
  (or (zen:get-macro name)
      (zen:error "Unable to find macro: {s}" name)))


(define (zen:expand-macro form)
  (apply (zen:need-macro (car form)) (cdr form)))


;; #f interpret compile eval
(define zen:walk-for
  (make-parameter #f))
