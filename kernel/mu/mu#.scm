;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Mu Header
;;;


;;;
;;;; Variable
;;;


;; tanzoo
(define-macro (mu:define-variable name . rest)
  (let ((expr (if (null? rest) #f (car rest))))
    `(begin
       (define ,name #f)
       (set! ,name ,expr))))


(define-macro (mu:define-variable-override name value)
  `(set! ,name ,value))
