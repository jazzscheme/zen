;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Syntax
;;;


(block kernel.syntax


;;;
;;;; Setting
;;;


(zen:define-macro (zen:define-setting name . rest)
  (let ((expr (if (%%null? rest) #f (%%car rest)))
        (global (zen:generate-symbol (symbol->string name))))
    `(begin
       (define ,global ,expr)
       (define (,name . rest)
         (if (%%null? rest)
             ,global
           (set! ,global (%%car rest)))))))


;;;
;;;; Variable
;;;


(zen:define-macro (zen:define-variable name . rest)
  (let ((expr (if (%%null? rest) #f (%%car rest))))
    `(begin
       (define ,name #f)
       (set! ,name ,expr))))


(zen:define-macro (zen:define-variable-override name value)
  `(set! ,name ,value)))
