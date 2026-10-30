;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Scheme Backend
;;;


(block scheme.scheme-backend


;;;
;;;; Define
;;;


(zen:define-class zen:Define zen:Field (constructor: zen:allocate-define accessors-type: macro)
  ((locator getter: generate)))


(define (zen:new-define name locator)
  (zen:allocate-define name locator))


(define (zen:register-define module-name name locator)
  (zen:register-module-entry module-name name (zen:new-define name locator))))
