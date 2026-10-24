;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Zen Backend
;;;


(unit protected zen.backend


;;;
;;;; Definition
;;;


(zen:define-class zen:Definition zen:Field (constructor: zen:allocate-definition)
  ((locator getter: generate)))


(define (zen:new-definition name locator)
  (zen:allocate-definition name locator))


(define (zen:register-definition module-name name locator)
  (zen:register-module-entry module-name name (zen:new-definition name locator))))
