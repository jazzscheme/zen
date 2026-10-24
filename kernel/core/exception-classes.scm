;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Exception Classes
;;;


(block core.exception-classes


;;;
;;;; Exception
;;;


(zen:define-virtual-syntax (zen:present-exception (zen:Exception exception)))
(zen:define-virtual-syntax (zen:exception-message (zen:Exception exception)))
(zen:define-virtual-syntax (zen:get-detail (zen:Exception exception))))
