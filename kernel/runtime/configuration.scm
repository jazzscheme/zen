;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Configuration
;;;


(block kernel.configuration


;;;
;;;; Source
;;;


(define zen:zen-source
  zen:source)


;;;
;;;; Configuration
;;;


(zen:define-structure Configuration () (constructor: zen:make-configuration)
  ((name                getter: generate)
   (system              getter: generate)
   (platform            getter: generate)
   (compiler            getter: generate)
   (processor           getter: generate)
   (windowing           getter: generate)
   (safety              getter: generate)
   (optimize?           getter: generate)
   (debug-environments? getter: generate)
   (debug-location?     getter: generate)
   (debug-source?       getter: generate)
   (debug-foreign?      getter: generate)
   (track-memory?       getter: generate)
   (mutable-bindings?   getter: generate)
   (kernel-interpret?   getter: generate)
   (destination         getter: generate)
   (features            getter: generate)
   (properties          getter: generate)
   (local?              getter: generate)))


(define (zen:configuration-directory configuration)
  (zen:destination-directory
    (zen:get-configuration-name configuration)
    (zen:get-configuration-destination configuration)
    "./")))
