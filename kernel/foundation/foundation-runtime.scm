;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Foundation Dialect Runtime
;;;


(block foundation.foundation-runtime


;;;
;;;; Dialect
;;;


(zen:define-class zen:Foundation-Dialect zen:Dialect (constructor: zen:allocate-foundation-dialect)
  ())


(define (zen:new-foundation-dialect name)
  (zen:allocate-foundation-dialect name (%%make-table test: eq?) (%%make-table test: eq?)))


(zen:define-method (zen:dialect-walker (zen:Foundation-Dialect dialect))
  (zen:new-foundation-walker))


;;;
;;;; Walker
;;;


(zen:define-class zen:Foundation-Walker zen:Walker (constructor: zen:allocate-foundation-walker)
  ())


(define (zen:new-foundation-walker)
  (zen:allocate-foundation-walker #f #f '() '() '() (zen:new-queue) (zen:new-queue) (%%make-table test: eq?) (%%make-table test: eq?) '()))


;;;
;;;; Register
;;;


(zen:define-dialect zen.foundation
  (zen:new-foundation-dialect 'zen.foundation))

(zen:define-dialect zen.foundation.dialect
  (zen:new-foundation-dialect 'zen.foundation.dialect))


(zen:define-walker-declaration require             zen.foundation zen:walk-require-declaration zen:walk-require)
(zen:define-walker-declaration export              zen.foundation zen:walk-export-declaration zen:walk-export)
(zen:define-walker-declaration import              zen.foundation zen:walk-import-declaration zen:walk-import)
(zen:define-walker-special     proclaim            zen.foundation zen:walk-proclaim)
(zen:define-walker-declaration native              zen.foundation zen:walk-native-declaration zen:walk-native)
(zen:define-walker-declaration native-syntax       zen.foundation zen:walk-native-syntax-declaration zen:walk-native-syntax)
(zen:define-walker-declaration macro               zen.foundation zen:walk-macro-declaration zen:walk-macro)
(zen:define-walker-declaration local-macro         zen.foundation zen:walk-local-macro-declaration zen:walk-local-macro)
(zen:define-walker-declaration syntax              zen.foundation zen:walk-syntax-declaration zen:walk-syntax)
(zen:define-walker-declaration define-syntax       zen.foundation zen:walk-define-syntax-declaration zen:walk-define-syntax)
(zen:define-walker-declaration define-local-syntax zen.foundation zen:walk-define-local-syntax-declaration zen:walk-define-local-syntax)
(zen:define-walker-special     let-syntax          zen.foundation zen:walk-let-syntax)
(zen:define-walker-special     letrec-syntax       zen.foundation zen:walk-letrec-syntax)
(zen:define-walker-special     walk-failed?        zen.foundation zen:walk-walk-failed))
