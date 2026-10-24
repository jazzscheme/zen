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


(zen:define-dialect foundation
  (zen:new-foundation-dialect 'foundation))

(zen:define-dialect foundation.dialect
  (zen:new-foundation-dialect 'foundation.dialect))


(zen:define-walker-declaration require             foundation zen:walk-require-declaration zen:walk-require)
(zen:define-walker-declaration export              foundation zen:walk-export-declaration zen:walk-export)
(zen:define-walker-declaration import              foundation zen:walk-import-declaration zen:walk-import)
(zen:define-walker-special     proclaim            foundation zen:walk-proclaim)
(zen:define-walker-declaration native              foundation zen:walk-native-declaration zen:walk-native)
(zen:define-walker-declaration native-syntax       foundation zen:walk-native-syntax-declaration zen:walk-native-syntax)
(zen:define-walker-declaration macro               foundation zen:walk-macro-declaration zen:walk-macro)
(zen:define-walker-declaration local-macro         foundation zen:walk-local-macro-declaration zen:walk-local-macro)
(zen:define-walker-declaration syntax              foundation zen:walk-syntax-declaration zen:walk-syntax)
(zen:define-walker-declaration define-syntax       foundation zen:walk-define-syntax-declaration zen:walk-define-syntax)
(zen:define-walker-declaration define-local-syntax foundation zen:walk-define-local-syntax-declaration zen:walk-define-local-syntax)
(zen:define-walker-special     let-syntax          foundation zen:walk-let-syntax)
(zen:define-walker-special     letrec-syntax       foundation zen:walk-letrec-syntax)
(zen:define-walker-special     walk-failed?        foundation zen:walk-walk-failed))
