;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Development
;;;


(block dialect.dialect-development


;;;
;;;; Parse
;;;


(define (zen:parse unit-name)
  (let ((src (zen:find-unit-src unit-name)))
    (let ((form-src (zen:read-toplevel-form src)))
      (pretty-print
        (zen:desourcify-all form-src)))))


(define (zen:parse-source unit-name)
  (parameterize ((zen:walk-for 'interpret))
    (let* ((src (zen:find-unit-src unit-name))
           (form-src (zen:read-toplevel-form src)))
      (pretty-print
        (zen:present-source form-src)))))


;;;
;;;; Shape
;;;


(define (zen:shape-unit unit-name)
  (zen:load-unit 'zen/dialect)
  (let ((shape (zen:module-shape #f #f #f unit-name)))
    (pp shape)))


;;;
;;;; Expand
;;;


(define (zen:expanding-unit unit-name thunk #!key (walk-for #f))
  (let ((src (zen:find-unit-src unit-name)))
    (parameterize ((zen:walk-for (or walk-for 'walk))
                   (zen:generate-symbol-for "%")
                   (zen:generate-symbol-context unit-name)
                   (zen:requested-unit-name unit-name)
                   (zen:requested-unit-resource src)
                   (zen:requested-pathname #f))
      (thunk))))


(define (zen:expand-unit unit-name #!key (walk-for #f))
  (zen:expanding-unit unit-name
    (lambda ()
      (let ((form (zen:read-toplevel-form (zen:requested-unit-resource))))
        (zen:expand-form form unit-name: unit-name walk-for: walk-for)))
     walk-for: walk-for))


(define (zen:expand-form form #!key (unit-name #f) (walk-for #f))
  (parameterize ((zen:walk-for (or walk-for 'walk)))
    (let ((kind (zen:source-code (car (zen:source-code form))))
          (rest (cdr (zen:source-code form))))
      (parameterize ((zen:generate-symbol-for "%")
                     (zen:generate-symbol-context (or unit-name (gensym))))
        (case kind
          ((unit) (zen:expand-unit-source rest))
          ((module) (zen:generate-module rest))
          ((script) (zen:generate-script rest))
          (else (zen:error "Ill-formed toplevel form: {s}" kind)))))))


(define (zen:expand-to unit-name . rest)
  (let ((port (current-output-port)))
    (zen:output-port-width-set! port 160)
    (apply zen:expand-to-port unit-name port rest)))


(define (zen:expand-to-port unit-name port . rest)
  (pretty-print
    (zen:desourcify-all (apply zen:expand-unit unit-name rest))
    port))


(define (zen:expand-to-file unit-name #!key (file #f) #!rest rest)
  (parameterize ((current-readtable zen:scheme-readtable))
    (call-with-output-file (or file "x.scm")
      (lambda (port)
        (apply zen:expand-to-port unit-name port rest)))))


(define (zen:expand-source unit-name . rest)
  (pretty-print
    (zen:present-source
      (apply zen:expand-unit unit-name rest))))


(define (zen:expanding-script path thunk)
  (parameterize ((zen:walk-for 'walk)
                 (zen:generate-symbol-for "%")
                 (zen:generate-symbol-context (gensym))
                 (zen:requested-unit-name #f)
                 (zen:requested-unit-resource #f)
                 (zen:requested-pathname path))
    (thunk)))


(define (zen:expand-script path)
  (zen:expanding-script path
    (lambda ()
      (let ((form (zen:read-toplevel-form path script?: #t)))
        (let ((rest (cdr (zen:source-code form))))
          (zen:generate-script rest))))))


;;;
;;;; Emit
;;;


(define (zen:emit-unit unit-name)
  (zen:expand-unit unit-name))


;;;
;;;; Check
;;;


(define (zen:check-unit unit-name)
  (zen:warnings? #t)
  (zen:expand-unit unit-name))


;;;
;;;; Verify
;;;


(define (zen:verify-unit unit-name)
  (zen:iterate-module-declaration (zen:walk-unit unit-name)
    pp))


;;;
;;;; Lookup
;;;


(define (zen:lookup module-name name access)
  (zen:lookup-declaration (zen:walk-unit module-name) name access #f)))
