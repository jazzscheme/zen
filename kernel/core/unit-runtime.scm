;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Unit Runtime
;;;


(block core.unit-runtime


(define zen:for-each-subunit-continue
  (%%make-parameter #f))


(define (zen:outline-subunit unit-name)
  (let ((continue (zen:for-each-subunit-continue)))
    (if continue
        (zen:catch-exception-filter
          (lambda (exc)
            (or (%%is? exc zen:Walk-Source-Not-Found)
                (%%is? exc zen:Walk-Problems)))
          (lambda (exc)
            (continue unit-name exc)
            #f)
          (lambda ()
            (zen:outline-unit unit-name)))
      (zen:outline-unit unit-name))))


(define (zen:for-each-subunit toplevel-name proc)
  (let ((subunits (%%make-table test: eq?)))
    (let iter ((unit-name toplevel-name) (declaration (zen:outline-subunit toplevel-name)) (phase #f))
      (define (process-require require)
        (let ((name (zen:get-require-invoice-name require))
              (phase (zen:get-require-invoice-phase require)))
          (iterate unit-name name phase)))
      
      (define (process-export export)
        (let ((reference (zen:get-module-invoice-module export)))
          (if reference
              (let ((name (zen:get-declaration-reference-name reference))
                    (phase (zen:get-module-invoice-phase export)))
                (iterate unit-name name phase)))))
      
      (define (descendant-unit? unit-name descendant-name)
        (let ((unit (%%symbol->string unit-name))
              (descendant (%%symbol->string descendant-name)))
          (let ((unit-length (%%string-length unit))
                (descendant-length (%%string-length descendant)))
            (and (%%fx> descendant-length unit-length)
                 (%%string=? (%%substring descendant 0 unit-length) unit)
                 (%%eqv? (%%string-ref descendant unit-length) #\.)))))
      
      (define (iterate parent-name unit-name phase)
        (let ((declaration (zen:outline-subunit unit-name)))
          (if (and declaration (%%eq? (zen:get-declaration-access declaration) 'protected))
              (if (%%not (descendant-unit? toplevel-name unit-name))
                  (zen:error "Illegal access from {a} to protected unit {a}" toplevel-name unit-name)
                #; ;; debugging
                (let ((actual (%%table-ref subunits unit-name #f)))
                  (if actual
                      (zen:debug 'duplicate 'export 'for unit-name '-> actual parent-name)))
                (begin
                  (%%table-set! subunits unit-name parent-name)
                  (iter unit-name declaration phase))))))
      
      (if declaration
          (begin
            (proc unit-name declaration phase)
            (if (zen:is? declaration zen:Unit-Declaration)
                (for-each process-require (zen:get-unit-declaration-requires declaration))
              (begin
                (for-each process-require (zen:get-module-declaration-requires declaration))
                (for-each process-export (zen:get-module-declaration-exports declaration))))))))))
