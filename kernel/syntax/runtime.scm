;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Syntax Runtime
;;;


(block kernel.runtime


;;;
;;;; Conditional
;;;


(define (zen:valid-conditional-requirement clause)
  (if (%%pair? clause)
      (let ((requirement (%%car clause)))
        (if (or (%%symbol? requirement)
                (and (%%pair? requirement)
                     (%%memq (%%car requirement) '(and or not))))
            requirement
          #f))
    #f))


(define (zen:conditional-satisfied? name requirement features)
  (cond ((%%symbol? requirement)
         (%%memq requirement features))
        ((and (%%pair? requirement)
              (%%eq? (%%car requirement) 'and))
         (zen:every? zen:feature-satisfied? (%%cdr requirement)))
        ((and (%%pair? requirement)
              (%%eq? (%%car requirement) 'or))
         (zen:some? zen:feature-satisfied? (%%cdr requirement)))
        ((and (%%pair? requirement)
              (%%eq? (%%car requirement) 'not)
              (%%pair? (%%cdr requirement)))
         (%%not (zen:feature-satisfied? (%%cadr requirement))))
        (else
         (error (zen:format "Invalid {a} requirement" name requirement)))))


(define (zen:process-conditional name conditional features fulfill? proc)
  (let ((clauses (%%cdr conditional)))
    (let iter ((scan clauses))
         (if (%%null? scan)
             (if fulfill?
                 (error (zen:format "Unfulfilled {a}" name)))
           (let ((clause (%%car scan)))
             (let ((requirement (zen:valid-conditional-requirement clause)))
               (if (%%not requirement)
                   (error (zen:format "Ill-formed {a} clause: {s}" name clause))
                 (if (or (zen:conditional-satisfied? name requirement features)
                         (%%eq? requirement 'else))
                     (proc (%%cdr clause))
                   (iter (%%cdr scan))))))))))


;;;
;;;; Feature
;;;


(define (zen:feature-satisfied? feature-requirement)
  (zen:conditional-satisfied? 'cond-expand feature-requirement (%%cond-expand-features)))


;;;
;;;; Invoice
;;;


(define (zen:filter-invoices invoices)
  (define (extract-feature-requirement invoice)
    (if (and (%%pair? invoice)
             (%%not (%%null? (%%cdr invoice)))
             (%%pair? (%%cadr invoice))
             (%%eq? (%%car (%%cadr invoice)) 'cond))
        (%%cadr (%%cadr invoice))
      #f))
  
  (%%apply append
           (map (lambda (invoice)
                  (let ((feature-requirement (extract-feature-requirement invoice)))
                    (cond ((%%not feature-requirement)
                           (%%list invoice))
                          ((zen:feature-satisfied? feature-requirement)
                           (%%list (%%cons (%%car invoice) (%%cddr invoice))))
                          (else
                           '()))))
                invoices)))


;;;
;;;; Unit
;;;


(define (zen:expand-unit-source rest)
  (define (parse rest proc)
    (let ((first (zen:source-code (%%car rest))))
      (if (%%memq first '(protected public))
          (proc (zen:source-code (%%cadr rest)) first (%%cddr rest))
        (proc (zen:source-code (%%car rest)) 'public (%%cdr rest)))))
  
  (parse rest
    (lambda (name access body)
      (if (%%not (%%symbol? name))
          (zen:error "Unit name must be a symbol: {s}" name)
        (if (and (zen:requested-unit-name) (%%neq? name (zen:requested-unit-name)))
            (zen:error "Unit at {s} is defining {s}" (zen:requested-unit-name) name)
          `(begin
             ,@(zen:declares 'unit)
             ,@body))))))


;;;
;;;; Simplify
;;;


(define (zen:simplify-begin form)
  (if (and (%%pair? form)
           (%%eq? (%%car form) 'begin)
           (%%pair? (%%cdr form))
           (%%null? (%%cddr form)))
      (%%cadr form)
    form))


(define (zen:simplify-let form)
  (if (and (%%pair? form)
           (%%eq? (%%car form) 'let)
           (%%pair? (%%cdr form))
           (%%pair? (%%cddr form)))
      (let ((bindings (%%cadr form))
            (body (%%cddr form)))
        (cond ((and (%%eqv? bindings '())
                    (%%null? (%%cdr body))
                    (let ((expr (%%car body)))
                      ;; inner define needs the let
                      (%%not (and (%%pair? expr)
                                  (%%eq? (%%car expr) 'define)))
                      ;; begin might start with a define needing the let
                      (%%not (and (%%pair? expr)
                                  (%%eq? (%%car expr) 'begin)))))
               (%%car body))
              ((and (%%pair? bindings)
                    (%%null? (%%cdr bindings))
                    (let ((binding (%%car bindings)))
                      (and (%%pair? binding)
                           (%%pair? (%%cdr binding))
                           (%%null? (%%cddr binding))
                           (%%symbol? (%%car binding))
                           (eq? (%%car binding) (%%car body)))))
               (%%car (%%cdar bindings)))
              (else
               form)))
    form))


;;;
;;;; Require
;;;


(define (zen:expand-require rest)
  (zen:simplify-begin
    `(begin
       ,@(map (lambda (require)
                (zen:parse-require (zen:listify require)
                  (lambda (unit-name feature-requirement phase)
                    #; ;; buggy
                    (if (%%eq? phase 'syntax)
                        (zen:load-unit unit-name))
                    `(zen:load-unit ',unit-name))))
              (zen:filter-invoices (map (lambda (src) (%%desourcify src)) rest))))))


(define (zen:parse-require require proc)
  (let ((name (%%car require))
        (scan (%%cdr require))
        (feature-requirement #f)
        (phase 'runtime))
    (if (and (%%pair? scan)
             (%%pair? (%%car scan))
             (%%eq? (%%caar scan) 'cond))
        (begin
          (set! feature-requirement (%%car (%%cdar scan)))
          (set! scan (%%cdr scan))))
    (if (and (%%pair? scan)
             (%%pair? (%%car scan))
             (%%eq? (%%caar scan) 'phase))
        (begin
          (set! phase (%%car (%%cdar scan)))
          (set! scan (%%cdr scan))))
    (proc name
          feature-requirement
          phase))))
