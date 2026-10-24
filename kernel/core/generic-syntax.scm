;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Generic Syntax
;;;


(block core.generic-syntax


;;;
;;;; Expansion
;;;


(define (zen:dynamic-parameter? parameter)
  (and (%%pair? parameter)
       (let ((type (%%car parameter)))
         (or (%%pair? type)
             (zen:composite-identifier? type)))
       (%%pair? (%%cdr parameter))))


(define (zen:dynamic-parameter-type parameter)
  (let ((type (%%car parameter)))
    (if (zen:specifier? type)
        (zen:specifier->name type)
      type)))


(define (zen:dynamic-parameter-types parameters)
  (let iterate ((parameters parameters))
       (if (and (%%pair? parameters)
                (zen:dynamic-parameter? (%%car parameters)))
           (%%cons (zen:dynamic-parameter-type (%%car parameters))
                   (iterate (%%cdr parameters)))
         '())))


(define (zen:dynamic-parameter-name parameter)
  (%%cadr parameter))


(define (zen:dynamic-parameter-names parameters)
  (let iterate ((parameters parameters))
       (if (and (%%pair? parameters)
                (zen:dynamic-parameter? (%%car parameters)))
           (%%cons (zen:dynamic-parameter-name (%%car parameters))
                   (iterate (%%cdr parameters)))
         '())))


(define (zen:generic-parameters parameters)
  (let iterate ((parameters parameters)
                (result '()))
       (if (%%pair? parameters)
           (let ((parameter (%%car parameters)))
             (cond ((zen:dynamic-parameter? parameter)
                    (iterate (%%cdr parameters) (%%cons (zen:dynamic-parameter-name parameter) result)))
                   ((%%symbol? parameter)
                    (iterate (%%cdr parameters) (%%cons parameter result)))
                   (else ; #!xxx -> parsing delayed to specific
                    (values (zen:reverse! result) parameters))))
         (values (zen:reverse! result) parameters))))


(define (zen:specific-parameters parameters)
  (let iterate ((parameters parameters))
       (if (%%pair? parameters)
           (let ((parameter (%%car parameters)))
             (cond ((zen:dynamic-parameter? parameter)
                    (%%cons (zen:dynamic-parameter-name parameter) (iterate (%%cdr parameters))))
                   (else
                    parameters)))
         parameters)))


;;;
;;;; Generic
;;;


(define (zen:expand-define-generic signature . body)
  (let* ((generic-method-locator (%%car signature))
         (parameters (%%cdr signature))
         (dynamic-signature (zen:dynamic-parameter-types parameters))
         (formal-signature (zen:specific-parameters parameters))
         (specific-implementation-locator (zen:implementation-locator generic-method-locator dynamic-signature))
         (generic-locator (zen:generic-object-locator generic-method-locator))
         (gensym-generic (zen:generate-symbol "generic"))
         (gensym-specific (zen:generate-symbol "specific")))
    (receive (mandatory-parameters extra-parameters) (zen:generic-parameters parameters)
      (let ((gensym-rest (if (%%null? extra-parameters) '() (zen:generate-symbol "rest"))))
        `(begin
           (define ,specific-implementation-locator
             ,(if (%%null? body)
                  `(lambda ,formal-signature
                     (zen:error "Generic {a} is abstract" ,generic-method-locator))
                `(lambda ,formal-signature
                   ,@body)))
           (define ,generic-locator
             ;; we do not reprocess generic - aka you must restart to change its signature
             (if (zen:global-bound? ',generic-locator)
                 (let ((,gensym-generic (zen:global-ref ',generic-locator)))
                   (zen:generic-reset ,gensym-generic ,specific-implementation-locator)
                   ,gensym-generic)
               (zen:new-generic ',generic-method-locator (lambda () (%%list ,@dynamic-signature)) ,specific-implementation-locator)))
           (define ,generic-method-locator
             (lambda (,@mandatory-parameters . ,gensym-rest)
               (%%when (%%not (%%null? (zen:get-generic-pending-specifics ,generic-locator)))
                 (zen:process-pending-specifics ,generic-locator))
               (let ((,gensym-specific (%%specific-dispatch ,generic-locator (%%list ,@(map (lambda (parameter) `(zen:class-of ,parameter))
                                                                                            (zen:dynamic-parameter-names parameters))))))
                 ,(if (%%null? extra-parameters)
                      `(,gensym-specific ,@mandatory-parameters)
                    `(apply ,gensym-specific ,@mandatory-parameters ,gensym-rest))))))))))


(define (zen:generic-object-locator locator)
  (%%string->symbol (%%string-append (%%symbol->string locator) "!generic")))


;;;
;;;; Specific
;;;


(define (zen:expand-define-specific signature modifier . body)
  (let* ((root? (%%eq? modifier 'root))
         (generic-method-locator (%%car signature))
         (parameters (%%cdr signature))
         (dynamic-signature (zen:dynamic-parameter-types parameters))
         (formal-signature (zen:specific-parameters parameters))
         (specific-implementation-locator (zen:implementation-locator generic-method-locator dynamic-signature))
         (generic-locator (zen:generic-object-locator generic-method-locator))
         (gensym-specific (zen:generate-symbol "specific"))
         (gensym-lambda (zen:generate-symbol "lambda"))
         (nextmethod-bindings (if root? (%%list) (%%list `(nextmethod (zen:get-specific-implementation (%%car (zen:get-specific-ancestor-specifics ,gensym-specific))))))))
    `(define ,specific-implementation-locator
       (let* ((,gensym-specific (zen:new-specific (lambda () (%%list ,@dynamic-signature)) #f))
              (,gensym-lambda (lambda ,formal-signature
                                (let (,@nextmethod-bindings)
                                  ,@body))))
         (zen:set-specific-implementation ,gensym-specific ,gensym-lambda)
         (zen:register-specific ,generic-locator ,gensym-specific)
         ,gensym-lambda))))


(define (zen:implementation-locator generic-locator dynamic-signature)
  (let ((generic-string (%%symbol->string generic-locator))
        (dynamic-signature-strings (map (lambda (class/call)
                                          (%%symbol->string (if (%%pair? class/call) (%%car class/call) class/call)))
                                        dynamic-signature)))
    (%%string->symbol (%%string-append generic-string ":implementation:" (zen:join-strings dynamic-signature-strings "/")))))


;;;
;;;; Generic
;;;


;; locator - a.b.c.foo
;; name - foo
;; root-specific - (maybe abstract) specific matching generic signature
;; pending-specifics - delayed evaluation at boot -> signatures are proc (unnecessary now?)


(zen:define-class zen:Generic zen:Object (constructor: zen:allocate-generic)
  ((locator              getter: generate)
   (name                 getter: generate)
   (root-specific        getter: generate setter: generate)
   (pending-specifics    getter: generate setter: generate)))


(zen:define-macro (zen:define-generic . rest)
  (%%apply zen:expand-define-generic rest))


(zen:define-macro (%%specific-dispatch generic dynamic-classes)
  `(zen:get-specific-implementation (zen:dispatch-from-root ,generic ,dynamic-classes)))


;;;
;;;; Specific
;;;


;; dynamic-signature - classes of the dynamic parameters
;; implementation - lambda to call
;; best-ancestor - nextmethod specific
;; ancestor-specifics - nextmethod specific + other direct ancestors
;; descendant-specifics - reverse relation


(zen:define-class zen:Specific zen:Object (constructor: zen:allocate-specific)
  ((dynamic-signature    getter: generate setter: generate)
   (implementation       getter: generate setter: generate)
   (ancestor-specifics   getter: generate setter: generate)
   (descendant-specifics getter: generate setter: generate)))


(zen:define-macro (zen:define-specific . rest)
  (%%apply zen:expand-define-specific rest)))
