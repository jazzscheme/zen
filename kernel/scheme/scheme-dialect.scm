;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Scheme Dialect
;;;


(block scheme.scheme-dialect


;;;
;;;; Dialect
;;;


(zen:define-class zen:Scheme-Dialect zen:Dialect (constructor: zen:allocate-scheme-dialect)
  ())


(define (zen:new-scheme-dialect name)
  (zen:allocate-scheme-dialect name (%%make-table test: eq?) (%%make-table test: eq?)))


(zen:define-method (zen:dialect-walker (zen:Scheme-Dialect dialect))
  (zen:new-scheme-walker))


;;;
;;;; Walker
;;;


(zen:define-class zen:Scheme-Walker zen:Walker (constructor: zen:allocate-scheme-walker)
  ())


(define (zen:new-scheme-walker)
  (zen:allocate-scheme-walker #f #f '() '() '() (zen:new-queue) (zen:new-queue) (%%make-table test: eq?) (%%make-table test: eq?) '()))


(zen:define-method (zen:runtime-export (zen:Scheme-Walker walker) declaration)
  (or (nextmethod walker declaration)
      (if (%%is? declaration zen:Define-Declaration)
          (zen:get-declaration-locator declaration)
        #f)))


;;;
;;;; Environment
;;;


(zen:define-method (zen:walker-declarations (zen:Scheme-Walker walker))
  (cons (zen:get-dialect-declarations (zen:get-dialect 'zen.scheme))
        (nextmethod walker)))


(zen:define-method (zen:walker-bindings (zen:Scheme-Walker walker))
  (cons (zen:get-dialect-bindings (zen:get-dialect 'zen.scheme))
        (nextmethod walker)))


;;;
;;;; Define
;;;


(zen:define-class zen:Define-Declaration zen:Declaration (constructor: zen:allocate-define-declaration)
  ((signature        getter: generate)
   (specifier-source getter: generate)
   (value            getter: generate setter: generate)))


(define (zen:new-define-declaration name type parent signature specifier-source)
  (let ((new-declaration (zen:allocate-define-declaration name type #f 'private 'uptodate '() '() #f parent #f #f #f signature specifier-source #f)))
    (zen:setup-declaration new-declaration)
    new-declaration))


;; Not 100% Scheme compatible as it is possible to set! a define at runtime...
(zen:define-method (zen:walk-binding-validate-call (zen:Define-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (let ((signature (zen:get-define-declaration-signature declaration)))
    (if signature
        (zen:validate-arguments walker resume source-declaration declaration signature arguments form-src))))


(zen:define-method (zen:emit-declaration (zen:Define-Declaration declaration) walker resume environment)
  (let ((value (zen:get-define-declaration-value declaration)))
    (let ((expression (zen:emit-type-cast (zen:emit-expression value declaration walker resume environment) (zen:get-lexical-binding-type declaration) (zen:get-expression-source value) declaration walker resume environment)))
      (zen:sourcify-deep-if
        (let ((locator (zen:get-declaration-locator declaration)))
          `(begin
             (define ,locator
               ,expression)
             ,@(let ((name (zen:get-lexical-binding-name declaration))
                     (parent (zen:get-declaration-parent declaration)))
                 (if (%%is? parent zen:Module-Declaration)
                     (if (zen:get-generate? 'register)
                         `((zen:register-define ',(zen:get-lexical-binding-name parent) ',name ',locator))
                       '())
                   `((zen:add-field ,(zen:get-declaration-locator parent) (zen:new-define ',name ',locator)))))))
        (zen:get-declaration-source declaration)))))


(zen:define-method (zen:emit-binding-reference (zen:Define-Declaration declaration) source-declaration walker resume environment)
  (zen:new-code
    (zen:get-declaration-locator declaration)
    zen:Any
    #f))


(zen:define-method (zen:walk-binding-validate-assignment (zen:Define-Declaration declaration) walker resume source-declaration symbol-src)
  (nextmethod declaration walker resume source-declaration symbol-src)
  (%%when (%%neq? (zen:get-declaration-toplevel declaration) (zen:get-declaration-toplevel source-declaration))
    (zen:walk-error walker resume source-declaration symbol-src "Illegal inter-module assignment to: {s}" (zen:get-lexical-binding-name declaration))))


(zen:define-method (zen:walk-binding-assignable? (zen:Define-Declaration declaration))
  #t)


(zen:define-method (zen:emit-binding-assignment (zen:Define-Declaration declaration) value source-declaration walker resume environment form-src)
  (let ((value-code (zen:emit-expression value source-declaration walker resume environment)))
    (zen:new-code
      (let ((locator (zen:get-declaration-locator declaration)))
        `(set! ,locator ,(zen:emit-type-cast value-code (zen:get-lexical-binding-type declaration) source-declaration declaration walker resume environment)))
      zen:Any
      form-src)))


(zen:define-method (zen:tree-fold (zen:Define-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (zen:tree-fold (zen:get-define-declaration-value declaration) down up here seed environment))


(define (zen:walk-define-declaration walker resume declaration environment form-src)
  (receive (name name-src specifier specifier-source value parameters) (zen:parse-define walker resume declaration #f form-src)
    (if (%%not (%%class-is? declaration zen:Namespace-Declaration))
        (zen:walk-error walker resume declaration form-src "Ill-placed define")
      (let ((type (if specifier (zen:walk-specifier walker resume declaration environment specifier) zen:Any))
            (signature (and parameters (zen:walk-parameters walker resume declaration environment parameters #t #f))))
        (let ((effective-type (if signature (zen:signature->function-type signature type) type)))
          (let ((new-declaration (or (zen:find-declaration-child declaration name)
                                     (zen:new-define-declaration name effective-type declaration signature specifier-source))))
            (zen:set-declaration-source new-declaration form-src)
            (zen:set-declaration-name-source new-declaration name-src)
            (let ((effective-declaration (zen:add-declaration-child walker resume declaration new-declaration)))
              effective-declaration)))))))


(define (zen:walk-define walker resume declaration environment form-src)
  (receive (name name-src specifier specifier-source value parameters) (zen:parse-define walker resume declaration #t form-src)
    (if (%%not (%%class-is? declaration zen:Namespace-Declaration))
        (zen:walk-error walker resume declaration form-src "Ill-placed define")
      (let* ((new-declaration (zen:require-declaration declaration name))
             (new-environment (%%cons new-declaration environment)))
        (zen:set-define-declaration-value new-declaration (zen:walk walker resume new-declaration new-environment value))
        (zen:set-declaration-source new-declaration form-src)
        (zen:set-declaration-name-source new-declaration name-src)
        new-declaration))))


(zen:define-method (zen:outline-extract (zen:Define-Declaration declaration) meta)
  #f)


;;;
;;;; Define Special Form
;;;


(zen:define-class zen:Define-Special-Form-Declaration zen:Declaration (constructor: zen:allocate-define-special-form-declaration)
  ((signature getter: generate setter: generate)
   (body      getter: generate setter: generate)))


(define (zen:new-define-special-form-declaration name type parent signature)
  (let ((new-declaration (zen:allocate-define-special-form-declaration name type #f 'public 'uptodate '() '() #f parent #f #f #f signature #f)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(zen:define-method (zen:walk-binding-walkable? (zen:Define-Special-Form-Declaration declaration))
  #t)


(zen:define-method (zen:walk-binding-walk-form (zen:Define-Special-Form-Declaration binding) walker resume declaration environment form-src)
  (let ((locator (zen:get-declaration-locator binding)))
    (if (%%eq? (zen:get-declaration-toplevel binding) (zen:get-declaration-toplevel declaration))
        (zen:walk-error walker resume declaration form-src "Special forms cannot be used from within the same file: {s}" locator)
      (let ((parent-declaration (zen:get-declaration-parent binding)))
        (zen:load-unit (zen:get-declaration-locator (zen:get-declaration-toplevel parent-declaration)))
        (let ((special-form (zen:need-special-form locator)))
          (special-form walker resume declaration environment form-src))))))


(zen:define-method (zen:tree-fold (zen:Define-Special-Form-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (zen:tree-fold-list (zen:get-signature-expressions (zen:get-define-special-form-declaration-signature declaration)) down up here seed environment)
  (zen:tree-fold (zen:get-define-special-form-declaration-body declaration) down up here seed environment))


(zen:define-method (zen:outline-extract (zen:Define-Special-Form-Declaration declaration) meta)
  (let ((signature (zen:get-define-special-form-declaration-signature declaration)))
    `(special-form (,(zen:get-lexical-binding-name declaration) ,@(zen:outline-generate-signature signature)))))


(define zen:Special-Forms
  (%%make-table test: eq?))


(define (zen:register-special-form name proc)
  (table-set! zen:Special-Forms name proc))


(define (zen:get-special-form name)
  (table-ref zen:Special-Forms name #f))


(define (zen:need-special-form name)
  (or (zen:get-special-form name)
      (zen:error "Unable to find special-form: {s}" name)))


(zen:define-macro (zen:define-special-form signature . body)
  (let ((name (car signature))
        (parameters (cdr signature)))
    (let ((src `(lambda ,parameters ,@body)))
      `(begin
         (define ,name
           ,src)
         (zen:register-special-form ',name ,name)))))


(zen:define-method (zen:emit-declaration (zen:Define-Special-Form-Declaration declaration) walker resume environment)
  (let ((locator (zen:get-declaration-locator declaration))
        (signature (zen:get-define-special-form-declaration-signature declaration))
        (body (zen:get-define-special-form-declaration-body declaration)))
    (zen:with-annotated-frame (zen:annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (cons frame environment)))
          (zen:sourcify-deep-if
            `(zen:define-special-form ,(%%cons locator (zen:emit-signature signature declaration walker resume augmented-environment))
               ,@(zen:sourcified-form (zen:emit-expression body declaration walker resume augmented-environment)))
            (zen:get-declaration-source declaration)))))))


(define (zen:parse-define-special-form walker resume declaration rest)
  (let* ((signature (%%desourcify (%%car rest)))
         (body (%%cdr rest))
         (name (%%car signature))
         (type zen:Any)
         (parameters (%%cdr signature)))
    (values name type parameters body)))


(define (zen:walk-define-special-form-declaration walker resume declaration environment form-src)
  (receive (name type parameters body) (zen:parse-define-special-form walker resume declaration (%%cdr (zen:source-code form-src)))
    (let ((signature (zen:walk-parameters walker resume declaration environment parameters #t #f)))
      (let ((new-declaration (or (zen:find-declaration-child declaration name)
                                 (zen:new-define-special-form-declaration name type declaration signature))))
        (zen:set-declaration-source new-declaration form-src)
        (let ((effective-declaration (zen:add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (zen:walk-define-special-form walker resume declaration environment form-src)
  (receive (name type parameters body) (zen:parse-define-special-form walker resume declaration (%%cdr (zen:source-code form-src)))
    (let* ((new-declaration (zen:require-declaration declaration name)))
      (receive (signature augmented-environment) (zen:walk-parameters walker resume declaration environment parameters #t #t)
        (zen:set-define-special-form-declaration-signature new-declaration signature)
        (zen:set-define-special-form-declaration-body new-declaration
          (zen:walk-body walker resume new-declaration augmented-environment body))
        (zen:set-declaration-source new-declaration form-src)
        new-declaration))))


;;;
;;;; Define Macro
;;;


(zen:define-class zen:Define-Macro-Declaration zen:Declaration (constructor: zen:allocate-define-macro-declaration)
  ((signature getter: generate setter: generate)
   (body      getter: generate setter: generate)))


(define (zen:new-define-macro-declaration name type parent signature)
  (let ((new-declaration (zen:allocate-define-macro-declaration name type #f 'public 'uptodate '() '() #f parent #f #f #f signature #f)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(zen:define-method (zen:walk-binding-expandable? (zen:Define-Macro-Declaration declaration))
  #t)


(zen:define-method (zen:walk-binding-expand-form (zen:Define-Macro-Declaration binding) walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((locator (zen:get-declaration-locator binding)))
      (if (%%eq? (zen:get-declaration-toplevel binding) (zen:get-declaration-toplevel declaration))
          (zen:walk-error walker resume declaration form-src "Macros cannot be used from within the same file: {s}" locator)
        (let ((parent-declaration (zen:get-declaration-parent binding)))
          (zen:load-unit (zen:get-declaration-locator (zen:get-declaration-toplevel parent-declaration)))
          (let ((expander (zen:need-macro locator)))
            (zen:with-walker-context walker resume declaration form-src
              (lambda ()
                (%%apply expander (%%cdr form))))))))))


(zen:define-method (zen:emit-declaration (zen:Define-Macro-Declaration declaration) walker resume environment)
  (let ((locator (zen:get-declaration-locator declaration))
        (signature (zen:get-define-macro-declaration-signature declaration))
        (body (zen:get-define-macro-declaration-body declaration)))
    (zen:with-annotated-frame (zen:annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (zen:sourcify-deep-if
            `(zen:define-macro ,(%%cons locator (zen:emit-signature signature declaration walker resume augmented-environment))
               ,@(zen:sourcified-form (zen:emit-expression body declaration walker resume augmented-environment)))
            (zen:get-declaration-source declaration)))))))


(define (zen:parse-define-macro walker resume declaration rest)
  (%%assertion (and (%%not-null? rest) (%%not-null? (%%cdr rest))) (zen:walk-error walker resume declaration rest "Ill-formed define-macro")
    (let* ((signature (%%desourcify (%%car rest)))
           (body (%%cdr rest))
           (name (%%car signature))
           (type zen:Any)
           (parameters (%%cdr signature)))
      (values name type parameters body))))


(define (zen:walk-define-macro-declaration walker resume declaration environment form-src)
  (receive (name type parameters body) (zen:parse-define-macro walker resume declaration (%%cdr (zen:source-code form-src)))
    (let ((signature (zen:walk-parameters walker resume declaration environment parameters #t #f)))
      (let ((new-declaration (or (zen:find-declaration-child declaration name)
                                 (zen:new-define-macro-declaration name type declaration signature))))
        (zen:set-declaration-source new-declaration form-src)
        (let ((effective-declaration (zen:add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (zen:walk-define-macro walker resume declaration environment form-src)
  (receive (name type parameters body) (zen:parse-define-macro walker resume declaration (%%cdr (zen:source-code form-src)))
    (let ((new-declaration (zen:require-declaration declaration name)))
      (receive (signature augmented-environment) (zen:walk-parameters walker resume declaration environment parameters #t #t)
        (zen:set-define-macro-declaration-signature new-declaration signature)
        (zen:set-define-macro-declaration-body new-declaration
          (zen:walk-body walker resume new-declaration augmented-environment body))
        (zen:set-declaration-source new-declaration form-src)
        new-declaration))))


(zen:define-method (zen:tree-fold (zen:Define-Macro-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (zen:tree-fold-list (zen:get-signature-expressions (zen:get-define-macro-declaration-signature declaration)) down up here seed environment)
  (zen:tree-fold (zen:get-define-macro-declaration-body declaration) down up here seed environment))


(zen:define-method (zen:outline-extract (zen:Define-Macro-Declaration declaration) meta)
  #f)


;;;
;;;; Lambda
;;;


(zen:define-class zen:Lambda zen:Expression (constructor: zen:allocate-lambda)
  ((signature getter: generate)
   (body      getter: generate)))


(define (zen:new-lambda type source signature body)
  (zen:allocate-lambda type source signature body))


(zen:define-variable-override zen:emit-unsafe
  (lambda (expression source declaration walker resume environment)
    (let ((type (zen:get-expression-type expression))
          (signature (zen:get-lambda-signature expression))
          (body (zen:get-lambda-body expression)))
      (zen:with-annotated-frame (zen:annotate-signature signature)
        (lambda (frame)
          (let ((augmented-environment (%%cons frame environment)))
            (let ((signature-emit (zen:emit-signature signature declaration walker resume augmented-environment)))
              (let ((body-code (zen:emit-expression body declaration walker resume augmented-environment)))
                (zen:emit-unsafe-code expression source declaration walker resume environment augmented-environment signature-emit body-code)))))))))


(define (zen:emit-unsafe-code expression source declaration walker resume environment augmented-environment signature-emit body-code)
  (let ((type (zen:get-expression-type expression))
        (body-emit `(begin ,@(zen:sourcified-form body-code))))
    (let ((cast-body (zen:emit-return-cast (zen:new-code body-emit (zen:get-code-type body-code) #f) type (zen:get-expression-source expression) declaration walker resume environment)))
      ;; the simplify-begin is a quicky to make sure we are
      ;; eq? to emit-return-cast that also calls simplify-begin
      #;
      (let ((cast-needed? (%%neq? (zen:simplify-begin body-emit) (zen:simplify-begin cast-body))))
        (%%when cast-needed?
          (zen:warning "Warning: In {a}{a}: Typed definition needs return cast"
                        (zen:get-declaration-locator declaration)
                        (zen:present-expression-location source #f))))
      (let ((signature-casts '())
            (cast-body (zen:simplify-begin cast-body)))
        (zen:new-code
          (zen:lambda-emit expression declaration walker resume environment signature-emit signature-casts cast-body)
          (zen:new-function-type '() '() '() #f (zen:get-code-type body-code))
          (zen:get-expression-source expression))))))


(zen:define-variable-override zen:emit-safe
  (lambda (expression source declaration walker resume environment)
    (let ((type (zen:get-expression-type expression))
          (signature (zen:get-lambda-signature expression))
          (body (zen:get-lambda-body expression)))
      (zen:with-annotated-frame (zen:annotate-signature signature)
        (lambda (frame)
          (let ((augmented-environment (%%cons frame environment)))
            (let ((signature-emit (zen:emit-signature signature declaration walker resume augmented-environment)))
              (let ((body-code (zen:emit-expression body declaration walker resume augmented-environment)))
                (zen:emit-safe-code expression source declaration walker resume environment augmented-environment signature-emit body-code)))))))))


(define (zen:emit-safe-code expression source declaration walker resume environment augmented-environment signature-emit body-code)
  (let ((signature (zen:get-lambda-signature expression)))
    (let ((signature-casts (zen:emit-signature-casts signature declaration walker resume augmented-environment))
          (unsafe-locator (zen:unsafe-locator (zen:get-declaration-locator declaration))))
      (let ((call-expression `(,unsafe-locator ,@(map zen:get-lexical-binding-name (zen:get-signature-positional signature)))))
        (zen:new-code
          (zen:lambda-emit expression declaration walker resume environment signature-emit signature-casts call-expression)
          (zen:new-function-type '() '() '() #f (zen:get-code-type body-code))
          (zen:get-expression-source expression))))))


(zen:define-variable-override zen:emit-safe/unsafe
  (lambda (expression source declaration walker resume environment)
    (let ((type (zen:get-expression-type expression))
          (signature (zen:get-lambda-signature expression))
          (body (zen:get-lambda-body expression)))
      (parameterize ((zen:stack-frame-body body))
        (zen:with-annotated-frame (zen:annotate-signature signature)
          (lambda (frame)
            (let ((augmented-environment (%%cons frame environment)))
              (let ((signature-emit (zen:emit-signature signature declaration walker resume augmented-environment)))
                (let ((body-code (zen:emit-expression body declaration walker resume augmented-environment)))
                  (values (zen:emit-safe-code expression source declaration walker resume environment augmented-environment signature-emit body-code)
                          (zen:emit-unsafe-code expression source declaration walker resume environment augmented-environment signature-emit body-code)))))))))))


(zen:define-method (zen:emit-expression (zen:Lambda expression) declaration walker resume environment)
  (let ((type (zen:get-expression-type expression))
        (signature (zen:get-lambda-signature expression))
        (body (zen:get-lambda-body expression)))
    (parameterize ((zen:stack-frame-body body))
      (zen:with-annotated-frame (zen:annotate-signature signature)
        (lambda (frame)
          (let ((augmented-environment (%%cons frame environment)))
            (let ((signature-emit (zen:emit-signature signature declaration walker resume augmented-environment)))
              (let ((body-code (zen:emit-expression body declaration walker resume augmented-environment)))
                (let ((signature-casts (and (zen:get-check? 'lambda) (zen:emit-signature-casts signature declaration walker resume augmented-environment)))
                      (body-emit `(begin ,@(zen:sourcified-form body-code))))
                  (let ((cast-body (zen:simplify-begin (zen:emit-return-cast (zen:new-code body-emit (zen:get-code-type body-code) #f) type (zen:get-expression-source expression) declaration walker resume environment))))
                    (zen:new-code
                      (zen:lambda-emit expression declaration walker resume environment signature-emit signature-casts cast-body)
                      (zen:new-function-type '() '() '() #f (zen:get-code-type body-code))
                      (zen:get-expression-source expression))))))))))))


(define (zen:lambda-emit expression declaration walker resume environment signature-emit signature-casts cast-body)
  `(lambda ,signature-emit
     ,@(zen:add-signature-casts signature-casts cast-body)))


(zen:define-method (zen:tree-fold (zen:Lambda expression) down up here seed environment)
  (let ((signature (zen:get-lambda-signature expression)))
    (zen:tree-fold-list (zen:get-signature-expressions signature) down up here seed environment)
    (zen:with-annotated-frame (zen:annotate-signature signature)
      (lambda (frame)
        (let ((aug-env (cons frame environment)))
          (up expression
              seed
              (zen:tree-fold (zen:get-lambda-body expression) down up here (down expression seed environment) aug-env)
              environment))))))


(define (zen:walk-lambda walker resume declaration environment form-src)
  (%%assertion (%%not-null? (%%cdr (zen:source-code form-src))) (zen:walk-error walker resume declaration form-src "Ill-formed lambda")
    (let ((parameters (zen:source-code (%%cadr (zen:source-code form-src)))))
      (zen:parse-specifier (%%cddr (zen:source-code form-src))
        (lambda (specifier specifier-source body)
          (receive (signature augmented-environment) (zen:walk-parameters walker resume declaration environment parameters #t #t)
            (let ((type (if specifier (zen:walk-specifier walker resume declaration environment specifier) zen:Any))
                  (effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body)))
              (zen:new-lambda type form-src signature
                (zen:walk-body walker resume declaration augmented-environment effective-body)))))))))


;;;
;;;; Binding
;;;


(define (zen:parse-binding walker resume declaration environment form-src)
  (%%assertion (and (list? (zen:source-code form-src))
                    (let ((len (%%length (zen:source-code form-src))))
                      (or (%%fx= len 2)
                          (and (%%fx= len 3)
                               (zen:specifier? (zen:source-code (%%cadr (zen:source-code form-src))))))))
      (zen:walk-error walker resume declaration form-src "Ill-formed binding: {s}" (%%desourcify form-src))
    (let ((symbol-src (%%car (zen:source-code form-src))))
      (let ((symbol (zen:source-code symbol-src)))
        (zen:parse-specifier (%%cdr (zen:source-code form-src))
          (lambda (specifier specifier-source rest)
            (let ((type (if specifier (zen:walk-specifier walker resume declaration environment specifier) #f))
                  (value (%%car rest)))
              (values (zen:new-variable symbol type symbol-src specifier-source) value))))))))


;;;
;;;; Let
;;;


(zen:define-class zen:Let zen:Expression (constructor: zen:allocate-let)
  ((bindings getter: generate)
   (body     getter: generate)))


(define (zen:new-let source bindings body)
  (zen:allocate-let #f source bindings body))


;; warning duplicated code with emit-expression
(define (zen:annotate-let expression declaration walker resume environment proc)
  (let ((bindings (zen:get-let-bindings expression)))
    (zen:with-annotated-frame (zen:annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (zen:get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (for-each (lambda (binding annotated-variable)
                      (let ((value (%%cdr binding)))
                        (let ((value-code (zen:emit-expression value declaration walker resume augmented-environment)))
                          (zen:extend-annotated-type frame annotated-variable (zen:get-code-type value-code)))))
                       bindings
                       variables)
          (proc augmented-environment))))))


(zen:define-method (zen:emit-expression (zen:Let expression) declaration walker resume environment)
  (let ((bindings (zen:get-let-bindings expression))
        (body (zen:get-let-body expression)))
    (zen:with-annotated-frame (zen:annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (zen:get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (zen:emit-expression value declaration walker resume augmented-environment))
                                 (src (zen:get-variable-source variable)))
                             (zen:extend-annotated-type frame annotated-variable (zen:get-code-type value-code))
                             (zen:sourcify-deep-if
                               `(,(zen:emit-binding-symbol variable declaration environment) ,(zen:emit-type-cast value-code (zen:get-lexical-binding-type variable) (zen:get-expression-source value) declaration walker resume environment))
                               src))))
                       bindings
                       variables))
                (body-code (zen:emit-expression body declaration walker resume augmented-environment)))
            (zen:new-code
              `(let ,bindings-output
                 ,@(zen:sourcified-form body-code))
              (zen:get-code-type body-code)
              (zen:get-expression-source expression))))))))


(zen:define-method (zen:tree-fold (zen:Let expression) down up here seed environment)
  (let* ((bindings (zen:get-let-bindings expression))
         (aug-env (cons (map car bindings) environment))
         (seed1 (down expression seed environment))
         (seed2 (zen:tree-fold-list (map cdr bindings) down up here seed1 environment)))
    (up expression seed (zen:tree-fold (zen:get-let-body expression) down up here seed2 aug-env) environment)))


(define (zen:walk-let walker resume declaration environment form-src)
  (let ((unwrapped (zen:unwrap-syntactic-closure form-src)))
    (%%assertion (%%not-null? (%%cdr unwrapped)) (zen:walk-error walker resume declaration form-src "Ill-formed let")
      (if (%%symbol? (zen:unwrap-syntactic-closure (%%cadr unwrapped)))
          (zen:walk-named-let walker resume declaration environment form-src)
        (let* ((bindings-src (%%cadr unwrapped))
               (bindings (zen:unwrap-syntactic-closure bindings-src))
               (body (%%cddr (zen:source-code form-src))))
          (if (and (%%pair? bindings) (%%symbol? (zen:unwrap-syntactic-closure (%%car bindings))))
              (zen:signature-named-let walker resume declaration environment form-src bindings body)
            (%%assertion (or (%%null? bindings) (%%pair? bindings)) (zen:walk-error walker resume declaration bindings-src "Ill-formed let bindings: {s}" bindings)
              (let ((effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body)))
                (let ((augmented-environment environment)
                      (expanded-bindings (zen:new-queue)))
                  (for-each (lambda (binding-form)
                              (continuation-capture
                                (lambda (resume)
                                  (receive (variable value) (zen:parse-binding walker resume declaration environment binding-form)
                                    (zen:enqueue expanded-bindings (%%cons variable (zen:walk walker resume declaration environment value)))
                                    (set! augmented-environment (%%cons variable augmented-environment))))))
                            bindings)
                  (zen:new-let form-src
                                (zen:queue-list expanded-bindings)
                                (zen:walk-body walker resume declaration augmented-environment effective-body)))))))))))


;;;
;;;; Named Let
;;;


(zen:define-class zen:Named-Let zen:Let (constructor: zen:allocate-named-let)
  ((variable getter: generate)))


(define (zen:new-named-let source variable bindings body)
  (zen:allocate-named-let #f source bindings body variable))


(zen:define-method (zen:emit-expression (zen:Named-Let expression) declaration walker resume environment)
  (let ((variable (zen:get-named-let-variable expression))
        (bindings (zen:get-let-bindings expression))
        (body (zen:get-let-body expression)))
    (zen:with-annotated-frame (%%cons (zen:new-annotated-variable variable zen:Any zen:Any) (zen:annotate-bindings bindings))
      (lambda (frame)
        (let ((variables (zen:get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((variable-emit (zen:emit-binding-symbol variable declaration environment))
                (bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (zen:emit-expression value declaration walker resume augmented-environment))
                                 (src (zen:get-variable-source variable)))
                             (zen:extend-annotated-type frame annotated-variable (zen:get-code-type value-code))
                             (zen:sourcify-deep-if
                               `(,(zen:emit-binding-symbol variable declaration environment) ,(zen:emit-type-cast value-code (zen:get-lexical-binding-type variable) (zen:get-expression-source value) declaration walker resume environment))
                               src))))
                       bindings
                       (%%cdr variables)))
                (body-code (zen:emit-expression body declaration walker resume augmented-environment)))
            (zen:new-code
              `(let ,variable-emit ,bindings-output
                 ,@(zen:sourcified-form body-code))
              (zen:get-code-type body-code)
              (zen:get-expression-source expression))))))))


(zen:define-method (zen:tree-fold (zen:Named-Let expression) down up here seed environment)
  (let* ((bindings (zen:get-let-bindings expression))
         (aug-env (cons (cons (zen:get-named-let-variable expression) (map car bindings)) environment))
         (seed1 (down expression seed environment))
         (seed2 (zen:tree-fold-list (map cdr bindings) down up here seed1 environment)))
    (up expression seed (zen:tree-fold (zen:get-let-body expression) down up here seed2 aug-env) environment)))


(define (zen:walk-named-let walker resume declaration environment form-src)
  (let ((name (zen:source-code (%%cadr (zen:source-code form-src))))
        (bindings (zen:source-code (%%car (%%cddr (zen:source-code form-src)))))
        (body (%%cdr (%%cddr (zen:source-code form-src)))))
    (let ((augmented-environment environment)
          (expanded-bindings (zen:new-queue)))
      (for-each (lambda (binding-form)
                  (continuation-capture
                    (lambda (resume)
                      (receive (variable value) (zen:parse-binding walker resume declaration environment binding-form)
                        (zen:enqueue expanded-bindings (%%cons variable (zen:walk walker resume declaration environment value)))
                        (set! augmented-environment (%%cons variable augmented-environment))))))
                bindings)
      (let ((variable (zen:new-variable name #f #f #f)))
        (set! augmented-environment (%%cons variable augmented-environment))
        (zen:new-named-let form-src variable
          (zen:queue-list expanded-bindings)
          (zen:walk-body walker resume declaration augmented-environment body))))))


(define (zen:signature-named-let walker resume declaration environment form-src bindings body)
  (let ((name (zen:source-code (%%car bindings)))
        (bindings (%%cdr bindings)))
    (zen:walk-named-let walker resume declaration environment
      (zen:sourcify-deep-if
        `(let ,name ,bindings
           ,@body)
        form-src))))


;;;
;;;; Letstar
;;;


(zen:define-class zen:Letstar zen:Expression (constructor: zen:allocate-letstar)
  ((bindings getter: generate)
   (body     getter: generate)))


(define (zen:new-letstar source bindings body)
  (zen:allocate-letstar #f source bindings body))


(zen:define-method (zen:emit-expression (zen:Letstar expression) declaration walker resume environment)
  (let ((bindings (zen:get-letstar-bindings expression))
        (body (zen:get-letstar-body expression)))
    (zen:with-annotated-frame (zen:annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (zen:get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (zen:emit-expression value declaration walker resume augmented-environment))
                                 (src (zen:get-variable-source variable)))
                             (zen:extend-annotated-type frame annotated-variable (zen:get-code-type value-code))
                             (zen:sourcify-deep-if
                               `(,(zen:emit-binding-symbol variable declaration environment) ,(zen:emit-type-cast value-code (zen:get-lexical-binding-type variable) (zen:get-expression-source value) declaration walker resume environment))
                               src))))
                       bindings
                       variables))
                (body-code (zen:emit-expression body declaration walker resume augmented-environment)))
            (zen:new-code
              `(let* ,bindings-output
                 ,@(zen:sourcified-form body-code))
              (zen:get-code-type body-code)
              (zen:get-expression-source expression))))))))


(zen:define-method (zen:tree-fold (zen:Letstar expression) down up here seed environment)
  (let lp ((ls (zen:get-letstar-bindings expression))
           (seed2 (down expression seed environment))
           (aug-env environment))
       (if (pair? ls)
           (lp (cdr ls)
               (zen:tree-fold (cdar ls) down up here seed2 aug-env)
               (cons (list (caar ls)) aug-env))
         (up expression seed (zen:tree-fold (zen:get-letstar-body expression) down up here seed2 aug-env) environment))))


(define (zen:walk-letstar walker resume declaration environment form-src)
  (%%assertion (%%not-null? (%%cdr (zen:unwrap-syntactic-closure form-src))) (zen:walk-error walker resume declaration form-src "Ill-formed let*")
    (let ((bindings (zen:unwrap-syntactic-closure (%%cadr (zen:unwrap-syntactic-closure form-src))))
          (body (%%cddr (zen:source-code form-src))))
      (let ((augmented-environment environment)
            (expanded-bindings (zen:new-queue)))
        (for-each (lambda (binding-form)
                    (continuation-capture
                      (lambda (resume)
                        (receive (variable value) (zen:parse-binding walker resume declaration environment binding-form)
                          (zen:enqueue expanded-bindings (%%cons variable (zen:walk walker resume declaration augmented-environment value)))
                          (set! augmented-environment (%%cons variable augmented-environment))))))
                  bindings)
        (zen:new-letstar form-src
                          (zen:queue-list expanded-bindings)
                          (zen:walk-body walker resume declaration augmented-environment body))))))


;;;
;;;; Letrec
;;;


(zen:define-class zen:Letrec zen:Expression (constructor: zen:allocate-letrec)
  ((bindings getter: generate)
   (body     getter: generate)))


(define (zen:new-letrec source bindings body)
  (zen:allocate-letrec #f source bindings body))


(zen:define-method (zen:emit-expression (zen:Letrec expression) declaration walker resume environment)
  (let ((bindings (zen:get-letrec-bindings expression))
        (body (zen:get-letrec-body expression)))
    (zen:with-annotated-frame (zen:annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (zen:get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (value (%%cdr binding)))
                           (let ((value-code (zen:emit-expression value declaration walker resume augmented-environment))
                                 (src (zen:get-variable-source variable)))
                             (zen:extend-annotated-type frame annotated-variable (zen:get-code-type value-code))
                             (zen:sourcify-deep-if
                               `(,(zen:emit-binding-symbol variable declaration environment) ,(zen:emit-type-cast value-code (zen:get-lexical-binding-type variable) (zen:get-expression-source value) declaration walker resume environment))
                               src))))
                       bindings
                       variables))
                (body-code (zen:emit-expression body declaration walker resume augmented-environment)))
            (zen:new-code
              `(letrec ,bindings-output
                 ,@(zen:sourcified-form body-code))
              (zen:get-code-type body-code)
              (zen:get-expression-source expression))))))))


(zen:define-method (zen:tree-fold (zen:Letrec expression) down up here seed environment)
  (let* ((bindings (zen:get-letrec-bindings expression))
         (aug-env (cons (map car bindings) environment))
         (seed1 (down expression seed environment))
         (seed2 (zen:tree-fold-list (map cdr bindings) down up here seed1 aug-env)))
    (up expression seed (zen:tree-fold (zen:get-letrec-body expression) down up here seed2 aug-env) environment)))


(define (zen:walk-letrec walker resume declaration environment form-src)
  (%%assertion (%%not-null? (%%cdr (zen:unwrap-syntactic-closure form-src))) (zen:walk-error walker resume declaration form-src "Ill-formed letrec")
    (let ((bindings (zen:unwrap-syntactic-closure (%%cadr (zen:unwrap-syntactic-closure form-src))))
          (body (%%cddr (zen:source-code form-src))))
      (let* ((new-variables (map (lambda (binding-form) (zen:new-variable (zen:source-code (%%car (zen:source-code binding-form))) #f #f #f)) bindings))
             (augmented-environment (%%append new-variables environment))
             (expanded-bindings (zen:new-queue)))
        (for-each (lambda (variable binding-form)
                    (continuation-capture
                      (lambda (resume)
                        (let ((value (%%cadr (zen:source-code binding-form))))
                          (zen:enqueue expanded-bindings (%%cons variable (zen:walk walker resume declaration augmented-environment value)))))))
                  new-variables
                  bindings)
        (zen:new-letrec form-src
                         (zen:queue-list expanded-bindings)
                         (zen:walk-body walker resume declaration augmented-environment body))))))


;;;
;;;; Let Macro
;;;


(zen:define-class zen:Let-Macro zen:Expression (constructor: zen:allocate-let-macro)
  ((body getter: generate)))


(define (zen:new-let-macro source body)
  (zen:allocate-let-macro #f source body))


(zen:define-method (zen:emit-expression (zen:Let-Macro expression) declaration walker resume environment)
  (let ((body (zen:get-let-macro-body expression)))
    (let ((body-emit (zen:emit-expression body declaration walker resume environment)))
      (zen:new-code
        (zen:simplify-begin
          `(begin
             ,@(zen:sourcified-form body-emit)))
        (zen:get-code-type body-emit)
        #f))))


(zen:define-method (zen:tree-fold (zen:Let-Macro expression) down up here seed environment)
  (let ((seed1 (down expression seed environment)))
    (up expression seed (zen:tree-fold (zen:get-let-macro-body expression) down up here seed1 environment) environment)))


;;;
;;;; Let Symbol
;;;


(zen:define-class zen:Let-Symbol zen:Expression (constructor: zen:allocate-let-symbol)
  ((body getter: generate)))


(define (zen:new-let-symbol source body)
  (zen:allocate-let-symbol #f source body))


(zen:define-method (zen:emit-expression (zen:Let-Symbol expression) declaration walker resume environment)
  (let ((body (zen:get-let-symbol-body expression)))
    (let ((body-emit (zen:emit-expression body declaration walker resume environment)))
      (zen:new-code
        (zen:simplify-begin
          `(begin
             ,@(zen:sourcified-form body-emit)))
        (zen:get-code-type body-emit)
        #f))))


(zen:define-method (zen:tree-fold (zen:Let-Symbol expression) down up here seed environment)
  (let ((seed1 (down expression seed environment)))
    (up expression seed (zen:tree-fold (zen:get-let-symbol-body expression) down up here seed1 environment) environment)))


;;;
;;;; Receive
;;;


(zen:define-class zen:Receive zen:Expression (constructor: zen:allocate-receive)
  ((variables  getter: generate)
   (expression getter: generate)
   (body       getter: generate)))


(define (zen:new-receive source variables expression body)
  (zen:allocate-receive #f source variables expression body))


(zen:define-method (zen:emit-expression (zen:Receive expression) declaration walker resume environment)
  (let ((variables (zen:get-receive-variables expression))
        (expr (zen:get-receive-expression expression))
        (body (zen:get-receive-body expression)))
    (let ((expression-output (zen:emit-expression expr declaration walker resume environment)))
      (zen:with-annotated-frame (zen:annotate-receive variables (zen:get-code-type expression-output))
        (lambda (frame)
          (let ((augmented-environment (%%cons frame environment)))
            (let ((bindings-output (map (lambda (variable)
                                          (zen:emit-binding-symbol variable declaration environment))
                                        variables))
                  (body-code (zen:emit-expression body declaration walker resume augmented-environment)))
              (zen:new-code
                `(receive ,bindings-output
                     ,(zen:sourcified-form expression-output)
                   ,@(zen:sourcified-form body-code))
                (zen:get-code-type body-code)
                (zen:get-expression-source expression)))))))))


(zen:define-method (zen:tree-fold (zen:Receive expression) down up here seed environment)
  (let* ((aug-env (cons (zen:get-receive-variables expression) environment))
         (seed1 (down expression seed environment))
         (seed2 (zen:tree-fold (zen:get-receive-expression expression) down up here seed1 environment)))
    (up expression seed (zen:tree-fold (zen:get-receive-body expression) down up here seed2 aug-env) environment)))


(define (zen:walk-receive walker resume declaration environment form-src)
  (define (walk-parameters parameters)
    (let ((queue (zen:new-queue)))
      (let iter ((scan parameters))
        (if (%%null? scan)
            (zen:queue-list queue)
          (let ((expr (zen:source-code (%%car scan))))
            (%%assert (%%symbol? expr)
              (zen:parse-specifier (%%cdr scan)
                (lambda (specifier specifier-source rest)
                  (let ((type (if specifier (zen:walk-specifier walker resume declaration environment specifier) zen:Any)))
                    (zen:enqueue queue (zen:new-variable expr type #f #f))
                    (iter rest))))))))))
  
  (%%assertion (%%not-null? (%%cdr (zen:unwrap-syntactic-closure form-src))) (zen:walk-error walker resume declaration form-src "Ill-formed receive")
    (let* ((parameters (zen:source-code (%%cadr (zen:unwrap-syntactic-closure form-src))))
           (expression (%%car (%%cddr (zen:unwrap-syntactic-closure form-src))))
           (body (%%cdr (%%cddr (zen:source-code form-src))))
           (variables (walk-parameters parameters))
           (new-environment (%%append variables environment)))
      (zen:new-receive form-src variables (continuation-capture
                                             (lambda (resume)
                                               (zen:walk walker resume declaration environment expression)))
        (zen:walk-body walker resume declaration new-environment body)))))


;;;
;;;; Let Macro
;;;


(define (zen:walk-let-macro walker resume declaration environment form-src)
  (let ((form (%%cdr (zen:source-code form-src))))
    (let* ((bindings (%%desourcify (%%car form)))
           (body (%%cdr form))
           (macro-forms (map (lambda (binding)
                               (let ((name (%%car binding))
                                     (expander (%%cadr binding)))
                                 (zen:new-let-macro-form name (eval expander))))
                             bindings))
           (new-environment (%%append macro-forms environment)))
      (zen:new-let-macro form-src
        (zen:walk-body walker resume declaration new-environment body)))))


;;;
;;;; Let Symbol
;;;


(define (zen:walk-let-symbol walker resume declaration environment form-src)
  (let ((form (%%cdr (zen:source-code form-src))))
    (let* ((bindings (%%desourcify (%%car form)))
           (body (%%cdr form))
           (macro-symbols (map (lambda (binding)
                                 (let ((name (%%car binding))
                                       (getter (%%cadr binding))
                                       (setter (%%car (%%cddr binding))))
                                   (zen:new-macro-symbol name (eval getter) (eval setter))))
                               bindings))
           (new-environment (%%append macro-symbols environment)))
      (zen:new-let-symbol form-src
         (zen:walk-body walker resume declaration new-environment body)))))


;;;
;;;; If
;;;


(define (zen:walk-if walker resume declaration environment form-src)
  (if (%%fx< (%%length (zen:source-code form-src)) 3)
      (zen:walk-error walker resume declaration form-src "Ill-formed if: {s}" (%%desourcify form-src))
    (let ((test (%%cadr (zen:source-code form-src)))
          (yes (%%car (%%cddr (zen:source-code form-src))))
          (no (%%cdr (%%cddr (zen:source-code form-src)))))
      (zen:new-if form-src
                   (continuation-capture
                     (lambda (resume)
                       (zen:walk walker resume declaration environment test)))
                   (continuation-capture
                     (lambda (resume)
                       (zen:walk walker resume declaration environment yes)))
                   (if (%%null? no)
                       #f
                     (zen:walk walker resume declaration environment
                       (%%cons 'begin no)))))))


;;;
;;;; Cond
;;;


(define (zen:walk-cond walker resume declaration environment form-src)
  (let ((clauses (%%cdr (zen:source-code form-src))))
    (if (%%null? clauses)
        (zen:walk-error walker resume declaration form-src "Ill-formed cond")
      (let ((expanded-clauses (zen:new-queue)))
        (for-each (lambda (clause)
                    (%%assertion (%%pair? (zen:source-code clause)) (zen:walk-error walker resume declaration clause "Ill-formed cond clause: {s}" (%%desourcify clause))
                      (let ((test (%%car (zen:source-code clause)))
                            (body (%%cdr (zen:source-code clause))))
                        (continuation-capture
                          (lambda (resume)
                            (zen:enqueue expanded-clauses
                                          (if (and (%%not-null? body) (%%eq? (zen:unwrap-syntactic-closure (%%car body)) '=>))
                                              (%%cons (zen:walk walker resume declaration environment test)
                                                      (%%cons #t (zen:walk walker resume declaration environment (%%cadr body))))
                                            (%%cons (if (%%eq? (zen:unwrap-syntactic-closure test) 'else)
                                                        (begin
                                                          (%%when (%%null? body)
                                                            (zen:walk-error walker resume declaration clause "Ill-formed else clause: {s}" (%%desourcify clause)))
                                                          #f)
                                                      (zen:walk walker resume declaration environment test))
                                                    (%%cons #f (and (%%not-null? body) (zen:walk-implicit-begin walker resume declaration environment clause body)))))))))))
                  clauses)
        (zen:new-cond form-src (zen:queue-list expanded-clauses))))))


;;;
;;;; Case
;;;


(define (zen:walk-case walker resume declaration environment form-src)
  (let ((form (%%cdr (zen:source-code form-src))))
    (if (%%null? form)
        (zen:walk-error walker resume declaration form-src "Ill-formed case")
      (let ((target (%%car form))
            (clauses (%%cdr form)))
        (if (%%null? clauses)
            (zen:walk-error walker resume declaration form-src "Ill-formed case")
          (zen:new-case form-src
                         (continuation-capture
                           (lambda (resume)
                             (zen:walk walker resume declaration environment target)))
                         (map (lambda (clause)
                                (if (%%not (%%pair? (zen:source-code clause)))
                                    (zen:walk-error walker resume declaration clause "Ill-formed case clause: {s}" (%%desourcify clause))
                                  (continuation-capture
                                    (lambda (resume)
                                      (let* ((tries-src (%%car (zen:source-code clause)))
                                             (tries (%%desourcify tries-src))
                                             (body (%%cdr (zen:source-code clause)))
                                             (effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body)))
                                        (if (or (%%eq? (zen:unwrap-syntactic-closure tries) 'else) (%%pair? tries))
                                            (%%cons tries (zen:walk-implicit-begin walker resume declaration environment clause effective-body))
                                          (zen:walk-error walker resume declaration tries-src "Ill-formed selector list: {s}" tries)))))))
                              clauses)))))))


;;;
;;;; And
;;;


(define (zen:walk-and walker resume declaration environment form-src)
  (zen:new-and form-src (zen:walk-list walker resume declaration environment (%%cdr (zen:source-code form-src)))))


;;;
;;;; Or
;;;


(define (zen:walk-or walker resume declaration environment form-src)
  (zen:new-or form-src (zen:walk-list walker resume declaration environment (%%cdr (zen:source-code form-src)))))


;;;
;;;; Begin
;;;


(define (zen:walk-begin walker resume declaration environment form-src)
  (let ((body (%%cdr (zen:source-code form-src))))
    (zen:new-begin form-src (zen:walk-list walker resume declaration environment body))))


(define (zen:walk-implicit-begin walker resume declaration environment form-src form-list)
  (if (%%null? form-list)
      (zen:walk walker resume declaration environment '(unspecified))
    (zen:new-begin form-src (zen:walk-list walker resume declaration environment form-list))))


;;;
;;;; Do
;;;


(zen:define-class zen:Do zen:Expression (constructor: zen:allocate-do)
  ((bindings getter: generate)
   (test     getter: generate)
   (result   getter: generate)
   (body     getter: generate)))


(define (zen:new-do bindings test result body)
  (zen:allocate-do #f #f bindings test result body))


(zen:define-method (zen:emit-expression (zen:Do expression) declaration walker resume environment)
  (let ((bindings (zen:get-do-bindings expression))
        (test (zen:get-do-test expression))
        (result (zen:get-do-result expression))
        (body (zen:get-do-body expression)))
    (zen:with-annotated-frame (zen:annotate-bindings bindings)
      (lambda (frame)
        (let ((variables (zen:get-annotated-frame-variables frame))
              (augmented-environment (%%cons frame environment)))
          (let ((bindings-output
                  (map (lambda (binding annotated-variable)
                         (let ((variable (%%car binding))
                               (init (%%cadr binding))
                               (step (%%cddr binding)))
                           (let ((init-code (zen:sourcified-form (zen:emit-expression init declaration walker resume augmented-environment)))
                                 (step-code-list (if step (%%list (zen:sourcified-form (zen:emit-expression step declaration walker resume augmented-environment))) '())))
                             `(,(zen:emit-binding-symbol variable declaration environment)
                               ,init-code
                               ,@step-code-list))))
                       bindings
                       variables))
                (test-code (zen:emit-expression test declaration walker resume augmented-environment))
                (result-code (zen:emit-expression result declaration walker resume augmented-environment))
                (body-code (zen:emit-expression body declaration walker resume augmented-environment)))
            (zen:new-code
              `(do ,bindings-output
                   (,(zen:sourcified-form test-code) ,@(zen:sourcified-form result-code))
                 ,@(zen:sourcified-form body-code))
              (zen:get-code-type result-code)
              #f)))))))


(zen:define-method (zen:tree-fold (zen:Do expression) down up here seed environment)
  (let* ((aug-env (cons (map car (zen:get-do-bindings expression)) environment))
         (seed1 (zen:tree-fold-list (map cadr (zen:get-do-bindings expression)) down up here (down expression seed environment) environment))
         (seed2 (zen:tree-fold-list (map (lambda (x) (or (cddr x) (cadr x))) (zen:get-do-bindings expression)) down up here seed1 aug-env)))
    (up expression
        seed
        (zen:tree-fold
          (zen:get-do-result expression) down up here
          (zen:tree-fold
            (zen:get-do-body expression) down up here
            (zen:tree-fold (zen:get-do-test expression) down up here seed2 aug-env)
            aug-env)
          aug-env)
        environment)))


(define (zen:walk-do walker resume declaration environment form-src)
  (let ((bindings (zen:source-code (%%cadr (zen:source-code form-src))))
        (test (%%car (zen:source-code (%%car (%%cddr (zen:source-code form-src))))))
        (result (%%cdr (zen:source-code (%%car (%%cddr (zen:source-code form-src))))))
        (body (%%cdr (%%cddr (zen:source-code form-src)))))
    (let* ((new-variables (map (lambda (binding-form) (zen:new-variable (zen:source-code (%%car (zen:source-code binding-form))) #f #f #f)) bindings))
           (augmented-environment (%%append new-variables environment))
           (expanded-bindings (zen:new-queue)))
      (for-each (lambda (variable binding-form)
                  (continuation-capture
                    (lambda (resume)
                      (let ((init (zen:walk walker resume declaration augmented-environment (%%cadr (zen:source-code binding-form))))
                            (step (if (%%null? (%%cddr (zen:source-code binding-form))) #f (zen:walk walker resume declaration augmented-environment (%%car (%%cddr (zen:source-code binding-form)))))))
                        (zen:enqueue expanded-bindings (%%cons variable (%%cons init step)))))))
                new-variables
                bindings)
      (zen:new-do (zen:queue-list expanded-bindings)
                   (continuation-capture
                     (lambda (resume)
                       (zen:walk walker resume declaration augmented-environment test)))
                   (continuation-capture
                     (lambda (resume)
                       (zen:walk-body walker resume declaration augmented-environment result)))
                   (zen:walk-body walker resume declaration augmented-environment body)))))

;;;
;;;; Delay
;;;


(define (zen:walk-delay walker resume declaration environment form-src)
  (let ((expression (%%cadr (zen:source-code form-src))))
    (zen:new-delay (zen:walk walker resume declaration environment expression))))


;;;
;;;; Unspecific
;;;


(zen:define-class zen:Unspecific zen:Expression (constructor: zen:allocate-unspecific)
  ((expressions getter: generate)))


(define (zen:new-unspecific source expressions)
  (zen:allocate-unspecific #f source expressions))


(zen:define-method (zen:emit-expression (zen:Unspecific expression) declaration walker resume environment)
  (let ((expressions (zen:get-unspecific-expressions expression)))
    (let ((code (zen:emit-statements-code expressions declaration walker resume environment)))
      (zen:new-code
        `(begin ,@(zen:sourcified-form code) (%%unspecified))
        (zen:get-code-type code)
        (zen:get-expression-source expression)))))


(zen:define-method (zen:tree-fold (zen:Unspecific expression) down up here seed environment)
  (up expression
      seed
      (zen:tree-fold-list
        (zen:get-unspecific-expressions expression) down up here
        (down expression seed environment)
        environment)
      environment))


(define (zen:walk-unspecific walker resume declaration environment form-src)
  (let ((body (%%cdr (zen:source-code form-src))))
    (zen:new-unspecific form-src (zen:walk-list walker resume declaration environment body))))


;;;
;;;; Reference Reification
;;;


(zen:define-class zen:Reference-Reification zen:Expression (constructor: zen:allocate-reference-reification)
  ((reference getter: generate)
   (resolver  getter: generate)))


(define (zen:new-reference-reification source reference resolver)
  (zen:allocate-reference-reification #f source reference resolver))


(zen:define-method (zen:emit-expression (zen:Reference-Reification expression) declaration walker resume environment)
  ;; all this should be cleanly obtained from the code walk but it requires a
  ;; non-trivial work of changing references to remember how they where obtained
  (define (determine-serialization reference)
    (let ((reified-reference (zen:get-reference-reification-reference expression)))
      (if (zen:composite-reference? reified-reference)
          (receive (module-name name) (zen:break-reference reified-reference)
            `(module-public ,module-name ,name))
        (let ((module (zen:get-declaration-toplevel declaration))
              (binding (zen:get-binding-reference-binding reference)))
          (if (%%is? binding zen:Declaration)
              (if (%%eq? (zen:get-declaration-toplevel binding) module)
                  `(module-private ,(zen:get-declaration-locator binding))
                (let ((import (find-import module reified-reference))
                      (name (zen:get-lexical-binding-name binding)))
                  (if import
                      `(module-public ,import ,name)
                    #f)))
            #f)))))
  
  (define (find-import module symbol)
    (let iter ((scan (zen:get-module-declaration-imports module)))
      (if (%%null? scan)
          #f
        (let ((module-invoice (%%car scan)))
          (let ((imported-module-declaration (zen:get-module-invoice-module module-invoice)))
            (let ((imported (zen:get-public-lookup imported-module-declaration)))
              (if (%%table-ref imported symbol #f)
                  (zen:get-lexical-binding-name (zen:get-module-invoice-module module-invoice))
                (iter (%%cdr scan)))))))))
  
  (let ((resolver (zen:get-reference-reification-resolver expression)))
    (let ((reference (%%car (zen:get-body-expressions (zen:get-lambda-body resolver)))))
      (%%assert (%%is? reference zen:Binding-Reference)
        (let ((serialization (determine-serialization reference)))
          (zen:new-code
            `(zen:new-runtime-reference ,(zen:get-code-form (zen:emit-expression resolver declaration walker resume environment)) ',serialization)
            zen:Any
            #f))))))


(define (zen:walk-reify-reference walker resume declaration environment form-src)
  (let ((reference-src (%%cadr (zen:source-code form-src))))
    (let ((resolver `(lambda () ,reference-src)))
      (zen:new-reference-reification form-src (zen:source-code reference-src) (zen:walk walker resume declaration environment resolver)))))


;;;
;;;; Register
;;;


(zen:define-dialect zen.scheme
  (zen:new-scheme-dialect 'zen.scheme))

(zen:define-dialect zen.scheme.dialect
  (zen:new-scheme-dialect 'zen.scheme))


(zen:define-walker-declaration define              zen.scheme zen:walk-define-declaration zen:walk-define)
(zen:define-walker-declaration define-macro        zen.scheme zen:walk-define-macro-declaration zen:walk-define-macro)
(zen:define-walker-declaration define-special-form zen.scheme zen:walk-define-special-form-declaration zen:walk-define-special-form)
;; quicky to put this here
(zen:define-walker-declaration special-form        zen.scheme zen:walk-define-special-form-declaration zen:walk-define-special-form)
(zen:define-walker-special     cond-expand         zen.scheme zen:walk-cond-expand)
(zen:define-walker-special     quote               zen.scheme zen:walk-quote)
(zen:define-walker-special     begin               zen.scheme zen:walk-begin)
(zen:define-walker-special     lambda              zen.scheme zen:walk-lambda)
(zen:define-walker-special     let                 zen.scheme zen:walk-let)
(zen:define-walker-special     let*                zen.scheme zen:walk-letstar)
(zen:define-walker-special     letrec              zen.scheme zen:walk-letrec)
(zen:define-walker-special     let-macro           zen.scheme zen:walk-let-macro)
(zen:define-walker-special     let-symbol          zen.scheme zen:walk-let-symbol)
(zen:define-walker-special     receive             zen.scheme zen:walk-receive)
(zen:define-walker-special     set!                zen.scheme zen:walk-setbang)
(zen:define-walker-special     and                 zen.scheme zen:walk-and)
(zen:define-walker-special     or                  zen.scheme zen:walk-or)
(zen:define-walker-special     if                  zen.scheme zen:walk-if)
(zen:define-walker-special     case                zen.scheme zen:walk-case)
(zen:define-walker-special     cond                zen.scheme zen:walk-cond)
(zen:define-walker-special     do                  zen.scheme zen:walk-do)
(zen:define-walker-special     delay               zen.scheme zen:walk-delay)
(zen:define-walker-special     quasiquote          zen.scheme zen:walk-quasiquote)
(zen:define-walker-special     unspecific          zen.scheme zen:walk-unspecific)
(zen:define-walker-special     reify-reference     zen.scheme zen:walk-reify-reference))
