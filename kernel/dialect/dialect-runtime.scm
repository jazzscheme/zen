;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Dialect Runtime
;;;


;; Concepts
;; - lookup: direct lookup of a symbol returning a declaration
;; - resolve: references are resolved only on first usage
;; - expand take a source code expr and expand it to equivalent scheme code
;;   - walk: take a source code expr and generate its expression tree
;;   - emit: take an expression tree and emit implementation scheme code
;;
;; Notes
;; - It is key to have compile time reference loading identical to runtime module loading
;;   to ensure that runtime problems are detected at compile time exactly as they would occur
;; - Autoload declarations are treated specially as they are the only case where different
;;   references to the same declaration generate different code: direct access will expand to
;;   just the locator while an autoload access must add code to load the unit
;;
;; Todo
;; - Is the extra indirection level of having declaration references really necessary?
;; - Convert and remove the temporary patch zen:register-autoload that was used to implement
;;   the old load
;; - Cleanup the probably not usefull new method zen:resolve-binding that I added to get
;;   things working


(block dialect.dialect-runtime


;; mega hack to test idea
(define zen:call-being-inlined
  (make-parameter #f))


;;;
;;;; Dialect
;;;


(zen:define-class zen:Dialect zen:Object ()
  ((name         getter: generate)
   (declarations getter: generate)
   (bindings     getter: generate)))


(zen:define-virtual (zen:dialect-walker (zen:Dialect dialect)))
(zen:define-virtual (zen:dialect-wrap (zen:Dialect dialect) body))


(zen:define-method (zen:dialect-walker (zen:Dialect dialect))
  #f)


(zen:define-method (zen:dialect-wrap (zen:Dialect dialect) body)
  body)


;;;
;;;; Dialects
;;;


(define zen:Dialects
  (%%make-table test: eq?))


(define (zen:get-dialect name)
  (%%table-ref zen:Dialects name #f))


(define (zen:require-dialect name)
  (%%unless (%%eq? name 'foundation.dialect)
    (zen:load-unit name))
  (or (zen:get-dialect name)
      (zen:error "Unknown dialect: {s}" name)))


(define (zen:register-dialect name dialect)
  (%%table-set! zen:Dialects name dialect))


(zen:define-macro (zen:define-dialect name dialect)
  `(zen:register-dialect ',name ,dialect))


;;;
;;;; Bindings
;;;


(define (zen:register-declaration dialect-name binding)
  (let ((dialect (zen:get-dialect dialect-name))
        (binding-name (zen:get-lexical-binding-name binding)))
    (%%table-set! (zen:get-dialect-declarations dialect) binding-name binding)))


(define (zen:register-binding dialect-name binding)
  (let ((dialect (zen:get-dialect dialect-name))
        (binding-name (zen:get-lexical-binding-name binding)))
    (%%table-set! (zen:get-dialect-bindings dialect) binding-name binding)))


(zen:define-macro (zen:define-walker-declaration name dialect-name declaration-method binding-method)
  `(begin
     (zen:register-declaration ',dialect-name (zen:new-declaration-form ',name ,declaration-method))
     (zen:register-binding ',dialect-name (zen:new-special-form ',name ,binding-method))))


(zen:define-macro (zen:define-walker-special name dialect-name method)
  `(zen:register-binding ',dialect-name (zen:new-special-form ',name ,method)))


(zen:define-macro (zen:define-walker-syntax name dialect-name method)
  `(zen:register-binding ',dialect-name (zen:new-syntax-form ',name ,method)))


(zen:define-macro (zen:define-walker-macro name dialect-name method)
  `(zen:register-binding ',dialect-name (zen:new-macro-form ',name ,method)))


;;;
;;;; Code Analysis
;;;


;; complex code analysis for tools
(define zen:analysis-mode?
  (%%make-parameter #f))


;; declarations analysis table
(define zen:analysis-data
  (%%make-table test: eq?))


;;;
;;;; Walk Access
;;;


;; access internal to the module
(define zen:private-access
  0)

;; access from external modules
(define zen:public-access
  1)

;; access through inheritance
(define zen:protected-access
  2)


(define (zen:make-access-lookups access-level)
  (let ((lookups (%%make-vector (%%fx+ access-level 1))))
    (let iter ((n 0))
      (if (%%fx<= n access-level)
          (begin
            (%%vector-set! lookups n (%%make-table test: eq?))
            (iter (%%fx+ n 1)))))
    lookups))


;;;
;;;; Walker
;;;


(zen:define-class zen:Walker zen:Object ()
  ((declarations getter: generate setter: generate)
   (bindings     getter: generate setter: generate)
   (warnings     getter: generate setter: generate)
   (errors       getter: generate setter: generate)
   (literals     getter: generate setter: generate)
   (variables    getter: generate)
   (statics      getter: generate)
   (exports      getter: generate)
   (references   getter: generate)
   (autoloads    getter: generate setter: generate)))


(zen:define-virtual (zen:walker-declarations (zen:Walker walker)))
(zen:define-virtual (zen:walker-bindings (zen:Walker walker)))
(zen:define-virtual (zen:walk-form (zen:Walker walker) resume declaration environment form-src))
(zen:define-virtual (zen:walk-symbol (zen:Walker walker) resume declaration environment symbol-src))
(zen:define-virtual (zen:walk-symbol-assignment (zen:Walker walker) resume declaration environment symbol-src value form-src))
(zen:define-virtual (zen:validate-proclaim (zen:Walker walker) resume declaration form-src))
(zen:define-virtual (zen:runtime-export (zen:Walker walker) declaration))
(zen:define-virtual (zen:lookup-environment (zen:Walker walker) resume declaration environment symbol-src symbol))
(zen:define-virtual (zen:lookup-analyse (zen:Walker walker) declaration symbol-src referenced-declaration))


;; provide virtual access to some walker slots via the module-declaration
(define (zen:get-module-declaration-walker-literals lib-decl)
  (zen:get-walker-literals (zen:get-module-declaration-walker lib-decl)))
(define (zen:set-module-declaration-walker-literals lib-decl value)
  (zen:set-walker-literals (zen:get-module-declaration-walker lib-decl) value))
(define (zen:get-module-declaration-walker-variables lib-decl)
  (zen:get-walker-variables (zen:get-module-declaration-walker lib-decl)))
(define (zen:get-module-declaration-walker-statics lib-decl)
  (zen:get-walker-statics (zen:get-module-declaration-walker lib-decl)))
(define (zen:get-module-declaration-walker-exports lib-decl)
  (zen:get-walker-exports (zen:get-module-declaration-walker lib-decl)))
(define (zen:get-module-declaration-walker-references lib-decl)
  (zen:get-walker-references (zen:get-module-declaration-walker lib-decl)))
(define (zen:get-module-declaration-walker-autoloads lib-decl)
  (zen:get-walker-autoloads (zen:get-module-declaration-walker lib-decl)))
(define (zen:set-module-declaration-walker-autoloads lib-decl value)
  (zen:set-walker-autoloads (zen:get-module-declaration-walker lib-decl) value))


;;;
;;;; Walk Binding
;;;


(zen:define-class zen:Walk-Binding zen:Type ()
  ())


(zen:define-method (zen:emit-type (zen:Walk-Binding type) source-declaration walker resume environment)
  (zen:sourcified-form (zen:emit-binding-reference type source-declaration walker resume environment)))


(zen:define-method (zen:specifiable? (zen:Walk-Binding binding))
  #f)


(zen:define-virtual (zen:walk-binding-lookup (zen:Walk-Binding binding) symbol source-declaration))
(zen:define-virtual (zen:walk-binding-referenced (zen:Walk-Binding binding)))
(zen:define-virtual (zen:walk-binding-validate-call (zen:Walk-Binding binding) walker resume source-declaration operator arguments form-src))
(zen:define-virtual (zen:walk-binding-validate-assignment (zen:Walk-Binding binding) walker resume source-declaration symbol-src))
(zen:define-virtual (zen:walk-binding-assignable? (zen:Walk-Binding binding)))
(zen:define-virtual (zen:walk-binding-walkable? (zen:Walk-Binding binding)))
(zen:define-virtual (zen:walk-binding-walk-form (zen:Walk-Binding binding) walker resume declaration environment form-src))
(zen:define-virtual (zen:walk-binding-expandable? (zen:Walk-Binding binding)))
(zen:define-virtual (zen:walk-binding-expand-form (zen:Walk-Binding binding) walker resume declaration environment form-src))
(zen:define-virtual (zen:emit-binding-symbol (zen:Walk-Binding binding) source-declaration environment))
(zen:define-virtual (zen:emit-binding-reference (zen:Walk-Binding binding) source-declaration walker resume environment))
(zen:define-virtual (zen:emit-binding-call (zen:Walk-Binding binding) binding-src arguments arguments-codes source-declaration walker resume environment))
(zen:define-virtual (zen:emit-inlined-binding-call (zen:Walk-Binding binding) arguments call source-declaration walker resume environment))
(zen:define-virtual (zen:emit-binding-assignment (zen:Walk-Binding binding) value source-declaration walker resume environment form-src))


(zen:define-method (zen:walk-binding-lookup (zen:Walk-Binding binding) symbol source-declaration)
  #f)


(zen:define-method (zen:walk-binding-referenced (zen:Walk-Binding binding))
  (zen:unspecified))


(zen:define-method (zen:walk-binding-validate-call (zen:Walk-Binding binding) walker resume source-declaration operator arguments form-src)
  (zen:unspecified))


(zen:define-method (zen:walk-binding-validate-assignment (zen:Walk-Binding binding) walker resume source-declaration symbol-src)
  (%%when (%%not (zen:walk-binding-assignable? binding))
    (zen:walk-error walker resume source-declaration symbol-src "Illegal assignment to: {s}" binding)))


(zen:define-method (zen:walk-binding-assignable? (zen:Walk-Binding binding))
  #f)


(zen:define-method (zen:walk-binding-walkable? (zen:Walk-Binding binding))
  #f)


(zen:define-method (zen:walk-binding-walk-form (zen:Walk-Binding binding) walker resume declaration environment form-src)
  (zen:call-into-abstract (zen:class-of binding) 'walk-binding-walk-form binding '()))


(zen:define-method (zen:walk-binding-expandable? (zen:Walk-Binding binding))
  #f)


(zen:define-method (zen:walk-binding-expand-form (zen:Walk-Binding binding) walker resume declaration environment form-src)
  (zen:call-into-abstract (zen:class-of binding) 'walk-binding-expand-form binding '()))


(zen:define-method (zen:emit-binding-symbol (zen:Walk-Binding binding) source-declaration environment)
  (zen:error "Unable to emit binding symbol for: {s}" binding))


(zen:define-method (zen:emit-binding-reference (zen:Walk-Binding binding) source-declaration walker resume environment)
  (zen:error "Unable to emit binding reference for: {s}" binding))


(zen:define-method (zen:emit-binding-call (zen:Walk-Binding binding) binding-src arguments arguments-codes source-declaration walker resume environment)
  (let ((type (zen:get-lexical-binding-type binding))
        (operator (zen:emit-binding-reference binding source-declaration walker resume environment)))
    (zen:new-code
      `(,(zen:sourcified-form2 operator binding-src)
        ,@(zen:codes-forms arguments-codes))
      (zen:call-return-type type)
      #f)))


(zen:define-method (zen:emit-inlined-binding-call (zen:Walk-Binding binding) arguments call source-declaration walker resume environment)
  #f)


(zen:define-method (zen:emit-binding-assignment (zen:Walk-Binding binding) value source-declaration walker resume environment form-src)
  (zen:unspecified))


(define (zen:call-return-type operator-type)
  (if (%%is? operator-type zen:Function-Type)
      (zen:get-function-type-result operator-type)
    zen:Any))


;;;
;;;; Lexical Binding
;;;


(zen:define-class zen:Lexical-Binding zen:Walk-Binding ()
  ((name getter: generate)
   (type getter: generate setter: generate)
   (hits getter: generate setter: generate)))


(zen:define-virtual (zen:resolve-binding (zen:Lexical-Binding binding)))


(zen:define-method (zen:resolve-binding (zen:Lexical-Binding binding))
  binding)


(define (zen:resolve-bindings bindings)
  (map (lambda (binding)
         (zen:resolve-binding binding))
       bindings))


(zen:define-method (zen:resolve-type (zen:Lexical-Binding binding))
  (zen:resolve-binding binding))


(zen:define-method (zen:print-object (zen:Lexical-Binding binding) output detail)
  (zen:format output "#<{a} {a} #{a}>"
               (%%get-category-identifier (%%get-object-class binding))
               (zen:get-lexical-binding-name binding)
               (zen:object->serial binding)))


(zen:define-method (zen:walk-binding-lookup (zen:Lexical-Binding binding) symbol source-declaration)
  (if (%%eq? (zen:get-lexical-binding-name binding) symbol)
      binding
    #f))


(define (zen:lexical-binding-hits binding)
  (or (zen:get-lexical-binding-hits binding)
      (let ((table (%%make-table test: eq?)))
        (zen:set-lexical-binding-hits binding table)
        table)))


(zen:define-method (zen:emit-binding-symbol (zen:Lexical-Binding binding) declaration environment)
  (zen:get-lexical-binding-name binding))


;;;
;;;; Declaration
;;;


(zen:define-class zen:Declaration zen:Lexical-Binding ()
  ((access        getter: generate)
   (compatibility getter: generate)
   (modifiers     getter: generate)
   (attributes    getter: generate)
   (toplevel      getter: generate setter: generate)
   (parent        getter: generate setter: generate)
   (locator       getter: generate setter: generate)
   (source        getter: generate setter: generate)
   (name-source   getter: generate setter: generate)))


(define (zen:setup-declaration new-declaration)
  (let ((parent (zen:get-declaration-parent new-declaration))
        (name (zen:get-lexical-binding-name new-declaration)))
    (zen:set-declaration-locator new-declaration (if (%%not parent) name (zen:compose-declaration-locator new-declaration)))
    (zen:set-declaration-toplevel new-declaration (if (%%not parent) new-declaration (zen:get-declaration-toplevel parent)))))


(define (zen:get-declaration-path declaration)
  (define (iter declaration)
    (let ((name (zen:get-lexical-binding-name declaration))
          (parent (zen:get-declaration-parent declaration)))
      (if (%%not parent)
          (%%list name)
        (%%cons name (iter parent)))))
  
  (zen:reverse! (iter declaration)))


(zen:define-method (zen:walk-binding-lookup (zen:Declaration binding) symbol source-declaration)
  (zen:lookup-declaration binding symbol zen:private-access source-declaration))


(zen:define-method (zen:walk-binding-validate-call (zen:Declaration declaration) walker resume source-declaration operator arguments form-src)
  (zen:walk-error walker resume source-declaration form-src "{a} is not callable" declaration))


(zen:define-virtual (zen:compose-declaration-locator (zen:Declaration declaration)))
(zen:define-virtual (zen:lookup-declaration (zen:Declaration declaration) symbol access source-declaration))
(zen:define-virtual (zen:get-declaration-inclusions (zen:Declaration declaration)))
(zen:define-virtual (zen:get-nextmethod-signature (zen:Declaration declaration)))
(zen:define-virtual (zen:emit-declaration (zen:Declaration declaration) walker resume environment))
(zen:define-virtual (zen:expand-referenced-declaration (zen:Declaration declaration)))
(zen:define-virtual (zen:outline-generate (zen:Declaration declaration) output))
(zen:define-virtual (zen:outline-extract (zen:Declaration declaration) meta))


(zen:define-method (zen:compose-declaration-locator (zen:Declaration declaration))
  (let ((parent (zen:get-declaration-parent declaration))
        (name (zen:get-lexical-binding-name declaration)))
    (%%compose-reference (zen:get-declaration-locator parent) name)))


(zen:define-method (zen:lookup-declaration (zen:Declaration declaration) symbol access source-declaration)
  #f)


(zen:define-method (zen:get-declaration-inclusions (zen:Declaration declaration))
  '())


(zen:define-method (zen:get-nextmethod-signature (zen:Declaration declaration))
  (zen:error "No nextmethod signature for: {s}" declaration))


(zen:define-method (zen:emit-declaration (zen:Declaration declaration) walker resume environment)
  (zen:error "Unable to emit: {s}" declaration))


(zen:define-method (zen:expand-referenced-declaration (zen:Declaration declaration))
  '())


(zen:define-method (zen:outline-generate (zen:Declaration declaration) output)
  (let ((expr (zen:outline-extract declaration '())))
    (if expr
        (zen:format output "{%}  {s}" expr))))


(zen:define-method (zen:outline-extract (zen:Declaration declaration) meta)
  (zen:error "Unable to outline extract: {s}" declaration))


;; inline not supported in outlines at the moment
(define (zen:outline-generate-modifiers declaration)
  (zen:remove 'inline (zen:get-declaration-modifiers declaration)))


(define (zen:outline-generate-filter-access declarations #!optional (public-lookup #f))
  declarations
  ;; only generating public definitions works for using
  ;; a .o1 as a library that can be imported but doesn't
  ;; work for ctrl-enter which needs access to all fields
  ;; maybe one day the debugger could generate or retrieve
  ;; the full outline and send it to the debuggee!?
  #;
  (zen:collect-if (lambda (decl)
                     (or (%%not (%%eq? (zen:get-declaration-access decl) 'private))
                         (and public-lookup
                              (%%table-ref public-lookup (zen:get-lexical-binding-name decl) #f))))
                   declarations))


(define (zen:outline-generate-signature signature #!optional (method? #f))
  (if (not signature)
      #f
    (let ((queue (zen:new-queue))
          (first-positional? #t))
      (define (add-positional parameter)
        (let ((name (zen:get-lexical-binding-name parameter)))
          (if (and method? first-positional?)
              (begin
                (zen:enqueue queue name)
                (set! first-positional? #f))
            (zen:enqueue-list queue `(,name ,@(zen:outline-generate-parameter-type-list parameter))))))
      
      ;; adding (unspecified) is a quick solution
      (define (add-optional parameter)
        (zen:enqueue queue `(,(zen:get-lexical-binding-name parameter) (unspecified))))
      
      ;; adding (unspecified) is a quick solution
      (define (add-keyword parameter)
        (let ((name (zen:get-lexical-binding-name parameter)))
          (zen:enqueue queue `(,(string->keyword (symbol->string name)) ,name (unspecified)))))
      
      (define (add-rest parameter)
        (zen:enqueue-list queue (zen:get-lexical-binding-name parameter)))
      
      (let ((positional (zen:get-signature-positional signature))
            (optional (zen:get-signature-optional signature))
            (named (zen:get-signature-named signature))
            (rest (zen:get-signature-rest signature)))
        (for-each add-positional positional)
        (for-each add-optional optional)
        (for-each add-keyword named)
        (%%when rest (add-rest rest)))
      (zen:queue-list queue))))


(define (zen:outline-generate-type-list type)
  (if (and type (%%is-not? type zen:Any-Class))
      (%%list (zen:type->specifier type))
    '()))


(define (zen:outline-generate-specifier-list specifier-source)
  (if specifier-source
      (%%list (zen:source-code specifier-source))
    '()))


(define (zen:outline-generate-parameter-type-list parameter)
  (let ((specifier-source (zen:get-variable-specifier-source parameter)))
    (if specifier-source
        (%%list (zen:source-code specifier-source))
      (zen:outline-generate-type-list (zen:get-lexical-binding-type parameter)))))


(define (zen:outline-generate-access-list declaration)
  (if (%%table-ref (zen:get-public-lookup (zen:get-declaration-toplevel declaration)) (zen:get-lexical-binding-name declaration) #f)
      '(public)
    (let ((access (zen:get-declaration-access declaration)))
      (if access
          (%%list access)
        '()))))


(define (zen:outline-generate-phase-list phase)
  (if (and phase (%%neq? phase 'runtime))
      `((phase ,phase))
    '()))


(define (zen:outline-generate-transformations transformations)
  (if (%%not transformations)
      '()
    transformations))


(define (zen:declaration-result)
  (if (%%eq? (zen:walk-for) 'eval)
      '((zen:unspecified))
    '()))


;; In order to be able to resolve internal declarations as we walk the code, the declaration
;; tree is pre-expanded including expanding macros when needed. Then, during the actual walk
;; we just find the declarations in this tree. This tree is also merged with any preexisting
;; declaration tree coming from the runtime catalog.


(define (zen:walk-declarations walker resume declaration environment forms)
  (define (walk forms)
    (for-each (lambda (form-src)
                (continuation-capture
                  (lambda (resume)
                    (zen:cond-expand walker resume declaration form-src
                      (lambda (expr expr?)
                        (%%when expr?
                          (let ((expansion (zen:expand-macros walker resume declaration environment expr)))
                            (cond ((zen:include-form? expansion)
                                   (walk (zen:include-forms (zen:include-filename expansion))))
                                  ((zen:begin-form? expansion)
                                   (walk (%%cdr (zen:unwrap-syntactic-closure expansion))))
                                  (else
                                   (walk-declaration resume expansion))))))))))
              forms))
  
  (define (walk-declaration resume form-src)
    (if (%%pair? (zen:unwrap-syntactic-closure form-src))
        (let ((first-src (%%car (zen:unwrap-syntactic-closure form-src))))
          (let ((first (zen:unwrap-syntactic-closure first-src))
                (declaration-environment (zen:walker-declaration-environment walker)))
            (let ((frame (%%car declaration-environment))) ;; unique frame for now
              (let ((binding (zen:walk-binding-lookup frame first #f)))
                (if binding
                    (let ((walk (zen:get-declaration-form-walk binding)))
                      (walk walker resume declaration environment form-src))
                  #; ;; todo validate outline errors
                  (zen:walk-free-reference walker resume declaration first-src))))))))
  
  (walk forms))


(define (zen:add-declaration-child walker resume namespace-declaration child)
  (let ((name (zen:get-lexical-binding-name child)))
    (%%when (%%not (zen:find-declaration-child namespace-declaration name))
      (zen:enqueue (zen:get-namespace-declaration-children namespace-declaration) child))
    (%%table-set! (zen:get-private-lookup namespace-declaration) name child)
    ;; for now everything not private is considered public
    (%%when (%%neq? (zen:get-declaration-access child) 'private)
      (%%table-set! (zen:get-public-lookup namespace-declaration) name child))
    child))


(define (zen:require-declaration namespace-declaration name)
  (let ((declaration (zen:find-declaration namespace-declaration name)))
    (%%assertion declaration (zen:error "Unable to find declaration: {a}" name)
      declaration)))


(define (zen:require-declaration-of-type type namespace-declaration name)
  (let ((decl (zen:require-declaration namespace-declaration name)))
    (if (%%is? decl type)
        decl
      (zen:error "Found conflicting declarations for {a}" name))))


(define (zen:find-declaration namespace-declaration name)
  (%%table-ref (zen:get-private-lookup namespace-declaration) name #f))


(define (zen:find-public-declaration namespace-declaration name)
  (%%table-ref (zen:get-public-lookup namespace-declaration) name #f))


(define (zen:find-declaration-child namespace-declaration name)
  (zen:find-if (lambda (decl)
                  (%%eq? (zen:get-lexical-binding-name decl) name))
                (zen:queue-list (zen:get-namespace-declaration-children namespace-declaration))))


(define (zen:find-declaration-child-of-type type namespace-declaration name)
  (let ((decl (zen:find-declaration-child namespace-declaration name)))
    (if (or (%%not decl)
            (%%is? decl type))
        decl
      (zen:error "Found conflicting declarations for {a}" name))))


(define (zen:remove-declaration-child namespace-declaration name)
  (let ((declaration (zen:find-declaration-child namespace-declaration name)))
    (%%when declaration
      (let ((queue (zen:get-namespace-declaration-children namespace-declaration)))
        (let ((lst (zen:remove! declaration (zen:queue-list queue))))
          ;; this two-step process might be inefficient
          (zen:reset-queue queue)
          (zen:enqueue-list queue lst))))))


(define (zen:begin-form? form)
  (and (%%pair? (zen:source-code form))
       (%%eq? (zen:source-code (%%car (zen:source-code form))) 'begin)))


(define (zen:define-form? form)
  (and (%%pair? (zen:source-code form))
       (%%eq? (zen:source-code (%%car (zen:source-code form))) 'define)))


(define (zen:declare-form? form)
  (and (%%pair? (zen:source-code form))
       (%%eq? (zen:source-code (%%car (zen:source-code form))) 'declare)))


(define (zen:proclaim-form? form)
  (and (%%pair? (zen:source-code form))
       (%%eq? (zen:source-code (%%car (zen:source-code form))) 'proclaim)))


;;;
;;;; Declaration Reference
;;;


(zen:define-class zen:Declaration-Reference zen:Object ()
  ((name        getter: generate)
   (declaration getter: generate setter: generate)))


(zen:define-virtual (zen:resolve-reference (zen:Declaration-Reference declaration-reference) module-declaration))


(zen:define-method (zen:resolve-reference (zen:Declaration-Reference declaration-reference) module-declaration)
  (or (zen:get-declaration-reference-declaration declaration-reference)
      (receive (name symbol) (zen:parse-exported-symbol module-declaration (zen:get-declaration-reference-name declaration-reference))
        (let ((declaration (zen:new-export-declaration name #f 'public 'uptodate '() '() #f symbol)))
          (zen:set-declaration-reference-declaration declaration-reference declaration)
          declaration))))


;;;
;;;; Module Reference
;;;


(zen:define-class zen:Module-Reference zen:Declaration-Reference (constructor: zen:allocate-module-reference)
  ())


(define (zen:new-module-reference name declaration)
  (zen:allocate-module-reference name declaration))


(zen:define-method (zen:resolve-reference (zen:Module-Reference declaration-reference) module-declaration)
  (or (zen:get-declaration-reference-declaration declaration-reference)
      (let ((declaration (zen:outline-module (zen:get-declaration-reference-name declaration-reference))))
        (zen:set-declaration-reference-declaration declaration-reference declaration)
        declaration)))


;;;
;;;; Export Reference
;;;


(zen:define-class zen:Export-Reference zen:Declaration-Reference (constructor: zen:allocate-export-reference)
  ((module-reference getter: generate)))


(define (zen:new-export-reference name declaration module-reference)
  (zen:allocate-export-reference name declaration module-reference))


(zen:define-method (zen:resolve-reference (zen:Export-Reference declaration-reference) module-declaration)
  (or (zen:get-declaration-reference-declaration declaration-reference)
      (receive (name symbol) (zen:parse-exported-symbol module-declaration (zen:get-declaration-reference-name declaration-reference))
        (let ((locator (zen:compose-reference (zen:get-lexical-binding-name module-declaration) name)))
          (let ((declaration (zen:new-export-declaration name #f 'public 'uptodate '() '() module-declaration locator)))
            (zen:set-declaration-reference-declaration declaration-reference declaration)
            declaration)))))


(define (zen:parse-exported-symbol module-declaration name)
  (cond ((zen:composite-namespace? name)
         (values (zen:namespace-name name) name))
        ((zen:composite-reference? name)
         (values (zen:reference-name name) name))
        (else
         (values name name))))


;;;
;;;; Autoload Reference
;;;


(zen:define-class zen:Autoload-Reference zen:Export-Reference (constructor: zen:allocate-autoload-reference)
  ((hubs? getter: generate)))


(define (zen:new-autoload-reference name declaration module-reference hubs?)
  (zen:allocate-autoload-reference name declaration module-reference hubs?))


(define (zen:autoload-reference=? x y)
  (and (%%eq? (zen:get-export-reference-module-reference x)
              (zen:get-export-reference-module-reference y))
       (%%eqv? (zen:get-autoload-reference-hubs? x)
               (zen:get-autoload-reference-hubs? y))))


(define (zen:cache-autoload-reference declaration-reference module-declaration exported-module-reference)
  (or (zen:get-declaration-reference-declaration declaration-reference)
      (let* ((name (zen:get-declaration-reference-name declaration-reference))
             (type zen:Any)
             (declaration (zen:new-autoload-declaration name type #f module-declaration exported-module-reference)))
        (%%assert declaration
          (zen:set-declaration-reference-declaration declaration-reference declaration)
          declaration))))


;;;
;;;; Unit
;;;


(zen:define-class zen:Unit-Declaration zen:Declaration (constructor: zen:allocate-unit-declaration)
  ((container getter: generate)
   (requires  getter: generate setter: generate)))


(define (zen:new-unit-declaration name access parent requires)
  (define (determine-package)
    (let ((resource (zen:requested-unit-resource)))
      (and resource
           (%%get-resource-package resource))))
  
  (let ((new-declaration (zen:allocate-unit-declaration name #f #f access 'uptodate '() '() #f parent #f #f #f (determine-package) requires)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(define (zen:parse-unit-declaration partial-form)
  (define (parse rest proc)
    (let ((first (zen:source-code (%%car rest))))
      (if (%%memq first '(protected public))
          (proc (zen:source-code (%%cadr rest)) first (%%cddr rest))
        (proc (zen:source-code (%%car rest)) 'public (%%cdr rest)))))

  (define (collect-requires body)
    (let ((requires '()))
      (for-each (lambda (expr)
                  (if (and (%%pair? (zen:source-code expr))
                           (%%eq? (zen:source-code (%%car (zen:source-code expr))) 'require))
                      (set! requires (append requires (map zen:listify (zen:filter-invoices (%%cdr (%%desourcify expr))))))))
                body)
      requires))
  
  (parse partial-form
    (lambda (name access body)
      (if (and (zen:requested-unit-name) (%%neq? name (zen:requested-unit-name)))
          (zen:error "Unit at {s} is defining {s}" (zen:requested-unit-name) name)
        (let ((requires (collect-requires body)))
          (let ((require-invoices (map (lambda (require)
                                         (zen:parse-require require
                                           (lambda (require-name feature-requirement phase)
                                             (zen:require-unit-src require-name)
                                             (zen:new-require-invoice require-name phase))))
                                       requires)))
            (zen:new-unit-declaration name access #f require-invoices)))))))


(zen:define-method (zen:outline-generate (zen:Unit-Declaration declaration) output)
  (zen:format output "(unit {a}" (zen:get-lexical-binding-name declaration))
  #; ;; requires are not needed and create problems when outlined
  (for-each (lambda (require-invoice)
              (let ((name (zen:get-require-invoice-name require-invoice))
                    (phase (zen:get-require-invoice-phase require-invoice)))
                (zen:format output "{%}  {s}" `(require (,name ,@(zen:outline-generate-phase-list phase))))))
            (zen:get-unit-declaration-requires declaration))
  (zen:format output "){%}"))


;;;
;;;; Namespace
;;;


(zen:define-class zen:Namespace-Declaration zen:Declaration ()
  ((lookups  getter: generate)
   (children getter: generate)
   (body     getter: generate setter: generate)))


(zen:define-method (zen:lookup-declaration (zen:Namespace-Declaration namespace-declaration) symbol access source-declaration)
  (define (add-to-hits declaration)
    (%%when (and declaration source-declaration (zen:analysis-mode?))
      (let ((hits-table (zen:lexical-binding-hits declaration)))
        (%%table-set! hits-table (zen:get-declaration-locator source-declaration) source-declaration))
      (%%when (%%is? declaration zen:Autoload-Declaration)
        (zen:set-analysis-data-autoload-reference (zen:get-analysis-data (zen:get-declaration-locator declaration)) declaration))))
  
  (let ((found (%%table-ref (zen:get-access-lookup namespace-declaration access) symbol #f)))
    (zen:add-to-module-references namespace-declaration found)
    (add-to-hits found)
    found))


(zen:define-method (zen:tree-fold (zen:Namespace-Declaration declaration) down up here seed environment)
  (up declaration
      seed
      (zen:tree-fold-list
        (zen:get-namespace-declaration-body declaration) down up here (down declaration seed environment) environment)
      environment))


(define (zen:get-private-lookup namespace-declaration)
  (zen:get-access-lookup namespace-declaration zen:private-access))

(define (zen:get-public-lookup namespace-declaration)
  (zen:get-access-lookup namespace-declaration zen:public-access))

(define (zen:get-protected-lookup namespace-declaration)
  (zen:get-access-lookup namespace-declaration zen:protected-access))


;;;
;;;; Module
;;;


(zen:define-class zen:Module-Declaration zen:Namespace-Declaration (constructor: zen:allocate-module-declaration)
  ((walker          getter: generate setter: generate)
   (container       getter: generate)
   (dialect-name    getter: generate)
   (dialect-invoice getter: generate)
   (requires        getter: generate setter: generate)
   (exports         getter: generate setter: generate)
   (imports         getter: generate setter: generate)
   (inclusions      getter: generate setter: generate)
   (local-macros    getter: generate)))


(define (zen:new-module-declaration name access modifiers parent walker dialect-name dialect-invoice)
  (define (determine-package)
    (let ((resource (zen:requested-unit-resource)))
      (and resource
           (%%get-resource-package resource))))
  
  (let ((new-declaration (zen:allocate-module-declaration name #f #f access 'uptodate modifiers '() #f parent #f #f #f (zen:make-access-lookups zen:public-access) (zen:new-queue) #f walker (determine-package) dialect-name dialect-invoice '() '() '() '() (%%make-table test: eq?))))
    (zen:setup-declaration new-declaration)
    new-declaration))


(define (zen:get-unit/module-container unit/module)
  (if (%%is? unit/module zen:Unit-Declaration)
      (zen:get-unit-declaration-container unit/module)
    (zen:get-module-declaration-container unit/module)))


(define (zen:get-unit/module-requires unit/module)
  (if (%%is? unit/module zen:Unit-Declaration)
      (zen:get-unit-declaration-requires unit/module)
    (zen:get-module-declaration-requires unit/module)))


(define (zen:add-module-require module-declaration require)
  (zen:parse-require require
    (lambda (require-name feature-requirement phase)
      (define (find-require requires)
        (zen:find-if (lambda (req)
                        (%%eq? (zen:get-require-invoice-name req) require-name))
                      requires))
      
      (%%when (%%eq? phase 'syntax)
        (zen:load-unit require-name))
      (let ((requires (zen:get-module-declaration-requires module-declaration)))
        (if (%%not (find-require requires))
            (begin
              (zen:require-unit-src require-name)
              (let ((require-invoice (zen:new-require-invoice require-name phase)))
                (zen:set-module-declaration-requires module-declaration (%%append requires (%%list require-invoice))))))))))


(define (zen:add-module-import walker resume declaration form-src module-declaration module-invoice register?)
  (define (merge-invoice actual new)
    ;; todo
    #f)
  
  (%%when (%%eq? (zen:get-module-invoice-phase module-invoice) 'syntax)
    (let ((module-declaration (zen:get-module-invoice-module module-invoice)))
      (zen:load-unit (zen:get-lexical-binding-name module-declaration))))
  (%%when register?
    (let ((imports (zen:get-module-declaration-imports module-declaration)))
      (let ((actual (zen:find-module-invoice imports module-invoice)))
        (if actual
            (merge-invoice actual module-invoice)
          (zen:set-module-declaration-imports module-declaration (%%append imports (%%list module-invoice)))))))
  (let ((private (zen:get-private-lookup module-declaration))
        (mangler (zen:generate-module-mangler (zen:get-module-invoice-transformations module-invoice))))
    (cond (mangler
            (let ((imported-module-declaration (zen:get-module-invoice-module module-invoice))
                  (imported (%%make-table test: eq?)))
              (table-for-each (lambda (key declaration)
                                (let ((mangled-key (mangler key)))
                                  (if mangled-key
                                      (%%table-set! imported mangled-key declaration))))
                              (zen:get-public-lookup imported-module-declaration))
              (zen:table-merge-reporting-conflicts! walker resume declaration form-src module-declaration "Import" private imported)))
          (else
           (let ((imported-module-declaration (zen:get-module-invoice-module module-invoice)))
             (let ((imported (zen:get-public-lookup imported-module-declaration)))
               (zen:table-merge-reporting-conflicts! walker resume declaration form-src module-declaration "Import" private imported)))))))


;; reset everything imported for code analysis
(define (zen:reset-module-imported module-declaration)
  (let ((private (zen:get-private-lookup module-declaration)))
    (for-each (lambda (module-invoice)
                (let ((invoice-module (zen:get-module-invoice-module module-invoice)))
                  (zen:table-iterate-safe private
                    (lambda (name declaration)
                      (%%when (%%eq? (zen:get-declaration-toplevel declaration) invoice-module)
                        (%%table-clear private name))))))
              (zen:get-module-declaration-imports module-declaration))))


(define (zen:add-module-export walker resume declaration form-src module-declaration module-invoice)
  (define (merge-invoice actual new)
    (let ((actual-autoload (zen:get-export-invoice-autoload actual))
          (new-autoload (zen:get-export-invoice-autoload new)))
      (%%when new-autoload
        (zen:set-export-invoice-autoload actual (if actual-autoload (zen:union actual-autoload new-autoload test: zen:autoload-reference=?) new-autoload)))))
  
  (define (add-to-module-exports declaration)
    (%%when (and declaration
                 (%%neq? module-declaration (zen:get-declaration-toplevel declaration)))
      (let ((exports-table (zen:get-module-declaration-walker-exports module-declaration)))
        (%%table-set! exports-table (zen:get-declaration-locator declaration) declaration))))
  
  (%%when (%%eq? (zen:get-module-invoice-phase module-invoice) 'syntax)
    (let ((module-declaration (zen:resolve-reference (zen:get-module-invoice-module module-invoice) module-declaration)))
      (zen:load-unit (zen:get-lexical-binding-name module-declaration))))
  (let ((exports (zen:get-module-declaration-exports module-declaration)))
    (let ((actual (zen:find-module-invoice exports module-invoice)))
      (if actual
          (merge-invoice actual module-invoice)
        (zen:set-module-declaration-exports module-declaration (%%append exports (%%list module-invoice))))))
  (let ((public (zen:get-public-lookup module-declaration))
        (mangler (zen:generate-module-mangler (zen:get-module-invoice-transformations module-invoice)))
        (autoload (zen:get-export-invoice-autoload module-invoice))
        (symbols (zen:get-export-invoice-symbols module-invoice)))
    (cond (autoload
           (let ((exported-module-reference (zen:get-module-invoice-module module-invoice)))
             (for-each (lambda (declaration-reference)
                         (let ((name (zen:reference-name (zen:get-declaration-reference-name declaration-reference)))
                               (declaration (zen:cache-autoload-reference declaration-reference module-declaration exported-module-reference))
                               (hubs? (zen:get-autoload-reference-hubs? declaration-reference)))
                           (%%table-set! public name declaration)
                           (add-to-module-exports declaration)
                           (if hubs?
                               (let ((unit-name (zen:get-declaration-reference-name exported-module-reference)))
                                 (let ((shape (zen:module-shape walker resume declaration unit-name)))
                                   (for-each (lambda (category)
                                               (let ((category-name (%%cadr category)))
                                                 (%%when (%%eq? category-name name)
                                                   (let ((hub-names (%%cddr category)))
                                                     (for-each (lambda (hub-name)
                                                                 (let ((meta? (%%pair? hub-name)))
                                                                   (let ((hub-name (if meta? (%%car hub-name) hub-name))
                                                                         (locator (zen:autoload-declaration-locator-heuristic declaration)))
                                                                     (let ((category-locator (if meta? (%%string->symbol (%%string-append (%%symbol->string locator) "~Class")) locator)))
                                                                       (let ((new-declaration (zen:new-hub-declaration hub-name zen:Any 'public 'uptodate '() '() module-declaration #f (%%list category-locator))))
                                                                         (let ((actual-declaration (%%table-ref public hub-name #f)))
                                                                           (if actual-declaration
                                                                               (if (%%class-is? actual-declaration zen:hub-declaration-class)
                                                                                   (let ((actual-hubs (zen:hub-declaration-hubs actual-declaration))
                                                                                         (actual-nodes (zen:hub-declaration-nodes actual-declaration)))
                                                                                     (zen:hub-declaration-hubs-set! actual-declaration (zen:union actual-hubs (%%list new-declaration)))
                                                                                     (zen:hub-declaration-nodes-set! actual-declaration (zen:union actual-nodes (%%list category-locator)))
                                                                                     (add-to-module-exports new-declaration))
                                                                                 (zen:error "Found conflicting declarations for {a}" hub-name))
                                                                             (begin
                                                                               (%%table-set! public hub-name new-declaration)
                                                                               (add-to-module-exports new-declaration)))))))))
                                                               hub-names)))))
                                             shape))))))
                       autoload)))
          (symbols
           (for-each (lambda (declaration-reference)
                       (let ((name (zen:reference-name (zen:get-declaration-reference-name declaration-reference))))
                         (let ((mangled-name (if mangler (mangler name) name)))
                           (if mangled-name
                               (let ((declaration (zen:resolve-reference declaration-reference module-declaration)))
                                 (%%table-set! public mangled-name declaration)
                                 (add-to-module-exports declaration))))))
                     symbols))
          (mangler
           (let ((exported-module-declaration (zen:resolve-reference (zen:get-module-invoice-module module-invoice) module-declaration))
                 (exported (%%make-table test: eq?)))
             (table-for-each (lambda (key declaration)
                               (let ((mangled-key (mangler key)))
                                 (if mangled-key
                                     (%%table-set! exported mangled-key declaration))))
                             (zen:get-public-lookup exported-module-declaration))
             (zen:table-merge-reporting-conflicts! walker resume declaration form-src module-declaration "Export" public exported)
             (table-for-each (lambda (key declaration)
                               (add-to-module-exports declaration))
                             exported)))
          (else
           (let* ((exported-module-declaration (zen:resolve-reference (zen:get-module-invoice-module module-invoice) module-declaration))
                  (exported-table (zen:get-public-lookup exported-module-declaration)))
             (zen:table-merge-reporting-conflicts! walker resume declaration form-src module-declaration "Export" public exported-table)
             (table-for-each (lambda (key declaration)
                               (add-to-module-exports declaration))
                             exported-table))))))


(define (zen:generate-module-mangler transformations)
  (define (only parameters)
    (lambda (key)
      (if (and key (%%memq key parameters))
          key
        #f)))
  
  (define (except parameters)
    (lambda (key)
      (if (and key (%%not (%%memq key parameters)))
          key
        #f)))
  
  (define (prefix parameters)
    (let ((prefix (%%car parameters))
          (explicit (%%cdr parameters)))
      (let ((prefix (cond ((%%keyword? prefix) (%%string-append (keyword->string prefix) ":"))
                          ((%%symbol? prefix) (%%symbol->string prefix))
                          ((%%string? prefix) prefix)
                          (else (zen:error "Invalid module prefix")))))
        (if (%%null? explicit)
            (lambda (key)
              (if key
                  (%%string->symbol (%%string-append prefix (%%symbol->string key)))
                #f))
          (lambda (key)
            (if (and key (%%memq key explicit))
                (%%string->symbol (%%string-append prefix (%%symbol->string key)))
              key))))))
  
  (define (rename parameters)
    (let ((mapping (%%make-table test: eq?)))
      (for-each (lambda (conversion)
                  (let ((key (%%car conversion))
                        (value (%%cadr conversion)))
                    (%%table-set! mapping key value)))
                parameters)
      (lambda (key)
        (if key
            (%%table-ref mapping key key)
          #f))))
  
  (define (find-mangler transformation)
    (case (%%car transformation)
      ((only) only)
      ((except) except)
      ((prefix) prefix)
      ((rename) rename)
      (else (zen:error "Invalid module operation"))))
  
  (if transformations
      (let iter ((scan transformations)
                 (mangler #f))
           (if (%%not (%%null? scan))
               (let ((transformation (%%car scan)))
                 (let ((proc ((find-mangler transformation) (%%cdr transformation))))
                   (iter (%%cdr scan)
                         (if mangler
                             (lambda (key)
                               (proc (mangler key)))
                           proc))))
             mangler))
    #f))


(define (zen:table-merge-reporting-conflicts! walker resume declaration form-src module-declaration prefix table add)
  (define (effective-locator decl)
    (cond ((%%is? decl zen:Export-Declaration)
           (zen:get-export-declaration-symbol decl))
          ((%%is? decl zen:Autoload-Declaration)
           (zen:autoload-declaration-locator-heuristic decl))
          ((and zen:hub-declaration-class (%%is? decl zen:hub-declaration-class))
           (%%string->symbol (%%string-append (%%symbol->string (zen:get-lexical-binding-name decl)) "<hub>")))
          (else
           (zen:get-declaration-locator decl))))
  
  (define (merge/report-conflicts)
    (zen:table-iterate-safe add
      (lambda (key new)
        (let ((actual (%%table-ref table key #f)))
          (if (and zen:hub-declaration-class
                   (%%is? new zen:hub-declaration-class)
                   (%%is? actual zen:hub-declaration-class))
              (%%unless (%%eq? new actual)
                (let ((new-hubs (zen:hub-declaration-hubs new))
                      (new-nodes (zen:hub-declaration-nodes new))
                      (actual-hubs (zen:hub-declaration-hubs actual))
                      (actual-nodes (zen:hub-declaration-nodes actual)))
                  (let ((name (zen:get-lexical-binding-name actual))
                        (type (zen:get-lexical-binding-type actual))
                        (access (zen:get-declaration-access actual))
                        (compatibility (zen:get-declaration-compatibility actual))
                        (modifiers (zen:get-declaration-modifiers actual))
                        (hubs (zen:union actual-hubs new-hubs))
                        (nodes (zen:union actual-nodes new-nodes)))
                    (let ((hub (zen:new-hub-declaration name type access compatibility modifiers '() declaration hubs nodes)))
                      (%%table-set! table key hub)))))
            (let ((new-locator (effective-locator new))
                  (actual-locator (effective-locator actual)))
              (%%when (%%neq? new-locator actual-locator)
                (zen:walk-conflict walker resume declaration prefix key actual-locator new-locator form-src))))))))
  
  (let ((table-count (%%table-length table))
        (add-count (%%table-length add)))
    (%%table-merge! table add #f)
    (%%when (%%not (%%fx= (%%table-length table) (%%fx+ table-count add-count)))
      ;; Can report no conflicts if the same declaration was imported from different modules
      (merge/report-conflicts))))


(define (zen:add-to-module-references declaration referenced-declaration)
  (%%when referenced-declaration
    (let ((module-declaration (zen:get-declaration-toplevel declaration))
          (referenced-module-declaration (zen:get-declaration-toplevel referenced-declaration)))
      (%%when (%%neq? module-declaration referenced-module-declaration)
        (if (and zen:hub-declaration-class (%%class-is? referenced-declaration zen:hub-declaration-class))
            (let ((module-declaration (zen:get-declaration-toplevel declaration)))
              (let ((references-table (zen:get-module-declaration-walker-references module-declaration)))
                (for-each (lambda (hub)
                            (%%table-set! references-table (zen:get-declaration-locator hub) hub))
                          (zen:hub-declaration-hubs referenced-declaration))))
          (let ((references-table (zen:get-module-declaration-walker-references module-declaration)))
            (%%table-set! references-table (zen:get-declaration-locator referenced-declaration) referenced-declaration)))))))


(define (zen:generate-reference-list module-declaration)
  (define (lesser name1 name2)
    (if (or (%%null? name1) (%%null? name2))
        (%%null? name1)
      (let ((string1 (%%symbol->string (%%car name1)))
            (string2 (%%symbol->string (%%car name2))))
        (or (%%string<? string1 string2)
            (and (%%string=? string1 string2)
                 (lesser (%%cdr name1) (%%cdr name2)))))))
  
  (define (merge-sorted item sorted)
    (cond ((%%null? sorted)
           (%%list item))
          ((lesser item (%%car sorted))
           (%%cons item sorted))
          (else
           (%%cons (%%car sorted) (merge-sorted item (%%cdr sorted))))))
  
  (define (compose-name root-declaration declaration)
    (let iter ((declaration declaration)
               (composite-identifier '()))
         (if (%%eq? root-declaration declaration)
             composite-identifier
           (iter (zen:get-declaration-parent declaration) (%%cons (zen:get-lexical-binding-name declaration) composite-identifier)))))
  
  (let ((partition (%%make-table test: eq?)))
    (define (merge-declaration locator declaration)
      (let ((resolved-declaration (zen:resolve-binding declaration)))
        (let ((module (zen:get-declaration-toplevel resolved-declaration)))
          (%%table-set! partition module
            (merge-sorted (compose-name module resolved-declaration) (%%table-ref partition module '()))))))
    
    ;; necessary to merge exports because of runtime access through module-ref
    (zen:table-iterate-safe (zen:get-module-declaration-walker-exports module-declaration) merge-declaration)
    (zen:table-iterate-safe (zen:get-module-declaration-walker-references module-declaration) merge-declaration)
    (let iter ((in (%%table->list partition))
               (out '()))
         (if (%%null? in)
             out
           (let ((module-locator (zen:get-declaration-locator (%%caar in)))
                 (declarations (map (lambda (declaration)
                                      (if (and (%%pair? declaration) (%%null? (%%cdr declaration)))
                                          (%%car declaration)
                                        declaration))
                                    (%%cdar in))))
             (iter (%%cdr in) (merge-sorted (%%cons module-locator declarations) out)))))))


(zen:define-method (zen:lookup-declaration (zen:Module-Declaration declaration) symbol access source-declaration)
  ;; code to detect unreferenced imports
  (%%when (zen:analysis-mode?)
    (for-each (lambda (module-invoice)
                (let ((imported-module-declaration (zen:get-module-invoice-module module-invoice)))
                  (let ((imported (zen:get-public-lookup imported-module-declaration)))
                    (%%when (%%table-ref imported symbol #f)
                      (zen:set-import-invoice-hit? module-invoice #t)))))
              (zen:get-module-declaration-imports declaration)))
  (nextmethod declaration symbol access source-declaration))


(zen:define-method (zen:emit-declaration (zen:Module-Declaration declaration) walker resume environment)
  (let ((body-expansion (zen:emit-namespace-statements (zen:get-namespace-declaration-body declaration) declaration walker resume environment))
        (inclusions-expansion (zen:emit-module-inclusions declaration walker resume environment))
        (literals-expansion (zen:emit-module-literals declaration walker resume environment))
        (variables-expansion (zen:emit-module-variables declaration walker resume environment))
        (statics-expansion (zen:emit-module-statics declaration walker resume environment))
        (autoloads-expansion (zen:emit-module-autoloads declaration walker resume environment))
        (registration-expansion (zen:emit-module-registration declaration walker resume environment)))
    `(begin
       ,@(case (zen:walk-for)
           ((eval) '())
           (else (zen:declares 'module)))
       ,@(let ((queue (zen:new-queue))
               (load-units (%%make-table test: eq?)))
           (define (enqueue-load-unit unit-name)
             (%%when (%%not (%%table-ref load-units unit-name #f))
               (%%table-set! load-units unit-name #t)
               (zen:enqueue queue `(zen:load-unit ',unit-name))))
           
           (let ((dialect-name (zen:get-module-declaration-dialect-name declaration)))
             (%%when (%%neq? dialect-name 'foundation.dialect)
               (enqueue-load-unit dialect-name)))
           (for-each (lambda (require-invoice)
                       (enqueue-load-unit (zen:get-require-invoice-name require-invoice)))
                     (zen:get-module-declaration-requires declaration))
           (for-each (lambda (module-invoice)
                       (let ((symbols (zen:get-export-invoice-symbols module-invoice))
                             (autoload (zen:get-export-invoice-autoload module-invoice)))
                         (%%when (and (%%not symbols) (%%not autoload))
                           (let ((module-declaration (zen:resolve-reference (zen:get-module-invoice-module module-invoice) declaration))
                                 (phase (zen:get-module-invoice-phase module-invoice)))
                             (%%when (and (%%neq? module-declaration declaration) (%%neq? phase 'syntax))
                               (enqueue-load-unit (zen:get-lexical-binding-name module-declaration)))))))
                     (zen:get-module-declaration-exports declaration))
           (for-each (lambda (module-invoice)
                       (let ((module-declaration (zen:get-module-invoice-module module-invoice))
                             (phase (zen:get-module-invoice-phase module-invoice)))
                         (%%when (and module-declaration (%%neq? phase 'syntax))
                           (enqueue-load-unit (zen:get-lexical-binding-name module-declaration)))))
                     (zen:get-module-declaration-imports declaration))
           (zen:queue-list queue))
       ,@registration-expansion
       ,@inclusions-expansion
       ,@autoloads-expansion
       ,@literals-expansion
       ,@variables-expansion
       ,@statics-expansion
       ,@body-expansion)))


;; hack around warnings being reported more than once if a
;; variable mutation triggers the annotation reset mecanism
(define zen:reported-warnings
  (%%make-table test: equal?))


(define (zen:warning fmt-string . rest)
  (let ((message (apply zen:format fmt-string rest)))
    (if (%%not (%%table-ref zen:reported-warnings message #f))
        (begin
          (if (%%not (zen:reporting?))
              (zen:feedback "{a}" message))
          (zen:report "{a}" message)
          (%%table-set! zen:reported-warnings message #t)))))


(define (zen:unsafe-warning fmt-string . rest)
  (let ((message (apply zen:format fmt-string rest)))
    (if (%%not (%%table-ref zen:reported-warnings message #f))
        (begin
          ;; always display warnings about unsafe code
          (zen:feedback "{a}" message)
          (zen:report "{a}" message)
          (%%table-set! zen:reported-warnings message #t)))))


(define (zen:keyword->symbol keyword)
  (%%string->symbol (%%keyword->string keyword)))


(define (zen:parse-module partial-form)
  (define (parse-modifiers rest)
    (let ((first (zen:source-code (%%car rest))))
      (if (%%memq first '(protected public))
          (values first (%%cdr rest))
        (values 'public rest))))
  
  (receive (access rest) (parse-modifiers partial-form)
    (let ((name (zen:source-code (%%car rest)))
          (dialect-src (%%cadr rest))
          (dialect (zen:desourcify (%%cadr rest)))
          (body (%%cddr rest)))
      (receive (dialect-name dialect-transformations) (zen:parse-dialect dialect)
        (%%assert (%%symbol? name)
          (values name
                  access
                  '()
                  dialect-name
                  dialect-transformations
                  dialect-src
                  body))))))


(define (zen:parse-script partial-form)
  (let ((dialect-src (%%car partial-form))
        (dialect (zen:desourcify (%%car partial-form)))
        (body (%%cdr partial-form)))
    (receive (dialect-name dialect-transformations) (zen:parse-dialect dialect)
      (values dialect-name
              dialect-transformations
              dialect-src
              body))))


(define (zen:parse-dialect dialect)
  (cond ((%%keyword? dialect)
         (values (zen:keyword->symbol dialect) #f))
        ((and (%%pair? dialect)
              (%%eq? (%%car dialect) 'dialect))
         (%%assert (%%pair? (%%cdr dialect))
           (let ((name (%%cadr dialect))
                 (transformations (%%cddr dialect)))
             (values name transformations))))
        (else
         (values dialect '()))))


(define (zen:parse-module-invoice walker resume declaration form-src specification)
  (define (ill-formed)
    (zen:walk-error walker resume declaration form-src "Ill-formed module invoice: {s}" specification))
  
  (if (%%not (%%pair? specification))
      (ill-formed)
    (let ((phase #f)
          (autoload #f)
          (transformation-queue (zen:new-queue)))
      (let ((name (%%car specification)))
        (if (%%symbol? name)
            (for-each (lambda (spec)
                        (if (%%pair? spec)
                            (let ((option (%%car spec))
                                  (arguments (%%cdr spec)))
                              (case option
                                ((phase)    (set! phase (%%car arguments)))
                                ((only)     (zen:enqueue transformation-queue spec))
                                ((except)   (zen:enqueue transformation-queue spec))
                                ((prefix)   (zen:enqueue transformation-queue spec))
                                ((rename)   (zen:enqueue transformation-queue spec))
                                ((autoload) (set! autoload arguments))
                                ((cond)      #f)
                                (else       (ill-formed))))
                          (ill-formed)))
                      (%%cdr specification))
          (ill-formed))
        (values name phase autoload (zen:queue-list transformation-queue))))))


(define (zen:parse-module-declaration partial-form)
  (receive (name access modifiers dialect-name dialect-transformations dialect-src body) (zen:parse-module partial-form)
    (if (and (zen:requested-unit-name) (%%neq? name (zen:requested-unit-name)))
        (zen:error "Module at {s} is defining {s}" (zen:requested-unit-name) name)
      (parameterize ((zen:walk-context (zen:new-walk-context #f name #f)))
        (let* ((dialect-invoice (and dialect-transformations (zen:load-dialect-invoice dialect-name dialect-transformations)))
               (dialect (zen:require-dialect dialect-name))
               (walker (zen:dialect-walker dialect)))
          (let ((declaration (zen:walk-module-declaration walker #f name access modifiers dialect-name dialect-invoice dialect-src body)))
            #; ;; todo validate outline errors
            (zen:validate-walk-problems walker)
            declaration))))))


(define (zen:walk-module-declaration walker actual name access modifiers dialect-name dialect-invoice dialect-src body)
  (let ((declaration (or actual (zen:new-module-declaration name access modifiers #f walker dialect-name dialect-invoice))))
    (%%when dialect-invoice
      (zen:add-module-import walker #f declaration dialect-src declaration dialect-invoice #f))
    ;; reset the walker if it was cached
    (zen:set-module-declaration-walker declaration walker)
    (zen:walk-declarations walker #f declaration (%%cons declaration (zen:walker-environment walker)) body)
    (zen:validate-walk-problems walker)
    declaration))


(define (zen:rename-identifier-conflicts expressions environment)
  (zen:tree-fold-list
    expressions
    (lambda (x seed env) env)
    (lambda (x seed child-seed env) seed)
    (lambda (x seed env)
      (%%when (%%is? x zen:Binding-Reference)
        (let* ((var (zen:get-binding-reference-binding x))
               (sym (zen:unwrap-syntactic-closure (zen:get-lexical-binding-name var))))
          (let lp1 ((e env))
               (cond ((pair? e)
                      (let lp2 ((ls (%%car e)) (found? #f))
                           (cond ((pair? ls)
                                  (let* ((binding (%%car ls))
                                         (same? (%%eq? var binding)))
                                    (cond ((and binding
                                                (%%not same?)
                                                (%%is? binding zen:Variable)
                                                (%%not (zen:get-symbol-binding-gensym binding))
                                                (%%eq? sym
                                                       (zen:unwrap-syntactic-closure
                                                         (zen:get-lexical-binding-name binding))))
                                           ;; if we shadow an existing declaration, gensym a unique symbol for it
                                           #; ;; debug
                                           (if (zen:testing?)
                                               (zen:debug shadow: binding 'by var))
                                           (zen:set-symbol-binding-gensym binding (zen:generate-symbol (%%symbol->string sym)))))
                                    (lp2 (%%cdr ls) (or found? same?))))
                                 ((%%not found?)
                                  (lp1 (%%cdr e))))))))))
      seed)
    #f
    (%%list environment)))


(define (zen:generate-module partial-form #!optional (emit? #t))
  (receive (name access modifiers dialect-name dialect-transformations dialect-src body) (zen:parse-module partial-form)
    (if (and (zen:requested-unit-name) (%%neq? name (zen:requested-unit-name)))
        (zen:error "Module at {s} is defining {s}" (zen:requested-unit-name) name)
      (parameterize ((zen:walk-context (zen:new-walk-context #f name #f)))
        (let* ((dialect-invoice (and dialect-transformations (zen:load-dialect-invoice dialect-name dialect-transformations)))
               (dialect (zen:require-dialect dialect-name))
               (walker (zen:dialect-walker dialect))
               (resume #f)
               (actual (zen:get-catalog-entry name))
               (body (zen:dialect-wrap dialect body))
               (declaration (zen:call-with-catalog-entry-lock name
                              (lambda ()
                                (let ((declaration (zen:walk-module-declaration walker actual name access modifiers dialect-name dialect-invoice dialect-src body)))
                                  (zen:set-catalog-entry name declaration)
                                  declaration))))
               (environment (%%cons declaration (zen:walker-environment walker)))
               (body (zen:walk-namespace walker resume declaration environment body)))
          (zen:validate-walk-problems walker)
          (zen:rename-identifier-conflicts body environment)
          (zen:set-namespace-declaration-body declaration body)
          (if emit?
              (let ((emitted (zen:emit-declaration declaration walker resume '())))
                (zen:validate-walk-problems walker)
                emitted)
            declaration))))))


(define (zen:generate-script partial-form #!optional (emit? #t))
  (let ((name (gensym 'script)))
    (receive (dialect-name dialect-transformations dialect-src body) (zen:parse-script partial-form)
      (parameterize ((zen:walk-context (zen:new-walk-context #f name #f)))
        (let* ((dialect-invoice (and dialect-transformations (zen:load-dialect-invoice dialect-name dialect-transformations)))
               (dialect (zen:require-dialect dialect-name))
               (walker (zen:dialect-walker dialect))
               (resume #f)
               (body (zen:dialect-wrap dialect body))
               (declaration (zen:walk-module-declaration walker #f name 'public '() dialect-name dialect-invoice dialect-src body))
               (environment (%%cons declaration (zen:walker-environment walker)))
               (body (zen:walk-namespace walker resume declaration environment body)))
          (zen:validate-walk-problems walker)
          (zen:rename-identifier-conflicts body environment)
          (zen:set-namespace-declaration-body declaration body)
          (if emit?
              (let ((emitted (zen:emit-declaration declaration walker resume '())))
                (zen:validate-walk-problems walker)
                emitted)
            declaration))))))


(define (zen:cond-expand walker resume declaration form-src cont)
  (if (and (%%pair? (zen:source-code form-src))
           (%%eq? (zen:source-code (%%car (zen:source-code form-src))) 'cond-expand))
      (zen:process-cond-expand walker resume declaration form-src cont)
    (cont form-src #t)))


(define (zen:process-cond-expand walker resume declaration form-src proc)
  (define (valid-feature-requirement clause)
    (if (%%pair? clause)
        (let ((requirement (zen:desourcify-all (%%car clause))))
          (if (or (%%symbol? requirement)
                  (and (%%pair? requirement)
                       (%%memq (%%car requirement) '(and or not))))
              requirement
            #f))
      #f))
  
  (let iter ((scan (%%cdr (zen:source-code form-src))))
       (if (%%null? scan)
           (zen:walk-error walker resume declaration form-src "Unfulfilled cond-expand")
         (let ((clause (zen:source-code (%%car scan))))
           (let ((feature-requirement (valid-feature-requirement clause)))
             (if (%%not feature-requirement)
                 (zen:error "Ill-formed cond-expand clause: {s}" (zen:desourcify-all clause))
               (if (or (zen:feature-satisfied? feature-requirement)
                       (%%eq? feature-requirement 'else))
                   (if (%%null? (%%cdr clause))
                       (proc #f #f)
                     (proc `(begin ,@(%%cdr clause)) #t))
                 (iter (%%cdr scan)))))))))


(define (zen:walk-namespace walker resume declaration environment form-list)
  (let ((queue (zen:new-queue)))
    (define (walk forms)
      (for-each (lambda (form-src)
                  (continuation-capture
                    (lambda (resume)
                      (zen:cond-expand walker resume declaration form-src
                        (lambda (expr expr?)
                          (%%when expr?
                            (cond ((zen:include-form? expr)
                                   (walk (zen:include-forms (zen:include-filename expr))))
                                  (else
                                   (zen:enqueue queue (zen:walk walker resume declaration environment expr))))))))))
                forms))
    
    (walk form-list)
    (zen:queue-list queue)))


(define (zen:load-dialect-invoice dialect-name dialect-transformations)
  (if (%%not (%%symbol? dialect-name))
      (zen:error "Dialect must be a symbol: {s}" dialect-name)
    (if (%%eq? dialect-name 'foundation.dialect)
        #f
      (zen:new-import-invoice
        dialect-name
        (zen:outline-module dialect-name)
        'syntax
        dialect-transformations))))


(define (zen:emit-module-inclusions module-declaration walker resume environment)
  (define (find-name name lst)
    (if (%%null? lst)
        #f
      (if (%%eq? (zen:get-lexical-binding-name (%%car lst)) name)
          #t
        (find-name name (%%cdr lst)))))
  
  (let ((queue (zen:new-queue)))
    (define (collect-declarations declaration)
      (for-each collect-declarations (zen:get-declaration-inclusions declaration))
      ;; This name based test if a quick solution to the complex problem of a declaration
      ;; replacing another one where there are references to the old one. Should we then just
      ;; replace or destructively modify the old one and what if the type of the one replacing
      ;; is incompatible...
      (%%when (%%not (find-name (zen:get-lexical-binding-name declaration) (zen:queue-list queue)))
        (zen:enqueue queue declaration)))
    
    (for-each collect-declarations (zen:get-module-declaration-inclusions module-declaration))
    (map (lambda (declaration)
           (zen:expand-referenced-declaration declaration))
         (zen:queue-list queue))))


(define (zen:emit-module-literals module-declaration walker resume environment)
  (map (lambda (info)
         (let ((name (%%car info))
               (value (%%cdr info)))
           `(define ,name ,(zen:sourcified-form (zen:emit-expression value module-declaration walker resume '())))))
       (zen:get-module-declaration-walker-literals module-declaration)))


(define (zen:emit-module-variables module-declaration walker resume environment)
  (map (lambda (variable)
         (let ((symbol (%%car variable))
               (value (%%cdr variable)))
           `(zen:define-variable ,symbol ,value)))
       (zen:queue-list (zen:get-module-declaration-walker-variables module-declaration))))


(define (zen:emit-module-statics module-declaration walker resume environment)
  (map (lambda (static)
         (let ((symbol (%%car static))
               (expr (%%cdr static)))
           `(zen:define-variable ,symbol ,(zen:sourcified-form (zen:emit-expression expr module-declaration walker resume '())))))
       (zen:queue-list (zen:get-module-declaration-walker-statics module-declaration))))


(define (zen:emit-module-autoloads module-declaration walker resume environment)
  (let ((queue (zen:new-queue))
        (locators (%%make-table test: eq?)))
    (for-each (lambda (autoload-declaration)
                (let ((referenced-declaration (zen:resolve-binding autoload-declaration)))
                  (let ((locator (zen:autoload-locator referenced-declaration)))
                    (%%when (%%not (%%table-ref locators locator #f))
                      (%%table-set! locators locator #t)
                      (zen:enqueue queue
                        (let ((declaration-locator (zen:get-declaration-locator (zen:get-declaration-toplevel referenced-declaration)))
                              (reference-locator (zen:sourcified-form (zen:emit-binding-reference referenced-declaration module-declaration walker resume environment))))
                          (if (%%symbol? reference-locator)
                              `(zen:define-variable ,locator
                                 (zen:cache-autoload
                                   ',declaration-locator
                                   ',reference-locator
                                   ',locator))
                            `(define ,locator
                               (let ((loaded? #f))
                                 (lambda ()
                                   (if (%%not loaded?)
                                       (begin
                                         (zen:load-unit ',(zen:get-declaration-locator (zen:get-declaration-toplevel referenced-declaration)))
                                         (set! loaded? #t)))
                                   ,(zen:sourcified-form (zen:emit-binding-reference referenced-declaration module-declaration walker resume environment))))))))))))
              (zen:get-module-declaration-walker-autoloads module-declaration))
    (zen:sort-list (lambda (x y) (%%string<? (%%symbol->string (%%cadr x)) (%%symbol->string (%%cadr y)))) (zen:queue-list queue))))


(define (zen:emit-module-registration declaration walker resume environment)
  `((zen:register-module ',(zen:get-lexical-binding-name declaration) ',(zen:get-declaration-access declaration)
      ',(let ((queue (zen:new-queue)))
          (for-each (lambda (module-invoice)
                      (%%when (%%neq? (zen:get-module-invoice-phase module-invoice) 'syntax)
                        (let ((autoload (zen:get-export-invoice-autoload module-invoice))
                              (symbols (zen:get-export-invoice-symbols module-invoice)))
                          (%%when (and (%%not autoload) (%%not symbols))
                            (zen:enqueue queue (zen:get-module-invoice-name module-invoice))))))
                    (zen:get-module-declaration-exports declaration))
          (zen:sort-list (lambda (x y) (%%string<? (%%symbol->string x) (%%symbol->string y))) (zen:queue-list queue)))
      ',(let ((walker (zen:get-module-declaration-walker declaration))
              (queue (zen:new-queue)))
          (zen:table-iterate-safe (zen:get-public-lookup declaration)
            (lambda (name decl)
              (%%when (or (%%eq? (zen:get-declaration-toplevel decl) declaration)
                          ;; quick hack
                          (%%is? decl zen:Autoload-Declaration))
                (let ((export (zen:runtime-export walker decl)))
                  (if export
                      (zen:enqueue queue (%%cons name export)))))))
          (zen:sort-list (lambda (x y) (%%string<? (%%symbol->string (%%car x)) (%%symbol->string (%%car y)))) (zen:queue-list queue))))))


(zen:define-method (zen:runtime-export (zen:Walker walker) declaration)
  (cond ((%%is? declaration zen:Export-Declaration)
         (zen:get-declaration-locator declaration))
        ((%%is? declaration zen:Autoload-Declaration)
         (let ((referenced-declaration (zen:resolve-binding declaration)))
           (%%cons (zen:get-declaration-locator (zen:get-declaration-toplevel referenced-declaration))
                   (zen:get-declaration-locator referenced-declaration))))
        (else
         #f)))


(zen:define-method (zen:outline-generate (zen:Module-Declaration declaration) output)
  (let ((declarations (zen:outline-generate-filter-access (zen:resolve-bindings (zen:queue-list (zen:get-namespace-declaration-children declaration))) (zen:get-public-lookup declaration))))
    (let ((name (zen:get-lexical-binding-name declaration))
          (dialect-name (zen:get-module-declaration-dialect-name declaration))
          (dialect-invoice (zen:get-module-declaration-dialect-invoice declaration)))
      (zen:format output "(module {s} {s}" name (if (%%not dialect-invoice) (string->keyword (symbol->string dialect-name)) dialect-name)))
    #; ;; requires are not needed and create problems when outlined
    (for-each (lambda (require-invoice)
                (let ((name (zen:get-require-invoice-name require-invoice))
                      (phase (zen:get-require-invoice-phase require-invoice)))
                  (zen:format output "{%}  {s}" `(require (,name ,@(zen:outline-generate-phase-list phase))))))
              (zen:get-module-declaration-requires declaration))
    (for-each (lambda (module-invoice)
                (let ((name (zen:get-module-invoice-name module-invoice))
                      (phase (zen:get-module-invoice-phase module-invoice))
                      (transformations (zen:get-module-invoice-transformations module-invoice)))
                  (if name
                      (let ((autoload (zen:get-export-invoice-autoload module-invoice)))
                        (if autoload
                            (let ((autoload-names
                                    (map (lambda (declaration-reference)
                                           (let ((name (zen:reference-name (zen:get-declaration-reference-name declaration-reference))))
                                             (if (zen:get-autoload-reference-hubs? declaration-reference)
                                                 (%%list name)
                                               name)))
                                         autoload)))
                              (zen:format output "{%}  {s}" `(export (,name (autoload ,@autoload-names)))))
                          (zen:format output "{%}  {s}" `(export (,name ,@(zen:outline-generate-phase-list phase) ,@(zen:outline-generate-transformations transformations)))))))))
              (zen:get-module-declaration-exports declaration))
    (for-each (lambda (module-invoice)
                (let ((name (zen:get-module-invoice-name module-invoice))
                      (phase (zen:get-module-invoice-phase module-invoice))
                      (transformations (zen:get-module-invoice-transformations module-invoice)))
                  (if name
                      (zen:format output "{%}  {s}" `(import (,name ,@(zen:outline-generate-phase-list phase) ,@(zen:outline-generate-transformations transformations)))))))
              (zen:get-module-declaration-imports declaration))
    (for-each (lambda (decl)
                (zen:outline-generate decl output))
              declarations)
    (zen:format output "){%}")))


(define (zen:iterate-module-declaration module-declaration proc)
  (zen:tree-fold
    module-declaration
    (lambda (obj seed env)
      (proc obj)
      env)
    (lambda (obj seed child-seed env)
      seed)
    (lambda (obj seed env)
      (proc obj)
      seed)
    #f
    #f))


;;;
;;;; Require Invoice
;;;


(zen:define-class zen:Require-Invoice zen:Object (constructor: zen:allocate-require-invoice)
  ((name  getter: generate)
   (phase getter: generate)))


(define (zen:new-require-invoice name phase)
  (zen:allocate-require-invoice name phase))


;;;
;;;; Module Invoice
;;;


(zen:define-class zen:Module-Invoice zen:Object ()
  ((name            getter: generate)
   (module          getter: generate)
   (phase           getter: generate)
   (transformations getter: generate)))


(define (zen:find-module-invoice invoices target)
  (let ((target-name (zen:get-module-invoice-name target))
        (target-phase (zen:get-module-invoice-phase target)))
    (zen:find-if (lambda (invoice)
                    (and (%%eq? (zen:get-module-invoice-name invoice) target-name)
                         (%%eq? (zen:get-module-invoice-phase invoice) target-phase)))
                  invoices)))


;;;
;;;; Export Invoice
;;;


(zen:define-class zen:Export-Invoice zen:Module-Invoice (constructor: zen:allocate-export-invoice)
  ((autoload getter: generate setter: generate)
   (symbols  getter: generate setter: generate)))


(define (zen:new-export-invoice name module phase transformations autoload symbols)
  (zen:allocate-export-invoice name module phase transformations autoload symbols))


;;;
;;;; Import Invoice
;;;


(zen:define-class zen:Import-Invoice zen:Module-Invoice (constructor: zen:allocate-import-invoice)
  ((hit? getter: generate setter: generate)))


(define (zen:new-import-invoice name module phase transformations)
  (zen:allocate-import-invoice name module phase transformations #f))


;;;
;;;; Export
;;;


(zen:define-class zen:Export-Declaration zen:Declaration (constructor: zen:allocate-export-declaration)
  ((symbol getter: generate)))


(define (zen:new-export-declaration name type access compatibility modifiers attributes parent symbol)
  (let ((new-declaration (zen:allocate-export-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f symbol)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(zen:define-method (zen:walk-binding-validate-call (zen:Export-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (zen:unspecified))


(zen:define-method (zen:emit-declaration (zen:Export-Declaration declaration) walker resume environment)
  (let ((name (zen:get-lexical-binding-name declaration))
        (symbol (zen:get-export-declaration-symbol declaration))
        (parent (zen:get-declaration-parent declaration)))
    (%%assert (%%is? parent zen:Module-Declaration))
    (if (zen:get-generate? 'register)
        `(zen:register-native ',(zen:get-lexical-binding-name parent) ',name ',symbol)
      `(begin))))


(zen:define-method (zen:emit-binding-reference (zen:Export-Declaration declaration) source-declaration walker resume environment)
  (zen:new-code
    (zen:get-export-declaration-symbol declaration)
    zen:Any
    #f))


(define (zen:walk-export-declaration walker resume declaration environment form-src)
  (define (walk-module-export export)
    (receive (module-name module-phase module-autoload module-transformations) (zen:parse-module-invoice walker resume declaration form-src export)
      (let ((module-reference (zen:new-module-reference module-name #f)))
        (zen:new-export-invoice module-name
                                 module-reference
                                 module-phase
                                 module-transformations
                                 (if (%%not module-autoload)
                                     #f
                                   (map (lambda (clause)
                                          (if (%%symbol? clause)
                                              (zen:new-autoload-reference clause #f #f #f)
                                            (%%assert (and (%%pair? clause)
                                                           (%%null? (%%cdr clause))
                                                           (%%symbol? (%%car clause)))
                                              (zen:new-autoload-reference (%%car clause) #f #f #t))))
                                        module-autoload))
                                 #f))))
  
  (define (walk-exports exports)
    (let ((partition (zen:partition exports symbol? assv)))
      (let ((symbols-exports (%%assq #t partition))
            (module-exports (%%assq #f partition)))
        (let ((symbol-references (if symbols-exports
                                     (map (lambda (symbol) (zen:new-export-reference symbol #f #f)) (%%cdr symbols-exports))
                                   #f)))
          (%%append (if symbols-exports
                        (%%list (zen:new-export-invoice #f #f #f #f #f symbol-references))
                      '())
                    (if module-exports
                        (map (lambda (export)
                               (walk-module-export export))
                             (%%cdr module-exports))
                      '()))))))
  
  (let ((form (%%desourcify form-src)))
    (let ((module-declaration (zen:get-declaration-toplevel declaration)))
      (let ((export-invoices (walk-exports (zen:filter-invoices (%%cdr form)))))
        (for-each (lambda (export-invoice)
                    (zen:add-module-export walker resume declaration form-src module-declaration export-invoice))
                  export-invoices)))))


(define (zen:walk-export walker resume declaration environment form-src)
  (zen:new-begin #f '()))


(zen:define-method (zen:outline-extract (zen:Export-Declaration declaration) meta)
  `(native ,@(zen:outline-generate-modifiers declaration) ,(zen:get-export-declaration-symbol declaration)))



;;;
;;;; Export Syntax
;;;


(zen:define-class zen:Export-Syntax-Declaration zen:Declaration (constructor: zen:allocate-export-syntax-declaration)
  ((symbol getter: generate)))


(define (zen:new-export-syntax-declaration name type access compatibility modifiers attributes parent symbol)
  (let ((new-declaration (zen:allocate-export-syntax-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f symbol)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(zen:define-method (zen:walk-binding-validate-call (zen:Export-Syntax-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (zen:unspecified))


(zen:define-method (zen:emit-declaration (zen:Export-Syntax-Declaration declaration) walker resume environment)
  `(begin))


(zen:define-method (zen:emit-binding-reference (zen:Export-Syntax-Declaration declaration) source-declaration walker resume environment)
  (zen:new-code
    (zen:get-export-syntax-declaration-symbol declaration)
    zen:Any
    #f))


(zen:define-method (zen:outline-extract (zen:Export-Syntax-Declaration declaration) meta)
  `(native-syntax ,@(zen:outline-generate-modifiers declaration) ,(zen:get-export-syntax-declaration-symbol declaration)))


;;;
;;;; Autoload
;;;


(zen:define-class zen:Autoload-Declaration zen:Declaration (constructor: zen:allocate-autoload-declaration)
  ((module           getter: generate)
   (exported-module  getter: generate)
   (declaration      getter: generate setter: generate)))


(define (zen:new-autoload-declaration name type parent module-declaration exported-module)
  (let ((new-declaration (zen:allocate-autoload-declaration name type #f 'public 'uptodate '() '() #f parent #f #f #f module-declaration exported-module #f)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(zen:define-method (zen:of-subtype? (zen:Autoload-Declaration declaration) subtype)
  ;; not sure calling resolve here is correct
  (zen:of-subtype? (zen:resolve-binding declaration)
                    ;; hack not sure this is the right way
                    (zen:resolve-type subtype)))


(zen:define-method (zen:specifiable? (zen:Autoload-Declaration declaration))
  ;; quick hack
  #t)


(zen:define-method (zen:resolve-binding (zen:Autoload-Declaration declaration))
  (or (zen:get-autoload-declaration-declaration declaration)
      (let* ((exported-module (zen:resolve-reference (zen:get-autoload-declaration-exported-module declaration) (zen:get-autoload-declaration-module declaration)))
             (name (zen:get-lexical-binding-name declaration))
             (decl (zen:lookup-declaration exported-module name zen:public-access declaration)))
        (%%assertion decl (zen:error "Unable to find autoload {s} in unit {s}" name (zen:get-declaration-locator exported-module))
          (zen:set-autoload-declaration-declaration declaration decl)
          decl))))


(zen:define-method (zen:walk-binding-validate-call (zen:Autoload-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (zen:walk-binding-validate-call (zen:resolve-binding declaration) walker resume source-declaration operator arguments form-src))


(zen:define-method (zen:emit-binding-reference (zen:Autoload-Declaration declaration) source-declaration walker resume environment)
  (let ((referenced-declaration (zen:resolve-binding declaration)))
    (zen:new-code
      `(,(zen:autoload-locator referenced-declaration))
      referenced-declaration
      #f)))


(zen:define-method (zen:emit-specifier (zen:Autoload-Declaration declaration))
  (zen:type->specifier declaration #t))


;; this heuristic used because we cannot call zen:resolve-binding at various points
;; is not 100% correct if the autoload was obtained through a reexported module...
(define (zen:autoload-declaration-locator-heuristic declaration)
  (zen:compose-reference (zen:get-declaration-reference-name (zen:get-autoload-declaration-exported-module declaration)) (zen:get-lexical-binding-name declaration)))


(define (zen:autoload-locator referenced-declaration)
  (%%string->symbol (%%string-append (%%symbol->string (zen:get-declaration-locator referenced-declaration))
                                     ":autoload")))


;;;
;;;; Literal
;;;


(zen:define-class zen:Literal-Class zen:Class (metaclass: zen:Class)
  ())


(zen:define-class zen:Literal zen:Object (constructor: zen:allocate-literal metaclass: zen:Literal-Class)
  ((name      getter: generate)
   (arguments getter: generate)))


(zen:define-variable-override zen:new-literal
  (lambda (name arguments)
    (zen:allocate-literal name arguments)))


(define (zen:walk-literal-constant walker resume declaration environment literal)
  (let ((module-declaration (zen:get-declaration-toplevel declaration)))
    (define (walk-literal walker resume declaration literal)
      (let ((environment (%%cons module-declaration (zen:walker-environment walker))))
        (zen:walk walker resume module-declaration environment
          (let ((name (zen:get-literal-name literal))
                (arguments (zen:get-literal-arguments literal)))
            (let ((constructor-reference (%%car (zen:require-literal-constructor (%%desourcify name)))))
              `(,constructor-reference ,@(map (lambda (arg)
                                                `(quote ,arg))
                                              arguments)))))))

    ;; calling zen:get-registered-literal to only register when not already there
    ;; doesnt work directly because some literals are interned and thus can be shared
    (let ((locator (zen:generate-global-symbol "lit")))
      ;; it is important to register before walking so subliterals come before us
      (let ((info (%%cons locator #f)))
        (zen:set-module-declaration-walker-literals module-declaration (%%cons info (zen:get-module-declaration-walker-literals module-declaration)))
        (%%set-cdr! info (walk-literal walker resume declaration literal)))
      ;; this way of getting a reference to the literal's class is a quick solution
      (let ((literal-type (%%desourcify (zen:get-literal-name literal))))
        (zen:new-constant locator (zen:lookup-reference walker resume declaration environment literal-type))))))


(define (zen:make-symbolic-chars alist)
  (%%list->table
    (map (lambda (pair)
           (%%cons (%%car pair) (integer->char (%%cdr pair))))
         alist)
    test: eq?))


(define zen:Symbolic-Chars
  (zen:make-symbolic-chars
    '((zero              . #x00)
      (home              . #x01)
      (enter             . #x03)
      (end               . #x04)
      (info              . #x05)
      (backspace         . #x08)
      (tab               . #x09)
      (linefeed          . #x0A)
      (page-up           . #x0B)
      (page-down         . #x0C)
      (return            . #x0D)
      (escape            . #x1B)
      (left-arrow        . #x1C)
      (right-arrow       . #x1D)
      (up-arrow          . #x1E)
      (down-arrow        . #x1F)
      (space             . #x20)
      (exclamation-point . #x21)
      (double-quote      . #x22)
      (sharp             . #x23)
      (ampersand         . #x26)
      (quote             . #x27)
      (open-parenthesis  . #x28)
      (close-parenthesis . #x29)
      (times             . #x2A)
      (plus              . #x2B)
      (comma             . #x2C)
      (minus             . #x2D)
      (period            . #x2E)
      (slash             . #x2F)
      (colon             . #x3A)
      (semi-colon        . #x3B)
      (question-mark     . #x3F)
      (at                . #x40)
      (open-bracket      . #x5B)
      (backslash         . #x5C)
      (close-bracket     . #x5D)
      (exponential       . #x5E)
      (underscore        . #x5F)
      (backquote         . #x60)
      (open-brace        . #x7B)
      (close-brace       . #x7D)
      (delete            . #x7F)
      (copyright         . #xA9))))


(define (zen:symbolic-char name)
  (%%table-ref zen:Symbolic-Chars name #f))


;;;
;;;; Void
;;;


(zen:define-class zen:Void-Class zen:Class (metaclass: zen:Class)
  ())


(zen:define-method (zen:of-subtype? (zen:Void-Class type) subtype)
  #f)


(zen:define-method (zen:emit-specifier (zen:Void-Class type))
  'void)


(zen:define-class zen:Void zen:Type (metaclass: zen:Void-Class)
  ())


;;;
;;;; Opt
;;;


(zen:define-class zen:Opt-Type zen:Type (metaclass: zen:Class constructor: zen:allocate-opt-type)
  ((type getter: generate)))


(define (zen:new-opt-type type)
  (zen:allocate-opt-type type))


(zen:define-method (zen:emit-specifier (zen:Opt-Type type))
  (let ((type-specifier (zen:type->specifier (zen:get-opt-type-type type) #t)))
    (%%string->symbol (%%string-append "opt<" (%%symbol->string type-specifier) ">"))))


;;;
;;;; Key
;;;


(zen:define-class zen:Key-Type zen:Type (metaclass: zen:Class constructor: zen:allocate-key-type)
  ((key  getter: generate)
   (type getter: generate)))


(define (zen:new-key-type key type)
  (zen:allocate-key-type key type))


(zen:define-method (zen:emit-specifier (zen:Key-Type type))
  (let ((key (zen:get-key-type-key type))
        (type-specifier (zen:type->specifier (zen:get-key-type-type type) #t)))
    (%%string->symbol (%%string-append "key<" (%%keyword->string key) ":" (%%symbol->string type-specifier) ">"))))


;;;
;;;; Rest
;;;


(zen:define-class zen:Rest-Type zen:Type (metaclass: zen:Class constructor: zen:allocate-rest-type)
  ((type getter: generate)))


(define (zen:new-rest-type type)
  (zen:allocate-rest-type type))


(zen:define-method (zen:emit-specifier (zen:Rest-Type type))
  (let ((type-specifier (zen:type->specifier (zen:get-rest-type-type type))))
    (%%string->symbol (%%string-append (%%symbol->string type-specifier) "*"))))


;;;
;;;; Function
;;;


;; should probably be unified with the Signature class


(zen:define-class zen:Function-Type zen:Type (metaclass: zen:Class constructor: zen:allocate-function-type)
  ((mandatory  getter: generate)
   (positional getter: generate)
   (optional   getter: generate)
   (named      getter: generate)
   (rest       getter: generate)
   (result     getter: generate)))


(define (zen:new-function-type positional optional named rest result)
  (let ((mandatory (%%length positional)))
    (zen:allocate-function-type mandatory positional optional named rest result)))


(zen:define-method (zen:of-subtype? (zen:Function-Type type) subtype)
  (or (zen:of-subtype? zen:Procedure subtype)
      (%%eq? (%%get-object-class subtype) zen:Function-Type)))


(zen:define-method (zen:emit-specifier (zen:Function-Type type))
  (let ((output (open-output-string)))
    (let ((first? #t))
      (for-each (lambda (type)
                  (if first?
                      (set! first? #f)
                    (write-char #\^ output))
                  (display (zen:type->specifier type) output))
                (zen:get-function-type-positional type))
      (let ((rest (zen:get-function-type-rest type)))
        (%%when rest
          (%%when (%%not first?)
            (write-char #\^ output))
          (display (zen:type->specifier rest) output))))
    (write-char #\: output)
    (display (zen:type->specifier (zen:get-function-type-result type)) output)
    (%%string->symbol (get-output-string output))))


(zen:define-method (zen:emit-cast (zen:Function-Type type) value source-declaration walker resume environment)
  `(if (%%procedure? ,value)
       ,value
     (zen:type-error ,value zen:Procedure)))


(define (zen:only-positional-function-type? function-type)
  (and (%%null? (zen:get-function-type-optional function-type))
       (%%null? (zen:get-function-type-named function-type))
       (%%not (zen:get-function-type-rest function-type))))


(define (zen:typed-function-type? function-type #!optional (skip-first-positional? #f))
  (define (typed? type)
    (%%neq? type zen:Any))
  
  (let ((positional (zen:get-function-type-positional function-type))
        (optional (zen:get-function-type-optional function-type))
        (named (zen:get-function-type-named function-type))
        (rest (zen:get-function-type-rest function-type)))
    (or (zen:some? typed? (if skip-first-positional? (%%cdr positional) positional))
        (zen:some? typed? optional)
        (zen:some? typed? named)
        (and rest (typed? rest)))))


;;;
;;;; Category
;;;


;; first draft. this type is used to support specializing new and the like


(zen:define-class zen:Category-Type zen:Type (metaclass: zen:Class constructor: zen:allocate-category-type)
  ((declaration getter: generate)))


(define (zen:new-category-type declaration)
  (zen:allocate-category-type declaration))


;; quicky solution to stop casts on this type
(zen:define-method (zen:of-subtype? (zen:Category-Type type) subtype)
  #t)


(zen:define-method (zen:emit-cast (zen:Category-Type type) value source-declaration walker resume environment)
  value)


(zen:define-method (zen:emit-specifier (zen:Category-Type type))
  (let ((output (open-output-string)))
    (display "category" output)
    (write-char #\< output)
    (display (zen:type->specifier (zen:get-category-type-declaration type) #t) output)
    (write-char #\> output)
    (%%string->symbol (get-output-string output))))


;;;
;;;; Values
;;;


(zen:define-class zen:Values-Type zen:Type (metaclass: zen:Class constructor: zen:allocate-values-type)
  ((types getter: generate)))


(define (zen:new-values-type types)
  (zen:allocate-values-type types))


(zen:define-method (zen:emit-specifier (zen:Values-Type type))
  (let ((output (open-output-string)))
    (display "values" output)
    (write-char #\< output)
    (let ((first? #t))
      (for-each (lambda (type)
                  (if first?
                      (set! first? #f)
                    (write-char #\/ output))
                  (display (zen:type->specifier type) output))
                (zen:get-values-type-types type)))
    (write-char #\> output)
    (%%string->symbol (get-output-string output))))


;;;
;;;; Restriction
;;;


(zen:define-class zen:Restriction-Type zen:Type (metaclass: zen:Class constructor: zen:allocate-restriction-type)
  ((base getter: generate)
   (type getter: generate)))


(define (zen:new-restriction-type base type)
  (zen:allocate-restriction-type base type))


;;;
;;;; Complement
;;;


(zen:define-class zen:Complement-Type zen:Type (metaclass: zen:Class constructor: zen:allocate-complement-type)
  ((type getter: generate)))


(define (zen:new-complement-type type)
  (zen:allocate-complement-type type))


;;;
;;;; Union
;;;


(zen:define-class zen:Union-Type zen:Type (metaclass: zen:Class constructor: zen:allocate-union-type)
  ((types getter: generate)))


(define (zen:new-union-type types)
  (zen:allocate-union-type types))


;;;
;;;; Template
;;;


;; future work. just here to make sure specifier syntax can express them


(zen:define-class zen:Template-Type zen:Type (metaclass: zen:Class constructor: zen:allocate-template-type)
  ((class getter: generate)
   (types getter: generate)))


(define (zen:new-template-type class types)
  (zen:allocate-template-type class types))


(zen:define-method (zen:emit-specifier (zen:Template-Type type))
  (let ((output (open-output-string)))
    (display (zen:type->specifier (zen:get-template-type-class type)) output)
    (write-char #\< output)
    (let ((first? #t))
      (for-each (lambda (type)
                  (if first?
                      (set! first? #f)
                    (write-char #\/ output))
                  (display (zen:type->specifier type) output))
                (zen:get-template-type-types type)))
    (write-char #\> output)
    (%%string->symbol (get-output-string output))))


;;;
;;;; Nillable
;;;


(zen:define-class zen:Nillable-Type zen:Type (metaclass: zen:Class constructor: zen:allocate-nillable-type)
  ((type getter: generate)))


(define (zen:new-nillable-type type)
  (zen:allocate-nillable-type type))


(zen:define-method (zen:of-subtype? (zen:Nillable-Type type) subtype)
  (or (zen:of-subtype? zen:Boolean subtype)
      (zen:of-subtype? (zen:get-nillable-type-type type) subtype)
      ;; quick solution to be thought through
      (and (%%class-is? subtype zen:Nillable-Type)
           (%%eq? (zen:get-nillable-type-type type) (zen:get-nillable-type-type subtype)))))


(zen:define-method (zen:emit-specifier (zen:Nillable-Type type))
  (let ((type-specifier (zen:type->specifier (zen:get-nillable-type-type type) #t)))
    (%%string->symbol (%%string-append (%%symbol->string type-specifier) "+"))))


(zen:define-method (zen:emit-test (zen:Nillable-Type type) value source-declaration walker resume environment)
  `(or (%%not ,value)
       ,(zen:emit-test (zen:get-nillable-type-type type) value source-declaration walker resume environment)))


(zen:define-method (zen:emit-cast (zen:Nillable-Type type) value source-declaration walker resume environment)
  `(if ,(zen:emit-test type value source-declaration walker resume environment)
       ,value
     (zen:type-error ,value ',(zen:emit-specifier type))))


;;;
;;;; Object
;;;


(define zen:object-declaration? #f)

(set! zen:object-declaration?
      (lambda (type)
        #f))


;;;
;;;; Any
;;;


(zen:define-class zen:Any-Class zen:Class (metaclass: zen:Class)
  ())


(zen:define-method (zen:of-subtype? (zen:Any-Class type) subtype)
  #t)


(zen:define-method (zen:emit-specifier (zen:Any-Class type))
  'any)


(zen:define-method (zen:emit-cast (zen:Any-Class type) value source-declaration walker resume environment)
  value)


(zen:define-class zen:Any zen:Type (metaclass: zen:Any-Class)
  ())


;;;
;;;; Cast
;;;

;; Really not sure what is the right place to put the zen:resolve-type
;; As a first try lets put them here at every type check


;; this should not have to be necessary for the code type but it seems it can also be false
(define (zen:resolve-type-safe type)
  (and type
       (zen:resolve-type type)))


(define (zen:emit-type-cast code type source source-declaration walker resume environment)
  (let ((code-type (zen:resolve-type-safe (zen:get-code-type code)))
        (expected-type (zen:resolve-type-safe type)))
    (cond ((or (%%not expected-type)
               (%%eq? expected-type zen:Void)
               (%%subtype? code-type expected-type))
           #; ;; too much at the moment
           (%%when (and (or (zen:reporting?) (zen:warnings?)) (zen:get-warn? 'optimizations))
             (zen:warning "Warning: In {a}{a}: Redundant cast"
                           (zen:get-declaration-locator source-declaration)
                           (zen:present-expression-location source #f)))
           (zen:sourcified-form code))
          ;; fixnum to flonum
          ((and (%%subtype? expected-type zen:Flonum)
                (%%subtype? code-type zen:Fixnum))
           (let ((code-emit (zen:sourcified-form code)))
             (if (%%fixnum? (zen:source-code code-emit))
                 (zen:sourcify-if (%%fixnum->flonum (zen:source-code code-emit)) (zen:get-code-source code))
               `(%%fixnum->flonum ,(zen:sourcified-form code)))))
          ;; ratnum to flonum
          ((and (%%subtype? expected-type zen:Flonum)
                (%%subtype? code-type zen:Ratnum))
           (let ((code-emit (zen:sourcified-form code)))
             (if (%%ratnum? (zen:source-code code-emit))
                 (zen:sourcify-if (%%ratnum->flonum (zen:source-code code-emit)) (zen:get-code-source code))
               `(%%ratnum->flonum ,(zen:sourcified-form code)))))
          ;; flonum to flovec
          ((and (%%subtype? expected-type zen:Flovec)
                (%%subtype? code-type zen:Flonum))
           (zen:sourcified-form code))
          (else
           #; ;; incompatible cast test
           (%%unless (%%eq? code-type zen:Any)
             (pp (zen:format "Casting {a} to incompatible type {a}" code-type expected-type) (current-output-port)))
           (if (zen:get-check? 'types)
               (let ((value (zen:generate-symbol "val")))
                 (zen:simplify-let
                   `(let ((,value ,(zen:simplify-let `(let () ,(zen:simplify-begin (zen:sourcified-form code))))))
                      ,(zen:emit-cast type value source-declaration walker resume environment))))
             (begin
               (%%when (%%subtype? expected-type zen:Flonum)
                 (zen:unsafe-warning "Unsafe: In {a}{a}: Unchecked untyped cast <fl> {a}"
                                      (zen:get-declaration-locator source-declaration)
                                      (zen:present-expression-location source #f)
                                      (zen:desourcify-all (zen:get-code-source code))))
               (zen:sourcified-form code)))))))


(define (zen:emit-implicit-cast code type)
  (let ((code-type (zen:resolve-type-safe (zen:get-code-type code)))
        (type (zen:resolve-type-safe type)))
    (cond ((or (%%not type)
               (%%eq? type zen:Void)
               (%%subtype? code-type type))
           (zen:sourcified-form code))
          ;; fixnum to flonum
          ((and (%%subtype? type zen:Flonum)
                (%%subtype? code-type zen:Fixnum))
           (let ((code-emit (zen:sourcified-form code)))
             (if (%%fixnum? (zen:source-code code-emit))
                 (zen:sourcify (%%fixnum->flonum (zen:source-code code-emit)) code-emit)
               `(%%fixnum->flonum ,(zen:sourcified-form code)))))
          ;; ratnum to flonum
          ((and (%%subtype? type zen:Flonum)
                (%%subtype? code-type zen:Ratnum))
           (let ((code-emit (zen:sourcified-form code)))
             (if (%%ratnum? (zen:source-code code-emit))
                 (zen:sourcify (%%ratnum->flonum (zen:source-code code-emit)) code-emit)
               `(%%ratnum->flonum ,(zen:sourcified-form code)))))
          (else
           (zen:sourcified-form code)))))


(define (zen:emit-parameter-cast code type source-declaration walker resume environment)
  (if (zen:get-check? 'types)
      (let ((type (zen:resolve-type-safe type)))
        (if (or (%%not type) (%%eq? type zen:Any) (%%object-class? type) (zen:object-declaration? type))
            #f
          (let ((parameter (zen:sourcified-form code)))
            (let ((cast (zen:emit-cast type parameter source-declaration walker resume environment)))
              (if cast
                  (%%list parameter cast)
                #f)))))
    #f))


(define (zen:emit-return-cast code type source source-declaration walker resume environment)
  (zen:emit-type-cast code type source source-declaration walker resume environment))


;;;
;;;; Specifier
;;;


;; <any>
;; <Point+>                            ;; Nillable
;; <category<Cell>>                    ;; Category
;; <fx^fx^opt<fx>^key<k:fx>^fx*:pair>  ;; Function with positional, optional, named and rest parameters
;; <vector<fx>>                        ;; Template
;; <values<fx/fl>>                     ;; Values
;; <Object>
;; <Point>
;; <pair>
;; <fx>
;; <void>


;; interning the type is a quick solution to
;; types being compared using eq? and also saves
;; some memory because of the size of classes

(define zen:fixed-types
  (%%make-table test: equal?))

(define (zen:new-fixed-type class size)
  (let ((key (%%cons class size)))
    (or (%%table-ref zen:fixed-types key #f)
        (let ((metaclass (%%get-object-class class))
              (name (%%get-category-identifier class)))
          (let ((type-class (zen:new-type-class metaclass name class size)))
            (%%table-set! zen:fixed-types key type-class)
            type-class)))))


(define zen:fixed-specifiers
  (%%make-table test: eq?))

(define zen:fixed-classes
  (%%make-table test: eq?))

(define zen:fixed-constructors
  (%%make-table test: eq?))

(define zen:fixed-makers
  (%%make-table test: eq?))


(define (zen:fixed-constructors-get)
  zen:fixed-constructors)

(define (zen:fixed-makers-get)
  zen:fixed-makers)


(define (zen:setup-fixed-metadata specifier class fixed-class fixed-metaclass constructor maker)
  (%%table-set! zen:fixed-specifiers specifier fixed-class)
  (%%table-set! zen:fixed-classes class fixed-metaclass)
  (%%table-set! zen:fixed-constructors constructor fixed-class)
  (%%table-set! zen:fixed-makers maker fixed-class))


(define (zen:specifier->fixed-class specifier)
  (%%table-ref zen:fixed-specifiers specifier #f))

(define (zen:class->fixed-metaclass class)
  (%%table-ref zen:fixed-classes class #f))

(define (zen:constructor->fixed-class locator)
  (%%table-ref zen:fixed-constructors locator #f))

(define (zen:maker->fixed-class locator)
  (%%table-ref zen:fixed-makers locator #f))


(zen:setup-fixed-metadata 'vector    zen:Vector    zen:FixedVector    zen:FixedVector-Class    'scheme.runtime:vector 'scheme.runtime:make-vector)
(zen:setup-fixed-metadata 's8vector  zen:S8Vector  zen:FixedS8Vector  zen:FixedS8Vector-Class  'gambit:s8vector       'gambit:make-s8vector)
(zen:setup-fixed-metadata 'u8vector  zen:U8Vector  zen:FixedU8Vector  zen:FixedU8Vector-Class  'gambit:u8vector       'gambit:make-u8vector)
(zen:setup-fixed-metadata 's16vector zen:S16Vector zen:FixedS16Vector zen:FixedS16Vector-Class 'gambit:s16vector      'gambit:make-s16vector)
(zen:setup-fixed-metadata 'u16vector zen:U16Vector zen:FixedU16Vector zen:FixedU16Vector-Class 'gambit:u16vector      'gambit:make-u16vector)
(zen:setup-fixed-metadata 's32vector zen:S32Vector zen:FixedS32Vector zen:FixedS32Vector-Class 'gambit:s32vector      'gambit:make-s32vector)
(zen:setup-fixed-metadata 'u32vector zen:U32Vector zen:FixedU32Vector zen:FixedU32Vector-Class 'gambit:u32vector      'gambit:make-u32vector)
(zen:setup-fixed-metadata 's64vector zen:S64Vector zen:FixedS64Vector zen:FixedS64Vector-Class 'gambit:s64vector      'gambit:make-s64vector)
(zen:setup-fixed-metadata 'u64vector zen:U64Vector zen:FixedU64Vector zen:FixedU64Vector-Class 'gambit:u64vector      'gambit:make-u64vector)
(zen:setup-fixed-metadata 'f32vector zen:F32Vector zen:FixedF32Vector zen:FixedF32Vector-Class 'gambit:f32vector      'gambit:make-f32vector)
(zen:setup-fixed-metadata 'f64vector zen:F64Vector zen:FixedF64Vector zen:FixedF64Vector-Class 'gambit:f64vector      'gambit:make-f64vector)


(define (zen:parse-specifier lst proc)
  (if (and (%%pair? lst) (zen:specifier? (zen:source-code (%%car lst))))
      (proc (zen:source-code (%%car lst)) (%%car lst) (%%cdr lst))
    (proc #f #f lst)))


(define (zen:walk-specifier walker resume declaration environment specifier)
  (let ((string (%%symbol->string specifier)))
    (let ((input (open-input-string string))
          (at 0))
      (define (ill-formed message)
        (let ((error-message (zen:format "Ill-formed specifier {s} : at {a} : {a}" specifier (%%substring string 0 at) message)))
          (if (%%not walker)
              (zen:error "{a}" error-message)
            (zen:walk-error walker resume declaration #f error-message))))
      
      (define (peekc)
        (peek-char input))
      
      (define (readc)
        (let ((c (read-char input)))
          (set! at (%%fx+ at 1))
          c))
      
      (define (consume c)
        (if (%%not (%%eqv? (readc) c))
            (ill-formed (zen:format "{s} expected" c))))
      
      (define (lookup-type name)
        (let ((type (or (zen:lookup-primitive-type name)
                        (zen:lookup-reference walker resume declaration environment name)
                        (ill-formed (zen:format "{s} not found" name)))))
          (%%assertion (zen:specifiable? type) (zen:error "Invalid specifier: {s}" name)
            type)))

      (define (parse-until separator terminator)
        (let ((queue (zen:new-queue)))
          (let iter ()
               (if (%%eqv? (peekc) terminator)
                   (begin
                     (readc)
                     (zen:queue-list queue))
                 (begin
                   (zen:enqueue queue (parse #t))
                   (let ((next (peekc)))
                     (cond ((%%eqv? next separator)
                            (readc)
                            (iter))
                           ((%%eqv? next terminator)
                            (iter))
                           (else
                            (ill-formed (zen:format "{s} terminator expected" terminator))))))))))
      
      (define (parse-name)
        (let ((output (open-output-string)))
          (let iter ()
               (let ((c (peekc)))
                 (if (or (%%eof-object? c)
                         (%%eqv? c #\<)
                         (%%eqv? c #\>)
                         (%%eqv? c #\^)
                         (%%eqv? c #\*)
                         (%%eqv? c #\#)
                         (%%eqv? c #\:)
                         (%%eqv? c #\/)
                         (%%eqv? c #\+))
                     (%%string->symbol (get-output-string output))
                   (begin
                     (readc)
                     (write-char c output)
                     (iter)))))))
      
      (define (parse-string)
        (let ((output (open-output-string)))
          (let iter ()
               (let ((c (peekc)))
                 (if (or (%%eqv? c #\+)
                         (%%eqv? c #\>))
                     (get-output-string output)
                   (begin
                     (readc)
                     (write-char c output)
                     (iter)))))))
      
      
      (define (parse-atomic)
        (let ((name (parse-name)))
          (let ((next (peekc)))
            (case next
              ((#\+)
               (readc)
               (zen:new-nillable-type (lookup-type name)))
              ((#\*)
               (readc)
               (zen:new-rest-type (lookup-type name)))
              ((#\#)
               (readc)
               (let ((class (zen:specifier->fixed-class name)))
                 (if class
                     (let ((str (parse-string)))
                       (let ((size (%%string->number str)))
                         (if (%%fixnum? size)
                             (if (%%eqv? (peekc) #\+)
                                 (begin
                                   (readc)
                                   (zen:new-nillable-type (zen:new-fixed-type class size)))
                               (zen:new-fixed-type class size))
                           (ill-formed (zen:format "size expected: {a}" str)))))
                   (ill-formed (zen:format "Invalid fixed type: {s}" name)))))
              ((#\<)
               (readc)
               (cond ((%%eq? name 'opt)
                      (let ((type (parse #t)))
                        (consume #\>)
                        (zen:new-opt-type type)))
                     ((%%eq? name 'key)
                      (let ((key (%%string->keyword (%%symbol->string (parse-name)))))
                        (consume #\:)
                        (let ((type (parse #t)))
                          (consume #\>)
                          (zen:new-key-type key type))))
                     ((%%eq? name 'category)
                      (zen:new-category-type (parse #t)))
                     ((%%eq? name 'values)
                      (zen:new-values-type (parse-until #\/ #\>)))
                     (else
                      (zen:new-template-type (lookup-type name) (parse-until #\/ #\>)))))
              (else
               (lookup-type name))))))
      
      (define (new-function-type parameters result)
        (define (split-parameters types proc)
          (if (%%null? types)
              (proc '() '() '() #f)
            (let ((last (zen:last types)))
              (if (%%class-is? last zen:Rest-Type)
                  (proc (zen:butlast types) '() '() last)
                (proc types '() '() #f)))))
        
        (split-parameters parameters
          (lambda (positional optional named rest)
            (zen:new-function-type positional optional named rest result))))
      
      (define (parse atomic?)
        (if (%%eqv? (peekc) #\<)
            (begin
              (readc)
              (let ((type (parse #f)))
                (consume #\>)
                type))
          (if (%%eqv? (peekc) #\:)
              (begin
                (readc)
                (new-function-type '() (parse #t)))
            (let ((type (parse-atomic)))
              (let ((next (peekc)))
                (case next
                  ((#\:)
                   (if atomic?
                       type
                     (begin
                       (readc)
                       (new-function-type (%%list type) (parse #t)))))
                  ((#\^)
                   (if atomic?
                       type
                     (begin
                       (readc)
                       (let ((parameters (%%cons type (parse-until #\^ #\:))))
                         (new-function-type parameters (parse #t))))))
                  (else
                   type)))))))
      
      (parse #f))))


(define (zen:specifier->type walker resume declaration environment specifier)
  (if specifier
      (zen:walk-specifier walker resume declaration environment specifier)
    #f))


(define (zen:type->specifier type #!optional (internal? #f))
  (define (name->specifier name)
    (if (and internal? (%%symbol? name))
        name
      (zen:name->specifier name)))
  
  (if (%%is? type zen:Declaration)
      (name->specifier (zen:get-lexical-binding-name type))
    (let ((symbol (zen:emit-specifier type)))
      (if (zen:specifier? symbol)
          symbol
        (name->specifier symbol)))))


;;;
;;;; Primitive Types
;;;


(define zen:primitive-types
  (%%make-table test: eq?))


(define (zen:add-primitive-type name type)
  (%%table-set! zen:primitive-types name type))


(define (zen:lookup-primitive-type name)
  (%%table-ref zen:primitive-types name #f))


(zen:add-primitive-type 'any          zen:Any)
(zen:add-primitive-type 'object       zen:Object)
(zen:add-primitive-type 'bool         zen:Bool)
(zen:add-primitive-type 'boolean      zen:Boolean)
(zen:add-primitive-type 'char         zen:Char)
(zen:add-primitive-type 'number       zen:Number)
(zen:add-primitive-type 'complex      zen:Complex)
(zen:add-primitive-type 'real         zen:Real)
(zen:add-primitive-type 'rational     zen:Rational)
(zen:add-primitive-type 'integer      zen:Integer)
(zen:add-primitive-type 'int          zen:Integer)
(zen:add-primitive-type 'fx           zen:Fixnum)
(zen:add-primitive-type 'rt           zen:Ratnum)
(zen:add-primitive-type 'fl           zen:Flonum)
(zen:add-primitive-type 'fv           zen:Flovec)
(zen:add-primitive-type 's64          zen:S64)
(zen:add-primitive-type 'list         zen:List)
(zen:add-primitive-type 'null         zen:Null)
(zen:add-primitive-type 'pair         zen:Pair)
(zen:add-primitive-type 'port         zen:Port)
(zen:add-primitive-type 'continuation zen:Continuation)
(zen:add-primitive-type 'procedure    zen:Procedure)
(zen:add-primitive-type 'string       zen:String)
(zen:add-primitive-type 'symbol       zen:Symbol)
(zen:add-primitive-type 'keyword      zen:Keyword)
(zen:add-primitive-type 'vector       zen:Vector)
(zen:add-primitive-type 's8vector     zen:S8Vector)
(zen:add-primitive-type 'u8vector     zen:U8Vector)
(zen:add-primitive-type 's16vector    zen:S16Vector)
(zen:add-primitive-type 'u16vector    zen:U16Vector)
(zen:add-primitive-type 's32vector    zen:S32Vector)
(zen:add-primitive-type 'u32vector    zen:U32Vector)
(zen:add-primitive-type 's64vector    zen:S64Vector)
(zen:add-primitive-type 'u64vector    zen:U64Vector)
(zen:add-primitive-type 'f32vector    zen:F32Vector)
(zen:add-primitive-type 'f64vector    zen:F64Vector)
(zen:add-primitive-type 'table        zen:Table)
(zen:add-primitive-type 'thread       zen:Thread)
(zen:add-primitive-type 'promise      zen:Promise)
(zen:add-primitive-type 'foreign      zen:Foreign)
(zen:add-primitive-type 'values       zen:Values)
(zen:add-primitive-type 'box          zen:Box)
(zen:add-primitive-type 'eof          zen:EOF)
(zen:add-primitive-type 'unspecified  zen:Unspecified)
(zen:add-primitive-type 'void         zen:Void)


(define zen:primitive-declarations
  (%%make-table test: eq?))


(define (zen:add-primitive-declaration decl name)
  (%%table-set! zen:primitive-declarations decl name))


(zen:add-primitive-declaration zen:Object       'Object)
(zen:add-primitive-declaration zen:Boolean      'Boolean)
(zen:add-primitive-declaration zen:Char         'Char)
(zen:add-primitive-declaration zen:Number       'Number)
(zen:add-primitive-declaration zen:Complex      'Complex)
(zen:add-primitive-declaration zen:Real         'Real)
(zen:add-primitive-declaration zen:Rational     'Rational)
(zen:add-primitive-declaration zen:Integer      'Integer)
(zen:add-primitive-declaration zen:Fixnum       'Fixnum)
(zen:add-primitive-declaration zen:Ratnum       'Ratnum)
(zen:add-primitive-declaration zen:Flonum       'Flonum)
(zen:add-primitive-declaration zen:List         'List)
(zen:add-primitive-declaration zen:Null         'Null)
(zen:add-primitive-declaration zen:Pair         'Pair)
(zen:add-primitive-declaration zen:Port         'Port)
(zen:add-primitive-declaration zen:Continuation 'Continuation)
(zen:add-primitive-declaration zen:Procedure    'Procedure)
(zen:add-primitive-declaration zen:String       'String)
(zen:add-primitive-declaration zen:Symbol       'Symbol)
(zen:add-primitive-declaration zen:Keyword      'Keyword)
(zen:add-primitive-declaration zen:Vector       'Vector)
(zen:add-primitive-declaration zen:S8Vector     'S8Vector)
(zen:add-primitive-declaration zen:U8Vector     'U8Vector)
(zen:add-primitive-declaration zen:S16Vector    'S16Vector)
(zen:add-primitive-declaration zen:U16Vector    'U16Vector)
(zen:add-primitive-declaration zen:S32Vector    'S32Vector)
(zen:add-primitive-declaration zen:U32Vector    'U32Vector)
(zen:add-primitive-declaration zen:S64Vector    'S64Vector)
(zen:add-primitive-declaration zen:U64Vector    'U64Vector)
(zen:add-primitive-declaration zen:F32Vector    'F32Vector)
(zen:add-primitive-declaration zen:F64Vector    'F64Vector)
(zen:add-primitive-declaration zen:Table        'Table)
(zen:add-primitive-declaration zen:Thread       'Thread)
(zen:add-primitive-declaration zen:Promise      'Promise)
(zen:add-primitive-declaration zen:Foreign      'Foreign)
(zen:add-primitive-declaration zen:Values       'Values)
(zen:add-primitive-declaration zen:EOF          'EOF)
(zen:add-primitive-declaration zen:Unspecified  'Unspecified)


;; quicky until we can somehow unify primitive types and declarations
(define (zen:patch-type-until-unification type)
  (let ((name (%%table-ref zen:primitive-declarations type #f)))
    (if name
        (let ((module-name 'zen.language.runtime))
          (let ((module-declaration (zen:get-catalog-entry module-name)))
            (if module-declaration
                (zen:lookup-declaration module-declaration name zen:public-access module-declaration)
              type)))
      type)))


;;;
;;;; Macro
;;;


(zen:define-class zen:Macro-Declaration zen:Declaration (constructor: zen:allocate-macro-declaration)
  ((signature getter: generate setter: generate)
   (body      getter: generate setter: generate)))


(define (zen:new-macro-declaration name type access compatibility modifiers attributes parent signature)
  (let ((new-declaration (zen:allocate-macro-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f signature #f)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(zen:define-method (zen:walk-binding-expandable? (zen:Macro-Declaration declaration))
  #t)


(zen:define-method (zen:walk-binding-expand-form (zen:Macro-Declaration binding) walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((locator (zen:get-declaration-locator binding)))
      (if (%%eq? (zen:get-declaration-toplevel binding) (zen:get-declaration-toplevel declaration))
          (zen:walk-error walker resume declaration form-src "Macros cannot be used from within the same file: {s}" locator)
        (let ((parent-declaration (zen:get-declaration-parent binding)))
          (zen:load-unit (zen:get-declaration-locator (zen:get-declaration-toplevel parent-declaration)))
          (let ((expander (zen:need-macro locator)))
            (%%apply expander (%%cdr form))))))))


(zen:define-method (zen:emit-declaration (zen:Macro-Declaration declaration) walker resume environment)
  (let ((locator (zen:get-declaration-locator declaration))
        (signature (zen:get-macro-declaration-signature declaration))
        (body (zen:get-macro-declaration-body declaration)))
    (zen:with-annotated-frame (zen:annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (zen:sourcify-deep-if
            `(zen:define-macro ,(%%cons locator (zen:emit-signature signature declaration walker resume augmented-environment))
               ,@(zen:sourcified-form (zen:emit-expression body declaration walker resume augmented-environment)))
            (zen:get-declaration-source declaration)))))))


(define zen:macro-modifiers
  '(((private protected package public) . private)
    ((deprecated undocumented uptodate) . uptodate)))


(define (zen:parse-macro walker resume declaration rest)
  (receive (access compatibility modifiers rest) (zen:parse-modifiers walker resume declaration zen:macro-modifiers rest)
    (let* ((signature (zen:source-code (%%car rest)))
           (body (%%cdr rest))
           (name (%%desourcify (%%car signature)))
           (type zen:Any)
           (parameters (%%cdr signature)))
      (values name type access compatibility modifiers parameters body))))


(define (zen:walk-macro-declaration walker resume declaration environment form-src)
  (receive (name type access compatibility modifiers parameters body) (zen:parse-macro walker resume declaration (%%cdr (zen:source-code form-src)))
    (let ((signature (zen:walk-parameters walker resume declaration environment parameters #t #f)))
      (let ((new-declaration (or (zen:find-declaration-child declaration name)
                                 (zen:new-macro-declaration name type access compatibility modifiers '() declaration signature))))
        (zen:set-declaration-source new-declaration form-src)
        (let ((effective-declaration (zen:add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (zen:walk-macro walker resume declaration environment form-src)
  (receive (name type access compatibility modifiers parameters body) (zen:parse-macro walker resume declaration (%%cdr (zen:source-code form-src)))
    (let ((new-declaration (zen:require-declaration declaration name)))
      (receive (signature augmented-environment) (zen:walk-parameters walker resume declaration environment parameters #t #t)
        (zen:set-macro-declaration-signature new-declaration signature)
        (zen:set-macro-declaration-body new-declaration
          (zen:walk-body walker resume new-declaration augmented-environment body))
        (zen:set-declaration-source new-declaration form-src)
        new-declaration))))


(define (zen:expand-macros walker resume declaration environment form-src)
  (if (%%not (%%pair? (zen:source-code form-src)))
      form-src
    (let ((procedure-expr (zen:source-code (%%car (zen:source-code form-src)))))
      (let ((binding (zen:lookup-procedure-binding walker resume declaration environment procedure-expr)))
        (if (and binding (zen:walk-binding-expandable? binding))
            (let ((expansion (zen:walk-binding-expand-form binding walker resume declaration environment form-src)))
              (zen:expand-macros walker resume declaration environment expansion))
          form-src)))))


(define (zen:lookup-procedure-binding walker resume declaration environment procedure-expr)
  (let ((binding
          (cond ((zen:identifier? procedure-expr)
                 (zen:lookup-symbol walker resume declaration environment procedure-expr))
                ((or (%%class-is? procedure-expr zen:Special-Form)
                     (%%class-is? procedure-expr zen:Declaration))
                 procedure-expr)
                ((%%class-is? procedure-expr zen:Binding-Reference)
                 (let ((ref (zen:get-binding-reference-binding procedure-expr)))
                   (and (or (%%class-is? ref zen:Special-Form)
                            (%%class-is? ref zen:Declaration))
                        ref)))
                (else #f))))
    (if (%%is? binding zen:Export-Declaration)
        (zen:require-declaration (zen:get-declaration-toplevel binding) (zen:get-lexical-binding-name binding))
      binding)))


(zen:define-method (zen:tree-fold (zen:Macro-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (zen:tree-fold-list (zen:get-signature-expressions (zen:get-macro-declaration-signature declaration)) down up here seed environment)
  (zen:tree-fold (zen:get-macro-declaration-body declaration) down up here seed environment))


(zen:define-method (zen:outline-extract (zen:Macro-Declaration declaration) meta)
  `(macro ,@(zen:outline-generate-modifiers declaration)
          (,(zen:get-lexical-binding-name declaration) ,@(zen:outline-generate-signature (zen:get-macro-declaration-signature declaration)))))


;;;
;;;; Local Macro
;;;


(zen:define-class zen:Local-Macro-Declaration zen:Declaration (constructor: zen:allocate-local-macro-declaration)
  ((signature getter: generate setter: generate)
   (body      getter: generate setter: generate)))


(define (zen:new-local-macro-declaration name type access compatibility modifiers attributes parent signature)
  (let ((new-declaration (zen:allocate-macro-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f signature #f)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(define (zen:need-local-macro module-declaration name)
  (or (%%table-ref (zen:get-module-declaration-local-macros module-declaration) name #f)
      (zen:error "Unable to find macro: {s}" name)))


(zen:define-method (zen:walk-binding-expandable? (zen:Local-Macro-Declaration declaration))
  #t)


(zen:define-method (zen:walk-binding-expand-form (zen:Local-Macro-Declaration binding) walker resume declaration environment form-src)
  (let ((form    (%%desourcify form-src))
        (locator (zen:get-declaration-locator binding))
        (parent  (zen:get-declaration-parent binding)))
    (let ((expander (zen:need-local-macro parent locator)))
      (%%apply expander (%%cdr form)))))


(zen:define-method (zen:emit-declaration (zen:Local-Macro-Declaration declaration) walker resume environment)
  `(begin))


(zen:define-method (zen:tree-fold (zen:Local-Macro-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (zen:tree-fold-list (zen:get-signature-expressions (zen:get-local-macro-declaration-signature declaration)) down up here seed environment)
  (zen:tree-fold (zen:get-local-macro-declaration-body declaration) down up here seed environment))


(define (zen:register-local-macro module-declaration name macro)
  (%%table-set! (zen:get-module-declaration-local-macros module-declaration) name macro))


(define (zen:walk-local-macro-declaration walker resume declaration environment form-src)
  (receive (name type access compatibility modifiers parameters body) (zen:parse-macro walker resume declaration (%%cdr (zen:source-code form-src)))
    (receive (signature augmented-environment) (zen:walk-parameters walker resume declaration environment parameters #t #t)
      (let* ((new-declaration (or (zen:find-declaration-child declaration name)
                                  (zen:new-local-macro-declaration name type access compatibility modifiers '() declaration signature)))
             (walked-body (zen:walk-body walker resume new-declaration augmented-environment body)))
        (zen:set-local-macro-declaration-signature new-declaration signature)
        (zen:set-local-macro-declaration-body new-declaration walked-body)
        (zen:set-declaration-source new-declaration form-src)
        (zen:register-local-macro declaration (zen:get-declaration-locator new-declaration)
          (zen:with-annotated-frame (zen:annotate-signature signature)
            (lambda (frame)
              (let ((augmented-environment (%%cons frame environment)))
                (eval `(lambda ,(zen:emit-signature signature declaration walker resume augmented-environment)
                         ,@(zen:sourcified-form (zen:emit-expression walked-body declaration walker resume augmented-environment))))))))
        (let ((effective-declaration (zen:add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (zen:walk-local-macro walker resume declaration environment form-src)
  (receive (name type access compatibility modifiers parameters body) (zen:parse-macro walker resume declaration (%%cdr (zen:source-code form-src)))
    (zen:require-declaration declaration name)))


;;;
;;;; Syntax
;;;


(zen:define-class zen:Syntax-Declaration zen:Declaration (constructor: zen:allocate-syntax-declaration)
  ((signature getter: generate setter: generate)
   (body      getter: generate setter: generate)))


(define (zen:new-syntax-declaration name type access compatibility modifiers attributes parent signature syntax-form)
  (let ((new-declaration (zen:allocate-syntax-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f signature #f)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(zen:define-method (zen:emit-declaration (zen:Syntax-Declaration declaration) walker resume environment)
  (let ((locator (zen:get-declaration-locator declaration))
        (signature (zen:get-syntax-declaration-signature declaration))
        (body (zen:get-syntax-declaration-body declaration)))
    (zen:with-annotated-frame (zen:annotate-signature signature)
     (lambda (frame)
       (let ((augmented-environment (%%cons frame environment))
             (current-unit-name
               (zen:get-declaration-locator
                 (zen:get-declaration-toplevel declaration))))
         (zen:sourcify-deep-if
           `(define ,locator
              (let* ((env
                       (%%list
                         (zen:new-walk-frame
                           (zen:get-dialect-bindings (zen:get-dialect 'scheme)))
                         (zen:new-walk-frame
                           (zen:get-dialect-bindings (zen:get-dialect 'foundation)))))
                     (env
                       (cond ((zen:get-dialect 'zen)
                              => (lambda (x)
                                   (cons (zen:new-walk-frame (zen:get-dialect-bindings x)) env)))
                             (else env)))
                     (env
                       (cond ((zen:outline-module ',current-unit-name)
                              => (lambda (x) (cons x env)))
                             (else env)))
                     (tmp
                       (zen:new-define-syntax-form
                         ',locator
                         ,@(zen:sourcified-form (zen:emit-expression body declaration walker resume augmented-environment))
                         env)))
                (zen:register-macro ',locator tmp)
                tmp))
           (zen:get-declaration-source declaration)))))))


(zen:define-method (zen:walk-binding-expandable? (zen:Syntax-Declaration declaration))
  #t)


(zen:define-method (zen:tree-fold (zen:Syntax-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (zen:tree-fold-list (zen:get-signature-expressions (zen:get-syntax-declaration-signature declaration)) down up here seed environment)
  (zen:tree-fold (zen:get-syntax-declaration-body declaration) down up here seed environment))


(zen:define-method (zen:outline-extract (zen:Syntax-Declaration declaration) meta)
  `(syntax ,@(zen:outline-generate-access-list declaration) ,(zen:get-lexical-binding-name declaration)))


(define zen:current-walker
  (%%make-parameter #f))

(define zen:current-resume
  (%%make-parameter #f))

(define zen:current-declaration
  (%%make-parameter #f))

(define zen:current-declaration-name
  (%%make-parameter #f))

(define zen:current-walk-error
  (%%make-parameter #f))


(define (zen:with-walker-context walker resume declaration form-src thunk)
  (parameterize ((zen:current-walker walker)
                 (zen:current-resume resume)
                 (zen:current-declaration declaration)
                 (zen:current-walk-error (lambda (fmt-string rest)
                                            (apply zen:walk-error walker resume declaration form-src fmt-string rest))))
    (thunk)))


(define (zen:call-walk-error fmt-string . rest)
  (let ((proc (zen:current-walk-error)))
    (if proc
        (proc fmt-string rest)
      (zen:error "Invalid walk context"))))


(zen:define-method (zen:walk-binding-expand-form (zen:Syntax-Declaration binding) walker resume declaration environment form-src)
  (let ((locator (zen:get-declaration-locator binding)))
    (if (%%eq? (zen:get-declaration-toplevel binding) (zen:get-declaration-toplevel declaration))
        (zen:walk-warning walker declaration form-src "Syntax shouldn't be used from within the same file: {s}" locator))
    (let ((parent-declaration (zen:get-declaration-parent binding)))
      (zen:load-unit (zen:get-declaration-locator (zen:get-declaration-toplevel parent-declaration)))
      (let* ((define-syntax-form (zen:need-macro locator))
             (expander (zen:get-syntax-form-expander define-syntax-form))
             (macro-environment (zen:get-define-syntax-form-environment define-syntax-form)))
        (zen:with-walker-context walker resume declaration form-src
          (lambda ()
            (expander form-src environment macro-environment)))))))


(define zen:syntax-modifiers
  '(((private protected package public) . private)
    ((deprecated undocumented uptodate) . uptodate)))


(define (zen:walk-syntax-declaration walker resume declaration environment form-src)
  (receive (access compatibility modifiers rest) (zen:parse-modifiers walker resume declaration zen:syntax-modifiers (%%cdr (zen:source-code form-src)))
    (let ((name (zen:source-code (%%car rest)))
          (name-src (%%car rest)))
      (let ((new-declaration (or (zen:find-declaration-child declaration name)
                                 (zen:new-syntax-declaration name zen:Any access compatibility modifiers '() declaration #f #f))))
        (zen:set-declaration-source new-declaration form-src)
        (zen:set-declaration-name-source new-declaration name-src)
        (let ((effective-declaration (zen:add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (zen:walk-syntax walker resume declaration environment form-src)
  (receive (access compatibility modifiers rest) (zen:parse-modifiers walker resume declaration zen:syntax-modifiers (%%cdr (zen:source-code form-src)))
    (let* ((name (zen:source-code (%%car rest)))
           (name-src (%%car rest))
           (body (%%cdr rest))
           (new-declaration (zen:require-declaration declaration name)))
      (receive (signature augmented-environment) (zen:walk-parameters walker resume declaration environment '() #t #t)
        (zen:set-syntax-declaration-signature new-declaration signature)
        (zen:set-syntax-declaration-body new-declaration
          (zen:walk-body walker resume new-declaration augmented-environment body))
        (zen:set-declaration-source new-declaration form-src)
        (zen:set-declaration-name-source new-declaration name-src)
        new-declaration))))


;;;
;;;; Define-Syntax
;;;


(zen:define-class zen:Define-Syntax-Declaration zen:Syntax-Declaration (constructor: zen:allocate-define-syntax-declaration)
  ())


(define (zen:new-define-syntax-declaration name type access compatibility modifiers attributes parent signature syntax-form)
  (let ((new-declaration (zen:allocate-define-syntax-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f signature #f)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(zen:define-method (zen:emit-declaration (zen:Define-Syntax-Declaration declaration) walker resume environment)
  (let ((locator (zen:get-declaration-locator declaration))
        (signature (zen:get-syntax-declaration-signature declaration))
        (body (zen:get-syntax-declaration-body declaration)))
    (zen:with-annotated-frame (zen:annotate-signature signature)
     (lambda (frame)
       (let ((augmented-environment (%%cons frame environment))
             (current-unit-name
               (zen:get-declaration-locator
                 (zen:get-declaration-toplevel declaration))))
         (zen:sourcify-deep-if
           `(define ,locator
              (let* ((env
                       (%%list
                         (zen:new-walk-frame
                           (zen:get-dialect-bindings (zen:get-dialect 'scheme)))
                         (zen:new-walk-frame
                           (zen:get-dialect-bindings (zen:get-dialect 'foundation)))))
                     (env
                       (cond ((zen:get-dialect 'zen)
                              => (lambda (x)
                                   (cons (zen:new-walk-frame (zen:get-dialect-bindings x)) env)))
                             (else env)))
                     (env
                       (cond ((zen:outline-module ',current-unit-name)
                              => (lambda (x) (cons x env)))
                             (else env)))
                     (tmp
                       (zen:new-define-syntax-form
                         ',locator
                         ,@(zen:sourcified-form (zen:emit-expression body declaration walker resume augmented-environment))
                         env)))
                (zen:register-macro ',locator tmp)
                tmp))
           (zen:get-declaration-source declaration)))))))


(zen:define-method (zen:walk-binding-expand-form (zen:Define-Syntax-Declaration binding) walker resume declaration environment form-src)
  (let ((locator (zen:get-declaration-locator binding)))
    (if (%%eq? (zen:get-declaration-toplevel binding) (zen:get-declaration-toplevel declaration))
        (zen:walk-warning walker declaration form-src "Syntax shouldn't be used from within the same file: {s}" locator))
    (let ((parent-declaration (zen:get-declaration-parent binding)))
      (zen:load-unit (zen:get-declaration-locator (zen:get-declaration-toplevel parent-declaration)))
      (let* ((define-syntax-form (zen:need-macro locator))
             (expander (zen:get-syntax-form-expander define-syntax-form))
             (macro-environment (zen:get-define-syntax-form-environment define-syntax-form)))
        (zen:with-walker-context walker resume declaration form-src
          (lambda ()
            (expander form-src environment macro-environment)))))))


(define (zen:walk-define-syntax-declaration walker resume declaration environment form-src)
  (receive (access compatibility modifiers rest) (zen:parse-modifiers walker resume declaration zen:syntax-modifiers (%%cdr (zen:source-code form-src)))
    (let ((name (zen:source-code (%%car rest)))
          (name-src (%%car rest)))
      (let ((new-declaration (or (zen:find-declaration-child declaration name)
                                 (zen:new-define-syntax-declaration name zen:Any access compatibility modifiers '() declaration '() #f))))
        (zen:set-declaration-source new-declaration form-src)
        (zen:set-declaration-name-source new-declaration name-src)
        (let ((effective-declaration (zen:add-declaration-child walker resume declaration new-declaration)))
          effective-declaration)))))


(define (zen:walk-define-syntax walker resume declaration environment form-src)
  (receive (access compatibility modifiers rest) (zen:parse-modifiers walker resume declaration zen:syntax-modifiers (%%cdr (zen:source-code form-src)))
    (let* ((name (zen:source-code (%%car rest)))
           (name-src (%%car rest))
           (body (%%cdr rest))
           (new-declaration (zen:require-declaration declaration name)))
      (receive (signature augmented-environment) (zen:walk-parameters walker resume declaration environment '() #t #t)
        (zen:set-syntax-declaration-signature new-declaration signature)
        (zen:set-syntax-declaration-body new-declaration
          (zen:walk-body walker resume new-declaration augmented-environment body))
        (zen:set-declaration-source new-declaration form-src)
        (zen:set-declaration-name-source new-declaration name-src)
        new-declaration))))


;;;
;;;; Define-Local-Syntax
;;;


(zen:define-class zen:Define-Local-Syntax-Declaration zen:Syntax-Declaration (constructor: zen:allocate-define-local-syntax-declaration)
  ())


(define (zen:new-define-local-syntax-declaration name type access compatibility modifiers attributes parent signature syntax-form)
  (let ((new-declaration (zen:allocate-define-local-syntax-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f signature #f)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(zen:define-method (zen:walk-binding-expandable? (zen:Define-Local-Syntax-Declaration declaration))
  #t)


(zen:define-method (zen:walk-binding-expand-form (zen:Define-Local-Syntax-Declaration binding) walker resume declaration environment form-src)
  (let* ((locator (zen:get-declaration-locator binding))
         (define-local-syntax-form (zen:need-local-macro (zen:get-declaration-parent binding) locator)))
    (let ((expander (zen:get-syntax-form-expander define-local-syntax-form))
          (macro-environment (zen:get-define-local-syntax-form-environment define-local-syntax-form)))
      (zen:with-walker-context walker resume declaration form-src
        (lambda ()
          (expander form-src environment macro-environment))))))


(zen:define-method (zen:emit-declaration (zen:Define-Local-Syntax-Declaration declaration) walker resume environment)
  `(begin))


(define (zen:walk-define-local-syntax-declaration walker resume declaration environment form-src)
  (receive (access compatibility modifiers rest) (zen:parse-modifiers walker resume declaration zen:syntax-modifiers (%%cdr (zen:source-code form-src)))
    (let ((name (zen:source-code (%%car rest)))
          (body (%%cdr rest)))
      (receive (signature augmented-environment) (zen:walk-parameters walker resume declaration environment '() #t #t)
        (let* ((new-declaration (or (zen:find-declaration-child declaration name)
                                    (zen:new-define-local-syntax-declaration name zen:Any access compatibility modifiers '() declaration signature #f)))
               (walked-body (zen:walk-body walker resume new-declaration augmented-environment body)))
          (zen:set-syntax-declaration-body new-declaration walked-body)
          (zen:set-declaration-source new-declaration form-src)
          (zen:register-local-macro declaration (zen:get-declaration-locator new-declaration)
            (zen:with-annotated-frame (zen:annotate-signature signature)
              (lambda (frame)
                (let ((augmented-environment (%%cons frame environment))
                      (current-unit-name
                        (zen:get-declaration-locator
                          (zen:get-declaration-toplevel declaration))))
                  (let* ((env
                           (%%list
                             (zen:new-walk-frame
                               (zen:get-dialect-bindings (zen:get-dialect 'scheme)))
                             (zen:new-walk-frame
                               (zen:get-dialect-bindings (zen:get-dialect 'foundation)))))
                         (env
                           (cond ((zen:get-dialect 'zen)
                                  => (lambda (x)
                                       (cons (zen:new-walk-frame (zen:get-dialect-bindings x)) env)))
                                 (else env))))
                    (zen:new-define-local-syntax-form
                      (zen:get-declaration-locator new-declaration)
                      (eval (car (zen:sourcified-form (zen:emit-expression walked-body declaration walker resume augmented-environment))))
                      env))))))
          (let ((effective-declaration (zen:add-declaration-child walker resume declaration new-declaration)))
            effective-declaration))))))


(define (zen:walk-define-local-syntax walker resume declaration environment form-src)
  (receive (access compatibility modifiers rest) (zen:parse-modifiers walker resume declaration zen:syntax-modifiers (%%cdr (zen:source-code form-src)))
    (zen:require-declaration declaration (zen:source-code (%%car rest)))))


;;;
;;;; Proclaim
;;;


(define (zen:setup-proclaims context)
  (let ((table (zen:get-walk-context-proclaims context)))
    (if zen:debug-user?
        (%%table-set! table 'check (%%list 'types 'zero 'bounds 'lambda)))))


(define (zen:get-proclaim proclaim-name)
  (%%table-ref (zen:effective-proclaims) proclaim-name '()))


(define (zen:set-proclaim proclaim-name value)
  (%%table-set! (zen:effective-proclaims) proclaim-name value))


(define zen:all-warnings
  '(optimizations))

(define zen:all-checks
  '(types zero bounds lambda))

(define zen:all-generates
  '(register))


(define (zen:parse-proclaim-clause clause)
  (define (parse-value value clause)
    (if (%%pair? clause)
        (let ((kind (%%car clause))
              (arguments (%%cdr clause)))
          (values value kind arguments))
      #f))
  
  (if (%%pair? clause)
      (case (%%car clause)
        ((default)
         (parse-value 'default (%%cdr clause)))
        ((not)
         (parse-value 'off (%%cdr clause)))
        (else
         (parse-value 'on clause)))
    #f))


(define (zen:proclaim clause)
  (receive (value kind arguments) (zen:parse-proclaim-clause clause)
    (case kind
      ((warn)
       (let ((warnings (if (%%null? arguments) zen:all-warnings arguments)))
         (for-each (lambda (warning)
                     (define (proclaim on?)
                       (if on?
                           (let ((proclaimed-warnings (zen:get-proclaim 'warn)))
                             (if (%%not (%%memq warning proclaimed-warnings))
                                 (zen:set-proclaim 'warn (%%cons warning proclaimed-warnings))))
                         (let ((proclaimed-warnings (zen:get-proclaim 'warn)))
                           (if (%%memq warning proclaimed-warnings)
                               (zen:set-proclaim 'warn (zen:remove warning proclaimed-warnings))))))
                     
                     (case value
                       ((default)
                        (proclaim (case warning
                                    ((optimizations)
                                     #f))))
                       ((on)
                        (proclaim #t))
                       ((off)
                        (proclaim #f))))
                   warnings)))
      ((check)
       (let ((checks (if (%%null? arguments) zen:all-checks arguments)))
         (for-each (lambda (check)
                     (define (proclaim on?)
                       (if on?
                           (let ((proclaimed-checks (zen:get-proclaim 'check)))
                             (if (%%not (%%memq check proclaimed-checks))
                                 (zen:set-proclaim 'check (%%cons check proclaimed-checks))))
                         (let ((proclaimed-checks (zen:get-proclaim 'check)))
                           (if (%%memq check proclaimed-checks)
                               (zen:set-proclaim 'check (zen:remove check proclaimed-checks))))))
                     
                     (case value
                       ((default)
                        (proclaim (case check
                                    ((types zero bounds lambda)
                                     zen:debug-user?))))
                       ((on)
                        (proclaim #t))
                       ((off)
                        (proclaim #f))))
                   checks)))
      ((generate)
       (let ((generates (if (%%null? arguments) zen:all-generates arguments)))
         (for-each (lambda (generate)
                     (define (proclaim on?)
                       (if on?
                           (let ((proclaimed-generates (zen:get-proclaim 'generate)))
                             (if (%%not (%%memq generate proclaimed-generates))
                                 (zen:set-proclaim 'generate (%%cons generate proclaimed-generates))))
                         (let ((proclaimed-generates (zen:get-proclaim 'generate)))
                           (if (%%memq generate proclaimed-generates)
                               (zen:set-proclaim 'generate (zen:remove generate proclaimed-generates))))))
                     
                     (case value
                       ((default)
                        (proclaim (case generate
                                    ((register)
                                     #f))))
                       ((on)
                        (proclaim #t))
                       ((off)
                        (proclaim #f))))
                   generates))))))


(define (zen:get-warn? warning-name)
  (%%memq warning-name (zen:get-proclaim 'warn)))

(define (zen:get-check? check-name)
  (or zen:debug-check?
      (%%memq check-name (zen:get-proclaim 'check))))

(define (zen:get-generate? generate-name)
  (%%memq generate-name (zen:get-proclaim 'generate)))


(define zen:current-proclaims
  (make-parameter #f))

(define (zen:effective-proclaims)
  (or (zen:current-proclaims)
      (zen:get-walk-proclaims)))


;;;
;;;; Walk Context
;;;


(zen:define-class zen:Walk-Context zen:Object (constructor: zen:allocate-walk-context)
  ((policy    getter: generate)
   (locator   getter: generate)
   (pathname  getter: generate)
   (proclaims getter: generate)))


(define (zen:new-walk-context policy locator pathname)
  (let ((context (zen:allocate-walk-context policy locator pathname (%%make-table test: eq?))))
    (zen:setup-proclaims context)
    context))


(define zen:walk-context
  (%%make-parameter #f))


(define (zen:get-walk-context)
  (zen:walk-context))


(define (zen:need-walk-context)
  (or (zen:walk-context)
      (zen:error "There is no active walk context")))


(define (zen:get-walk-policy)
  (let ((context (zen:get-walk-context)))
    (if (%%not context)
        #f
      (zen:get-walk-context-policy context))))


(define (zen:get-walk-locator)
  (let ((context (zen:get-walk-context)))
    (if (%%not context)
        #f
      (zen:get-walk-context-locator context))))


(define (zen:get-walk-pathname)
  (let ((context (zen:get-walk-context)))
    (if (%%not context)
        #f
      (zen:get-walk-context-pathname context))))


(define (zen:get-walk-proclaims)
  (let ((context (zen:need-walk-context)))
    (zen:get-walk-context-proclaims context)))


;;;
;;;; Walk Location
;;;


(zen:define-class zen:Walk-Location-Class zen:Class ()
  ())


(zen:define-class zen:Walk-Location zen:Object (metaclass: zen:Walk-Location-Class constructor: zen:allocate-walk-location)
  ((unit-locator        getter: generate)
   (declaration-locator getter: generate)
   (locat               getter: generate)
   (path                getter: generate)))


(define (zen:new-walk-location unit-locator declaration-locator locat path)
  (zen:allocate-walk-location unit-locator declaration-locator locat path))


(define (zen:walk-location walker declaration locat)
  (zen:new-walk-location
    (zen:get-walk-locator)
    (zen:get-declaration-locator declaration)
    locat
    (if locat (%%container->path (%%locat-container locat)) #f)))


;;;
;;;; Walk Source Not Found
;;;


(zen:define-class zen:Walk-Source-Not-Found zen:Error (constructor: zen:allocate-walk-source-not-found)
  ((unit-name getter: generate)))


(define (zen:new-walk-source-not-found message unit-name)
  (zen:allocate-walk-source-not-found message unit-name))


;;;
;;;; Walk Problem
;;;


(zen:define-class zen:Walk-Problem zen:Error ()
  ((location getter: generate)))


;;;
;;;; Walk Problems
;;;


(zen:define-class zen:Walk-Problems zen:Error (constructor: zen:allocate-walk-problems)
  ((warnings getter: generate)
   (errors   getter: generate)))


(define (zen:new-walk-problems message warnings errors)
  (zen:allocate-walk-problems message warnings errors))


(zen:define-method (zen:get-detail (zen:Walk-Problems problems))
  (define (add-details problems queue)
    (for-each (lambda (problem)
                (zen:enqueue queue (zen:new-exception-detail "Green" (zen:present-exception problem) (zen:get-walk-problem-location problem) '())))
              problems))
  
  (zen:new-exception-detail "ErrorStop" "Walk problems encountered" #f
    (let ((all (%%append (zen:get-walk-problems-warnings problems)
                         (zen:get-walk-problems-errors problems))))
      (map (lambda (partition)
             (zen:bind (unit-locator . problems) partition
               (let ((prefix (if (%%not unit-locator) -1 (%%string-length (%%symbol->string unit-locator)))))
                 (zen:new-exception-detail "Document" (or unit-locator "<console>") #f
                   (let ((unit-details (zen:new-queue)))
                     (for-each (lambda (partition)
                                 (zen:bind (declaration-locator . problems) partition
                                   (if (%%fx= (%%string-length declaration-locator) prefix)
                                       (add-details problems unit-details)
                                     (zen:enqueue unit-details
                                       (zen:new-exception-detail "Project" (%%substring declaration-locator (%%fx+ prefix 1) (%%string-length declaration-locator)) #f
                                         (let ((declaration-details (zen:new-queue)))
                                           (add-details problems declaration-details)
                                           (zen:queue-list declaration-details)))))))
                               (zen:partition-walk-problems-declaration problems))
                     (zen:queue-list unit-details))))))
           (zen:partition-walk-problems-unit all)))))


;;;
;;;; Walk Warning
;;;


(zen:define-class zen:Walk-Warning zen:Walk-Problem (constructor: zen:allocate-walk-warning)
  ())


(define (zen:new-walk-warning location message)
  (zen:allocate-walk-warning message location))


;;;
;;;; Walk Error
;;;


(zen:define-class zen:Walk-Error zen:Walk-Problem (constructor: zen:allocate-walk-error)
  ())


(define (zen:new-walk-error location message)
  (zen:allocate-walk-error message location))


;;;
;;;; Conflict Error
;;;


(zen:define-class zen:Conflict-Error zen:Walk-Error (constructor: zen:allocate-conflict-error)
  ((prefix getter: generate)
   (symbol getter: generate)
   (first  getter: generate)
   (second getter: generate)))


(define (zen:new-conflict-error location prefix symbol first second)
  (zen:allocate-conflict-error #f location prefix symbol first second))


(zen:define-method (zen:present-exception (zen:Conflict-Error error))
  (zen:format "{a} conflict: {s} {s} {s}"
               (zen:get-conflict-error-prefix error)
               (zen:get-conflict-error-symbol error)
               (zen:get-conflict-error-first error)
               (zen:get-conflict-error-second error)))


;;;
;;;; Unresolved Error
;;;


(zen:define-class zen:Unresolved-Error zen:Walk-Error (constructor: zen:allocate-unresolved-error)
  ((symbol getter: generate)))


(define (zen:new-unresolved-error location symbol)
  (zen:allocate-unresolved-error #f location symbol))


(zen:define-method (zen:present-exception (zen:Unresolved-Error error))
  (zen:format "Unresolved symbol: {s}"
               (zen:get-unresolved-error-symbol error)))


;;;
;;;; Walk Frame
;;;


(zen:define-class zen:Walk-Frame zen:Walk-Binding (constructor: zen:allocate-walk-frame)
  ((bindings getter: generate)))


(define (zen:new-walk-frame bindings)
  (zen:allocate-walk-frame (if (%%table? bindings) (%%list bindings) bindings)))


(zen:define-method (zen:walk-binding-lookup (zen:Walk-Frame binding) symbol source-declaration)
  (let iter ((scan (zen:get-walk-frame-bindings binding)))
       (if (%%null? scan)
           #f
         (let ((table (%%car scan)))
           (or (%%table-ref table symbol #f)
               (iter (%%cdr scan)))))))


;;;
;;;; Signature
;;;


(zen:define-class zen:Signature zen:Object (constructor: zen:allocate-signature)
  ((mandatory   getter: generate)
   (positional  getter: generate)
   (optional    getter: generate)
   (named       getter: generate)
   (rest        getter: generate)
   (expressions getter: generate)))


(define (zen:new-signature positional optional named rest expressions)
  (let ((mandatory (%%length positional)))
    (zen:allocate-signature mandatory positional optional named rest expressions)))


(define (zen:only-positional-signature? signature)
  (and (%%null? (zen:get-signature-optional signature))
       (%%null? (zen:get-signature-named signature))
       (%%not (zen:get-signature-rest signature))))


(define (zen:typed-signature? signature #!optional (skip-first-positional? #f))
  (define (typed? parameter)
    (zen:get-lexical-binding-type parameter))
  
  (let ((positional (zen:get-signature-positional signature))
        (optional (zen:get-signature-optional signature))
        (named (zen:get-signature-named signature))
        (rest (zen:get-signature-rest signature)))
    (or (zen:some? typed? (if skip-first-positional? (%%cdr positional) positional))
        (zen:some? typed? optional)
        (zen:some? typed? named)
        (and rest (typed? rest)))))


;;;
;;;; Symbol Binding
;;;


(zen:define-class zen:Symbol-Binding zen:Lexical-Binding ()
  ((gensym getter: generate setter: generate)))


(zen:define-method (zen:emit-binding-symbol (zen:Symbol-Binding binding) declaration environment)
  (or (zen:get-symbol-binding-gensym binding)
      (zen:unwrap-syntactic-closure (zen:get-lexical-binding-name binding))))


;;;
;;;; Variable
;;;


(zen:define-class zen:Variable zen:Symbol-Binding (constructor: zen:allocate-variable)
  ((source           getter: generate)
   (specifier-source getter: generate)
   (reference-count  getter: generate setter: generate)))


(define (zen:new-variable name type source specifier-source)
  (%%assertion (zen:variable-name-valid? name) (zen:error "Invalid variable name: {s}" (zen:desourcify-all name))
    (zen:allocate-variable name type #f #f source specifier-source 0)))


(define (zen:variable-name-valid? name)
  (define (variable-name-valid-symbol? name)
    (and (%%symbol? name)
         (%%not (zen:specifier? name))))
  
  (variable-name-valid-symbol?
    (if (zen:syntactic-closure? name)
        (zen:get-syntactic-closure-form name)
      name)))


(zen:define-method (zen:walk-binding-referenced (zen:Variable binding))
  (zen:set-variable-reference-count binding (%%fx+ (zen:get-variable-reference-count binding) 1)))


(zen:define-method (zen:emit-binding-reference (zen:Variable binding) source-declaration walker resume environment)
  (zen:new-code
    (zen:emit-binding-symbol binding source-declaration environment)
    (zen:find-annotated-type binding environment)
    #f))


(zen:define-method (zen:walk-binding-assignable? (zen:Variable binding))
  #t)


(zen:define-method (zen:emit-binding-assignment (zen:Variable binding) value source-declaration walker resume environment form-src)
  (let ((value-code (zen:emit-expression value source-declaration walker resume environment)))
    (receive (annotated-frame annotated-variable annotated-type) (zen:find-annotated binding environment)
      (%%when (%%class-is? annotated-variable zen:Annotated-Variable)
        (zen:extend-annotated-type annotated-frame annotated-variable (zen:get-code-type value-code))))
    (let ((binding-code (zen:emit-binding-symbol binding source-declaration environment)))
      (zen:new-code
        `(set! ,binding-code ,(zen:emit-type-cast value-code (zen:get-lexical-binding-type binding) source-declaration source-declaration walker resume environment))
        zen:Any
        form-src))))


(define (zen:register-variable declaration suffix value)
  (let ((module-declaration (zen:get-declaration-toplevel declaration)))
    (let ((symbol (zen:generate-global-symbol suffix)))
      (let ((variable (%%cons symbol value)))
        (zen:enqueue (zen:get-module-declaration-walker-variables module-declaration) variable)
        variable))))


(define (zen:register-static declaration suffix expr)
  (let ((module-declaration (zen:get-declaration-toplevel declaration)))
    (let ((symbol (zen:generate-global-symbol suffix)))
      (let ((static (%%cons symbol expr)))
        (zen:enqueue (zen:get-module-declaration-walker-statics module-declaration) static)
        static))))


;;;
;;;; Parameter
;;;


(zen:define-class zen:Parameter zen:Variable (constructor: zen:allocate-parameter)
  ())


(define (zen:new-parameter name type source specifier-source)
  (%%assertion (zen:variable-name-valid? name) (zen:error "Invalid variable name: {s}" (zen:desourcify-all name))
    (zen:allocate-parameter name type #f #f source specifier-source 0)))


(zen:define-virtual (zen:emit-check? (zen:Parameter parameter)))
(zen:define-virtual (zen:emit-parameter (zen:Parameter parameter) declaration walker resume environment))


(zen:define-method (zen:emit-check? (zen:Parameter parameter))
  #t)


(zen:define-method (zen:emit-parameter (zen:Parameter parameter) declaration walker resume environment)
  (zen:emit-binding-symbol parameter declaration environment))


;;;
;;;; Dynamic Parameter
;;;


(zen:define-class zen:Dynamic-Parameter zen:Parameter (constructor: zen:allocate-dynamic-parameter)
  ((class getter: generate)))


(define (zen:new-dynamic-parameter name type source specifier-source class)
  (zen:allocate-dynamic-parameter name type #f #f source specifier-source 0 class))


(zen:define-method (zen:emit-parameter (zen:Dynamic-Parameter parameter) declaration walker resume environment)
  (let ((class (zen:get-dynamic-parameter-class parameter)))
    (%%list (zen:sourcified-form (zen:emit-expression class declaration walker resume environment)) (zen:emit-binding-symbol parameter declaration environment))))


;;;
;;;; Optional Parameter
;;;


(zen:define-class zen:Optional-Parameter zen:Parameter (constructor: zen:allocate-optional-parameter)
  ((default getter: generate setter: generate)))


(define (zen:new-optional-parameter name type source specifier-source default)
  (zen:allocate-optional-parameter name type #f #f source specifier-source 0 default))


(zen:define-method (zen:emit-parameter (zen:Optional-Parameter parameter) declaration walker resume environment)
  (let ((default (zen:get-optional-parameter-default parameter)))
    (%%list (zen:emit-binding-symbol parameter declaration environment) (zen:sourcified-form (zen:emit-expression default declaration walker resume environment)))))


;;;
;;;; Named Parameter
;;;


(zen:define-class zen:Named-Parameter zen:Parameter (constructor: zen:allocate-named-parameter)
  ((default getter: generate setter: generate)))


(define (zen:new-named-parameter name type source specifier-source default)
  (zen:allocate-named-parameter name type #f #f source specifier-source 0 default))


(zen:define-method (zen:emit-parameter (zen:Named-Parameter parameter) declaration walker resume environment)
  (let ((default (zen:get-named-parameter-default parameter)))
    (%%list (zen:get-lexical-binding-name parameter) (zen:sourcified-form (zen:emit-expression default declaration walker resume environment)))))


(zen:define-method (zen:emit-binding-symbol (zen:Named-Parameter parameter) declaration environment)
  (zen:get-lexical-binding-name parameter))


;;;
;;;; Rest Parameter
;;;


(zen:define-class zen:Rest-Parameter zen:Parameter (constructor: zen:allocate-rest-parameter)
  ())


(define (zen:new-rest-parameter name type source specifier-source)
  (zen:allocate-rest-parameter name type #f #f source specifier-source 0))


(zen:define-method (zen:emit-parameter (zen:Rest-Parameter parameter) declaration walker resume environment)
  (zen:emit-binding-symbol parameter declaration environment))


;;;
;;;; Local-Variable-Binding
;;;


(zen:define-class zen:Local-Variable-Binding zen:Lexical-Binding (constructor: zen:allocate-local-variable-binding)
  ((variable getter: generate)))


(define (zen:new-local-variable-binding type variable)
  (zen:allocate-local-variable-binding variable type #f variable))


(zen:define-method (zen:emit-binding-reference (zen:Local-Variable-Binding declaration) source-declaration walker resume environment)
  (zen:new-code
    (zen:get-local-variable-binding-variable declaration)
    zen:Any
    #f))


;;;
;;;; Macro Symbol
;;;


(zen:define-class zen:Macro-Symbol zen:Symbol-Binding (constructor: zen:allocate-macro-symbol)
  ((getter getter: generate)
   (setter getter: generate)))


(define (zen:new-macro-symbol name getter setter)
  (zen:allocate-macro-symbol name #f #f #f getter setter))


#; ;; convert to walk / emit
(zen:define-method (zen:emit-binding-reference (zen:Macro-Symbol binding) source-declaration walker resume environment)
  (let ((getter (zen:get-macro-symbol-getter binding)))
    (zen:walk walker resume source-declaration environment (getter))))


#; ;; convert to walk / emit
(zen:define-method (zen:walk-binding-assignable? (zen:Macro-Symbol declaration))
  #t)


#; ;; convert to walk / emit
(zen:define-method (zen:emit-binding-assignment (zen:Macro-Symbol binding) value source-declaration walker resume environment)
  (let ((setter (zen:get-macro-symbol-setter binding)))
    (zen:walk walker resume source-declaration environment (setter value))))


;;;
;;;; Form Binding
;;;


(zen:define-class zen:Form-Binding zen:Lexical-Binding ()
  ())


;;;
;;;; Declaration Form
;;;


(zen:define-class zen:Declaration-Form zen:Form-Binding (constructor: zen:allocate-declaration-form)
  ((walk getter: generate)))


(define (zen:new-declaration-form name walk)
  (zen:allocate-declaration-form name #f #f walk))


;;;
;;;; Special Form
;;;


(define zen:special-forms
  '())


(define (zen:add-special-form symbol special-form)
  (set! zen:special-forms (%%cons (%%cons symbol special-form) zen:special-forms)))


(define (zen:find-special-form symbol)
  #f
  #; ;; walk is undefined
  (let ((found (assq walk zen:special-forms)))
    (if found (cdr found) #f)))


(zen:define-class zen:Special-Form zen:Form-Binding (constructor: zen:allocate-special-form)
  ((walk getter: generate)))


(define (zen:new-special-form name walk)
  (zen:allocate-special-form name #f #f walk))


(zen:define-method (zen:walk-binding-walkable? (zen:Special-Form binding))
  #t)


(zen:define-method (zen:emit-binding-reference (zen:Special-Form binding) source-declaration walker resume environment)
  (zen:get-lexical-binding-name binding))


(zen:define-method (zen:walk-binding-walk-form (zen:Special-Form binding) walker resume declaration environment form-src)
  (let ((walk-proc/symbol (zen:get-special-form-walk binding)))
    (let ((walk (if (%%symbol? walk-proc/symbol)
                    ;; we should cache it
                    (zen:find-special-form walk-proc/symbol)
                  walk-proc/symbol)))
      (walk walker resume declaration environment form-src))))


;;;
;;;; Macro Form
;;;


(zen:define-class zen:Macro-Form zen:Form-Binding (constructor: zen:allocate-macro-form)
  ((expander getter: generate)))


(define (zen:new-macro-form name expander)
  (zen:allocate-macro-form name #f #f expander))


(zen:define-method (zen:walk-binding-expandable? (zen:Macro-Form binding))
  #t)


(zen:define-method (zen:walk-binding-expand-form (zen:Macro-Form binding) walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((expander (zen:get-macro-form-expander binding)))
      (apply expander walker resume declaration environment (%%cdr form)))))


;;;
;;;; Syntax Form
;;;


(zen:define-class zen:Syntax-Form zen:Form-Binding (constructor: zen:allocate-syntax-form)
  ((expander getter: generate)))


(define (zen:new-syntax-form name expander)
  (zen:allocate-syntax-form name #f #f expander))


(zen:define-method (zen:walk-binding-expandable? (zen:Syntax-Form binding))
  #t)


(zen:define-method (zen:walk-binding-expand-form (zen:Syntax-Form binding) walker resume declaration environment form-src)
  (let ((expander (zen:get-syntax-form-expander binding)))
    (expander walker resume declaration environment form-src)))


;;;
;;;; Define-Syntax Form
;;;


(zen:define-class zen:Define-Syntax-Form zen:Syntax-Form (constructor: zen:allocate-define-syntax-form)
  ((environment getter: generate)))


(define (zen:new-define-syntax-form name expander environment)
  (zen:allocate-define-syntax-form name #f #f expander environment))


(zen:define-method (zen:walk-binding-expand-form (zen:Define-Syntax-Form binding) walker resume declaration environment form-src)
  (let ((expander (zen:get-syntax-form-expander binding))
        (macro-environment (zen:get-define-syntax-form-environment binding)))
    (zen:with-walker-context walker resume declaration form-src
      (lambda ()
        (expander form-src environment macro-environment)))))


;;;
;;;; Define-Local-Syntax Form
;;;


(zen:define-class zen:Define-Local-Syntax-Form zen:Syntax-Form (constructor: zen:allocate-define-local-syntax-form)
  ((environment getter: generate)))


(define (zen:new-define-local-syntax-form name expander environment)
  (zen:allocate-define-local-syntax-form name #f #f expander environment))


(zen:define-method (zen:walk-binding-expand-form (zen:Define-Local-Syntax-Form binding) walker resume declaration environment form-src)
  (let ((expander (zen:get-syntax-form-expander binding))
        (macro-environment (zen:get-define-local-syntax-form-environment binding)))
    (zen:with-walker-context walker resume declaration form-src
      (lambda ()
        (expander form-src environment macro-environment)))))


;;;
;;;; Let Macro Form
;;;


(zen:define-class zen:Let-Macro-Form zen:Form-Binding (constructor: zen:allocate-let-macro-form)
  ((expander getter: generate)))


(define (zen:new-let-macro-form name expander)
  (zen:allocate-let-macro-form name #f #f expander))


(zen:define-method (zen:walk-binding-expandable? (zen:Let-Macro-Form binding))
  #t)


(zen:define-method (zen:walk-binding-expand-form (zen:Let-Macro-Form binding) walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((expander (zen:get-let-macro-form-expander binding)))
      (apply expander (%%cdr form)))))


;;;
;;;; Syntactic Closure
;;;


(zen:define-class zen:Syntactic-Closure zen:Object (constructor: zen:allocate-syntactic-closure)
  ((environment getter: generate)
   (variables   getter: generate)
   ;; quick solution to form often not containing source info
   ;; for example in (increase! x) (ask Alex Shinn for the correct solution)
   (expression  getter: generate)
   (form        getter: generate)))


(zen:define-method (zen:print-object (zen:Syntactic-Closure sc) output detail)
  (zen:format output "#<{a} {a} #{a}>"
               (%%get-category-identifier (%%get-object-class sc))
               (zen:get-syntactic-closure-form sc)
               (zen:object->serial sc)))


(define (zen:syntactic-closure? x)
  (%%class-is? x zen:Syntactic-Closure))


(define (zen:make-syntactic-closure env vars expression form)
  (if (or (%%symbol? form)
          (%%pair? form)
          (and (%%source? form)
               (or (%%symbol? (%%source-code form))
                   (%%pair? (%%source-code form)))))
      (zen:allocate-syntactic-closure env vars expression form)
    form))


(define (zen:syntactic-closure-form sc)
  (zen:get-syntactic-closure-form sc))


(define (zen:unwrap-syntactic-closure x)
  (cond ((zen:syntactic-closure? x)
         (zen:unwrap-syntactic-closure (zen:get-syntactic-closure-form x)))
        ((%%source? x)
         (zen:unwrap-syntactic-closure (%%source-code x)))
        (else
         x)))


(define (zen:strip-syntactic-closures x)
  (cond ((zen:syntactic-closure? x)
         (zen:strip-syntactic-closures (zen:get-syntactic-closure-form x)))
        ((%%source? x)
         (zen:strip-syntactic-closures (%%source-code x)))
        ((%%pair? x)
         (cons (zen:strip-syntactic-closures (%%car x))
               (zen:strip-syntactic-closures (%%cdr x))))
        ((%%vector? x)
         (%%list->vector (zen:strip-syntactic-closures (%%vector->list x))))
        (else
         x)))


(define (zen:strip-source-info x)
  (cond ((%%source? x)
         (zen:strip-source-info (%%source-code x)))
        ((%%pair? x)
         (cons (zen:strip-source-info (%%car x)) (zen:strip-source-info (%%cdr x))))
        ((%%vector? x)
         (%%list->vector (zen:strip-source-info (%%vector->list x))))
        (else
         x)))


(define (zen:identifier? x)
  (cond ((zen:syntactic-closure? x) (zen:identifier? (zen:syntactic-closure-form x)))
        ((%%source? x) (zen:identifier? (%%source-code x)))
        (else (%%symbol? x))))


;; identifiers are equal if they resolve to the same lexical binding
(define (zen:identifier=? x-env x y-env y)
  (define (lookup-identifier symbol environment)
    (if (zen:composite-reference? symbol)
        symbol
      (zen:find-in (lambda (binding)
                      (zen:walk-binding-lookup binding symbol #f))
                    environment)))
  
  (define (binding-name x)
    (if (%%class-is? x zen:Lexical-Binding)
        (zen:get-lexical-binding-name x)
      x))
  
  (or (%%eq? x y)
      (let ((x^ (lookup-identifier (zen:unwrap-syntactic-closure x) x-env))
            (y^ (lookup-identifier (zen:unwrap-syntactic-closure y) y-env)))
        (if x^
            (%%eq? x^ y^)
          (and (%%not y^)
               (%%eq? (zen:unwrap-syntactic-closure x)
                      (zen:unwrap-syntactic-closure y)))))))


(define zen:sc-macro-transformer
  (lambda (f)
    (lambda (expr usage-env macro-env)
      (zen:make-syntactic-closure macro-env '() expr (f expr usage-env)))))


(define zen:rsc-macro-transformer
  (lambda (f)
    (lambda (expr usage-env macro-env)
      (f expr macro-env))))


(define zen:er-macro-transformer
  (lambda (f)
    (lambda (expr usage-env macro-env)
      (let ((rename
              (let ((renames '()))
                (lambda (identifier)
                  (cond ((assq identifier renames) => cdr)
                        (else
                         (let ((name (zen:make-syntactic-closure macro-env '() expr identifier)))
                           (set! renames (cons (cons identifier name) renames))
                           name))))))
            (compare
              (lambda (x y)
                (zen:identifier=? usage-env x usage-env y))))
        (f expr rename compare)))))


;;;
;;;; Annotated Variable
;;;


(zen:define-class zen:Annotated-Variable zen:Object (constructor: zen:allocate-annotated-variable)
  ((variable      getter: generate)
   (declared-type getter: generate)
   (type          getter: generate setter: generate)))


(define (zen:new-annotated-variable variable declared-type type)
  (zen:allocate-annotated-variable variable declared-type type))


;;;
;;;; Restricted Binding
;;;


(zen:define-class zen:Restricted-Binding zen:Object (constructor: zen:allocate-restricted-binding)
  ((binding getter: generate)
   (type    getter: generate)))


(define (zen:new-restricted-binding binding type)
  (zen:allocate-restricted-binding binding type))


;;;
;;;; Annotated Frame
;;;


(zen:define-class zen:Annotated-Frame zen:Object (constructor: zen:allocate-annotated-frame)
  ((variables getter: generate)
   (reset     getter: generate)))


(define (zen:new-annotated-frame variables reset)
  (zen:allocate-annotated-frame variables reset))


;; put those in a cond-expand when cond-expand supports multiple features
(define (zen:inspect-annotated-variable variable)
  (let ((serial (zen:object->serial-symbol variable)))
    (if (%%class-is? variable zen:Restricted-Binding)
        `(:restricted
          ,(zen:get-lexical-binding-name (zen:get-restricted-binding-binding variable))
          ,(zen:get-restricted-binding-type variable) serial)
      `(:variable
        ,(zen:get-lexical-binding-name (zen:get-annotated-variable-variable variable))
        ,(zen:get-annotated-variable-type variable) serial))))


(define (zen:inspect-annotated-frame frame)
  `(:frame
    ,@(map zen:inspect-annotated-variable (zen:get-annotated-frame-variables frame))))


(define (zen:inspect-annotated-environment environment)
  `(:environment
    ,@(map zen:inspect-annotated-frame environment)))


;;;
;;;; Code
;;;


(zen:define-class zen:Code zen:Object (constructor: zen:allocate-code)
  ((form   getter: generate)
   (type   getter: generate)
   (source getter: generate)))


(define (zen:new-code form type source)
  (zen:allocate-code form type source))


(define (zen:codes-forms codes)
  (map (lambda (code)
         (zen:sourcified-form code))
       codes))


(define (zen:codes-types codes)
  (map (lambda (code)
         (zen:get-code-type code))
       codes))


;; this approach is clearly costly in memory and is just to experiment
(define (zen:sourcify-code code src)
  (if (or (%%not src) (%%not (%%source? src)))
      code
    (zen:new-code (zen:get-code-form code)
                   (zen:get-code-type code)
                   src)))


;; temp try
(define (zen:sourcified-form code)
  (let ((form (zen:get-code-form code))
        (src (zen:get-code-source code)))
    (zen:sourcify-if form src)))


;; temp try... 2
(define (zen:sourcified-form2 code src)
  (let ((form (zen:get-code-form code)))
    (zen:sourcify-if form src)))


;;;
;;;; Annotation
;;;


(define (zen:annotate-signature signature)
  (let ((positional (zen:get-signature-positional signature))
        (optional (zen:get-signature-optional signature))
        (named (zen:get-signature-named signature))
        (rest (zen:get-signature-rest signature))
        (queue (zen:new-queue)))
    (define (annotate parameter)
      (let ((declared-type (zen:get-lexical-binding-type parameter)))
        (let ((type (or declared-type zen:Any)))
          (zen:enqueue queue (zen:new-annotated-variable parameter declared-type type)))))
    (for-each annotate positional)
    (for-each annotate optional)
    (for-each annotate named)
    (%%when rest
      (annotate rest))
    (zen:queue-list queue)))


;; not 100% sure about this special case of using the argument type when the parameter is typeless
(define (zen:annotate-inlined-signature signature arguments)
  (let ((positional (zen:get-signature-positional signature))
        (queue (zen:new-queue)))
    (for-each (lambda (parameter argument)
                (let ((declared-type (zen:get-lexical-binding-type parameter)))
                  (let ((type (or declared-type (zen:get-code-type argument))))
                    (zen:enqueue queue (zen:new-annotated-variable parameter declared-type type)))))
              positional
              arguments)
    (zen:queue-list queue)))


(define (zen:annotate-bindings bindings)
  (map (lambda (binding)
         (let ((variable (%%car binding)))
           (let ((declared-type (zen:get-lexical-binding-type variable)))
             (let ((type (or declared-type zen:Void)))
               (zen:new-annotated-variable variable declared-type type)))))
       bindings))


(define (zen:annotate-receive parameters expr-type)
  (let ((expr-types (and (%%is? expr-type zen:Values-Type) (zen:get-values-type-types expr-type))))
    (map (lambda (parameter)
           (let ((expr-type #f))
             (%%when expr-types
               (set! expr-type (%%car expr-types))
               (set! expr-types (%%cdr expr-types)))
             (let ((declared-type (zen:get-lexical-binding-type parameter)))
               (let ((type (or expr-type declared-type zen:Any)))
                 (zen:new-annotated-variable parameter declared-type type)))))
         parameters)))


(define (zen:annotate-internal-defines internal-defines)
  (zen:collect (lambda (internal-define)
                  (and (%%is-not? internal-define zen:declare-class)
                       (let ((variable (zen:get-internal-define-variable internal-define)))
                         (let ((declared-type (zen:get-lexical-binding-type variable)))
                           (let ((type (or declared-type zen:Any)))
                             (zen:new-annotated-variable variable declared-type type))))))
                internal-defines))


(define (zen:with-annotated-frame variables proc)
  (let ((reset #f))
    (continuation-capture
      (lambda (k)
        (set! reset k)))
    (proc (zen:new-annotated-frame variables reset))))


(define (zen:find-annotated variable environment)
  (let ((type #f))
    (let iter-frames ((frames environment))
      (if (%%null? frames)
          #f
        (let ((annotated-frame (%%car frames)))
          (or (let iter-variables ((variables
                                     (cond ((%%class-is? annotated-frame zen:Annotated-Frame)
                                            (zen:get-annotated-frame-variables annotated-frame))
                                           ((%%class-is? annotated-frame zen:Variable)
                                            (%%list annotated-frame))
                                           (else
                                            '()))))
                   (if (%%null? variables)
                       #f
                     (let ((annotated-variable (%%car variables)))
                       (if (%%class-is? annotated-variable zen:Restricted-Binding)
                           (let ((binding (zen:get-restricted-binding-binding annotated-variable)))
                             ;; this is really for slots so i need to think about this
                             (if (and (%%class-is? binding zen:Declaration) (%%eq? binding variable))
                                 (values #f annotated-variable (zen:get-restricted-binding-type annotated-variable))
                               (begin
                                 ;; keep outermost type
                                 (if (and (%%not type) (%%eq? binding variable))
                                     (set! type (zen:get-restricted-binding-type annotated-variable)))
                                 (iter-variables (%%cdr variables)))))
                         (if (%%eq? (zen:get-annotated-variable-variable annotated-variable) variable)
                             (values annotated-frame annotated-variable (or type (zen:get-annotated-variable-type annotated-variable)))
                           (iter-variables (%%cdr variables)))))))
              (iter-frames (%%cdr frames))))))))


(define (zen:find-annotated-type binding environment)
  ;; big time kludge to test it out
  (if (%%class-is? binding zen:Variable)
      (receive (frame variable type) (zen:find-annotated binding environment)
        type)
    ;; here it is a slot declaration
    (let ((info (zen:find-annotated binding environment)))
      (if info
          (receive (frame variable type) info
            type)
        (zen:get-lexical-binding-type binding)))))


(define (zen:extend-annotated-type frame variable new-type)
  (let ((declared-type (zen:get-annotated-variable-declared-type variable))
        (actual-type (zen:get-annotated-variable-type variable)))
    (if declared-type
        ;; here should validate that new-type is castable to declared-type and generate a cast
        #f
      ;; this should not happen as variables are always created initialized
      (if (%%eq? actual-type zen:Void)
          (zen:set-annotated-variable-type variable new-type)
        (%%when (%%not (%%subtype? new-type actual-type))
          ;; should probably just call zen:extend-type but it currently
          ;; bugs recursively on unioning function types together...
          (let ((extended-type
                  (if (%%subtype? actual-type new-type)
                      new-type
                    ;; should find the most specific common supertype
                    zen:Any)))
            (zen:set-annotated-variable-type variable zen:Any)
            ;;(zen:debug 'reset (zen:get-lexical-binding-name (zen:get-annotated-variable-variable variable)) actual-type new-type extended-type)
            (let ((reset (zen:get-annotated-frame-reset frame)))
              (continuation-return reset #f))))))))


(define (zen:extend-type type1 type2)
  (cond ((or (%%not type1) (%%not type2))
         zen:Any)
        ;; the void test should be integrated in the type framework
        ((or (%%eq? type1 zen:Void) (%%subtype? type1 type2))
         type2)
        ;; the void test should be integrated in the type framework
        ((or (%%eq? type2 zen:Void) (%%subtype? type2 type1))
         type1)
        (else
         ;; should find the most specific common supertype
         zen:Any)))


(define (zen:extend-types types)
  (if (%%null? types)
      zen:Void
    (zen:extend-type (%%car types) (zen:extend-types (%%cdr types)))))


(define (zen:type-union types)
  (zen:new-union-type types))


(define (zen:type-difference type1 type2)
  #f)


;;;
;;;; Problems
;;;


(define zen:*raise-walk-problems?*
  (%%make-parameter #f))


(define (zen:walk-warning walker declaration src fmt-string . rest)
  (let ((location (zen:walk-location walker declaration (zen:source-locat src)))
        (message (apply zen:format fmt-string rest)))
    (zen:walker-warning walker (zen:new-walk-warning location message))))


(define (zen:walk-error walker resume declaration src fmt-string . rest)
  (let ((location (zen:walk-location walker declaration (zen:source-locat src)))
        (message (apply zen:format fmt-string rest)))
    (zen:walker-error walker resume (zen:new-walk-error location message))))


(define (zen:walk-conflict walker resume declaration prefix symbol first second form-src)
  (let ((location (zen:walk-location walker declaration (zen:source-locat form-src))))
    (zen:walker-error walker resume (zen:new-conflict-error location prefix symbol first second))))


(define (zen:walk-unresolved walker resume declaration symbol-src)
  (let ((location (zen:walk-location walker declaration (zen:source-locat symbol-src))))
    (zen:walker-error walker resume (zen:new-unresolved-error location (zen:unwrap-syntactic-closure symbol-src)))))


(define (zen:walker-warning walker warning)
  (cond ((zen:*raise-walk-problems?*)
         (raise warning))
        ((zen:warnings?)
         (zen:set-walker-warnings walker (%%append (zen:get-walker-warnings walker) (%%list warning))))))


(define (zen:walker-error walker resume error)
  (cond ((zen:*raise-walk-problems?*)
         (raise error))
        (else
         (zen:set-walker-errors walker (%%append (zen:get-walker-errors walker) (%%list error)))
         (if (and resume (zen:delay-reporting?))
             (continuation-return resume (zen:unspecified))
           (zen:validate-walk-problems walker)))))


(define (zen:validate-walk-problems walker)
  (let ((warnings (zen:get-walker-warnings walker))
        (errors (zen:get-walker-errors walker)))
    (%%when (or (%%not-null? warnings) (%%not-null? errors))
      (let ((output (open-output-string))
            (all (%%append warnings errors)))
        (zen:format output "Walk problems encountered{%}")
        (for-each (lambda (partition)
                    (zen:bind (unit-locator . problems) partition
                      (zen:format output "  In {a}" (or unit-locator "<console>"))
                      (let ((prefix (if (%%not unit-locator) -1 (%%string-length (%%symbol->string unit-locator)))))
                        (for-each (lambda (partition)
                                    (zen:bind (declaration-locator . problems) partition
                                      (let ((toplevel? (%%fx= (%%string-length declaration-locator) prefix)))
                                        (if (%%not toplevel?)
                                            (zen:format output "{%}    At {a}"
                                              (%%substring declaration-locator (%%fx+ prefix 1) (%%string-length declaration-locator))))
                                        (for-each (lambda (problem)
                                                    (zen:format output "{%}{a}    {a}"
                                                      (if toplevel? "" "  ")
                                                      (zen:present-exception problem)))
                                                  problems))))
                                  (zen:partition-walk-problems-declaration problems)))))
                  (zen:partition-walk-problems-unit all))
        (let ((message (get-output-string output)))
          (raise (zen:new-walk-problems message warnings errors)))))))


(define (zen:partition-walk-problems-unit problems)
  (zen:partition problems
                  (lambda (problem)
                    (zen:get-walk-location-unit-locator (zen:get-walk-problem-location problem)))
                  assv))


(define (zen:partition-walk-problems-declaration problems)
  (zen:partition problems
                  (lambda (problem)
                    (%%symbol->string (zen:get-walk-location-declaration-locator (zen:get-walk-problem-location problem))))
                  assoc))


;;;
;;;; Parse
;;;


(define (zen:parse-modifiers walker resume declaration infos rest)
  (define (is-modifier? infos x)
    (cond ((%%null? infos) #f)
          ((%%memq x (%%caar infos)) #t)
          (else (is-modifier? (%%cdr infos) x))))
  
  (define (skip-modifiers infos ls)
    (define (skip orig ls)
      (if (%%pair? ls)
          (let ((expr (zen:unwrap-syntactic-closure (%%car ls))))
            (if (is-modifier? infos expr)
                (skip (%%cons expr orig) (%%cdr ls))
              (values (zen:reverse! orig) ls)))
        (values (%%reverse orig) ls)))
    
    (skip '() ls))
  
  (define (get-modifier names from to)
    (cond ((%%eq? from to)
           #f)
          ((%%memq (zen:unwrap-syntactic-closure (%%car from)) names)
           (cond ((get-modifier names (%%cdr from) to)
                  => (lambda (x) (zen:walk-error "Ambiguous modifiers: {s} {s}" walker resume declaration (%%car from) x)))
                 (else
                  (zen:unwrap-syntactic-closure (%%car from)))))
          (else
           (get-modifier names (%%cdr from) to))))
  
  (let ((modifiers rest))
    (receive (originals rest) (skip-modifiers infos rest)
      (let lp ((ls infos) (res '()))
           (cond ((%%null? ls)
                  (%%apply values (%%reverse (%%cons rest (%%cons originals res)))))
                 ((get-modifier (%%caar ls) modifiers rest)
                  => (lambda (x) (lp (%%cdr ls) (%%cons x res))))
                 (else
                  (lp (%%cdr ls) (%%cons (%%cdar ls) res))))))))


;;;
;;;; Environment
;;;


(zen:define-method (zen:walker-declarations (zen:Walker walker))
  (%%list (zen:get-dialect-declarations (zen:get-dialect 'foundation))))


(zen:define-method (zen:walker-bindings (zen:Walker walker))
  (%%list (zen:get-dialect-bindings (zen:get-dialect 'foundation))))


(define (zen:walker-declaration-environment walker)
  (or (zen:get-walker-declarations walker)
      (let ((environment (%%list (zen:new-walk-frame (zen:walker-declarations walker)))))
        (zen:set-walker-declarations walker environment)
        environment)))


(define (zen:walker-environment walker)
  (or (zen:get-walker-bindings walker)
      (let ((environment (%%list (zen:new-walk-frame (zen:walker-bindings walker)))))
        (zen:set-walker-bindings walker environment)
        environment)))


;;;
;;;; Dependencies
;;;


(define (zen:register-autoload-declaration module-declaration autoload-declaration)
  (let ((declarations (zen:get-module-declaration-walker-autoloads module-declaration)))
    (%%when (%%not (%%memq autoload-declaration declarations))
      (zen:set-module-declaration-walker-autoloads module-declaration (%%cons autoload-declaration declarations)))))


;;;
;;;; Lookup
;;;


(define (zen:lookup-reference walker resume declaration environment symbol)
  (or (zen:lookup-symbol walker resume declaration environment symbol)
      (zen:walk-unresolved walker resume declaration symbol)))


;;;
;;;; Expression
;;;


(zen:define-class zen:Expression zen:Object ()
  ((type   initialize: #f getter: generate)
   (source                getter: generate)))


(zen:define-virtual (zen:emit-expression (zen:Expression expression) declaration walker resume environment))
(zen:define-virtual (zen:emit-call (zen:Expression expression) arguments arguments-codes declaration walker resume environment))


(zen:define-method (zen:emit-expression (zen:Expression expression) declaration walker resume environment)
  (zen:error "Unable to emit code for: {s}" expression))


(zen:define-method (zen:emit-call (zen:Expression expression) arguments arguments-codes declaration walker resume environment)
  (let ((operator (zen:emit-expression expression declaration walker resume environment)))
    (zen:new-code
      `(,(zen:sourcified-form operator) ,@(zen:codes-forms arguments-codes))
      zen:Any
      #f)))


(define (zen:emit-one-expression expression declaration walker resume environment)
  (zen:emit-expression expression declaration walker resume environment))


(define (zen:emit-expressions expressions declaration walker resume environment)
  (map (lambda (expression)
         (zen:emit-expression expression declaration walker resume environment))
       expressions))


;; quick hack
(define (zen:call-emit-expression expr declaration walker resume environment)
  (zen:emit-expression expr declaration walker resume environment))


(define (zen:tree-fold-list ls down up here seed environment)
  (if (null? ls)
      seed
    (zen:tree-fold-list (cdr ls) down up here (zen:tree-fold (car ls) down up here seed environment) environment)))


(define (zen:present-expression-location expression-src operator-src)
  (define (present expr-src)
    (and expr-src
         (let ((src (if (zen:syntactic-closure? expr-src)
                        (zen:get-syntactic-closure-expression expr-src)
                      expr-src)))
           (if (%%source? src)
               (let ((location (zen:locat->container/line/col (zen:source-locat src))))
                 (%%string->symbol (%%string-append "@" (%%number->string (%%fx+ (%%cadr location) 1)) "." (%%number->string (%%fx+ (%%car (%%cddr location)) 1)))))
             #f))))
  
  (or (present expression-src)
      (present operator-src)
      ""))


;;;
;;;; Binding Reference
;;;


(zen:define-class zen:Binding-Reference zen:Expression (constructor: zen:allocate-binding-reference)
  ((binding getter: generate)))


(define (zen:new-binding-reference symbol-src binding)
  (zen:allocate-binding-reference #f symbol-src binding))


(zen:define-method (zen:emit-expression (zen:Binding-Reference expression) declaration walker resume environment)
  (zen:sourcify-code (zen:emit-binding-reference (zen:get-binding-reference-binding expression) declaration walker resume environment)
                      (zen:get-expression-source expression)))


(zen:define-method (zen:emit-call (zen:Binding-Reference expression) arguments arguments-codes declaration walker resume environment)
  (zen:sourcify-code (zen:emit-binding-call (zen:get-binding-reference-binding expression) (zen:get-expression-source expression) arguments arguments-codes declaration walker resume environment)
                      (zen:get-expression-source expression)))


;;;
;;;; Body
;;;


(define zen:stack-frame-body
  (make-parameter #f))


(zen:define-class zen:Body zen:Expression (constructor: zen:allocate-body)
  ((internal-proclaims getter: generate)
   (internal-defines   getter: generate)
   (expressions        getter: generate)
   (stack-frame        getter: generate setter: generate)))


(define (zen:new-body internal-proclaims internal-defines expressions)
  (zen:allocate-body #f #f internal-proclaims internal-defines expressions #f))


(zen:define-method (zen:emit-expression (zen:Body expression) declaration walker resume environment)
  (let ((internal-proclaims (zen:get-body-internal-proclaims expression)))
    (define (emit)
      (let ((internal-defines (zen:get-body-internal-defines expression))
            (expressions (zen:get-body-expressions expression)))
        (zen:with-annotated-frame (zen:annotate-internal-defines internal-defines)
          (lambda (frame)
            (let ((augmented-environment (%%cons frame environment)))
              (let ((internal-defines (zen:emit-expressions internal-defines declaration walker resume augmented-environment))
                    (expressions (zen:emit-expressions expressions declaration walker resume augmented-environment)))
                (zen:new-code
                  (let ((forms (%%append (zen:codes-forms internal-defines)
                                         (zen:codes-forms expressions)))
                        (stack-frame (zen:get-body-stack-frame expression)))
                    (if (not stack-frame)
                        forms
                      `((%%stack-frame ,stack-frame ,@forms))))
                  (if (%%null? expressions) zen:Any (zen:get-code-type (zen:last expressions)))
                  #f)))))))
    
    (if (%%null? internal-proclaims)
        (emit)
      (parameterize ((zen:current-proclaims (table-copy (zen:effective-proclaims))))
        (for-each zen:proclaim internal-proclaims)
        (emit)))))


(zen:define-method (zen:tree-fold (zen:Body expression) down up here seed environment)
  (up expression
      seed
      (zen:tree-fold-list
        (zen:get-body-expressions expression) down up here
        (zen:tree-fold-list
          (zen:get-body-internal-defines expression) down up here
          (down expression seed environment)
          environment)
        environment)
      environment))


;;;
;;;; Internal-Define
;;;


(zen:define-variable zen:emit-unsafe)
(zen:define-variable zen:emit-safe)
(zen:define-variable zen:emit-safe/unsafe)


(zen:define-class zen:Internal-Define zen:Expression (constructor: zen:allocate-internal-define)
  ((variable getter: generate)
   (value    getter: generate)))


(define (zen:new-internal-define source variable value type)
  (zen:allocate-internal-define type source variable value))


(zen:define-method (zen:emit-expression (zen:Internal-Define expression) declaration walker resume environment)
  (let ((variable (zen:get-internal-define-variable expression))
        (value (zen:get-internal-define-value expression))
        (type (zen:get-expression-type expression))
        (source (zen:get-expression-source expression)))
    (let ((type (zen:get-lexical-binding-type variable)))
      (if (and (%%is? type zen:Function-Type)
               ;; safe first iteration simplification
               (zen:only-positional-function-type? type)
               (zen:typed-function-type? type))
          (zen:new-code
            `(define ,(zen:emit-binding-symbol variable declaration environment)
               ,(zen:sourcified-form (zen:emit-unsafe value source declaration walker resume environment)))
            type
            source)
        (zen:new-code
          `(define ,(zen:emit-binding-symbol variable declaration environment)
             ,(zen:sourcified-form (zen:emit-expression value declaration walker resume environment)))
          type
          source)))))


(zen:define-method (zen:tree-fold (zen:Internal-Define expression) down up here seed environment)
  (up expression
      seed
      (zen:tree-fold
        (zen:get-internal-define-value expression) down up here
        (down expression seed environment)
        environment)
      environment))


;;;
;;;; Internal-Define-Variable
;;;


(zen:define-class zen:Internal-Define-Variable zen:Variable (constructor: zen:allocate-internal-define-variable)
  ((signature getter: generate)))


(define (zen:new-internal-define-variable name type source specifier-source signature)
  (%%assertion (zen:variable-name-valid? name) (zen:error "Invalid variable name: {s}" (zen:desourcify-all name))
    (zen:allocate-internal-define-variable name type #f #f source specifier-source 0 signature)))


(zen:define-method (zen:walk-binding-validate-call (zen:Internal-Define-Variable declaration) walker resume source-declaration operator arguments form-src)
  (let ((signature (zen:get-internal-define-variable-signature declaration)))
    (if signature
        (zen:validate-arguments walker resume source-declaration declaration signature arguments form-src))))


;;;
;;;; Begin
;;;


(zen:define-class zen:Begin zen:Expression (constructor: zen:allocate-begin)
  ((expressions getter: generate)))


(define (zen:new-begin source expressions)
  (zen:allocate-begin #f source expressions))


(zen:define-method (zen:emit-expression (zen:Begin expression) declaration walker resume environment)
  (let ((expressions (zen:get-begin-expressions expression)))
    (let ((code (zen:emit-statements-code expressions declaration walker resume environment)))
      (zen:new-code
        `(begin ,@(zen:sourcified-form code))
        (zen:get-code-type code)
        (zen:get-expression-source expression)))))


(zen:define-method (zen:tree-fold (zen:Begin expression) down up here seed environment)
  (up expression
      seed
      (zen:tree-fold-list
        (zen:get-begin-expressions expression) down up here
        (down expression seed environment)
        environment)
      environment))


;;;
;;;; Declare
;;;


(zen:define-variable zen:declare-class)


;;;
;;;; Call
;;;


(zen:define-variable zen:emit-specialized-call)
(zen:define-variable zen:emit-new-call)
(zen:define-variable zen:emit-primitive-call)
(zen:define-variable zen:emit-inlined-call)
(zen:define-variable zen:emit-unsafe-call)


(zen:define-class zen:Call zen:Expression (constructor: zen:allocate-call)
  ((operator  getter: generate)
   (arguments getter: generate)))


(define (zen:new-call source operator arguments)
  (zen:allocate-call #f source operator arguments))


(define (zen:call-emit expression declaration walker resume environment)
  (let ((operator (zen:get-call-operator expression))
        (arguments (zen:get-call-arguments expression)))
    (let ((locator (if (%%class-is? operator zen:Binding-Reference)
                       (let ((binding (zen:get-binding-reference-binding operator)))
                         (if (%%class-is? binding zen:Declaration)
                             (zen:get-declaration-locator binding)
                           #f))
                     #f))
          (arguments-codes (zen:emit-expressions arguments declaration walker resume environment)))
      (zen:sourcify-code
        (or (zen:emit-specialized-call operator locator arguments arguments-codes expression declaration walker resume environment)
            (zen:emit-new-call operator locator arguments arguments-codes declaration walker resume environment)
            (zen:emit-primitive-call operator locator arguments arguments-codes declaration walker resume environment)
            (zen:emit-inlined-call operator arguments-codes expression declaration walker resume environment)
            (and zen:emit-unsafe-call
                 (zen:emit-unsafe-call operator locator arguments arguments-codes declaration walker resume environment))
            (zen:emit-call operator arguments arguments-codes declaration walker resume environment))
        (zen:get-expression-source expression)))))


(zen:define-method (zen:emit-expression (zen:Call expression) declaration walker resume environment)
  (or (zen:call-emit expression declaration walker resume environment)
      (let ((operator (zen:get-call-operator expression))
            (arguments (zen:get-call-arguments expression)))
        (let ((arguments-codes (zen:emit-expressions arguments declaration walker resume environment)))
          (zen:sourcify-code
            (zen:emit-call operator arguments arguments-codes declaration walker resume environment)
            (zen:get-expression-source expression))))))


(zen:define-method (zen:tree-fold (zen:Call expression) down up here seed environment)
  (up expression
      seed
      (zen:tree-fold-list
        (cons (zen:get-call-operator expression) (zen:get-call-arguments expression))
        down up here
        (down expression seed environment)
        environment)
      environment))


(define (zen:walk-call walker resume declaration environment procedure-binding form-src)
  (let ((operator (%%car (zen:source-code form-src)))
        (arguments (%%cdr (zen:source-code form-src))))
    (if procedure-binding
        (zen:walk-binding-validate-call procedure-binding walker resume declaration operator (zen:desourcify-all arguments) form-src))
    (zen:new-call form-src
                   (continuation-capture
                     (lambda (resume)
                       (zen:walk walker resume declaration environment operator)))
                   (zen:walk-list walker resume declaration environment arguments))))


(define (zen:validate-arguments walker resume source-declaration declaration signature arguments form-src)
  (let ((mandatory (zen:get-signature-mandatory signature))
        (rest (zen:get-signature-rest signature))
        (passed (%%length arguments))
        (name (zen:get-lexical-binding-name declaration))
        ;; todo improve handling of optional and named parameters
        (rest? (or (zen:get-signature-rest signature)
                   (%%not-null? (zen:get-signature-optional signature))
                   (%%not-null? (zen:get-signature-named signature)))))
    (cond ((and (%%not rest?) (%%fx> passed mandatory))
           (zen:walk-error walker resume source-declaration form-src "Too many arguments for: {a}" name))
          ((%%fx< passed mandatory)
           (zen:walk-error walker resume source-declaration form-src "Not enough arguments for: {a}" name)))))


;;;
;;;; Constant
;;;


(zen:define-class zen:Constant zen:Expression (constructor: zen:allocate-constant)
  ((expansion getter: generate)))


(define (zen:new-constant expansion type)
  (zen:allocate-constant type #f expansion))


(zen:define-method (zen:emit-expression (zen:Constant expression) declaration walker resume environment)
  (zen:new-code
    (zen:get-constant-expansion expression)
    (zen:get-expression-type expression)
    #f))


(define (zen:walk-cond-expand walker resume declaration environment form-src)
  (zen:process-cond-expand walker resume declaration form-src
    (lambda (form form?)
      (zen:walk walker resume declaration environment (or form '(unspecified))))))


(define (zen:walk-quote walker resume declaration environment form-src)
  (define (scheme-data? expr)
    (or (%%null? expr)
        (%%boolean? expr)
        (%%char? expr)
        (%%string? expr)
        (%%keyword? expr)
        (%%number? expr)
        (%%symbol? expr)
        (%%vector? expr)
        (and (%%pair? expr)
             (scheme-data? (%%car expr))
             (scheme-data? (%%cdr expr)))))
  
  (let ((form (%%cdr (zen:strip-syntactic-closures form-src))))
    (if (or (%%null? form)
            (%%not-null? (%%cdr form)))
        (zen:walk-error walker resume declaration form-src "Ill-formed quote")
      (let ((expression (%%car form)))
        (if (scheme-data? expression)
            (zen:walk-constant walker resume declaration environment expression)
          (let ((module-declaration (zen:get-declaration-toplevel declaration))
                (locator (zen:generate-global-symbol "lit")))
            ;; it is important to register before walking so subliterals come before us
            (let ((info (%%cons locator #f)))
              (zen:set-module-declaration-walker-literals module-declaration (%%cons info (zen:get-module-declaration-walker-literals module-declaration)))
              (%%set-cdr! info (zen:walk-quasiquote walker resume declaration environment form-src)))
            (zen:new-constant locator zen:Any)))))))


(define (zen:walk-keyword walker keyword)
  (zen:new-constant keyword zen:Keyword))


(define (zen:walk-enumerator walker enumerator)
  (zen:new-constant (%%list 'quote enumerator) zen:Symbol))


(define (zen:walk-constant walker resume declaration environment form-src)
  (let ((form (zen:source-code form-src)))
    (cond ((%%boolean? form)
           (zen:new-constant form-src zen:Boolean))
          ((%%char? form)
           (zen:new-constant form-src zen:Char))
          ((%%string? form)
           (zen:new-constant form-src zen:String))
          ((%%keyword? form)
           (zen:new-constant form-src zen:Keyword))
          ((%%fixnum? form)
           ;; quick hack to minimize the danger of the fixnum
           ;; primitive patterns that are not formally correct
           (if (%%fx<= form 4096)
               (zen:new-constant form-src zen:Fixnum)
             (zen:new-constant form-src zen:Integer)))
          ((%%ratnum? form)
           (zen:new-constant form-src zen:Ratnum))
          ((%%flonum? form)
           (zen:new-constant form-src zen:Flonum))
          ((%%complex? form)
           (zen:new-constant form-src zen:Complex))
          ((%%number? form)
           (zen:new-constant form-src zen:Number))
          ((%%symbol? form)
           (zen:new-constant `(quote ,form-src) zen:Symbol))
          ((%%null? form)
           (zen:new-constant `(quote ,form-src) zen:Null))
          ((%%pair? form)
           (zen:new-constant `(quote ,form-src) zen:Pair))
          ((%%vector? form)
           (zen:new-constant `(quote ,form-src) zen:Vector))
          ((%%s8vector? form)
           (zen:new-constant `(quote ,form-src) zen:S8Vector))
          ((%%u8vector? form)
           (zen:new-constant `(quote ,form-src) zen:U8Vector))
          ((%%s16vector? form)
           (zen:new-constant `(quote ,form-src) zen:S16Vector))
          ((%%u16vector? form)
           (zen:new-constant `(quote ,form-src) zen:U16Vector))
          ((%%s32vector? form)
           (zen:new-constant `(quote ,form-src) zen:S32Vector))
          ((%%u32vector? form)
           (zen:new-constant `(quote ,form-src) zen:U32Vector))
          ((%%s64vector? form)
           (zen:new-constant `(quote ,form-src) zen:S64Vector))
          ((%%u64vector? form)
           (zen:new-constant `(quote ,form-src) zen:U64Vector))
          ((%%f32vector? form)
           (zen:new-constant `(quote ,form-src) zen:F32Vector))
          ((%%f64vector? form)
           (zen:new-constant `(quote ,form-src) zen:F64Vector))
          ((%%values? form)
           (zen:new-constant `(quote ,form-src) zen:Values))
          ((or (%%box? form)
               (%%eq? form #!optional)
               (%%eq? form #!key)
               (%%eq? form #!rest)
               (%%eq? form #!void))
           (zen:new-constant `(quote ,form-src) zen:Any))
          ((%%is? form zen:Literal)
           (zen:walk-literal-constant walker resume declaration environment form))
          (else
           (zen:walk-error walker resume declaration #f "Unable to walk constant: {s}" form)))))


;; extracted from zen:walk logic
(define (zen:constant? form)
  (%%not (or (%%symbol? form)
             (%%pair? form))))


;;;
;;;; Quasiquote
;;;


(zen:define-class zen:Quasiquote zen:Expression (constructor: zen:allocate-quasiquote)
  ((form        getter: generate)
   (expressions getter: generate)))


(define (zen:new-quasiquote form expressions)
  (zen:allocate-quasiquote #f #f form expressions))


(zen:define-method (zen:emit-expression (zen:Quasiquote expression) declaration walker resume environment)
  (define (emit form)
    (if (%%pair? form)
        (if (or (%%eq? (%%car form) 'unquote)
                (%%eq? (%%car form) 'unquote-splicing))
            (%%list (%%car form) (zen:sourcified-form (zen:emit-expression (%%cadr form) declaration walker resume environment)))
          (%%cons (emit (%%car form)) (emit (%%cdr form))))
      form))
  
  (zen:new-code
    (%%list 'quasiquote (emit (zen:get-quasiquote-form expression)))
    zen:List
    #f))


(define (zen:walk-quasiquote walker resume declaration environment form-src)
  (let ((expressions (zen:new-queue)))
    (define (walk form-src)
      (cond ((%%pair? (zen:source-code form-src))
             (let ((head (%%car (zen:source-code form-src)))
                   (tail (%%cdr (zen:source-code form-src))))
               (let ((first (zen:source-code head)))
                 (if (or (%%eq? first 'unquote)
                         (%%eq? first 'unquote-splicing))
                     (let ((expr (zen:walk walker resume declaration environment (%%cadr (zen:source-code form-src)))))
                       (zen:enqueue expressions expr)
                       (%%list first expr))
                   (%%cons (walk-car head) (walk-cdr tail))))))
            ((%%is? (zen:source-code form-src) zen:Literal)
             (%%list 'unquote (zen:walk walker resume declaration environment form-src)))
            (else
             form-src)))
    
    (define (walk-car form-src)
      (if (%%is? (zen:source-code form-src) zen:Literal)
          (%%list 'unquote (zen:walk walker resume declaration environment form-src))
        (walk form-src)))
    
    (define (walk-cdr form-src)
      (if (%%is? (zen:source-code form-src) zen:Literal)
          (%%list (%%list 'unquote-splicing (zen:walk walker resume declaration environment form-src)))
        (walk form-src)))
    
    (let ((form (walk (%%cadr (zen:source-code form-src)))))
      (zen:new-quasiquote form (zen:queue-list expressions)))))


(zen:define-method (zen:tree-fold (zen:Quasiquote expression) down up here seed environment)
  (up expression
      seed
      (zen:tree-fold-list
        (zen:get-quasiquote-expressions expression) down up here (down expression seed environment) environment)
      environment))


;;;
;;;; Assignment
;;;


(zen:define-class zen:Assignment zen:Expression (constructor: zen:allocate-assignment)
  ((binding       getter: generate)
   (value         getter: generate)
   (symbol-source getter: generate)))


(define (zen:new-assignment source binding value symbol-src)
  (zen:allocate-assignment #f source binding value symbol-src))


(zen:define-method (zen:emit-expression (zen:Assignment expression) declaration walker resume environment)
  (zen:emit-binding-assignment (zen:get-assignment-binding expression) (zen:get-assignment-value expression) declaration walker resume environment (zen:get-expression-source expression)))


(zen:define-method (zen:tree-fold (zen:Assignment expression) down up here seed environment)
  (up expression
      seed
      (zen:tree-fold (zen:get-assignment-value expression) down up here (down expression seed environment) environment)
      environment))


(zen:define-method (zen:walk-symbol-assignment (zen:Walker walker) resume declaration environment symbol-src value form-src)
  (let ((binding (zen:lookup-symbol walker resume declaration environment symbol-src)))
    (if binding
        (begin
          (zen:walk-binding-validate-assignment binding walker resume declaration symbol-src)
          (zen:new-assignment form-src binding (zen:walk walker resume declaration environment value) symbol-src))
      (zen:walk-free-assignment walker resume declaration symbol-src))))


(define (zen:walk-free-assignment walker resume declaration symbol-src)
  (zen:walk-unresolved walker resume declaration symbol-src))


;;;
;;;; Special Expression
;;;


(zen:define-class zen:Special-Expression zen:Expression (constructor: zen:allocate-special-expression)
  ((emit getter: generate)))


(define (zen:new-special-expression emit)
  (zen:allocate-special-expression #f #f emit))


(zen:define-method (zen:emit-expression (zen:Special-Expression expression) declaration walker resume environment)
  (let ((emit (zen:get-special-expression-emit expression)))
    (emit expression declaration environment)))


;;;
;;;; Walk-Failed Special
;;;


(zen:define-class zen:Walk-Failed-Special zen:Expression (constructor: zen:allocate-walk-failed)
  ((answer getter: generate)))


(define (zen:new-walk-failed-special answer)
  (zen:allocate-walk-failed #f #f answer))


(zen:define-method (zen:emit-expression (zen:Walk-Failed-Special expression) declaration walker resume environment)
  (let ((answer (zen:get-walk-failed-special-answer expression)))
    (zen:new-code
      answer
      zen:Boolean
      #f)))


;;;
;;;; Analysis Data
;;;


(zen:define-class zen:Analysis-Data zen:Object (constructor: zen:allocate-analysis-data)
  ((autoload-reference     getter: generate setter: generate)
   (declaration-references getter: generate setter: generate)))


(define (zen:new-analysis-data)
  (zen:allocate-analysis-data #f #f))


(define (zen:get-analysis-data locator)
  (or (%%table-ref zen:analysis-data locator #f)
      (let ((data (zen:new-analysis-data)))
        (%%table-set! zen:analysis-data locator data)
        data)))


;;;
;;;; Statement
;;;


(define (zen:emit-namespace-statements statements declaration walker resume environment)
  (let ((queue (zen:new-queue)))
    (for-each (lambda (statement)
                (if (%%class-is? statement zen:Declaration)
                    (zen:enqueue queue (zen:emit-declaration statement walker resume environment))
                  (let ((code (zen:emit-expression statement declaration walker resume environment)))
                    (if code
                        (zen:enqueue queue (zen:sourcified-form code))))))
              statements)
    (zen:queue-list queue)))


(define (zen:emit-statements-code statements declaration walker resume environment)
  (let ((last-type #f))
    (let ((emited
            (map (lambda (statement)
                   (if (%%class-is? statement zen:Declaration)
                       (zen:emit-declaration statement walker resume environment)
                     (let ((code (zen:emit-expression statement declaration walker resume environment)))
                       (set! last-type (zen:get-code-type code))
                       (zen:sourcified-form code))))
                 statements)))
      (zen:new-code emited last-type #f))))


;;;
;;;; Walk
;;;


(define (zen:walk walker resume declaration environment form-src)
  (let ((form (zen:source-code form-src)))
    (cond ((zen:identifier? form)
           (zen:walk-symbol walker resume declaration environment form-src))
          ((%%null? form)
           (zen:walk-error walker resume declaration form-src "Ill-formed expression: ()"))
          ((%%pair? form)
           (zen:walk-form walker resume declaration environment form-src))
          ((zen:syntactic-closure? form)
           (zen:walk walker resume declaration (append (zen:get-syntactic-closure-environment form) environment) (zen:get-syntactic-closure-form form)))
          (else
           (zen:walk-constant walker resume declaration environment form-src)))))


(define (zen:walk-list walker resume declaration environment form-list)
  (let ((queue (zen:new-queue)))
    (for-each (lambda (form)
                (continuation-capture
                  (lambda (resume)
                    (zen:enqueue queue (zen:walk walker resume declaration environment form)))))
              form-list)
    (zen:queue-list queue)))


(define (zen:walk-body walker resume declaration environment form-list)
  (define (walk-internal-define environment form-src variable)
    (receive (name name-src specifier specifier-source value parameters) (zen:parse-define walker resume declaration #t form-src)
      (let ((type (if specifier (zen:walk-specifier walker resume declaration environment specifier) zen:Any))
            (signature (and parameters (zen:walk-parameters walker resume declaration environment parameters #t #f))))
        (let ((effective-type (if signature (zen:signature->function-type signature type) type)))
          (zen:new-internal-define form-src variable (zen:walk walker resume declaration environment value) effective-type)))))
  
  (let ((internal-proclaims '())
        (internal-defines '()))
    (define (process form)
      (cond ((zen:begin-form? form)
             (let ((state #f))
               (for-each (lambda (sub)
                           (let ((substate (process sub)))
                             (if (%%not state)
                                 (set! state substate)
                               (if (%%neq? substate state)
                                   (zen:error "Inconsistant internal defines")))))
                         (%%cdr (zen:source-code form)))
               state))
            ((zen:proclaim-form? form)
             (set! internal-proclaims (%%append internal-proclaims (%%cdr (%%desourcify form))))
             'defines)
            ((or (zen:define-form? form)
                 (zen:declare-form? form))
             (set! internal-defines (%%cons form internal-defines))
             'defines)
            (else
             'expressions)))
    
    (let iter ((scan form-list))
      (if (or (%%null? scan)
              (%%eq? (process (%%car scan)) 'expressions))
          (if (%%null? internal-defines)
              (zen:new-body internal-proclaims '() (zen:walk-list walker resume declaration environment scan))
            (let ((variables (zen:new-queue))
                  (augmented-environment environment)
                  (internal-defines (zen:reverse! internal-defines)))
              (for-each (lambda (internal-define)
                          (if (or (zen:declare-form? internal-define)
                                  (zen:proclaim-form? internal-define))
                              (zen:enqueue variables #f)
                            (let ((internal (%%cdr (zen:source-code internal-define))))
                              (%%assertion (%%pair? internal) (zen:walk-error walker resume declaration internal-define "Ill-formed internal define")
                                (let ((signature (zen:source-code (%%car internal))))
                                  (let ((name (if (%%symbol? signature)
                                                  signature
                                                (zen:source-code (%%car signature))))
                                        (signature (if (%%symbol? signature)
                                                       #f
                                                     (zen:walk-parameters walker resume declaration environment (%%cdr signature) #t #f))))
                                    (let ((variable (zen:new-internal-define-variable name #f #f #f signature)))
                                      (zen:enqueue variables variable)
                                      (set! augmented-environment (%%cons variable augmented-environment)))))))))
                        internal-defines)
              (zen:new-body internal-proclaims
                             (map (lambda (internal-define variable)
                                    (if (zen:declare-form? internal-define)
                                        (zen:walk walker resume declaration environment internal-define)
                                      (let ((internal-define (walk-internal-define augmented-environment internal-define variable)))
                                        ;; ideally we should specify the type when creating the variable
                                        ;; we could then remove the generated lexical-binding type setter
                                        (zen:set-lexical-binding-type variable (zen:get-expression-type internal-define))
                                        internal-define)))
                                  internal-defines
                                  (zen:queue-list variables))
                             (zen:walk-list walker resume declaration augmented-environment scan))))
        (iter (%%cdr scan))))))


(define (zen:parse-define walker resume declaration walk? form-src)
  (let ((rest (%%cdr (zen:source-code form-src))))
    (if (%%not (%%pair? rest))
        (zen:walk-error walker resume declaration form-src "Ill-formed define: {s}" (zen:desourcify-all form-src))
      (let ((first (zen:source-code (%%car rest))))
        (cond ((%%symbol? first)
               (if (%%not (%%pair? (%%cdr rest)))
                   (zen:walk-error walker resume declaration form-src "Ill-formed define: {s}" (zen:desourcify-all form-src))
                 (let ((name (zen:source-code (%%car rest)))
                       (name-src (%%car rest)))
                   (zen:parse-specifier (%%cdr rest)
                     (lambda (specifier specifier-source rest)
                       (values name name-src specifier specifier-source (and walk? (%%car rest)) #f))))))
              ((and (%%pair? first)
                    (%%symbol? (zen:source-code (%%car first))))
               (let ((name (zen:source-code (%%car (zen:source-code (%%car rest)))))
                     (name-src (%%car (zen:source-code (%%car rest))))
                     (parameters (%%cdr (zen:source-code (%%car rest)))))
                 (zen:parse-specifier (%%cdr rest)
                   (lambda (specifier specifier-source body)
                     (let ((specifier-list (if specifier (%%list specifier) '())))
                       (values name name-src specifier specifier-source `(lambda ,parameters ,@specifier-list ,@body) parameters))))))
              (else
               (zen:walk-error walker resume declaration form-src "Ill-formed define variable: {s}" (zen:desourcify-all first))))))))


;; Until I unify signature and function type
;; Only positional parameters as a first draft
(define (zen:signature->function-type signature result-type)
  (define (parameter-type parameter)
    (or (zen:get-lexical-binding-type parameter)
        zen:Any))
  
  (zen:new-function-type
    (map parameter-type (zen:get-signature-positional signature))
    (map parameter-type (zen:get-signature-optional signature))
    (map parameter-type (zen:get-signature-named signature))
    (let ((rest (zen:get-signature-rest signature)))
      (and rest (parameter-type rest)))
    (or result-type zen:Any)))


;;;
;;;; Symbol
;;;


(zen:define-method (zen:walk-symbol (zen:Walker walker) resume declaration environment symbol-src)
  (let ((symbol (zen:source-code symbol-src)))
    (let ((binding (zen:lookup-symbol walker resume declaration environment symbol-src)))
      (if binding
          (cond ((or (zen:walk-binding-walkable? binding)
                     (zen:walk-binding-expandable? binding))
                 (zen:walk-error walker resume declaration symbol-src "Illegal access to syntax: {s}" symbol))
                (else
                 (if (%%class-is? binding zen:Variable)
                     (zen:walk-binding-referenced binding))
                 (zen:new-binding-reference symbol-src binding)))
        (zen:walk-free-reference walker resume declaration symbol-src)))))


(define (zen:walk-setbang walker resume declaration environment form-src)
  (if (%%not (%%fx= (%%length (zen:source-code form-src)) 3))
      (zen:walk-error walker resume declaration form-src "Ill-formed set!: {s}" (%%desourcify form-src))
    (let ((variable (%%cadr (zen:source-code form-src)))
          (value (%%car (%%cddr (zen:source-code form-src)))))
      (if (%%symbol? (zen:unwrap-syntactic-closure variable))
          (zen:walk-symbol-assignment walker resume declaration environment variable value form-src)
        (zen:walk-error walker resume declaration variable "Illegal set! of {s}" (%%desourcify variable))))))


(define (zen:lookup-symbol walker resume declaration environment symbol-src)
  (define (validate-compatibility walker declaration referenced-declaration)
    (if (%%eq? (zen:get-declaration-compatibility referenced-declaration) 'deprecated)
        (let ((referenced-locator (zen:get-declaration-locator referenced-declaration)))
          (zen:walk-warning walker declaration symbol-src "Deprecated access to {s}" referenced-locator))))
  
  (let ((referenced-declaration
         (if (zen:syntactic-closure? symbol-src)
             (or (zen:lookup-environment walker resume declaration environment symbol-src symbol-src)
                 (zen:lookup-environment walker resume declaration (zen:get-syntactic-closure-environment symbol-src) symbol-src (zen:syntactic-closure-form symbol-src))
                 (zen:lookup-environment walker resume declaration (zen:get-syntactic-closure-environment symbol-src) symbol-src (zen:source-code (zen:syntactic-closure-form symbol-src))))
           (zen:lookup-environment walker resume declaration environment symbol-src (zen:source-code symbol-src)))))
    (if (and referenced-declaration (%%class-is? referenced-declaration zen:Declaration))
        (validate-compatibility walker declaration referenced-declaration))
    (if (%%class-is? referenced-declaration zen:Autoload-Declaration)
        (let ((module (zen:get-declaration-toplevel declaration)))
          (zen:register-autoload-declaration module referenced-declaration)))
    (if (zen:analysis-mode?)
        (zen:lookup-analyse walker declaration symbol-src referenced-declaration))
    referenced-declaration))


(zen:define-method (zen:lookup-environment (zen:Walker walker) resume declaration environment symbol-src symbol)
  (let ((raw-symbol (zen:unwrap-syntactic-closure symbol)))
    (let lp ((env environment))
      (and (%%pair? env)
           (or (zen:walk-binding-lookup (%%car env) symbol declaration)
               (lp (%%cdr env)))))))


(zen:define-method (zen:lookup-analyse (zen:Walker walker) declaration symbol-src referenced-declaration)
  #f)


;;;
;;;; Reference
;;;


(define (zen:walk-free-reference walker resume declaration symbol-src)
  (zen:walk-unresolved walker resume declaration symbol-src))


;;;
;;;; Form
;;;


(zen:define-method (zen:walk-form (zen:Walker walker) resume declaration environment form-src)
  (let ((procedure-expr (zen:source-code (%%car (zen:source-code form-src)))))
    (let ((binding (zen:lookup-procedure-binding walker resume declaration environment procedure-expr)))
      (cond ;; special form
            ((and binding (zen:walk-binding-walkable? binding))
             (zen:walk-binding-walk-form binding walker resume declaration environment form-src))
            ;; macro
            ((and binding (zen:walk-binding-expandable? binding))
             (let ((expansion (zen:walk-binding-expand-form binding walker resume declaration environment form-src)))
               (zen:walk walker resume declaration environment expansion)))
            ;; call
            (else
             (zen:walk-call walker resume declaration environment binding form-src))))))


;;;
;;;; Include
;;;


(define (zen:include-form? form)
  (and (%%pair? (zen:source-code form))
       (%%eq? (zen:source-code (%%car (zen:source-code form))) 'include)))


(define (zen:include-filename form)
  (zen:source-code (%%cadr (zen:source-code form))))


(define (zen:include-resource filename)
  (let ((pathname (zen:requested-pathname)))
    (if pathname
        (zen:pathname-brother pathname filename)
      (let ((resource (zen:requested-unit-resource)))
        (%%make-resource (%%get-resource-package resource)
                         (zen:pathname-brother (%%get-resource-path resource) filename)
                         #f
                         #f)))))


(define (zen:include-forms filename)
  (zen:read-toplevel-forms (if (or (zen:string-starts-with? filename "/")
                                    (zen:string-starts-with? filename "~"))
                                filename
                              (zen:include-resource filename))))


;;;
;;;; Require
;;;


(define (zen:walk-require-declaration walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((module-declaration (zen:get-declaration-toplevel declaration)))
      (let ((requires (zen:filter-invoices (%%cdr form))))
        (for-each (lambda (clause)
                    (%%unless (or (%%symbol? clause)
                                  (and (%%pair? clause)
                                       (%%symbol? (%%car clause))))
                      (zen:walk-error walker resume declaration form-src "Ill-formed require clause: {s}" clause)))
                  requires)
        ;; so units with errors are not added to the module when evaluating code
        (%%when (%%eq? (zen:walk-for) 'eval)
          (for-each zen:load-unit requires))
        (for-each (lambda (require)
                    (zen:add-module-require module-declaration (zen:listify require)))
                  requires)))))


(define (zen:walk-require walker resume declaration environment form-src)
  (zen:new-begin #f '()))


;;;
;;;; Import
;;;


(define (zen:walk-import-declaration walker resume declaration environment form-src)
  (define (walk-module-import import)
    (define (lookup-module name)
      (or (zen:outline-module name error?: #f)
          (zen:walk-unresolved walker resume declaration name)))
    
    (receive (module-name module-phase module-autoload module-transformations) (zen:parse-module-invoice walker resume declaration form-src import)
      (zen:new-import-invoice module-name
                               (lookup-module module-name)
                               module-phase
                               module-transformations)))
  
  (define (walk-imports imports)
    (map (lambda (import)
           (walk-module-import (zen:listify import)))
         imports))
  
  (let ((form (%%desourcify form-src)))
    (let ((module-declaration (zen:get-declaration-toplevel declaration)))
      (let ((import-invoices (walk-imports (zen:filter-invoices (%%cdr form)))))
        ;; so units with errors are not added to the module when evaluating code
        #;
        ;; commented as this is not correct and creates problems in the
        ;; worker by loading code when only doing a code walk expansion
        (%%when (%%eq? (zen:walk-for) 'eval)
          (for-each (lambda (import-invoice)
                      (let ((module-declaration (zen:get-module-invoice-module import-invoice)))
                        (zen:load-unit (zen:get-lexical-binding-name module-declaration))))
                    import-invoices))
        (for-each (lambda (import-invoice)
                    (zen:add-module-import walker resume declaration form-src module-declaration import-invoice #t))
                  import-invoices)))))


(define (zen:walk-import walker resume declaration environment form-src)
  (zen:new-begin #f '()))


;;;
;;;; Native
;;;


(define zen:native-modifiers
  '(((private protected package public) . public)
    ((deprecated undocumented uptodate) . uptodate)))

(define zen:native-keywords
  '())


(define (zen:parse-native walker resume declaration form-src)
  (define (ill-formed)
    (zen:walk-error walker resume declaration form-src "Ill-formed native: {s}" (%%desourcify form-src)))
  
  (let ((rest (%%cdr (zen:source-code form-src))))
    (receive (access compatibility modifiers rest) (zen:parse-modifiers walker resume declaration zen:native-modifiers rest)
      (if (%%null? rest)
          (ill-formed)
        (let ((name (zen:source-code (%%car rest)))
              (name-src (%%car rest)))
          (if (%%symbol? name)
              (zen:parse-specifier (%%cdr rest)
                (lambda (specifier specifier-source rest)
                  (if (%%null? rest)
                      (values name name-src specifier access compatibility modifiers)
                    (ill-formed))))
            (ill-formed)))))))


(define (zen:walk-native-declaration walker resume declaration environment form-src)
  (receive (name name-src specifier access compatibility modifiers) (zen:parse-native walker resume declaration form-src)
    (receive (name symbol) (zen:parse-exported-symbol declaration name)
      (let ((type (if specifier (zen:walk-specifier walker resume declaration environment specifier) zen:Any)))
        (let ((new-declaration (or (zen:find-declaration-child declaration name)
                                   (zen:new-export-declaration name type access compatibility modifiers '() declaration symbol))))
          (zen:set-declaration-source new-declaration form-src)
          (zen:set-declaration-name-source new-declaration name-src)
          (let ((effective-declaration (zen:add-declaration-child walker resume declaration new-declaration)))
            effective-declaration))))))


(define (zen:walk-native walker resume declaration environment form-src)
  (receive (name name-src specifier access compatibility modifiers) (zen:parse-native walker resume declaration form-src)
    (receive (name symbol) (zen:parse-exported-symbol declaration name)
      (zen:require-declaration declaration name))))


;;;
;;;; Native Syntax
;;;


(define zen:native-syntax-modifiers
  '(((private protected package public) . public)
    ((deprecated undocumented uptodate) . uptodate)))

(define zen:native-syntax-keywords
  '())


(define (zen:parse-native-syntax walker resume declaration form-src)
  (define (ill-formed)
    (zen:walk-error walker resume declaration form-src "Ill-formed native-syntax: {s}" (%%desourcify form-src)))
  
  (let ((rest (%%cdr (zen:source-code form-src))))
    (receive (access compatibility modifiers rest) (zen:parse-modifiers walker resume declaration zen:native-syntax-modifiers rest)
      (if (%%null? rest)
          (ill-formed)
        (let ((name (zen:source-code (%%car rest)))
              (name-src (%%car rest)))
          (if (%%symbol? name)
              (zen:parse-specifier (%%cdr rest)
                (lambda (specifier specifier-source rest)
                  (if (%%null? rest)
                      (values name name-src specifier access compatibility modifiers)
                    (ill-formed))))
            (ill-formed)))))))


(define (zen:walk-native-syntax-declaration walker resume declaration environment form-src)
  (receive (name name-src specifier access compatibility modifiers) (zen:parse-native-syntax walker resume declaration form-src)
    (receive (name symbol) (zen:parse-exported-symbol declaration name)
      (let ((type (if specifier (zen:walk-specifier walker resume declaration environment specifier) zen:Any)))
        (let ((new-declaration (or (zen:find-declaration-child declaration name)
                                   (zen:new-export-syntax-declaration name type access compatibility modifiers '() declaration symbol))))
          (zen:set-declaration-source new-declaration form-src)
          (zen:set-declaration-name-source new-declaration name-src)
          (let ((effective-declaration (zen:add-declaration-child walker resume declaration new-declaration)))
            effective-declaration))))))


(define (zen:walk-native-syntax walker resume declaration environment form-src)
  (receive (name name-src specifier access compatibility modifiers) (zen:parse-native-syntax walker resume declaration form-src)
    (receive (name symbol) (zen:parse-exported-symbol declaration name)
      (zen:require-declaration declaration name))))


;;;
;;;; Let-Syntax
;;;


(define (zen:walk-let-syntax walker resume declaration environment form-src)
  (let* ((bindings-src (%%cadr (zen:unwrap-syntactic-closure form-src)))
         (bindings (zen:unwrap-syntactic-closure bindings-src))
         (body (%%cddr (zen:unwrap-syntactic-closure form-src))))
    (%%assertion (or (%%null? bindings) (%%pair? bindings)) (zen:walk-error walker resume declaration bindings-src "Ill-formed let-syntax bindings: {s}" bindings)
      (let ((augmented-environment
             (append
              (map
               (lambda (binding)
                 (let* ((name (zen:source-code (%%car (zen:unwrap-syntactic-closure binding))))
                        (value (%%cadr (zen:unwrap-syntactic-closure binding)))
                        (expander-src (zen:emit-expression (zen:walk walker resume declaration environment value) declaration walker resume environment))
                        (expander (eval (zen:get-code-form expander-src))))
                   (zen:new-define-syntax-form name expander environment)))
               bindings)
              environment)))
        (zen:new-begin form-src (zen:walk-list walker resume declaration augmented-environment (zen:unwrap-syntactic-closure body)))))))


;;;
;;;; Letrec-Syntax
;;;


(define (zen:walk-letrec-syntax . args)
  (apply zen:walk-let-syntax args))


;;;
;;;; Parameters
;;;


;; symbol : standard positional parameter
;; (specifier/non-symbol/non-keyword-expression symbol) : dynamic positional parameter
;; (symbol expression) : optional parameter
;; (keyword symbol expression) : named parameter
;; . symbol : rest parameter
(define (zen:walk-parameters walker resume declaration environment parameters extended? walk? #!optional (first-positional-parameter #f))
  (let ((augmented-environment environment)
        (dsssl-section #f)
        (dynamics (zen:new-queue))
        (positionals (zen:new-queue))
        (optionals (zen:new-queue))
        (keywords (zen:new-queue))
        (expressions (zen:new-queue)))
    (define (iterate-parameters scan sections)
      (define (augment-environment expression)
        (%%when walk? (set! augmented-environment (%%cons expression augmented-environment))))
      
      (define (parameter-section parameter)
        (or dsssl-section
            (if (%%pair? parameter)
                (let ((first (zen:source-code (%%car parameter))))
                  (cond ((or (zen:specifier? first)
                             ;; quicky support for autoload expression
                             (%%pair? first))
                         'dynamic)
                        ((%%keyword? first)
                         'keyword)
                        (else
                         'optional)))
              'positional)))
      
      (define (allowed? section)
        (case section
          ((positional) #t)
          ((dynamic) extended?)
          (else extended?)))
      
      (cond ((%%pair? scan)
             (let* ((parameter-src (%%car scan))
                    (parameter (zen:source-code parameter-src)))
               (if (%%memq parameter '(#!optional #!key #!rest))
                   (begin
                     (set! dsssl-section parameter)
                     (iterate-parameters (%%cdr scan) sections))
                 (let ((section (parameter-section parameter)))
                   (cond ((%%not (allowed? section))
                          (zen:walk-error walker resume declaration parameter-src "Ill-formed lambda parameter: {s}" (zen:desourcify-all parameter-src)))
                         ((%%not (%%memq section sections))
                          (zen:walk-error walker resume declaration parameter-src "Misplaced {s} parameter {s} in {s}" section (zen:desourcify-all parameter-src) (zen:desourcify-all parameters))))
                   (case section
                     ((dynamic)
                      ;; should compare specifier to dynamic specifier
                      (let* ((specifier (zen:source-code (%%car parameter)))
                             (specifier-source (%%car parameter))
                             (code (if (zen:specifier? specifier) (zen:specifier->name specifier) (%%car parameter)))
                             (variable (zen:source-code (%%cadr parameter)))
                             (dynamic-parameter (zen:new-dynamic-parameter variable zen:Any #f specifier-source (zen:walk walker resume declaration augmented-environment code))))
                        (zen:enqueue dynamics dynamic-parameter)
                        (augment-environment dynamic-parameter))
                      (iterate-parameters (%%cdr scan) (%%memq section sections)))
                     ((positional)
                      (zen:parse-specifier (%%cdr scan)
                        (lambda (specifier specifier-source rest)
                          (define (make-positional-parameter)
                            (if (and first-positional-parameter
                                     (%%null? (zen:queue-list positionals)))
                                (first-positional-parameter parameter specifier)
                              (let ((type (if specifier (zen:walk-specifier walker resume declaration environment specifier) #f)))
                                (zen:new-parameter parameter type #f specifier-source))))
                          
                          (let ((positional-parameter (make-positional-parameter)))
                            (zen:enqueue positionals positional-parameter)
                            (augment-environment positional-parameter))
                          (iterate-parameters rest (%%memq section sections)))))
                     ((optional #!optional)
                      (zen:parse-specifier (%%cdr parameter)
                        (lambda (specifier specifier-source rest)
                          (%%when (and walk? (%%not (%%fx= (%%length rest) 1)))
                            (zen:walk-error walker resume declaration parameter-src "Ill-formed optional parameter: {s}" (zen:desourcify-all parameter-src)))
                          (let ((variable (zen:source-code (%%car parameter)))
                                (type (if specifier (zen:walk-specifier walker resume declaration environment specifier) #f))
                                (default (if walk? (zen:walk walker resume declaration augmented-environment (%%car rest)) #f)))
                            (%%when walk?
                              (zen:enqueue expressions default))
                            (let ((optional-parameter (zen:new-optional-parameter variable type #f specifier-source default)))
                              (zen:enqueue optionals optional-parameter)
                              (augment-environment optional-parameter)))))
                      (iterate-parameters (%%cdr scan) (%%memq section sections)))
                     ((keyword #!key)
                      (%%when (%%not (%%pair? (%%cdr parameter)))
                        (zen:walk-error walker resume declaration parameter-src "Ill-formed keyword parameter: {s}" (zen:desourcify-all parameter-src)))
                      (zen:parse-specifier (if (%%eq? section #!key) (%%cdr parameter) (%%cddr parameter))
                        (lambda (specifier specifier-source rest)
                          (%%when (and walk? (%%not (%%fx= (%%length rest) 1)))
                            (zen:walk-error walker resume declaration parameter-src "Ill-formed keyword parameter: {s}" (zen:desourcify-all parameter-src)))
                          (let ((keyword (if (%%eq? section #!key) (%%string->keyword (%%symbol->string (zen:source-code (%%car parameter)))) (zen:source-code (%%car parameter))))
                                (variable (if (%%eq? section #!key) (zen:source-code (%%car parameter)) (zen:source-code (%%cadr parameter))))
                                (type (if specifier (zen:walk-specifier walker resume declaration environment specifier) #f))
                                (default (if walk? (zen:walk walker resume declaration augmented-environment (%%car rest)) #f)))
                            (%%when (%%not (%%eq? (%%string->symbol (%%keyword->string keyword)) variable))
                              (zen:walk-error walker resume declaration parameter-src "Mismatched key/name for keyword parameter: {s}" (zen:desourcify-all parameter-src)))
                            (%%when walk?
                              (zen:enqueue expressions default))
                            (let ((keyword-parameter (zen:new-named-parameter variable type #f specifier-source default)))
                              (zen:enqueue keywords keyword-parameter)
                              (augment-environment keyword-parameter)))))
                      (iterate-parameters (%%cdr scan) (%%memq section sections)))
                     ((#!rest)
                      (let ((rest parameter))
                        (%%when (%%not (%%symbol? rest))
                          (zen:walk-error walker resume declaration scan "Ill-formed rest parameter: {s}" rest))
                        (let ((parameter-expression (zen:new-rest-parameter rest zen:List #f #f)))
                          (augment-environment parameter-expression)
                          parameter-expression))))))))
            ((%%not (%%null? scan))
             (let ((rest (zen:source-code scan)))
               (%%when (%%not (%%symbol? rest))
                 (zen:walk-error walker resume declaration scan "Ill-formed rest parameter: {s}" rest))
               (let ((parameter-expression (zen:new-rest-parameter rest zen:List #f #f)))
                 (augment-environment parameter-expression)
                 parameter-expression)))
            (else
             #f)))
    
    (let ((rest (iterate-parameters parameters '(dynamic positional optional #!optional keyword #!key #!rest)))
          (dynamics (zen:queue-list dynamics))
          (positionals (zen:queue-list positionals))
          (expressions (zen:queue-list expressions)))
      (let ((signature
              (zen:new-signature
                (append dynamics positionals)
                (zen:queue-list optionals)
                (zen:queue-list keywords)
                rest
                expressions)))
        (if walk?
            (values signature augmented-environment)
          signature)))))


(define (zen:emit-signature-casts signature source-declaration walker resume environment)
  (let ((queue #f))
    (define (process parameter)
      (let ((type (zen:get-lexical-binding-type parameter)))
        (if (and type (%%neq? type zen:Any) (zen:emit-check? parameter))
            (let ((cast (zen:emit-parameter-cast (zen:emit-binding-reference parameter source-declaration walker resume environment) type source-declaration walker resume environment)))
              (if cast
                  (begin
                    ;; optimize the by far more frequent case of signatures having no types
                    (if (%%not queue)
                        (set! queue (zen:new-queue)))
                    (zen:enqueue queue cast)))))))
    
    (for-each process (zen:get-signature-positional signature))
    (for-each process (zen:get-signature-optional signature))
    (for-each process (zen:get-signature-named signature))
    (if (%%not queue)
        #f
      (zen:queue-list queue))))


(define (zen:add-signature-casts signature-casts body-emit)
  (if (or (%%not signature-casts)
          (%%null? signature-casts))
      `(,body-emit)
    `((let ,signature-casts
        ,body-emit))))


(define (zen:emit-signature signature declaration walker resume environment)
  (let ((positional (zen:get-signature-positional signature))
        (optional (zen:get-signature-optional signature))
        (named (zen:get-signature-named signature))
        (rest (zen:get-signature-rest signature))
        (queue (zen:new-queue)))
    (define (emit parameter)
      (zen:enqueue queue (zen:emit-parameter parameter declaration walker resume environment)))
    
    (for-each emit positional)
    (%%when (%%not (%%null? optional))
      (zen:enqueue queue #!optional)
      (for-each emit optional))
    (%%when (%%not (%%null? named))
      (zen:enqueue queue #!key)
      (for-each emit named))
    (%%when rest
      (zen:enqueue queue #!rest)
      (emit rest))
    (zen:queue-list queue)))


;;;
;;;; Walk Failed
;;;


(define (zen:walk-walk-failed walker resume declaration environment form-src)
  (let ((form (%%cadr (%%desourcify form-src)))
        (rest (%%cddr (%%desourcify form-src))))
    (let ((problem-subclass
            (cond ((%%null? rest) zen:Walk-Problem)
                  ((%%eq? (%%car rest) ':warning) zen:Walk-Warning)
                  ((%%eq? (%%car rest) ':error) zen:Walk-Error)
                  (else (zen:error "Ill-formed walk-failed?")))))
      (let ((answer
              (zen:catch-exception-filter
                (lambda (exc)
                  #t)
                (lambda (exc)
                  (and (%%object? exc)
                       (if (%%is? exc zen:Walk-Problem)
                           (%%is? exc problem-subclass)
                         ;; because some walk problems still throw explicit errors
                         (zen:is? exc zen:Error))))
                (lambda ()
                  (parameterize ((zen:*raise-walk-problems?* #t))
                    (zen:walk walker resume declaration environment form)
                    #f)))))
        (zen:new-walk-failed-special answer)))))


;;;
;;;; Catalog
;;;


(define zen:Catalog
  (%%make-table test: eq?))


(define (zen:get-catalog-table)
  zen:Catalog)


(define (zen:get-catalog-entry unit-name)
  (%%table-ref zen:Catalog unit-name #f))


(define (zen:set-catalog-entry unit-name entry)
  (%%table-set! zen:Catalog unit-name entry))


(define (zen:set-catalog-entry-status unit-name status)
  (let ((declaration (let ((entry (zen:get-catalog-entry unit-name)))
                       (if (%%pair? entry)
                           (%%cdr entry)
                         entry))))
    (zen:set-catalog-entry unit-name (if status (%%cons status declaration) declaration))))


(define (zen:valid-catalog-entry unit-name)
  (let ((entry (zen:get-catalog-entry unit-name)))
    (if (%%pair? entry)
        (zen:circular-dependency-error unit-name (map cdr (zen:current-load-stack)))
      entry)))


(define (zen:release-catalog-entries)
  (zen:table-iterate zen:Catalog
    (lambda (unit-name entry)
      (if (%%pair? entry)
          (zen:set-catalog-entry unit-name (%%cdr entry))))))


(define (zen:call-with-catalog-entry-lock unit-name thunk)
  (zen:call-with-load-lock
    (lambda ()
      (let ((entry (zen:get-catalog-entry unit-name)))
        (if (%%pair? entry)
            (zen:circular-dependency-error unit-name (map cdr (zen:current-load-stack)))
          (parameterize ((zen:current-load-stack (%%cons (%%cons ':walk unit-name) (zen:current-load-stack))))
            (dynamic-wind
              (lambda ()
                (zen:set-catalog-entry-status unit-name ':walking))
              thunk
              (lambda ()
                (if (%%pair? (zen:get-catalog-entry unit-name))
                    (zen:set-catalog-entry-status unit-name #f))))))))))


(define zen:outline-feedback
  (%%make-parameter #f))


(define (zen:bin->otl bin)
  (let ((path (%%get-resource-path bin)))
    (%%make-resource (%%get-resource-package bin)
                     (zen:pathname-brother path (zen:add-extension (zen:pathname-base path) "otl"))
                     (%%get-resource-underscore? bin)
                     #f)))


(define zen:outline-not-found-hook
  #f)

(define (zen:get-outline-not-found-hook)
  zen:outline-not-found-hook)

(define (zen:set-outline-not-found-hook hook)
  (set! zen:outline-not-found-hook hook))


(define zen:outline-hook
  #f)

(define (zen:get-outline-hook)
  zen:outline-hook)

(define (zen:set-outline-hook hook)
  (set! zen:outline-hook hook))


(define (zen:outline-unit unit-name #!key (error? #t))
  (define (load-toplevel-declaration)
    (let try-again ((first-try? #t))
      (zen:with-unit-resources unit-name #f
        (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
          (let ((src (if (%%not src)
                         (if (%%not bin)
                             #f
                           (let ((otl (zen:bin->otl bin)))
                             (let ((otl-path (zen:resource-pathname otl)))
                               (if (file-exists? otl-path)
                                   otl
                                 #f))))
                       src)))
            (if (%%not src)
                (if (and zen:unit-not-found-hook first-try?)
                    (begin
                      (zen:outline-not-found-hook unit-name)
                      (try-again #f))
                  (if (%%not error?)
                      #f
                    (raise (zen:new-walk-source-not-found (zen:format "Unable to locate unit source: {s}" unit-name) unit-name))))
              (zen:with-verbose (zen:outline-verbose?) "outlining" (zen:resource-pathname src)
                (lambda ()
                  ;; not reading the literals is necessary as reading a literal will load units
                  (let ((form (zen:read-toplevel-form src read-literals?: #f)))
                    (parameterize ((zen:requested-unit-name unit-name)
                                   (zen:requested-unit-resource src)
                                   (zen:requested-pathname #f)
                                   (zen:walk-for 'interpret)
                                   (zen:generate-symbol-for "%outline^")
                                   (zen:generate-symbol-context unit-name))
                      (let ((kind (zen:source-code (%%car (zen:source-code form)))))
                        (case kind
                          ((unit)
                           (zen:parse-unit-declaration (%%cdr (zen:source-code form))))
                          ((module)
                           (zen:parse-module-declaration (%%cdr (zen:source-code form))))))))))))))))
  
  (or (let ((declaration (zen:get-catalog-entry unit-name)))
        (if (%%pair? declaration)
            #f
          declaration))
      (zen:call-with-catalog-entry-lock unit-name
        (lambda ()
          (let ((feedback (zen:outline-feedback)))
            (if feedback
                (feedback unit-name)))
          (let ((declaration (load-toplevel-declaration)))
            (if (%%not declaration)
                (if error?
                    (zen:error "Unable to locate unit declaration: {s}" unit-name))
              (zen:set-catalog-entry unit-name
                                      (if zen:outline-hook
                                          (zen:outline-hook unit-name declaration)
                                        declaration)))
            declaration)))))


(define (zen:outline-module unit-name #!key (error? #t))
  (let ((declaration (zen:outline-unit unit-name error?: error?)))
    (%%assert (or (%%not declaration) (%%class-is? declaration zen:Module-Declaration))
      declaration)))


(define zen:read-literals?
  (%%make-parameter #t))


(define (zen:read-toplevel-forms pathname/resource #!key (read-literals? #t))
  (let ((source (if (%%string? pathname/resource) pathname/resource (zen:resource-pathname pathname/resource))))
    (zen:with-extension-reader (zen:pathname-extension source)
      (lambda ()
        (let ((char-encoding (if (%%string? pathname/resource) zen:default-char-encoding (zen:resource-char-encoding pathname/resource)))
              (eol-encoding 'cr-lf))
          (call-with-input-file (%%list path: source char-encoding: char-encoding eol-encoding: eol-encoding)
            (lambda (port)
              (parameterize ((zen:read-literals? read-literals?))
                (zen:read-source-all port)))))))))


(define (zen:read-toplevel-form pathname/resource #!key (read-literals? #t) (script? #f))
  (let ((source (if (%%string? pathname/resource) pathname/resource (zen:resource-pathname pathname/resource)))
        (all (zen:read-toplevel-forms pathname/resource read-literals?: read-literals?)))
    (if (%%null? all)
        (zen:error "Found empty unit declaration in {a}" source)
      (let ((form-src (%%car all))
            (extraneous? (%%not-null? (%%cdr all))))
        (if (and (%%pair? (zen:source-code form-src)) (%%memq (zen:source-code (%%car (zen:source-code form-src))) (if script? '(script) '(unit module))))
            (if (%%not extraneous?)
                form-src
              (zen:error "Found extraneous expressions after unit declaration in {a}" source))
          (zen:error "Found invalid unit declaration in {a}" source))))))


(define (zen:walk-unit unit-name)
  (zen:generate-unit unit-name #f))


;; useful for units that cannot be walked
(define (zen:walk/outline-unit unit-name)
  (or (zen:walk-unit unit-name)
      (zen:outline-unit unit-name)))


(define (zen:generate-unit unit-name #!optional (emit? #t))
  (let ((src (zen:find-unit-src unit-name)))
    (parameterize ((zen:requested-unit-name unit-name)
                   (zen:requested-unit-resource src)
                   (zen:requested-pathname #f)
                   (zen:walk-for 'interpret)
                   (zen:generate-symbol-for "%")
                   (zen:generate-symbol-context unit-name))
      (let ((form (zen:read-toplevel-form src)))
        (case (zen:source-code (%%car (zen:source-code form)))
          ((unit)
           #f)
          ((module)
           (zen:generate-module (%%cdr (zen:source-code form)) emit?)))))))


;;;
;;;; Access
;;;


;; to debug sourcification
(define (resourcify)
  (define (transform-src src)
    (let ((code (cadr src))
          (line (- (caddr src) 1))
          (col (- (cadddr src) 1)))
      (##make-source (transform code) (##make-locat  "/Users/cartier/Devel/together/app/devel/zen/lib/zen.test/src/zen/test/_test.zen" (##make-filepos line col 0)))))
  
  (define (transform-list lst)
    (cons (transform (car lst))
          (transform (cdr lst))))
  
  (define (transform obj)
    (cond ((and (pair? obj)
                (eq? (car obj) 'source))
           (transform-src obj))
          ((%%pair? obj)
           (transform-list obj))
          (else
           obj)))
  
  (transform
    '(source begin 41 1)))


(zen:define-syntax module
  (lambda (form-src)
    (let ((emit (zen:generate-module (%%cdr (zen:source-code form-src)))))
      (zen:save-emit-if emit)
      emit)))


(zen:define-syntax script
  (lambda (form-src)
    (zen:generate-script (%%cdr (zen:source-code form-src)))))


;;;
;;;; Module
;;;


(zen:define-class-syntax zen:Module zen:Object (constructor: zen:allocate-module accessors-type: macro)
  ((name    getter: generate)
   (access  getter: generate)
   (exports getter: generate)
   (entries getter: generate setter: generate)))


(zen:define-class-runtime zen:Module)


(define (zen:new-module name access)
  (zen:allocate-module name access (%%make-table test: eq?) #f))


;;;
;;;; Native
;;;


(zen:define-class-syntax zen:Native zen:Field (constructor: zen:allocate-native accessors-type: macro)
  ((symbol getter: generate)))


(zen:define-class-runtime zen:Native)


(define (zen:new-native name symbol)
  (zen:allocate-native name symbol))


(define (zen:register-native module-name name symbol)
  (zen:register-module-entry module-name name (zen:new-native name symbol)))


;;;
;;;; Runtime Reference
;;;


(zen:define-class-syntax zen:Runtime-Reference zen:Object (constructor: zen:allocate-runtime-reference accessors-type: macro)
  ((resolver      getter: generate)
   (serialization getter: generate)))


(zen:define-class-runtime zen:Runtime-Reference)


(define (zen:new-runtime-reference resolver serialization)
  (zen:allocate-runtime-reference resolver serialization))


(define (zen:resolve-runtime-reference runtime-reference)
  (%%debug-assert (%%is? runtime-reference zen:Runtime-Reference)
    (let ((resolver (%%get-runtime-reference-resolver runtime-reference)))
      (resolver))))


(define (zen:serialize-runtime-reference runtime-reference)
  (%%debug-assert (%%is? runtime-reference zen:Runtime-Reference)
    (%%get-runtime-reference-serialization runtime-reference)))


(define (zen:deserialize-runtime-reference serialization)
  (define (deserialize-module-private)
    (zen:new-runtime-reference (lambda ()
                                  (let ((locator (%%cadr serialization)))
                                    (zen:load-unit (zen:reference-unit locator))
                                    (zen:global-ref locator)))
                                serialization))
  
  (define (deserialize-module-public)
    (zen:new-runtime-reference (lambda ()
                                  (let ((module-name (%%cadr serialization))
                                        (name (%%car (%%cddr serialization))))
                                    (zen:module-ref module-name name)))
                                serialization))
  
  (or (if (%%pair? serialization)
          (case (%%car serialization)
            ((module-private) (deserialize-module-private))
            ((module-public) (deserialize-module-public))
            (else #f))
        #f)
      (zen:error "Unable to deserialize runtime reference: {s}" serialization)))


;;;
;;;; Modules
;;;


(define zen:Modules
  (%%make-table test: eq?))


(define (zen:get-modules-table)
  zen:Modules)


(define (zen:register-module name access exported-modules exported-symbols)
  (let ((module (or (zen:find-module name) (zen:new-module name access))))
    (let ((exports (%%get-module-exports module)))
      (for-each (lambda (module-name)
                  (zen:table-iterate-safe (%%get-module-exports (zen:require-module module-name))
                    (lambda (name info)
                      (%%table-set! exports name info))))
                exported-modules)
      (for-each (lambda (pair)
                  (let ((name (%%car pair))
                        (info (%%cdr pair)))
                    (%%table-set! exports name info)))
                exported-symbols)
      (%%table-set! zen:Modules name module)
      module)))


(define (zen:find-module name)
  (%%table-ref zen:Modules name #f))


(define (zen:require-module name)
  (zen:load-unit name)
  (or (zen:find-module name)
      (zen:error "Unknown module: {s}" name)))


(define (zen:get-module-entry module-name entry-name)
  (let ((entries (%%get-module-entries (zen:find-module module-name))))
    (and entries (%%table-ref entries entry-name #f))))

(define (zen:set-module-entry module-name entry-name entry)
  (let ((module (zen:find-module module-name)))
    (let ((entries (%%get-module-entries module)))
      (if entries
          (%%table-set! entries entry-name entry)
        (let ((table (%%make-table test: eq?)))
          (%%set-module-entries module table)
          (%%table-set! table entry-name entry))))))

(define (zen:register-module-entry module-name entry-name entry)
  (zen:set-module-entry module-name entry-name entry))


(define (zen:module-get module-name name #!key (not-found #f))
  (let ((module (zen:require-module module-name)))
    (let ((info (%%table-ref (%%get-module-exports module) name #f)))
      (if info
          (if (%%symbol? info)
              (zen:global-ref info)
            (zen:bind (unit-name . locator) info
              (zen:load-unit unit-name)
              (zen:global-ref locator)))
        not-found))))


(define zen:module-ref
  (let ((not-found (box #f)))
    (lambda (module-name name)
      (let ((obj (zen:module-get module-name name not-found: not-found)))
        (if (%%eq? obj not-found)
            (zen:error "Unable to find '{s} in: {s}" name module-name)
          obj)))))


(define (zen:module-set! module-name name value)
  (let ((module (zen:require-module module-name)))
    (let ((info (%%table-ref (%%get-module-exports module) name #f)))
      (if info
          (if (%%symbol? info)
              (zen:global-set! info value)
            (zen:bind (unit-name . locator) info
              (zen:load-unit unit-name)
              (zen:global-set! locator value)))
        (zen:error "Unable to find '{s} in: {s}" name module-name)))))


;;;
;;;; Error
;;;


(define (zen:type-error value type)
  (zen:error "{s} expected: {s}" type value))


(define (zen:dispatch-error field value category)
  (zen:error "Inconsistent dispatch on method {a}: {s} is not of the expected {s} type" (%%get-field-name field) value (%%get-category-identifier category))))
