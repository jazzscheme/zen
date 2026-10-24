;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Zen Dialect
;;;


(unit protected zen.dialect.runtime


;;;
;;;; Definition-Declaration
;;;


(zen:define-class zen:Definition-Declaration zen:Declaration (constructor: zen:allocate-definition-declaration)
  ((expansion        getter: generate)
   (signature        getter: generate setter: generate)
   (specifier-source getter: generate)
   (value            getter: generate setter: generate)))


(define (zen:new-definition-declaration name type access compatibility modifiers attributes parent expansion signature specifier-source)
  (let ((new-declaration (zen:allocate-definition-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f expansion signature specifier-source #f)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(zen:define-method (zen:walk-binding-validate-call (zen:Definition-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (let ((signature (zen:get-definition-declaration-signature declaration)))
    (if signature
        (zen:validate-arguments walker resume source-declaration declaration signature arguments form-src))))


(zen:define-method (zen:emit-inlined-binding-call (zen:Definition-Declaration declaration) arguments call source-declaration walker resume environment)
  (let ((value (zen:get-definition-declaration-value declaration)))
    (if (%%class-is? value zen:Lambda)
        (if (%%eq? (zen:get-definition-declaration-expansion declaration) 'inline)
            (let ((signature (zen:get-lambda-signature value))
                  (body (zen:get-lambda-body value)))
              (if (zen:only-positional-signature? signature)
                  (if (%%fx= (zen:get-signature-mandatory signature) (%%length arguments))
                      (zen:with-annotated-frame (zen:annotate-inlined-signature signature arguments)
                        (lambda (frame)
                          (let ((augmented-environment (%%cons frame environment)))
                            (let ((type (zen:call-return-type (zen:get-lexical-binding-type declaration)))
                                  (body-code (parameterize ((zen:call-being-inlined call))
                                               (zen:emit-expression body source-declaration walker resume augmented-environment))))
                              (zen:new-code
                                (zen:simplify-let
                                  `(let ,(map (lambda (parameter argument)
                                                `(,(zen:emit-binding-symbol parameter source-declaration environment)
                                                  ,(zen:emit-type-cast argument (zen:get-lexical-binding-type parameter) (zen:get-expression-source value) source-declaration walker resume environment)))
                                              (zen:get-signature-positional signature)
                                              arguments)
                                     #;
                                     ,(zen:sourcify-deep (zen:desourcify-all (zen:simplify-begin (zen:emit-return-cast (zen:new-code (zen:simplify-begin `(begin ,@(zen:get-code-form body-code))) (zen:get-code-type body-code) #f) type (zen:get-declaration-source declaration) declaration walker resume environment))) (zen:get-expression-source call))
                                     ;; an interesting idea to get the position in the inlined code but
                                     ;; it turns out it is really annoying not to be at the place of call
                                     ,(zen:simplify-begin (zen:emit-return-cast (zen:new-code (zen:simplify-begin `(begin ,@(zen:get-code-form body-code))) (zen:get-code-type body-code) #f) type (zen:get-declaration-source declaration) declaration walker resume environment))))
                                type
                                #f)))))
                    (zen:error "Wrong number of arguments passed to {s}" (zen:get-lexical-binding-name declaration)))
                (zen:error "Only positional parameters are supported in inlining: {s}" (zen:get-lexical-binding-name declaration))))
          #f)
      #f #;
      ;; not correct as the value is always #f when looking up external declarations!
      ;; we need to walk the value even at parse time for inline declarations
      (zen:error "Constant inlining is not yet supported: {s}" (zen:get-lexical-binding-name declaration)))))


;; quick solution for now as some inlined definitions like /= will change the semantics if not inlined
;; (not needed anymore now that we always inline even in debug)
(define (zen:untyped-inline-definition? value)
  (zen:every? (lambda (parameter)
                 (%%not (zen:get-lexical-binding-type parameter)))
               (zen:get-signature-positional (zen:get-lambda-signature value))))


(define (zen:definition-emit declaration walker resume environment expression unsafe-expression)
  (zen:sourcify-deep-if
    (let ((locator (zen:get-declaration-locator declaration)))
      (let ((unsafe-locator (and unsafe-expression (zen:unsafe-locator locator))))
        (zen:simplify-begin
          `(begin
             ,@(if unsafe-locator
                   `((define ,unsafe-locator
                       ,unsafe-expression))
                 '())
             (define ,locator
               ,expression)
             ,@(let ((name (zen:get-lexical-binding-name declaration))
                     (parent (zen:get-declaration-parent declaration)))
                 (if (%%is? parent zen:Module-Declaration)
                     (if (zen:get-generate? 'register)
                         `((zen:register-definition ',(zen:get-lexical-binding-name parent) ',name ',locator))
                       '())
                   `((zen:add-field ,(zen:get-declaration-locator parent) (zen:new-definition ',name ',locator)))))))))
    (zen:get-declaration-source declaration)))


(zen:define-method (zen:emit-declaration (zen:Definition-Declaration declaration) walker resume environment)
  (let ((value (zen:get-definition-declaration-value declaration))
        (type (zen:get-lexical-binding-type declaration))
        (source (zen:get-declaration-source declaration)))
    (if (and zen:debug-user?
             (%%class-is? value zen:Lambda)
             ;; no need to emit unsafe and safe when inline because as the checks are not included
             ;; in the body it is possible to define the function itself with checks so it is safe
             ;; used not inlined and only insert checks at call site when the types don't match
             (%%neq? (zen:get-definition-declaration-expansion declaration) 'inline)
             ;; safe first iteration simplification
             (zen:only-positional-signature? (zen:get-lambda-signature value))
             (zen:typed-signature? (zen:get-lambda-signature value)))
        (receive (safe-code unsafe-code) (zen:emit-safe/unsafe value source declaration walker resume environment)
          (let ((safe (zen:emit-type-cast safe-code type (zen:get-expression-source value) declaration walker resume environment))
                (unsafe (zen:emit-type-cast unsafe-code type (zen:get-expression-source value) declaration walker resume environment)))
            (zen:definition-emit declaration walker resume environment safe unsafe)))
      (let ((expression (zen:emit-type-cast (zen:emit-expression value declaration walker resume environment) type (zen:get-expression-source value) declaration walker resume environment)))
        (zen:definition-emit declaration walker resume environment expression #f)))))


(zen:define-method (zen:emit-binding-reference (zen:Definition-Declaration declaration) source-declaration walker resume environment)
  (zen:new-code
    (let ((value (zen:get-definition-declaration-value declaration)))
      (if (and (%%eq? (zen:get-definition-declaration-expansion declaration) 'inline)
               (%%is? value zen:Constant))
          (zen:get-code-form (zen:emit-expression value declaration walker resume environment))
        (zen:get-declaration-locator declaration)))
    (or (zen:find-annotated-type declaration environment)
        zen:Any)
    #f))


(zen:define-method (zen:walk-binding-validate-assignment (zen:Definition-Declaration declaration) walker resume source-declaration symbol-src)
  (nextmethod declaration walker resume source-declaration symbol-src)
  (%%when (%%neq? (zen:get-declaration-toplevel declaration) (zen:get-declaration-toplevel source-declaration))
    (zen:walk-error walker resume source-declaration symbol-src "Illegal inter-module assignment to: {s}" (zen:get-lexical-binding-name declaration))))


(zen:define-method (zen:walk-binding-assignable? (zen:Definition-Declaration declaration))
  #t)


(zen:define-method (zen:emit-binding-assignment (zen:Definition-Declaration declaration) value source-declaration walker resume environment form-src)
  (let ((value-code (zen:emit-expression value source-declaration walker resume environment)))
    (zen:new-code
      (let ((locator (zen:get-declaration-locator declaration)))
        `(set! ,locator ,(zen:emit-type-cast value-code (zen:get-lexical-binding-type declaration) source-declaration declaration walker resume environment)))
      zen:Any
      form-src)))


(zen:define-method (zen:tree-fold (zen:Definition-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (zen:tree-fold (zen:get-definition-declaration-value declaration) down up here seed environment))


(zen:define-method (zen:outline-extract (zen:Definition-Declaration declaration) meta)
  ;; not sure if this breaks together's script evaluation
  (if (%%eq? (zen:get-declaration-access declaration) 'private)
      #f
    (let ((signature (zen:get-definition-declaration-signature declaration)))
      (if (not signature)
          `(definition ,@(zen:outline-generate-modifiers declaration) ,(zen:get-lexical-binding-name declaration) ,@(zen:outline-generate-type-list (zen:get-lexical-binding-type declaration)))
        `(definition ,@(zen:outline-generate-modifiers declaration) (,(zen:get-lexical-binding-name declaration) ,@(zen:outline-generate-signature signature)) ,@(zen:outline-generate-specifier-list (zen:get-definition-declaration-specifier-source declaration)))))))


;;;
;;;; Specialize
;;;


(zen:define-class zen:Specialize zen:Expression (constructor: zen:allocate-specialize)
  ())


(define (zen:new-specialize)
  (zen:allocate-specialize #f #f))


(zen:define-method (zen:emit-expression (zen:Specialize expression) declaration walker resume environment)
  (zen:new-code
    `(begin)
    zen:Any
    #f))


;;;
;;;; Generic-Declaration
;;;


(zen:define-class zen:Generic-Declaration zen:Declaration (constructor: zen:allocate-generic-declaration)
  ((dispatch-types getter: generate)
   (signature      getter: generate setter: generate)
   (body           getter: generate setter: generate)))


(define (zen:new-generic-declaration name type access compatibility modifiers attributes parent dispatch-types signature)
  (let ((new-declaration (zen:allocate-generic-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f dispatch-types signature #f)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(zen:define-method (zen:walk-binding-validate-call (zen:Generic-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (zen:validate-arguments walker resume source-declaration declaration (zen:get-generic-declaration-signature declaration) arguments form-src))


(zen:define-method (zen:emit-declaration (zen:Generic-Declaration declaration) walker resume environment)
  (let ((signature (zen:get-generic-declaration-signature declaration))
        (body (zen:get-generic-declaration-body declaration)))
    (zen:with-annotated-frame (zen:annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (let ((signature-emit (zen:emit-signature signature declaration walker resume augmented-environment))
                (body-emit (zen:emit-expression body declaration walker resume augmented-environment)))
            (zen:sourcify-deep-if
              (let ((generic-locator (zen:get-declaration-locator declaration)))
                `(zen:define-generic ,(%%cons generic-locator signature-emit)
                  ,@(zen:sourcified-form body-emit)))
              (zen:get-declaration-source declaration))))))))


(zen:define-method (zen:emit-binding-reference (zen:Generic-Declaration declaration) source-declaration walker resume environment)
  (zen:new-code
    (zen:get-declaration-locator declaration)
    zen:Any
    #f))


(zen:define-method (zen:tree-fold (zen:Generic-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (zen:tree-fold-list (zen:get-signature-expressions (zen:get-generic-declaration-signature declaration)) down up here seed environment)
  (zen:tree-fold (zen:get-generic-declaration-body declaration) down up here seed environment))


(zen:define-method (zen:outline-extract (zen:Generic-Declaration declaration) meta)
  `(generic (,(zen:get-lexical-binding-name declaration) ,@(zen:outline-generate-signature (zen:get-generic-declaration-signature declaration)))))


;;;
;;;; Specific-Declaration
;;;


(zen:define-class zen:Specific-Declaration zen:Declaration (constructor: zen:allocate-specific-declaration)
  ((generic   getter: generate)
   (signature getter: generate)
   (body      getter: generate setter: generate)
   (root?     getter: generate)))


(define (zen:new-specific-declaration name type access compatibility modifiers attributes parent generic signature root?)
  (let ((new-declaration (zen:allocate-specific-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f generic signature #f root?)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(zen:define-method (zen:get-nextmethod-signature (zen:Specific-Declaration declaration))
  (zen:get-specific-declaration-signature declaration))


(zen:define-method (zen:emit-declaration (zen:Specific-Declaration declaration) walker resume environment)
  (let* ((generic-declaration (zen:get-specific-declaration-generic declaration))
         (generic-locator (zen:get-declaration-locator generic-declaration))
         (generic-object-locator (zen:generic-object-locator generic-locator))
         (signature (zen:get-specific-declaration-signature declaration))
         (body (zen:get-specific-declaration-body declaration)))
    (zen:with-annotated-frame (zen:annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (let ((signature-emit (zen:emit-signature signature declaration walker resume augmented-environment))
                (body-emit (zen:emit-expression body declaration walker resume augmented-environment)))
            (zen:sourcify-deep-if
              (let ((generic-declaration (zen:get-specific-declaration-generic declaration)))
                (let ((generic-locator (zen:get-declaration-locator generic-declaration))
                      (modifier (if (zen:get-specific-declaration-root? declaration) 'root 'child)))
                  `(zen:define-specific ,(%%cons generic-locator signature-emit) ,modifier
                     ,@(zen:sourcified-form body-emit))))
              (zen:get-declaration-source declaration))))))))


(zen:define-method (zen:tree-fold (zen:Specific-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (zen:tree-fold-list (zen:get-signature-expressions (zen:get-specific-declaration-signature declaration)) down up here seed environment)
  (zen:tree-fold (zen:get-specific-declaration-body declaration) down up here seed environment))


;;;
;;;; Category-Declaration
;;;


(zen:define-class zen:Category-Declaration zen:Namespace-Declaration ()
  ((implementor         getter: generate)
   (metaclass           getter: generate)
   (metaclass-explicit? getter: generate)))


(zen:define-method (zen:emit-binding-reference (zen:Category-Declaration declaration) source-declaration walker resume environment)
  (zen:new-code
    (zen:get-declaration-locator declaration)
    zen:Category-Declaration
    #f))


(zen:define-method (zen:emit-specifier (zen:Category-Declaration declaration))
  (zen:get-lexical-binding-name declaration))


;;;
;;;; Class-Declaration
;;;


(zen:define-class zen:Class-Declaration zen:Category-Declaration (constructor: zen:allocate-class-declaration)
  ((ascendant          getter: generate)
   (ascendant-relation getter: generate)
   (ascendant-base     getter: generate)
   (interfaces         getter: generate)))


(define (zen:new-class-declaration name type access compatibility modifiers attributes parent implementor metaclass metaclass-explicit? ascendant ascendant-relation ascendant-base interfaces)
  (let ((new-declaration (zen:allocate-class-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f (zen:make-access-lookups zen:protected-access) (zen:new-queue) #f implementor metaclass metaclass-explicit? ascendant ascendant-relation ascendant-base interfaces)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(define (zen:setup-class-lookups class-declaration)
  (define (resolve-class decl)
    (if decl
        (let ((class-declaration (zen:resolve-binding decl)))
          (%%assert (%%is? class-declaration zen:Class-Declaration))
          class-declaration)
      #f))
  
  (define (resolve-interface decl)
    (if decl
        (let ((interface-declaration (zen:resolve-binding decl)))
          (%%assert (%%is? interface-declaration zen:Interface-Declaration))
          interface-declaration)
      #f))
  
  (let ((ascendant (resolve-class (zen:get-class-declaration-ascendant class-declaration)))
        (interfaces (map resolve-interface (zen:get-class-declaration-interfaces class-declaration))))
    
    (let ((private (zen:get-private-lookup class-declaration)))
      (if ascendant
          (let ((same-module? (%%eq? (zen:get-declaration-toplevel class-declaration)
                                     (zen:get-declaration-toplevel ascendant))))
            (%%table-merge! private (zen:get-access-lookup ascendant (if same-module? zen:private-access zen:public-access)) #t)))
      (for-each (lambda (interface)
                  (%%table-merge! private (zen:get-public-lookup interface)))
                interfaces))
    
    (let ((public (zen:get-public-lookup class-declaration)))
      (if ascendant
          (%%table-merge! public (zen:get-public-lookup ascendant)))
      (for-each (lambda (interface)
                  (%%table-merge! public (zen:get-public-lookup interface)))
                interfaces))
    
    ;; zen:add-declaration-child does not set zen:protected-access
    (let ((not-private (zen:get-public-lookup class-declaration)))
      (%%vector-set! (zen:get-namespace-declaration-lookups class-declaration) zen:protected-access not-private))
    
    #;
    (let ((protected (zen:get-protected-lookup class-declaration)))
      (if ascendant
          (%%table-merge! protected (zen:get-public-lookup ascendant)))
      (for-each (lambda (interface)
                  (%%table-merge! protected (zen:get-public-lookup interface)))
                interfaces))))


(zen:define-method (zen:emit-binding-reference (zen:Class-Declaration declaration) source-declaration walker resume environment)
  (zen:new-code
    (zen:get-declaration-locator declaration)
    (or (zen:get-category-declaration-metaclass declaration)
        zen:Class-Declaration)
    #f))


(zen:define-method (zen:of-subtype? (zen:Class-Declaration declaration) subtype)
  (if (zen:object-declaration? declaration)
      #t
    (and (%%class-is? subtype zen:Class-Declaration)
         (let iter ((target subtype))
           (if (%%not target)
               #f
             (let ((target-declaration (zen:resolve-binding target)))
               (if (%%eq? target-declaration declaration)
                   #t
                 (iter (zen:get-class-declaration-ascendant target-declaration)))))))))


(zen:define-method (zen:specifiable? (zen:Class-Declaration declaration))
  #t)


(zen:define-method (zen:emit-declaration (zen:Class-Declaration declaration) walker resume environment)
  (let ((name (zen:get-lexical-binding-name declaration))
        (locator (zen:get-declaration-locator declaration))
        (ascendant-declaration (zen:get-class-declaration-ascendant declaration))
        (interface-declarations (zen:get-class-declaration-interfaces declaration))
        (body (zen:get-namespace-declaration-body declaration)))
    (let ((level-locator (%%compose-helper locator 'level)))
      (zen:sourcify-deep-if
        `(begin
           ,@(if (zen:core-class? name)
                 (let ((core-class (zen:get-core-class name)))
                   (if (%%not (%%symbol? core-class))
                       (zen:validate-core-class core-class declaration))
                   (let ((core-class-locator (if (%%symbol? core-class) core-class (%%get-category-identifier core-class)))
                         (ascendant-access (if (%%not ascendant-declaration) #f (zen:sourcified-form (zen:emit-binding-reference ascendant-declaration declaration walker resume environment)))))
                     `((define ,locator ,core-class-locator)
                       (define ,level-locator (%%get-class-level ,locator))
                       (%%set-category-identifier ,locator ',locator)
                       (zen:set-core-class-redefined ',name ',core-class-locator))))
               (let ((metaclass-declaration (zen:get-category-declaration-metaclass declaration)))
                 (let ((ascendant-access (zen:emit-ascendant-access declaration walker resume environment)))
                   (let ((metaclass-access (if (%%not metaclass-declaration) (if (%%not ascendant-access) 'zen:Object-Class `(%%get-object-class ,ascendant-access)) (zen:sourcified-form (zen:emit-binding-reference metaclass-declaration declaration walker resume environment))))
                         (interface-accesses (map (lambda (declaration) (zen:sourcified-form (zen:emit-binding-reference declaration declaration walker resume environment))) interface-declarations)))
                     `((define ,locator
                         ;; this is a quicky that needs to be well tought out
                         (if (zen:global-bound? ',locator)
                             (zen:global-ref ',locator)
                           (zen:new-class ,metaclass-access ',locator ,ascendant-access (%%list ,@interface-accesses))))
                       (define ,level-locator (%%get-class-level ,locator)))))))
           ,@(let ((toplevel-declaration (zen:get-declaration-toplevel declaration)))
               (if (zen:get-generate? 'register)
                   `((zen:register-module-entry ',(zen:get-lexical-binding-name toplevel-declaration) ',name ,locator))
                 '()))
           ,@(zen:emit-namespace-statements body declaration walker resume environment))
        (zen:get-declaration-source declaration)))))


(define (zen:emit-ascendant-access declaration walker resume environment)
  (let ((ascendant (zen:get-class-declaration-ascendant declaration))
        (ascendant-relation (zen:get-class-declaration-ascendant-relation declaration))
        (ascendant-base (zen:get-class-declaration-ascendant-base declaration)))
    (cond ((%%not ascendant)
           #f)
          ((%%not ascendant-relation)
           (zen:sourcified-form (zen:emit-binding-reference ascendant declaration walker resume environment)))
          (else
           (let rec ((rel ascendant-relation))
             (if (%%null? rel)
                 (zen:sourcified-form (zen:emit-binding-reference ascendant-base declaration walker resume environment))
               `(%%get-object-class ,(rec (%%cdr rel)))))))))


(define (zen:find-class-declaration declaration)
  (let iter ((decl declaration))
    (cond ((%%not decl)
           (zen:error "Unable to find class declaration for {s}" declaration))
          ((%%class-is? decl zen:Class-Declaration)
           decl)
          (else
           (iter (zen:get-declaration-parent decl))))))


(set! zen:object-declaration?
      (lambda (type)
        (and (%%class-is? type zen:Class-Declaration)
             (%%not (zen:get-class-declaration-ascendant type)))))


;;;
;;;; Validate
;;;


(define (zen:validate-core-class core-class declaration)
  (define (validate-category)
    (define (validate-ascendant)
      (let* ((core-class-ascendant (%%get-class-ascendant core-class))
             (core-class-ascendant-name (if (%%not core-class-ascendant) '() (zen:reference-name (%%get-category-identifier core-class-ascendant))))
             (declaration-ascendant (zen:get-class-declaration-ascendant declaration))
             (declaration-ascendant-name (if (%%not declaration-ascendant) '() (zen:reference-name (zen:get-declaration-locator declaration-ascendant)))))
        (%%when (%%not (%%eq? core-class-ascendant-name declaration-ascendant-name))
          (zen:error "Inconsistant core-class/class ascendant for {s}: {s} / {s}" (zen:get-lexical-binding-name declaration) core-class-ascendant-name declaration-ascendant-name))))
    
    (define (validate-interfaces)
      (let ((declaration-interfaces (zen:get-class-declaration-interfaces declaration)))
        (%%when (%%not (%%null? declaration-interfaces))
          (zen:error "Interfaces are not supported in open classes: {s}" (zen:get-lexical-binding-name declaration)))))
    
    (validate-ascendant)
    (validate-interfaces))
  
  (define (validate-slots)
    (define (collect-slots lst)
      (let ((queue (zen:new-queue)))
        (define (process obj)
          (cond ((%%is? obj zen:Slot-Declaration)
                 (zen:enqueue queue obj))
                ((%%is? obj zen:Begin)
                 (for-each process (zen:get-begin-expressions obj)))))
        
        (for-each process lst)
        (zen:queue-list queue)))
    
    (let ((core-class-slot-names (map (lambda (name/slot) (if (%%symbol? name/slot) name/slot (%%get-field-name name/slot))) (%%get-class-slots core-class)))
          (declaration-slot-names (map (lambda (decl) (zen:get-lexical-binding-name decl)) (collect-slots (zen:get-namespace-declaration-body declaration)))))
      (%%when (%%not (%%equal? core-class-slot-names declaration-slot-names))
        (zen:error "Inconsistant core-class/class slots for {s}: {s} / {s}" (zen:get-lexical-binding-name declaration) core-class-slot-names declaration-slot-names))))
  
  (validate-category)
  (validate-slots))


(zen:define-method (zen:outline-generate (zen:Class-Declaration declaration) output)
  (let ((name (symbol->string (zen:get-lexical-binding-name declaration)))
        (metaclass (zen:get-category-declaration-metaclass declaration))
        (metaclass-explicit? (zen:get-category-declaration-metaclass-explicit? declaration))
        (ascendant (zen:get-class-declaration-ascendant declaration))
        (interfaces (zen:get-class-declaration-interfaces declaration)))
    (%%unless (zen:string-ends-with? name "~Class")
      (let ((name (zen:get-lexical-binding-name declaration))
            (metaclass (and metaclass metaclass-explicit? (zen:get-lexical-binding-name (zen:resolve-binding metaclass))))
            (ascendant (and ascendant (zen:get-lexical-binding-name (zen:resolve-binding ascendant))))
            (interfaces (map zen:get-lexical-binding-name (zen:resolve-bindings interfaces))))
        (zen:format output "{%}  (class {a}" name)
        (%%when metaclass
          (zen:format output " metaclass {a}" metaclass))
        (%%when ascendant
          (let ((ascendant-name (%%symbol->string ascendant))
                (class-suffix "~Class"))
            (if (zen:string-ends-with? ascendant-name class-suffix)
                (zen:format output " extends (:class {a})" (substring ascendant-name 0 (- (string-length ascendant-name) (string-length class-suffix))))
              (zen:format output " extends {a}" ascendant))))
        (%%when (%%not-null? interfaces)
          (if (%%null? (%%cdr interfaces))
              (zen:format output " implements {a}" (%%car interfaces))
            (zen:format output " implements {a}" interfaces))))
      (%%when (and metaclass (%%not metaclass-explicit?))
        (let ((children (zen:outline-generate-filter-access (zen:queue-list (zen:get-namespace-declaration-children metaclass)))))
          (for-each (lambda (decl)
                      (let ((expr (zen:outline-extract decl '(meta))))
                        (if expr
                            (zen:format output "{%}    {s}" expr))))
                    children)))
      (let ((children (zen:outline-generate-filter-access (zen:queue-list (zen:get-namespace-declaration-children declaration)))))
        (let ((generated-accessors (%%make-table test: eq?)))
          (for-each (lambda (decl)
                      (%%when (%%is? decl zen:Slot-Declaration)
                        (let ((getter-name (zen:get-slot-declaration-getter-name decl))
                              (setter-name (zen:get-slot-declaration-setter-name decl)))
                          (%%when (and getter-name (%%eq? (zen:get-slot-declaration-getter-generation decl) 'generate))
                            (%%table-set! generated-accessors getter-name #t))
                          (%%when (and setter-name (%%eq? (zen:get-slot-declaration-setter-generation decl) 'generate))
                            (%%table-set! generated-accessors setter-name #t)))))
                    children)
          (for-each (lambda (decl)
                      (%%unless (and (%%is? decl zen:Method-Declaration)
                                     (%%table-ref generated-accessors (zen:get-lexical-binding-name decl) #f))
                        (let ((expr (zen:outline-extract decl '())))
                          (if expr
                              (zen:format output "{%}    {s}" expr)))))
                    children)))
      (zen:format output ")"))))


;;;
;;;; Interface-Declaration
;;;


(zen:define-class zen:Interface-Declaration zen:Category-Declaration (constructor: zen:allocate-interface-declaration)
  ((ascendants getter: generate)))


(define (zen:new-interface-declaration name type access compatibility modifiers attributes parent implementor metaclass metaclass-explicit? ascendants)
  (let ((new-declaration (zen:allocate-interface-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f (zen:make-access-lookups zen:protected-access) (zen:new-queue) #f implementor metaclass metaclass-explicit? ascendants)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(define (zen:setup-interface-lookups interface-declaration)
  (define (resolve-interface decl)
    (if decl
        (let ((interface-declaration (zen:resolve-binding decl)))
          (%%assert (%%is? interface-declaration zen:Interface-Declaration))
          interface-declaration)
      #f))
  
  (let ((ascendants (map resolve-interface (zen:get-interface-declaration-ascendants interface-declaration))))
    
    (let ((private (zen:get-private-lookup interface-declaration)))
      (for-each (lambda (interface)
                  (%%table-merge! private (zen:get-public-lookup interface) #t))
                ascendants))
    
    ;; a quick test
    (let ((private (zen:get-private-lookup interface-declaration)))
      (%%vector-set! (zen:get-namespace-declaration-lookups interface-declaration) zen:public-access private)
      (%%vector-set! (zen:get-namespace-declaration-lookups interface-declaration) zen:protected-access private))
    
    #;
    (let ((public (zen:get-public-lookup interface-declaration)))
      (for-each (lambda (interface)
                  (%%table-merge! public (zen:get-public-lookup interface)))
                ascendants))
    
    #;
    (let ((protected (zen:get-protected-lookup interface-declaration)))
      (for-each (lambda (interface)
                  (%%table-merge! protected (zen:get-public-lookup interface)))
                ascendants))))


(zen:define-method (zen:of-subtype? (zen:Interface-Declaration declaration) subtype)
  ;; quicky to fill later on
  #f)


(zen:define-method (zen:specifiable? (zen:Interface-Declaration declaration))
  #t)


(zen:define-method (zen:emit-declaration (zen:Interface-Declaration declaration) walker resume environment)
  (let* ((name (zen:get-lexical-binding-name declaration))
         (locator (zen:get-declaration-locator declaration))
         (rank-locator (%%compose-helper locator 'rank))
         (ascendant-declarations (zen:get-interface-declaration-ascendants declaration))
         (metaclass-declaration (zen:get-category-declaration-metaclass declaration))
         (metaclass-access (if (%%not metaclass-declaration) 'zen:Interface (zen:sourcified-form (zen:emit-binding-reference metaclass-declaration declaration walker resume environment))))
         (ascendant-accesses (map (lambda (declaration) (zen:sourcified-form (zen:emit-binding-reference declaration declaration walker resume environment))) ascendant-declarations))
         (body (zen:get-namespace-declaration-body declaration)))
    (zen:sourcify-deep-if
      `(begin
         (define ,locator
           ;; this is a quicky that needs to be well tought out
           (if (zen:global-bound? ',locator)
               (zen:global-ref ',locator)
             (zen:new-interface ,metaclass-access ',locator (%%list ,@ascendant-accesses))))
         (define ,rank-locator
           (%%get-interface-rank ,locator))
         ,@(let ((toplevel-declaration (zen:get-declaration-toplevel declaration)))
             (if (zen:get-generate? 'register)
                 `((zen:register-module-entry ',(zen:get-lexical-binding-name toplevel-declaration) ',name ,locator))
               '()))
         ,@(zen:emit-namespace-statements body declaration walker resume environment))
      (zen:get-declaration-source declaration))))


(zen:define-method (zen:outline-generate (zen:Interface-Declaration declaration) output)
  (let ((metaclass (zen:get-category-declaration-metaclass declaration))
        (metaclass-explicit? (zen:get-category-declaration-metaclass-explicit? declaration))
        (ascendants (map zen:get-lexical-binding-name (zen:resolve-bindings (zen:get-interface-declaration-ascendants declaration)))))
    (zen:format output "{%}  (interface {a}" (zen:get-lexical-binding-name declaration))
    (%%when (and metaclass metaclass-explicit?)
      (zen:format output " metaclass {a}" (zen:get-lexical-binding-name (zen:resolve-binding metaclass))))
    (%%when (%%not-null? ascendants)
      (if (%%null? (%%cdr ascendants))
          (zen:format output " extends {a}" (%%car ascendants))
        (zen:format output " extends {a}" ascendants)))
    (%%when (and metaclass (%%not metaclass-explicit?))
      (let ((children (zen:outline-generate-filter-access (zen:queue-list (zen:get-namespace-declaration-children metaclass)))))
        (for-each (lambda (decl)
                    (let ((expr (zen:outline-extract decl '(meta))))
                      (if expr
                          (zen:format output "{%}    {s}" expr))))
                  children)))
    (let ((children (zen:outline-generate-filter-access (zen:queue-list (zen:get-namespace-declaration-children declaration)))))
      (for-each (lambda (decl)
                  (let ((expr (zen:outline-extract decl '())))
                    (if expr
                        (zen:format output "{%}    {s}" expr))))
                children))
    (zen:format output ")")))


;;;
;;;; Field-Declaration
;;;


(zen:define-class zen:Field-Declaration zen:Declaration ()
  ())


;;;
;;;; Slot-Declaration
;;;


(zen:define-class zen:Slot-Declaration zen:Field-Declaration (constructor: zen:allocate-slot-declaration)
  ((specifier-source  getter: generate)
   (initialize        getter: generate setter: generate)
   (getter-name       getter: generate)
   (setter-name       getter: generate)
   (getter-generation getter: generate)
   (setter-generation getter: generate)
   (dynamic?          getter: generate)))


(define (zen:new-slot-declaration name type access compatibility modifiers attributes parent specifier-source initialize getter-name setter-name getter-generation setter-generation dynamic?)
  (let ((new-declaration (zen:allocate-slot-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f specifier-source initialize getter-name setter-name getter-generation setter-generation dynamic?)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(zen:define-method (zen:walk-binding-validate-call (zen:Slot-Declaration declaration) walker resume source-declaration operator arguments form-src)
  #f)


(zen:define-method (zen:emit-declaration (zen:Slot-Declaration declaration) walker resume environment)
  (let* ((name (zen:get-lexical-binding-name declaration))
         (locator (zen:get-declaration-locator declaration))
         (class-declaration (zen:get-declaration-parent declaration))
         (class-locator (zen:get-declaration-locator class-declaration))
         (allocate? (%%neq? (zen:get-lexical-binding-type declaration) zen:Void))
         (core? (zen:core-class? (zen:get-lexical-binding-name class-declaration)))
         (initialize (zen:get-slot-declaration-initialize declaration))
         (initialize? (and allocate? (%%not core?) initialize))
         (initialize-locator (and initialize? (%%compose-helper locator 'initialize)))
         (initialize-emit (and initialize? (zen:emit-expression initialize declaration walker resume environment)))
         (slot-locator (%%compose-helper locator 'slot))
         (offset-locator (%%compose-helper locator 'offset)))
    (zen:sourcify-deep-if
      `(begin
         ,@(if initialize?
               `((define (,initialize-locator self)
                   ,(zen:sourcified-form initialize-emit)))
             '())
         (define ,slot-locator
           (zen:add-slot ,class-locator ',name ,initialize-locator ,allocate?))
         (define ,offset-locator
           (%%get-slot-offset ,slot-locator))
         ,@(zen:declaration-result))
      (zen:get-declaration-source declaration))))


(zen:define-method (zen:emit-binding-reference (zen:Slot-Declaration declaration) source-declaration walker resume environment)
  (let ((self (zen:*self*)))
    (if self
        (zen:new-code
          (let ((locator (zen:get-declaration-locator declaration)))
            (if (zen:get-slot-declaration-dynamic? declaration)
                (let ((slot-locator (%%compose-helper locator 'slot)))
                  `(zen:slot-ref ,(zen:sourcified-form self) ,slot-locator))
              (let ((offset-locator (%%compose-helper locator 'offset)))
                `(%%object-ref ,(zen:sourcified-form self) ,offset-locator))))
          (zen:find-annotated-type declaration environment)
          #f)
      (zen:error "Illegal reference to a slot: {s}" (zen:get-declaration-locator declaration)))))


(zen:define-method (zen:walk-binding-assignable? (zen:Slot-Declaration declaration))
  #t)


(zen:define-method (zen:emit-binding-assignment (zen:Slot-Declaration declaration) value source-declaration walker resume environment form-src)
  (let ((self (zen:*self*)))
    (if self
        (let ((value-code (zen:emit-expression value source-declaration walker resume environment)))
          (zen:new-code
            (let ((locator (zen:get-declaration-locator declaration))
                  (value (zen:emit-type-cast value-code (zen:get-lexical-binding-type declaration) source-declaration declaration walker resume environment)))
              (if (zen:get-slot-declaration-dynamic? declaration)
                  (let ((slot-locator (%%compose-helper locator 'slot)))
                    `(zen:slot-set! ,(zen:sourcified-form self) ,slot-locator ,value))
                (let ((offset-locator (%%compose-helper locator 'offset)))
                  `(%%object-set! ,(zen:sourcified-form self) ,offset-locator ,value))))
            zen:Any
            form-src))
      (zen:error "Illegal assignment to a slot: {s}" (zen:get-declaration-locator declaration)))))


(zen:define-method (zen:outline-extract (zen:Slot-Declaration declaration) meta)
  `(slot ,@meta
         ,@(zen:outline-generate-access-list declaration)
         ,(zen:get-lexical-binding-name declaration)
         ,@(zen:outline-generate-specifier-list (zen:get-slot-declaration-specifier-source declaration))
         ,@(zen:outline-generate-accessors declaration)))


(define (zen:outline-generate-accessors declaration)
  (let ((getter-name (zen:get-slot-declaration-getter-name declaration))
        (setter-name (zen:get-slot-declaration-setter-name declaration))
        (getter-generation (zen:get-slot-declaration-getter-generation declaration))
        (setter-generation (zen:get-slot-declaration-setter-generation declaration)))
    (cond ((and getter-name setter-name)
           (if (%%eq? getter-generation setter-generation)
               `(accessors ,getter-generation)
             `(getter ,getter-generation setter ,setter-generation)))
          (getter-name `(getter ,getter-generation))
          (setter-name `(setter ,setter-generation))
          (else '()))))


;;;
;;;; Property
;;;


(zen:define-class zen:Property-Declaration zen:Slot-Declaration (constructor: zen:allocate-property-declaration)
  ((getter getter: generate setter: generate)
   (setter getter: generate setter: generate)))


(define (zen:new-property-declaration name type access compatibility modifiers attributes parent specifier-source initialize getter-name setter-name getter-generation setter-generation dynamic?)
  (let ((new-declaration (zen:allocate-property-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f specifier-source initialize getter-name setter-name getter-generation setter-generation dynamic? #f #f)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(zen:define-method (zen:emit-declaration (zen:Property-Declaration declaration) walker resume environment)
  (let* ((name (zen:get-lexical-binding-name declaration))
         (locator (zen:get-declaration-locator declaration))
         (class-declaration (zen:get-declaration-parent declaration))
         (class-locator (zen:get-declaration-locator class-declaration))
         (allocate? (%%neq? (zen:get-lexical-binding-type declaration) zen:Void))
         (core? (zen:core-class? (zen:get-lexical-binding-name class-declaration)))
         (initialize (zen:get-slot-declaration-initialize declaration))
         (initialize? (and allocate? (%%not core?) initialize))
         (initialize-locator (and initialize? (%%compose-helper locator 'initialize)))
         (initialize-emit (and initialize? (zen:emit-expression initialize declaration walker resume environment)))
         (slot-locator (%%compose-helper locator 'slot))
         (offset-locator (%%compose-helper locator 'offset))
         (getter (zen:get-property-declaration-getter declaration))
         (setter (zen:get-property-declaration-setter declaration)))
    ;; hack in a literal self instead of the hygienically renamed self
    (define (fix-self expr)
      (if (pair? expr)
          (cons (car expr) (cons (cons 'self (cdadr expr)) (cddr expr)))
        expr))
    (zen:sourcify-deep-if
      `(begin
         ,@(if initialize?
               `((define (,initialize-locator self)
                   ,(zen:sourcified-form initialize-emit)))
             '())
         (define ,slot-locator
           (zen:add-property ,class-locator ',name ,initialize-locator ,allocate?
             ,(fix-self (zen:sourcified-form (zen:emit-expression getter declaration walker resume environment)))
             ,(fix-self (zen:sourcified-form (zen:emit-expression setter declaration walker resume environment)))))
         (define ,offset-locator
           (%%get-slot-offset ,slot-locator))
         ,@(zen:declaration-result))
      (zen:get-declaration-source declaration))))


(define (zen:expand-property walker resume declaration environment form-src)
  (zen:expand-slot-form walker resume declaration form-src '%property))


(zen:define-method (zen:outline-extract (zen:Property-Declaration declaration) meta)
  `(property ,@meta
             ,@(zen:outline-generate-access-list declaration)
             ,(zen:get-lexical-binding-name declaration)
             ,@(zen:outline-generate-specifier-list (zen:get-slot-declaration-specifier-source declaration))
             ,@(zen:outline-generate-accessors declaration)))


;;;
;;;; Method-Declaration
;;;


(zen:define-class zen:Method-Declaration zen:Field-Declaration (constructor: zen:allocate-method-declaration)
  ((root             getter: generate)
   (propagation      getter: generate)
   (abstraction      getter: generate)
   (expansion        getter: generate)
   (remote           getter: generate)
   (synchronized     getter: generate)
   (signature        getter: generate setter: generate)
   (specifier-source getter: generate)
   (body             getter: generate setter: generate)))


(define (zen:new-method-declaration name type access compatibility modifiers attributes parent root propagation abstraction expansion remote synchronized signature specifier-source)
  (let ((new-declaration (zen:allocate-method-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f root propagation abstraction expansion remote synchronized signature specifier-source #f)))
    (zen:setup-declaration new-declaration)
    new-declaration))


(define (zen:method-dispatch-info declaration)
  (let ((root (zen:get-method-declaration-root declaration))
        (propagation (zen:get-method-declaration-propagation declaration)))
    (if (and (%%not root) (%%eq? propagation 'final))
        (values 'final declaration)
      (let ((root-method-declaration (zen:get-method-declaration-root declaration)))
        (let ((method-declaration (or root-method-declaration declaration)))
          (let ((category-declaration (zen:get-declaration-parent method-declaration)))
            (cond ((%%class-is? category-declaration zen:Class-Declaration)
                   (values 'class method-declaration))
                  ((%%class-is? category-declaration zen:Interface-Declaration)
                   (values 'interface method-declaration))
                  (else
                   (error "Invalid category declaration: {s}" category-declaration)))))))))


(define (zen:primitive-category? category-declaration)
  (%%eq? (zen:get-category-declaration-implementor category-declaration) 'primitive))


(define (zen:emit-class-of category-declaration object-code)
  (if (zen:primitive-category? category-declaration)
      `(zen:class-of ,object-code)
    `(%%get-object-class ,object-code)))


(define (zen:emit-method-dispatch object-argument object-code operator-src arguments arguments-codes declaration source-declaration walker resume environment)
  (let ((name (zen:get-lexical-binding-name declaration)))
    (receive (dispatch-type method-declaration) (zen:method-dispatch-info declaration)
      (let ((category-declaration (zen:get-declaration-parent method-declaration)))
        (zen:add-to-module-references source-declaration method-declaration)
        (let ((object-type (zen:get-code-type object-code))
              (object-cast (zen:emit-type-cast object-code category-declaration operator-src source-declaration walker resume environment))
              (return-type (zen:call-return-type (zen:get-lexical-binding-type method-declaration))))
          (case dispatch-type
            ((final)
             (let ((type (zen:get-lexical-binding-type method-declaration))
                   (implementation-locator (zen:get-declaration-locator method-declaration)))
               (if (and zen:debug-user?
                        arguments
                        ;; fail safe as this should never occur as inlined-call is before
                        (%%neq? (zen:get-method-declaration-expansion method-declaration) 'inline)
                        ;; safe first iteration simplification
                        (zen:only-positional-function-type? type)
                        (zen:typed-function-type? type #t))
                   (let ((types (zen:codes-types arguments-codes)))
                     (let ((mismatch (zen:signature-mismatch (%%cons object-argument arguments) (%%cons object-type types) type #t)))
                       (if (or (%%not mismatch)
                               (%%not (zen:get-check? 'types)))
                           (let ((locator (zen:unsafe-locator implementation-locator))
                                 (type (zen:get-lexical-binding-type declaration)))
                             (zen:new-code
                               `((%%final-dispatch ,(zen:emit-class-of category-declaration object-cast) ,locator)
                                 ,(zen:sourcified-form object-code)
                                 ,@(map (lambda (code type)
                                          (zen:emit-implicit-cast code type))
                                        arguments-codes
                                        (%%cdr (zen:get-function-type-positional type))))
                               return-type
                               operator-src))
                         (begin
                           (%%when (and (or (zen:reporting?) (zen:warnings?)) (zen:get-warn? 'optimizations))
                             (let ((expression (and (%%pair? mismatch) (%%car mismatch))))
                               (zen:warning "Warning: In {a}{a}: Unmatched call to typed method {a}"
                                             (zen:get-declaration-locator declaration)
                                             (zen:present-expression-location (and expression (zen:get-expression-source expression)) operator-src)
                                             (zen:reference-name (zen:get-declaration-locator declaration)))))
                           (zen:new-code
                             `((%%final-dispatch ,(zen:emit-class-of category-declaration object-cast) ,implementation-locator)
                               ,(zen:sourcified-form object-code)
                               ,@(zen:codes-forms arguments-codes))
                             return-type
                             operator-src)))))
                 (zen:new-code
                   `((%%final-dispatch ,(zen:emit-class-of category-declaration object-cast) ,implementation-locator)
                     ,(zen:sourcified-form object-code)
                     ,@(zen:codes-forms arguments-codes))
                   return-type
                   operator-src))))
            ((class)
             (let ((class-level-locator (%%compose-helper (zen:get-declaration-locator category-declaration) 'level))
                   (method-rank-locator (%%compose-helper (zen:get-declaration-locator method-declaration) 'rank)))
               (zen:new-code
                 `((%%class-dispatch ,(zen:emit-class-of category-declaration object-cast) ,class-level-locator ,method-rank-locator)
                   ,(zen:sourcified-form object-code)
                   ,@(zen:codes-forms arguments-codes))
                 return-type
                 operator-src)))
            ((interface)
             (let ((interface-rank-locator (%%compose-helper (zen:get-declaration-locator category-declaration) 'rank))
                   (method-rank-locator (%%compose-helper (zen:get-declaration-locator method-declaration) 'rank)))
               (zen:new-code
                 `((%%interface-dispatch ,(zen:emit-class-of category-declaration object-cast) ,interface-rank-locator ,method-rank-locator)
                   ,(zen:sourcified-form object-code)
                   ,@(zen:codes-forms arguments-codes))
                 return-type
                 operator-src)))))))))


(zen:define-method (zen:compose-declaration-locator (zen:Method-Declaration declaration))
  (if (%%eq? (zen:get-method-declaration-abstraction declaration) 'core)
      (let ((class-name (%%compose-reference 'zen (zen:get-lexical-binding-name (zen:get-declaration-parent declaration))))
            (name (%%compose-reference 'zen (zen:get-lexical-binding-name declaration))))
        (zen:method-implementation-name class-name name))
    (nextmethod declaration)))


(zen:define-method (zen:emit-binding-reference (zen:Method-Declaration declaration) source-declaration walker resume environment)
  (let ((name (zen:get-lexical-binding-name declaration))
        (source #f))
    (zen:dispatch-reference-emit name source declaration walker resume environment)))


(define (zen:dispatch-reference-emit name source declaration walker resume environment)
  (zen:new-code
    `(lambda (object . rest)
       (apply (zen:dispatch (zen:class-of object) ',name) object rest))
    zen:Any
    source))


(zen:define-method (zen:walk-binding-validate-call (zen:Method-Declaration declaration) walker resume source-declaration operator arguments form-src)
  ;; GAZOUM the self test is a quick hack
  (if (%%null? arguments)
      (zen:walk-error walker resume source-declaration form-src "Not enough arguments for: {a}" (zen:get-lexical-binding-name declaration))
    (%%when (%%eq? (%%car arguments) 'self)
      (let ((signature (zen:get-method-declaration-signature declaration)))
        (if signature
            (zen:validate-arguments walker resume source-declaration declaration signature arguments form-src))))))


(zen:define-method (zen:emit-binding-call (zen:Method-Declaration declaration) binding-src arguments arguments-codes source-declaration walker resume environment)
  (let ((name (zen:get-lexical-binding-name declaration))
        (nodes #f)
        (object-argument (%%car arguments))
        (object-code (%%car arguments-codes))
        (others-arguments (%%cdr arguments))
        (others-codes (%%cdr arguments-codes)))
    (zen:dispatch-call-emit name nodes binding-src source-declaration walker resume environment object-argument object-code others-arguments others-codes)))


(define (zen:dispatch-call-emit name nodes source declaration walker resume environment object-argument object-code others-arguments others-codes)
  (define (resolve-type object-code)
    (let ((object-type (zen:patch-type-until-unification (zen:get-code-type object-code))))
      (if (%%class-is? object-type zen:Autoload-Declaration)
          (zen:resolve-binding object-type)
        object-type)))
  
  (define (lookup-method object-code)
    (let ((object-type (resolve-type object-code)))
      (if (%%class-is? object-type zen:Category-Declaration)
          (let ((declaration (zen:lookup-declaration object-type name zen:public-access declaration)))
            (if (and declaration (%%class-is? declaration zen:Method-Declaration))
                declaration
              #f))
        #f)))
  
  (define (lookup-method/warn object-code)
    (let ((method-declaration (lookup-method object-code)))
      (if (%%not method-declaration)
          (begin
            (%%when (and (or (zen:reporting?) (zen:warnings?)) (zen:get-warn? 'optimizations))
              (zen:warning "Warning: In {a}{a}: Unable to find dispatch method {a}"
                            (zen:get-declaration-locator declaration)
                            (zen:present-expression-location source #f)
                            name))
            #f)
        (begin
          (zen:add-to-module-references declaration method-declaration)
          method-declaration))))
  
  (define (zen:with-code-value code proc)
    (let ((form (zen:get-code-form code)))
      (if (%%symbol? form)
          (proc code)
        (let ((value (zen:generate-symbol "val")))
          (let ((code (proc (zen:new-code value (zen:get-code-type code) #f))))
            (zen:new-code
              `(let ((,value ,form))
                 ,(zen:get-code-form code))
              (zen:get-code-type code)
              (zen:get-code-source code)))))))
  
  (let ((method-declaration (lookup-method/warn object-code)))
    (if method-declaration
        (or (zen:inlined-final-dispatch-call-emit source method-declaration object-code others-codes declaration walker resume environment)
            (zen:with-code-value object-code
              (lambda (code)
                (zen:emit-method-dispatch object-argument code source others-arguments others-codes method-declaration declaration walker resume environment))))
      (let ((dv (zen:register-variable declaration (%%string-append (%%symbol->string name) "!d") #f)))
        (let ((d (%%car dv)))
          (%%set-cdr! dv `(zen:cache-dispatch ',name ',d ',nodes))
          (zen:new-code
            (zen:with-uniqueness (zen:sourcified-form object-code)
              (lambda (object)
                `((,d ,object) ,object ,@(zen:codes-forms others-codes))))
            zen:Any
            source))))))


(define (zen:inlined-final-dispatch-call-emit source declaration object arguments source-declaration walker resume environment)
  ;; mostly copy/pasted and adapted from method declaration. need to unify the code
  (if (%%eq? (zen:get-method-declaration-expansion declaration) 'inline)
      (receive (dispatch-type method-declaration) (zen:method-dispatch-info declaration)
        (case dispatch-type
          ((final)
           (let ((signature (zen:get-method-declaration-signature declaration))
                 (body (zen:get-method-declaration-body declaration)))
             (if (zen:only-positional-signature? signature)
                 ;; the +1 is for the self that is now part of the method signature
                 (if (%%fx= (zen:get-signature-mandatory signature) (%%fx+ 1 (%%length arguments)))
                     (zen:with-annotated-frame (zen:annotate-signature signature)
                       (lambda (frame)
                         (let ((augmented-environment (%%cons frame environment)))
                           (let ((body-code (zen:emit-expression body source-declaration walker resume augmented-environment)))
                             (zen:new-code
                               `(let ,(map (lambda (parameter argument)
                                             `(,(zen:emit-binding-symbol parameter source-declaration environment)
                                               ,(zen:emit-type-cast argument (zen:get-lexical-binding-type parameter) source source-declaration walker resume environment)))
                                           (zen:get-signature-positional signature)
                                           (%%cons object arguments))
                                  ,(zen:sourcify-deep-if (zen:desourcify-all (zen:get-code-form body-code)) source)
                                  ;; an interesting idea to get the position in the inlined code but
                                  ;; it turns out it is really annoying not to be at the place of call
                                  #;
                                  ,(zen:get-code-form body-code))
                               (zen:call-return-type (zen:get-lexical-binding-type declaration))
                               #f)))))
                   (zen:error "Wrong number of arguments passed to {s}" (zen:get-lexical-binding-name declaration)))
               (zen:error "Only positional parameters are supported in inlining: {s}" (zen:get-lexical-binding-name declaration)))))
          (else
           #f)))
    #f))


(zen:define-method (zen:get-nextmethod-signature (zen:Method-Declaration declaration))
  (define (lookup category-declaration method-name)
    (if (%%is? category-declaration zen:Autoload-Declaration)
        (let ((category-declaration (zen:get-autoload-declaration-declaration category-declaration)))
          (zen:lookup-declaration category-declaration method-name zen:private-access category-declaration))
      (zen:lookup-declaration category-declaration method-name zen:private-access category-declaration)))
  
  (define (get-next-method-declaration)
    (let ((method-name (zen:get-lexical-binding-name declaration))
          (category-declaration (zen:get-declaration-parent declaration)))
      (or (let ((ascendant (zen:get-class-declaration-ascendant category-declaration)))
            (lookup ascendant method-name))
          (let iter ((scan (zen:get-class-declaration-interfaces category-declaration)))
               (if (null? scan)
                   #f
                 (or (lookup (car scan) method-name)
                     (iter (cdr scan))))))))
  
  (let ((next-method-declaration (get-next-method-declaration)))
    (if next-method-declaration
        (zen:get-method-declaration-signature next-method-declaration))))


(zen:define-method (zen:emit-declaration (zen:Method-Declaration declaration) walker resume environment)
  (let ((signature (zen:get-method-declaration-signature declaration)))
    (zen:with-annotated-frame (zen:annotate-signature signature)
      (lambda (frame)
        (let* ((augmented-environment (%%cons frame environment))
               (signature-emit (zen:emit-signature signature declaration walker resume augmented-environment))
               (signature-casts (zen:emit-signature-casts signature declaration walker resume augmented-environment))
               (body (zen:get-method-declaration-body declaration))
               (body-type (let ((type (zen:get-lexical-binding-type declaration)))
                            (if (%%is? type zen:Function-Type) (zen:get-function-type-result type) zen:Any)))
               (body-emit (and body (let ((body-code (zen:emit-expression body declaration walker resume augmented-environment)))
                                      (zen:emit-type-cast body-code body-type (zen:get-expression-source body) declaration walker resume augmented-environment))))
               (generate-unsafe? (and zen:debug-user?
                                      ;; no need to emit unsafe and safe when inline because as the checks are not included
                                      ;; in the body it is possible to define the function itself with checks so it is safe
                                      ;; used not inlined and only insert checks at call site when the types don't match
                                      (%%neq? (zen:get-method-declaration-expansion declaration) 'inline)
                                      ;; safe first iteration simplification
                                      (zen:only-positional-signature? signature)
                                      (zen:typed-signature? signature #t)))
               (unsafe-signature (and generate-unsafe? signature)))
          (zen:method-emit declaration walker resume environment signature-emit signature-casts body-emit unsafe-signature))))))


(define (zen:method-emit declaration walker resume environment signature-emit signature-casts body-emit unsafe-signature)
  (let* ((name (zen:get-lexical-binding-name declaration))
         (abstraction (zen:get-method-declaration-abstraction declaration))
         (propagation (zen:get-method-declaration-propagation declaration))
         (category-declaration (zen:get-declaration-parent declaration))
         (class-name (zen:get-lexical-binding-name category-declaration))
         (class-locator (zen:get-declaration-locator category-declaration))
         (core-method-node-locator (zen:method-implementation-name (%%compose-reference 'zen class-name) name))
         (method-locator (zen:get-declaration-locator declaration))
         (method-rank-locator (%%compose-helper method-locator 'rank))
         (method-node-locator (%%compose-helper method-locator 'node))
         (unsafe-locator (and unsafe-signature (zen:unsafe-locator method-locator)))
         (add-method-proc (cond ((%%class-is? category-declaration zen:Class-Declaration)     (case propagation
                                                                                                 ((override)        'zen:add-method-node)
                                                                                                 ((final)           'zen:add-final-method)
                                                                                                 ((virtual chained) 'zen:add-virtual-method)))
                                ((%%class-is? category-declaration zen:Interface-Declaration) (case propagation
                                                                                                 ((override)        'zen:add-method-node)
                                                                                                 ((virtual)         'zen:add-virtual-method))))))
    (zen:sourcify-deep-if
      (case add-method-proc
        ((zen:add-final-method)
         `(begin
            ,@(if unsafe-locator
                  `((define (,unsafe-locator ,@signature-emit)
                      ,body-emit)
                    (define (,method-locator ,@signature-emit)
                      ,@(zen:add-signature-casts signature-casts
                        `(,unsafe-locator ,@(map zen:get-lexical-binding-name (zen:get-signature-positional unsafe-signature))))))
                `((define (,method-locator ,@signature-emit)
                    ,@(zen:add-signature-casts signature-casts body-emit))))
            (,add-method-proc ,class-locator ',name ,method-locator)
            ,@(zen:declaration-result)))
        ((zen:add-virtual-method)
         `(begin
            ,@(if body-emit
                  `((define (,method-locator ,@signature-emit)
                      ,@(zen:add-signature-casts signature-casts body-emit)))
                (if (eq? abstraction 'abstract)
                    `((define (,method-locator self . rest)
                        (zen:call-into-abstract ',class-locator ',name self rest)))
                  '()))
            (define ,method-rank-locator
              (,add-method-proc ,class-locator ',name
                ,(if (and (%%eq? abstraction 'core) (%%not body-emit))
                     core-method-node-locator
                   method-locator)))
            ,@(zen:declaration-result)))
        ((zen:add-method-node)
         (let ((node (zen:generate-symbol "node")))
           `(begin
              ,@(if body-emit
                    `((define (,method-locator ,@signature-emit)
                        (let ((nextmethod (%%get-method-node-next-implementation ,method-node-locator)))
                          ,@(zen:add-signature-casts signature-casts body-emit))))
                  '())
              (define ,method-node-locator
                (,add-method-proc ,class-locator ',name
                  ,(if (and (%%eq? abstraction 'core) (%%not body-emit))
                       core-method-node-locator
                     method-locator)))
              ,@(zen:declaration-result)))))
      (zen:get-declaration-source declaration))))


(zen:define-method (zen:tree-fold (zen:Method-Declaration declaration) down up here seed environment)
  (here declaration seed environment)
  (zen:tree-fold-list (zen:get-signature-expressions (zen:get-method-declaration-signature declaration)) down up here seed environment)
  (let ((body (zen:get-method-declaration-body declaration)))
    (%%when body
      (zen:tree-fold body down up here seed environment))))


(zen:define-method (zen:outline-extract (zen:Method-Declaration declaration) meta)
  ;; not sure if this breaks together's script evaluation
  (if (%%eq? (zen:get-declaration-access declaration) 'private)
      #f
    `(method ,@meta
             ,@(zen:outline-generate-modifiers declaration)
             (,(zen:get-lexical-binding-name declaration) ,@(zen:outline-generate-signature (zen:get-method-declaration-signature declaration) #t))
             ,@(zen:outline-generate-specifier-list (zen:get-method-declaration-specifier-source declaration)))))


;;;
;;;; Dialect
;;;


(zen:define-class zen:Zen-Dialect zen:Dialect (constructor: zen:allocate-zen-dialect)
  ())


(define (zen:new-zen-dialect name)
  (zen:allocate-zen-dialect name (%%make-table test: eq?) (%%make-table test: eq?)))


(zen:define-method (zen:dialect-walker (zen:Zen-Dialect dialect))
  (zen:new-zen-walker))


;;;
;;;; Walker
;;;


(zen:define-class zen:Zen-Walker zen:Scheme-Walker (constructor: zen:allocate-zen-walker)
  ())


(define (zen:new-zen-walker)
  (zen:allocate-zen-walker #f #f '() '() '() (zen:new-queue) (zen:new-queue) (%%make-table test: eq?) (%%make-table test: eq?) '()))


(zen:define-virtual-syntax (zen:zen-walker-supports-selfdot? (zen:Zen-Walker walker)))
(zen:define-virtual-syntax (zen:zen-walker-supports-composite? (zen:Zen-Walker walker)))


(zen:define-virtual (zen:zen-walker-supports-selfdot? (zen:Zen-Walker walker)))
(zen:define-virtual (zen:zen-walker-supports-composite? (zen:Zen-Walker walker)))


(zen:define-method (zen:zen-walker-supports-selfdot? (zen:Zen-Walker walker))
  #t)

(zen:define-method (zen:zen-walker-supports-composite? (zen:Zen-Walker walker))
  #t)


(zen:define-method (zen:runtime-export (zen:Zen-Walker walker) declaration)
  (or (nextmethod walker declaration)
      (if (or (%%is? declaration zen:Definition-Declaration)
              (%%is? declaration zen:Generic-Declaration)
              (%%is? declaration zen:Category-Declaration))
          (zen:get-declaration-locator declaration)
        #f)))


;;;
;;;; Environment
;;;


(zen:define-method (zen:walker-declarations (zen:Zen-Walker walker))
  (cons (zen:get-dialect-declarations (zen:get-dialect 'zen))
        (nextmethod walker)))


(zen:define-method (zen:walker-bindings (zen:Zen-Walker walker))
  (cons (zen:get-dialect-bindings (zen:get-dialect 'zen))
        (nextmethod walker)))


;;;
;;;; Parse
;;;


(define (zen:parse-keywords keywords rest)
  (let ((table (%%make-table test: eq?))
        (done? #f))
    (%%while (and (%%not done?) (%%not (%%null? rest)))
      (let ((symbol (zen:source-code (%%car rest))))
        (if (%%not (%%memq symbol keywords))
            (set! done? #t)
          (begin
            (%%table-set! table symbol (%%desourcify (%%cadr rest)))
            (set! rest (%%cddr rest))))))
    (%%apply values (%%append (map (lambda (keyword)
                                     (%%table-ref table keyword (zen:unspecified)))
                                   keywords)
                              (%%list rest)))))


;;;
;;;; Specialize
;;;


(zen:define-variable-override zen:emit-specialized-locator
  (lambda (locator arguments environment)
    (case locator
      ((zen.dialect.kernel:class-of)
       (%%assert (and (%%pair? arguments) (%%null? (%%cdr arguments)))
         (zen:emit-specialized-class-of (%%car arguments) environment)))
      (else
       #f))))


(define (zen:emit-specialized-class-of object environment)
  (zen:new-code
    `(zen:class-of ,(zen:sourcified-form object))
    (let ((type (zen:get-code-type object)))
      (if (%%class-is? type zen:Class-Declaration)
          (zen:get-category-declaration-metaclass type)
        zen:Class-Declaration))
    #f))


;;;
;;;; Unsafe Call
;;;


(zen:define-variable-override zen:emit-unsafe-call
  (lambda (operator locator arguments arguments-codes declaration walker resume environment)
    (and zen:debug-user?
         (and (%%class-is? operator zen:Binding-Reference)
              (let ((binding (zen:get-binding-reference-binding operator)))
                (cond ((%%class-is? binding zen:Definition-Declaration)
                       (let ((type (zen:get-lexical-binding-type binding)))
                         (and (%%is? type zen:Function-Type)
                              ;; fail safe as this should never occur as inlined-call is before
                              (%%neq? (zen:get-definition-declaration-expansion binding) 'inline)
                              ;; safe first iteration simplification
                              (zen:only-positional-function-type? type)
                              (zen:typed-function-type? type)
                              (let ((types (zen:codes-types arguments-codes)))
                                (let ((mismatch (zen:signature-mismatch arguments types type #t)))
                                  ;; quick solution to calling an inline function from
                                  ;; a script that does not have access to source code
                                  (cond ((%%not zen:kernel-source-access?)
                                         #f)
                                        ((or (%%not mismatch)
                                             (%%not (zen:get-check? 'types)))
                                         (zen:new-code
                                           (let ((locator (zen:unsafe-locator locator)))
                                             `(,locator ,@(map (lambda (code type)
                                                                 (zen:emit-implicit-cast code type))
                                                               arguments-codes
                                                               (zen:get-function-type-positional type))))
                                           (zen:get-function-type-result type)
                                           #f))
                                        (else
                                         (%%when (and (or (zen:reporting?) (zen:warnings?)) (zen:get-warn? 'optimizations))
                                           (let ((expression (and (%%pair? mismatch) (%%car mismatch))))
                                             (zen:warning "Warning: In {a}{a}: Unmatched call to typed definition {a}"
                                                           (zen:get-declaration-locator declaration)
                                                           (zen:present-expression-location (and expression (zen:get-expression-source expression)) (zen:get-expression-source operator))
                                                           (zen:reference-name locator))))
                                         #f)))))))
                      ((%%class-is? binding zen:Internal-Define-Variable)
                       (let ((type (zen:get-lexical-binding-type binding)))
                         (and (%%is? type zen:Function-Type)
                              ;; safe first iteration simplification
                              (zen:only-positional-function-type? type)
                              (zen:typed-function-type? type)
                              (let ((types (zen:codes-types arguments-codes)))
                                (let ((mismatch (zen:signature-mismatch arguments types type #t)))
                                  (if (or (%%not mismatch)
                                          (%%not (zen:get-check? 'types)))
                                      (zen:new-code
                                        `(,(zen:get-lexical-binding-name binding) ,@(map (lambda (code type)
                                                                                            (zen:emit-implicit-cast code type))
                                                                                          arguments-codes
                                                                                          (zen:get-function-type-positional type)))
                                        (zen:get-function-type-result type)
                                        #f)
                                    (begin
                                      (let ((expression (and (%%pair? mismatch) (%%car mismatch))))
                                        (zen:unsafe-warning "Unsafe: In {a}{a}: Unmatched call to typed internal define {a}"
                                                             (zen:get-declaration-locator declaration)
                                                             (zen:present-expression-location (and expression (zen:get-expression-source expression)) (zen:get-expression-source operator))
                                                             (zen:get-lexical-binding-name binding))
                                        #; ;; waiting for warning tooltip
                                        (display (zen:format "  Unsafe: {l}{%}" (%%apply append (map (lambda (arg type)
                                                                                                        (if (%%class-is? arg zen:Binding-Reference)
                                                                                                            (let ((variable (zen:get-binding-reference-binding arg)))
                                                                                                              (%%list (zen:get-lexical-binding-name variable)
                                                                                                                      (zen:type->specifier type)))
                                                                                                          (%%list (cond ((%%is? arg zen:Constant)
                                                                                                                         (zen:desourcify-all (zen:get-constant-expansion arg)))
                                                                                                                        (else
                                                                                                                         arg))
                                                                                                                  (zen:type->specifier type))))
                                                                                                      arguments
                                                                                                      types)))))
                                      #f)))))))
                      (else
                       #f)))))))


;;;
;;;; New
;;;


(zen:define-variable-override zen:emit-new-call
  (lambda (operator locator arguments arguments-codes declaration walker resume environment)
    (if (%%eq? locator 'zen.dialect.kernel:new)
        (%%assert (%%pair? arguments)
          (let ((class-expression (%%car arguments)))
            (if (%%class-is? class-expression zen:Binding-Reference)
                (let ((binding (zen:get-binding-reference-binding class-expression)))
                  (if (or (%%class-is? binding zen:Class-Declaration)
                          (%%class-is? binding zen:Autoload-Declaration))
                      (let ((values-codes (%%cdr arguments-codes)))
                        (zen:new-code
                          `(%%new ,@(zen:codes-forms arguments-codes))
                          binding
                          #f))
                    #f))
              #f)))
      #f)))


;;;
;;;; Symbol
;;;


(zen:define-method (zen:walk-symbol (zen:Zen-Walker walker) resume declaration environment symbol-src)
  (let ((symbol (zen:unwrap-syntactic-closure symbol-src)))
    (if (zen:enumerator? symbol)
        (zen:walk-enumerator walker symbol)
      (let ((name (zen:extract-selfdot symbol)))
        (if name
            (let ((slot-declaration (zen:lookup-declaration (zen:find-class-declaration declaration) name zen:private-access declaration)))
              (%%assertion (%%class-is? slot-declaration zen:Slot-Declaration) (zen:error "Slot expected: self.{s}" name)
                (zen:new-binding-reference symbol-src slot-declaration)))
          (nextmethod walker resume declaration environment symbol-src))))))


(define (zen:extract-selfdot symbol)
  (let ((str (%%symbol->string symbol)))
    (if (zen:string-starts-with? str "self.")
        (%%string->symbol (%%substring str 5 (%%string-length str)))
      #f)))


(zen:define-method (zen:lookup-environment (zen:Zen-Walker walker) resume declaration environment symbol-src symbol)
  (define (lookup-composite walker environment symbol)
    (receive (module-name name) (zen:break-reference symbol)
      (let ((exported-module-reference (zen:outline-module module-name)))
        (let ((decl (zen:lookup-declaration exported-module-reference name zen:public-access declaration)))
          (if decl
              (let ((autoload-decl 
                      (if (%%is? decl zen:Autoload-Declaration)
                          decl
                        (zen:new-autoload-declaration name #f #f (zen:get-declaration-toplevel declaration) (zen:new-module-reference module-name #f)))))
                ;; manually add since lookup-declaration was not done on the local module
                (zen:add-to-module-references declaration autoload-decl)
                autoload-decl)
            (zen:walk-error walker resume declaration symbol-src "Unable to find {s} in unit {s}" name module-name))))))
  
    (if (and (zen:composite-reference? symbol) (zen:zen-walker-supports-composite? walker))
        (lookup-composite walker environment symbol)
      (nextmethod walker resume declaration environment symbol-src symbol)))


(zen:define-method (zen:lookup-analyse (zen:Zen-Walker walker) declaration symbol-src referenced-declaration)
  (if (and (%%is? declaration zen:Method-Declaration)
           (%%eq? (zen:get-method-declaration-propagation declaration) 'final)
           (or (%%eq? (zen:source-code symbol-src) 'self)
               (%%is? referenced-declaration zen:Slot-Declaration)
               (%%is? referenced-declaration zen:Method-Declaration)))
      (let ((data (zen:get-analysis-data (zen:get-declaration-locator declaration))))
        (zen:set-analysis-data-declaration-references data (%%cons referenced-declaration (zen:get-analysis-data-declaration-references data))))))


;;;
;;;; Assignment
;;;


(zen:define-method (zen:walk-symbol-assignment (zen:Zen-Walker walker) resume declaration environment symbol-src value form-src)
  (let ((name (zen:extract-selfdot (zen:source-code symbol-src))))
    (if (and name (zen:zen-walker-supports-selfdot? walker))
        (let ((slot-declaration (zen:lookup-declaration (zen:find-class-declaration declaration) name zen:private-access declaration)))
          (%%assertion (%%class-is? slot-declaration zen:Slot-Declaration) (zen:error "Slot expected: self.{s}" name)
            (zen:new-assignment form-src slot-declaration (zen:walk walker resume declaration environment value) symbol-src)))
      (nextmethod walker resume declaration environment symbol-src value form-src))))


;;;
;;;; With-Self
;;;


(zen:define-class zen:With-Self zen:Expression (constructor: zen:allocate-with-self)
  ((body getter: generate)))


(define (zen:new-with-self type body)
  (zen:allocate-with-self type #f body))


(zen:define-method (zen:emit-expression (zen:With-Self expression) declaration walker resume environment)
  (let ((type (zen:get-expression-type expression))
        (body (zen:get-with-self-body expression)))
    (let ((body-emit (parameterize ((zen:*self* (zen:new-code 'self type #f))
                                    (zen:stack-frame-body body))
                       (zen:emit-expression body declaration walker resume environment))))
      (zen:new-code
        (zen:simplify-begin
          `(begin
             ,@(zen:sourcified-form body-emit)))
        (zen:get-code-type body-emit)
        #f))))


(zen:define-method (zen:tree-fold (zen:With-Self expression) down up here seed environment)
  (let ((aug-env environment #; (cons 'self environment))
        (seed1 (down expression seed environment)))
    (up expression seed (zen:tree-fold (zen:get-with-self-body expression) down up here seed1 aug-env) environment)))


;;;
;;;; With-Dynamic-Self
;;;


(zen:define-class zen:With-Dynamic-Self zen:Expression (constructor: zen:allocate-with-dynamic-self)
  ((code getter: generate)
   (body getter: generate)))


(define (zen:new-with-dynamic-self type code body)
  (zen:allocate-with-dynamic-self type #f code body))


(zen:define-method (zen:emit-expression (zen:With-Dynamic-Self expression) declaration walker resume environment)
  ;; should type be (zen:get-expression-type expression) like for with-self
  (let ((type declaration)
        (code (zen:get-with-dynamic-self-code expression))
        (body (zen:get-with-dynamic-self-body expression)))
    (let ((body-emit (parameterize ((zen:*self* (zen:new-code code type #f)))
                       (zen:emit-statements-code body declaration walker resume environment))))
      (zen:new-code
        (zen:simplify-begin
          `(begin
             ,@(zen:sourcified-form body-emit)))
        (zen:get-code-type body-emit)
        #f))))


(define (zen:parse-with-dynamic-self form)
  (let ((code (%%car form))
        (body (%%cdr form)))
    (values code body)))


(define (zen:walk-with-dynamic-self-declaration walker resume declaration environment form-src)
  (receive (code body) (zen:parse-with-dynamic-self (%%cdr (zen:source-code form-src)))
    (zen:walk-declarations walker resume declaration environment body)))


(define (zen:walk-with-dynamic-self walker resume declaration environment form-src)
  (receive (code body) (zen:parse-with-dynamic-self (%%cdr (zen:source-code form-src)))
    (let ((new-environment (%%cons (zen:new-dynamic-self-binding #f code) environment)))
      (zen:new-with-dynamic-self
        ;; should type be (zen:find-class-declaration declaration) like for walk-with-self
        #f
        code
        (zen:walk-list walker resume declaration new-environment body)))))


;;;
;;;; Cast
;;;


(zen:define-class zen:Cast zen:Expression (constructor: zen:allocate-cast)
  ((expression getter: generate)))


(define (zen:new-cast type expression)
  (zen:allocate-cast type #f expression))


(zen:define-method (zen:emit-expression (zen:Cast expression) declaration walker resume environment)
  (let ((type (zen:get-expression-type expression))
        (expression (zen:get-cast-expression expression)))
    (let ((expression-emit (zen:emit-expression expression declaration walker resume environment)))
      (zen:new-code
        (zen:emit-type-cast
          expression-emit
          type
          (zen:get-expression-source expression)
          declaration
          walker
          resume
          environment)
        type
        #f))))


(zen:define-method (zen:tree-fold (zen:Cast expression) down up here seed environment)
  (up expression
      seed
      (zen:tree-fold (zen:get-cast-expression expression) down up here (down expression seed environment) environment)
      environment))


(define (zen:walk-cast walker resume declaration environment form-src)
  (let ((form (%%cdr (zen:source-code form-src))))
    (if (and (%%pair? form)
             (%%pair? (%%cdr form)))
        (let ((specifier (zen:source-code (%%car form)))
              (expression (%%cadr form)))
          (zen:new-cast (zen:walk-specifier walker resume declaration environment specifier)
                         (zen:walk walker resume declaration environment expression)))
      (zen:walk-error walker resume declaration form-src "Ill-formed cast"))))


;;;
;;;; Allege
;;;


(zen:define-class zen:Allege zen:Expression (constructor: zen:allocate-allege)
  ((test    getter: generate)
   (expr    getter: generate)
   (message getter: generate)))


(define (zen:new-allege source test expr message)
  (zen:allocate-allege #f source test expr message))


(zen:define-method (zen:emit-expression (zen:Allege expression) declaration walker resume environment)
  (let ((test (zen:get-allege-test expression))
        (expr (zen:get-allege-expr expression))
        (message (zen:get-allege-message expression)))
    (zen:bind (yes-environment . no-environment) (zen:branch-types test environment)
      (let ((expr (zen:emit-expression expr declaration walker resume yes-environment)))
        (if (zen:get-check? 'types)
            (let ((test (zen:emit-expression test declaration walker resume environment)))
              (zen:new-code
                `(if ,(zen:sourcified-form test)
                     ,(zen:sourcified-form expr)
                   (error ,message))
                (zen:get-code-type expr)
                (zen:get-expression-source expression)))
          (zen:new-code
            (zen:sourcified-form expr)
            (zen:get-code-type expr)
            (zen:get-expression-source expression)))))))


(zen:define-method (zen:tree-fold (zen:Allege expression) down up here seed environment)
  (up expression
      seed
      (zen:tree-fold
        (zen:get-allege-expr expression) down up here
        (zen:tree-fold
          (zen:get-allege-test expression) down up here (down expression seed environment) environment)
        environment)
      environment))


(define (zen:walk-%allege walker resume declaration environment form-src)
  (let ((form (zen:source-code form-src)))
    (let ((test (%%cadr form))
          (expr (%%car (%%cddr form))))
      (let ((message (and zen:debug-user? (let ((port (open-output-string)))
                                             (display "Allege " port)
                                             (write (zen:desourcify-all test) port)
                                             (display " failed" port)
                                             (get-output-string port)))))
        (zen:new-allege form-src
                         (zen:walk walker resume declaration environment test)
                         (zen:walk walker resume declaration environment expr)
                         message)))))


;;;
;;;; Allocate
;;;


(zen:define-class zen:Allocate zen:Expression (constructor: zen:allocate-allocate)
  ((class  getter: generate)
   (values getter: generate)))


(define (zen:new-allocate class values)
  (zen:allocate-allocate #f #f class values))


(zen:define-method (zen:emit-expression (zen:Allocate expression) declaration walker resume environment)
  (let ((class (zen:get-allocate-class expression))
        (values (zen:get-allocate-values expression)))
    (let ((class-emit (zen:emit-expression class declaration walker resume environment))
          (values-emit (zen:emit-expressions values declaration walker resume environment)))
      (zen:new-code
        `(%%object ,(zen:sourcified-form class-emit)
                   ,@(zen:codes-forms values-emit))
        (zen:get-binding-reference-binding class)
        #f))))


(zen:define-method (zen:tree-fold (zen:Allocate expression) down up here seed environment)
  (let ((class (zen:get-allocate-class expression))
        (values (zen:get-allocate-values expression)))
    (up expression
        seed
        (zen:tree-fold-list
          (%%cons class values) down up here
          (down expression seed environment)
          environment)
        environment)))


(define (zen:walk-allocate walker resume declaration environment form-src)
  (let ((form (%%cdr (zen:source-code form-src))))
    (let ((class (%%car form))
          (values (%%cdr form)))
      (zen:new-allocate (zen:walk walker resume declaration environment class)
                         (zen:walk-list walker resume declaration environment values)))))


;;;
;;;; Static
;;;


(zen:define-class zen:Static zen:Expression (constructor: zen:allocate-static)
  ((expression getter: generate)))


(define (zen:new-static expression)
  (zen:allocate-static #f #f expression))


(zen:define-method (zen:emit-expression (zen:Static expression) declaration walker resume environment)
  (let ((expr (zen:get-static-expression expression)))
    (let ((static (zen:register-static declaration "static" expr)))
      (let ((code (zen:emit-expression (%%cdr static) declaration walker resume environment)))
        (zen:new-code
          (%%car static)
          (zen:get-code-type code)
          #f)))))


(zen:define-method (zen:tree-fold (zen:Static expression) down up here seed environment)
  (up expression
      seed
      (zen:tree-fold (zen:get-static-expression expression) down up here (down expression seed environment) environment)
      environment))


(define (zen:walk-static walker resume declaration environment form-src)
  (let ((form (zen:source-code form-src)))
    (let ((expression (zen:walk walker resume declaration environment (%%cadr form))))
      (zen:new-static expression))))


;;;
;;;; Stack
;;;


(zen:define-class zen:Stack zen:Expression (constructor: zen:allocate-stack)
  ((init getter: generate)))


(define (zen:new-stack type init)
  (zen:allocate-stack type #f init))


(define (zen:require-stack-body declaration)
  (or (zen:stack-frame-body)
      (zen:error "Ill-formed stack")))


;; size is returned in 32 bit quads to match fixnums being shifted by 2
(define (zen:stack-type-info type init)
  (cond ((%%subtype? type zen:Flonum)
         (values (%%fx* 2 2) (%%fx* 1 8) zen:subtype-flonum #f))
        ((%%subtype? type zen:FixedF64Vector)
         (let ((len (%%get-class-user-data type)))
           (values (%%fx* (%%fx+ 1 len) 2) (%%fx* len 8) zen:subtype-f64vector #f)))
        ((%%subtype? type zen:F64Vector)
         (%%assert (%%pair? init)
           (let ((constant (%%car init)))
             (%%assert (%%is? constant zen:Constant)
               (let ((len (zen:source-code (zen:get-constant-expansion constant))))
                 (values (%%fx* (%%fx+ 1 len) 2) (%%fx* len 8) zen:subtype-f64vector #f))))))
        ((%%subtype? type zen:FixedF32Vector)
         (let ((len (%%get-class-user-data type)))
           (let ((words (%%fx+ (%%fxquotient len 2) (%%fxremainder len 2))))
             (values (%%fx* (%%fx+ 1 words) 2) (%%fx* len 4) zen:subtype-f32vector #f))))
        ((%%subtype? type zen:F32Vector)
         (%%assert (%%pair? init)
           (let ((constant (%%car init)))
             (%%assert (%%is? constant zen:Constant)
               (let ((len (zen:source-code (zen:get-constant-expansion constant))))
                 (let ((words (%%fx+ (%%fxquotient len 2) (%%fxremainder len 2))))
                   (values (%%fx* (%%fx+ 1 words) 2) (%%fx* len 4) zen:subtype-f32vector #f)))))))
        ((%%subtype? type zen:FixedS32Vector)
         (let ((len (%%get-class-user-data type)))
           (let ((words (%%fx+ (%%fxquotient len 2) (%%fxremainder len 2))))
             (values (%%fx* (%%fx+ 1 words) 2) (%%fx* len 4) zen:subtype-s32vector #f))))
        ((%%subtype? type zen:Vector)
         (let ((len (%%length init)))
           (values (%%fx* (%%fx+ 1 len) 2) (%%fx* len 8) zen:subtype-vector 'vector)))
        ((%%subtype? type zen:Values)
         (let ((len (%%length init)))
           (values (%%fx* (%%fx+ 1 len) 2) (%%fx* len 8) zen:subtype-boxvalues 'values)))
        ((%%subtype? type zen:Box)
         (let ((len (%%length init)))
           (%%assert (%%fx= len 1)
             (values (%%fx* (%%fx+ 1 len) 2) (%%fx* len 8) zen:subtype-boxvalues 'box))))
        ((%%class-is? type zen:Class-Declaration)
         (let ((len (%%length init)))
           (values (%%fx* (%%fx+ 1 len) 2) (%%fx* len 8) zen:subtype-zen 'zen)))
        (else
         (zen:error "Ill-formed stack"))))


(define (zen:register-stack body quads)
  (let ((offset (or (zen:get-body-stack-frame body) 0)))
    (zen:set-body-stack-frame body (%%fx+ offset quads))
    offset))


(zen:define-method (zen:emit-expression (zen:Stack expression) declaration walker resume environment)
  (let ((body (zen:require-stack-body declaration))
        (type (zen:get-expression-type expression))
        (init (zen:get-stack-init expression)))
    (receive (quads bytes subtype kind) (zen:stack-type-info type init)
      (let ((offset (zen:register-stack body quads)))
        (zen:new-code
          (cond ((%%not kind)
                 `(%%stack ,offset ,bytes ,subtype))
                ((%%eq? kind 'vector)
                 (let ((elements-emit (zen:codes-forms (zen:emit-expressions init declaration walker resume environment))))
                   `(let ()
                      (declare (not interrupts-enabled))
                      (let ((obj (%%stack ,offset ,bytes ,subtype)))
                        ,@(map (lambda (element-emit rank)
                                 `(%%vector-set! obj ,rank ,element-emit))
                               elements-emit
                               (zen:naturals 0 (%%length elements-emit)))
                        obj))))
                ((%%eq? kind 'values)
                 (let ((elements-emit (zen:codes-forms (zen:emit-expressions init declaration walker resume environment))))
                   `(let ()
                      (declare (not interrupts-enabled))
                      (let ((obj (%%stack ,offset ,bytes ,subtype)))
                        ,@(map (lambda (element-emit rank)
                                 `(%%values-set! obj ,rank ,element-emit))
                               elements-emit
                               (zen:naturals 0 (%%length elements-emit)))
                        obj))))
                ((%%eq? kind 'box)
                 (let ((element-emit (zen:sourcified-form (zen:emit-expression (%%car init) declaration walker resume environment))))
                   `(let ()
                      (declare (not interrupts-enabled))
                      (let ((obj (%%stack ,offset ,bytes ,subtype)))
                        (%%set-box! obj ,element-emit)
                        obj))))
                (else
                 (let ((class-emit (zen:sourcified-form (zen:emit-expression (%%car init) declaration walker resume environment)))
                       (slots-emit (zen:codes-forms (zen:emit-expressions (%%cdr init) declaration walker resume environment))))
                   `(let ()
                      (declare (not interrupts-enabled))
                      (let ((obj (%%stack ,offset ,bytes ,subtype)))
                        (%%set-object-class obj ,class-emit)
                        ,@(map (lambda (slot-emit rank)
                                 `(%%object-set! obj ,rank ,slot-emit))
                               slots-emit
                               (zen:naturals 1 (%%fx+ (%%length slots-emit) 1)))
                        obj)))))
          type
          #f)))))


(zen:define-method (zen:tree-fold (zen:Stack expression) down up here seed environment)
  (up expression
      seed
      (zen:tree-fold-list
        (zen:get-stack-init expression) down up here
        (down expression seed environment)
        environment)
      environment))


(define (zen:walk-stack walker resume declaration environment form-src)
  (let ((form (%%cdr (zen:source-code form-src))))
    (let ((specifier (zen:source-code (%%car form)))
          (init (zen:walk-list walker resume declaration environment (%%cdr form))))
      (let ((type (zen:walk-specifier walker resume declaration environment specifier)))
        (zen:new-stack type init)))))


;;;
;;;; Dispatch
;;;


(define (zen:cache-dispatch name global nodes)
  (lambda (object)
    (let ((class (zen:class-of object)))
      (let ((category (zen:locate-method-owner class name)))
        (%%assertion category (zen:error "Unable to find method {s} in: {s}" name object)
          (let ((field (%%get-category-field category name)))
            (%%assertion (%%class-is? field zen:Method) (zen:error "Field {s} is not a method of {s}" name object)
              ;; temporarely comment out for jedi stability
              ;; (%%assertion (or (%%not nodes) (%%memq (zen:get-category-identifier category) nodes)) (zen:error "Missing import for {s}.{s}" (zen:get-category-identifier category) name)
                (let ((proc
                        (case (%%get-method-dispatch-type field)
                          ((final)
                           (zen:final-dispatch field category))
                          ((class)
                           (zen:class-dispatch field category))
                          ((interface)
                           (zen:interface-dispatch field category)))))
                  (%%global-var-set! global proc)
                  (proc object)))))))))


(define (zen:final-dispatch field type)
  (lambda (object)
    (%%debug-assertion (%%category-is? object type) (zen:dispatch-error field object type)
      (%%final-dispatch (zen:class-of object) (%%get-method-implementation field)))))


(define (zen:class-dispatch field type)
  (let ((class-level (%%get-method-category-rank field))
        (implementation-rank (%%get-method-implementation-rank field)))
    (lambda (object)
      (%%debug-assertion (%%category-is? object type) (zen:dispatch-error field object type)
        (%%class-dispatch (zen:class-of object) class-level implementation-rank)))))


(define (zen:interface-dispatch field type)
  (let ((interface-rank (%%get-method-category-rank field))
        (implementation-rank (%%get-method-implementation-rank field)))
    (lambda (object)
      (%%debug-assertion (%%category-is? object type) (zen:dispatch-error field object type)
        (%%interface-dispatch (zen:class-of object) interface-rank implementation-rank)))))


(define (zen:dispatch class name)
  (or (zen:find-dispatch class name)
      (zen:error "Unable to find method {s} in: {s}" name class)))


(define (zen:find-dispatch class name)
  (let ((category (zen:locate-method-owner class name)))
    (if (%%not category)
        #f
      (let ((field (%%get-category-field category name)))
        (if (not (%%class-is? field zen:Method))
            (zen:error "Field {s} is not a method of {s}" name class))
        (case (%%get-method-dispatch-type field)
          ((final)
           (%%final-dispatch class (%%get-method-implementation field)))
          ((class)
           (%%class-dispatch class (%%get-method-category-rank field) (%%get-method-implementation-rank field)))
          ((interface)
           (%%interface-dispatch class (%%get-method-category-rank field) (%%get-method-implementation-rank field))))))))


;;;
;;;; Autoload
;;;


(define (zen:cache-autoload unit-name reference-locator global)
  (lambda ()
    (zen:load-unit unit-name)
    (let ((object (%%global-var-ref reference-locator)))
      (%%global-var-set! global (lambda () object))
      object)))


;;;
;;;; Hub
;;;


(zen:define-class zen:Hub-Declaration zen:Declaration (constructor: zen:allocate-hub-declaration)
  ((hubs  getter: generate setter: generate)
   (nodes getter: generate setter: generate)))


(zen:define-variable-override zen:new-hub-declaration
  (lambda (name type access compatibility modifiers attributes parent hubs nodes)
    (let ((new-declaration (zen:allocate-hub-declaration name type #f access compatibility modifiers attributes #f parent #f #f #f #f nodes)))
      (zen:set-hub-declaration-hubs new-declaration (or hubs (%%list new-declaration)))
      (zen:setup-declaration new-declaration)
      new-declaration)))


(zen:define-method (zen:emit-declaration (zen:Hub-Declaration declaration) walker resume environment)
  (zen:sourcify-deep-if
    `(begin)
    (zen:get-declaration-source declaration)))


(zen:define-method (zen:walk-binding-validate-call (zen:Hub-Declaration declaration) walker resume source-declaration operator arguments form-src)
  #f)


(zen:define-method (zen:emit-binding-reference (zen:Hub-Declaration declaration) source-declaration walker resume environment)
  (let ((name (zen:get-lexical-binding-name declaration))
        (source #f))
    (zen:dispatch-reference-emit name source declaration walker resume environment)))


(zen:define-method (zen:emit-binding-call (zen:Hub-Declaration declaration) binding-src arguments arguments-codes source-declaration walker resume environment)
  (if (%%null? arguments)
      (zen:error "Ill-formed hub call: ({a})" (zen:get-lexical-binding-name declaration))
    (let ((name (zen:get-lexical-binding-name declaration))
          (nodes (zen:get-hub-declaration-nodes declaration))
          (object-argument (%%car arguments))
          (object-code (%%car arguments-codes))
          (others-arguments (%%cdr arguments))
          (others-codes (%%cdr arguments-codes)))
      (zen:dispatch-call-emit name nodes binding-src source-declaration walker resume environment object-argument object-code others-arguments others-codes))))


(zen:define-method (zen:outline-extract (zen:Hub-Declaration declaration) meta)
  #f
  #; ;; not sure if this breaks together's script evaluation
  `(hub ,(zen:get-lexical-binding-name declaration)))


(zen:define-variable-override zen:hub-declaration-class
  zen:Hub-Declaration)

(zen:define-variable-override zen:hub-declaration-hubs
  zen:get-hub-declaration-hubs)

(zen:define-variable-override zen:hub-declaration-hubs-set!
  zen:set-hub-declaration-hubs)

(zen:define-variable-override zen:hub-declaration-nodes
  zen:get-hub-declaration-nodes)

(zen:define-variable-override zen:hub-declaration-nodes-set!
  zen:set-hub-declaration-nodes)


(zen:define-variable-override zen:manifest-ignore?
  (lambda (obj)
    (%%is? obj zen:Hub-Declaration)))


;;;
;;;; Node
;;;


(define (zen:parse-node walker resume declaration rest)
  (zen:bind (access name category-name) rest
    (let ((access (zen:source-code access))
          (name (zen:source-code name))
          (category-name (zen:source-code category-name)))
      (values name category-name zen:Any access 'uptodate '()))))


(define (zen:walk-node-declaration walker resume declaration environment form-src)
  (receive (name category-name type access compatibility modifiers) (zen:parse-node walker resume declaration (%%cdr (zen:source-code form-src)))
    (let ((category-locator (zen:get-declaration-locator (zen:lookup-reference walker resume declaration environment category-name)))
          (actual-declaration (or (zen:find-declaration-child declaration name)
                                  (%%table-ref (zen:get-private-lookup declaration) name #f))))
      (let ((new-declaration (zen:new-hub-declaration name type access compatibility modifiers '() declaration #f (%%list category-locator))))
        (zen:set-declaration-source new-declaration form-src)
        (if actual-declaration
            (if (%%class-is? actual-declaration zen:hub-declaration-class)
                (let ((actual-hubs (zen:get-hub-declaration-hubs actual-declaration))
                      (actual-nodes (zen:get-hub-declaration-nodes actual-declaration)))
                  (zen:set-hub-declaration-hubs actual-declaration (zen:union actual-hubs (%%list new-declaration)))
                  (zen:set-hub-declaration-nodes actual-declaration (zen:union actual-nodes (%%list category-locator)))
                  (zen:add-declaration-child walker resume declaration actual-declaration)
                  actual-declaration)
              (zen:error "Found conflicting declarations for {a}" name))
          (let ((effective-declaration (zen:add-declaration-child walker resume declaration new-declaration)))
            effective-declaration))))))


(define (zen:walk-node walker resume declaration environment form-src)
  (receive (name category-name type access compatibility modifiers) (zen:parse-node walker resume declaration (%%cdr (zen:source-code form-src)))
    (let ((new-declaration (zen:require-declaration declaration name)))
      (zen:set-declaration-source new-declaration form-src)
      new-declaration)))


;;;
;;;; Shape
;;;


(define zen:module-shapes
  (%%make-table test: eq?))


(define zen:module-shapes-mutex
  (make-mutex 'module-shapes))


(zen:define-variable-override zen:module-shape
  (lambda (walker resume declaration unit-name)
    (or (%%table-ref zen:module-shapes unit-name #f)
        (begin
          (mutex-lock! zen:module-shapes-mutex)
          (let ((result (or (%%table-ref zen:module-shapes unit-name #f)
                            (let ((shape (zen:shape-module walker resume declaration unit-name)))
                              (%%table-set! zen:module-shapes unit-name shape)
                              shape))))
            (mutex-unlock! zen:module-shapes-mutex)
            result)))))


(define (zen:shape-module walker resume declaration unit-name)
  (define (shape-class toplevel)
    (receive (access abstraction compatibility implementor modifiers rest) (zen:parse-modifiers walker resume declaration zen:class-modifiers (%%cdr toplevel))
      (shape-category 'class rest)))
  
  (define (shape-interface toplevel)
    (receive (access compatibility implementor modifiers rest) (zen:parse-modifiers walker resume declaration zen:interface-modifiers (%%cdr toplevel))
      (shape-category 'interface rest)))
  
  (define (shape-remotable-stub toplevel)
    (define method-stub-modifiers
      '(((private protected package public) . private)
        ((post send call) . call)
        ((reference value) . reference)))
    
    (receive (access compatibility implementor modifiers rest) (zen:parse-modifiers walker resume declaration zen:interface-modifiers (%%cdr toplevel))
      (let ((category-name (zen:source-code (%%car rest))))
        (let ((stub-interface (%%string->symbol (%%string-append (%%symbol->string category-name) "-Stub"))))
          (%%cons 'interface
                  (%%cons stub-interface
                          (zen:collect (lambda (form-src)
                                          (let ((form (zen:source-code form-src)))
                                            (and (%%pair? form)
                                                 (let ((form-name (zen:source-code (%%car form))))
                                                   (and (%%eq? form-name 'method)
                                                        (receive (access invocation passage modifiers rest) (zen:parse-modifiers walker resume declaration method-stub-modifiers (%%cdr form))
                                                          (zen:source-code (%%car (zen:source-code (%%car rest))))))))))
                                        (%%cdr rest))))))))

  (define (shape-category declaration-name rest)
    (let ((category-name (zen:source-code (%%car rest))))
      (%%cons declaration-name
              (%%cons category-name
                      (let ((hubs (zen:new-queue)))
                        (for-each (lambda (form-src)
                                    (let ((form (zen:source-code form-src)))
                                      (%%when (%%pair? form)
                                        (let ((form-name (zen:source-code (%%car form))))
                                          (cond ((%%eq? form-name 'attributes)
                                                 (shape-attributes form-src hubs))
                                                ((or (%%eq? form-name 'slot)
                                                     (%%eq? form-name 'property))
                                                 (shape-slot form-src hubs))
                                                ((%%eq? form-name 'method)
                                                 (shape-method form hubs)))))))
                                  (%%cdr rest))
                        (zen:queue-list hubs))))))
  
  (define (shape-attributes form-src hubs)
    (for-each (lambda (form-src)
                (let ((form (zen:source-code form-src)))
                  (%%when (%%pair? form)
                    (let ((form-name (zen:source-code (%%car form))))
                      (%%when (%%eq? form-name 'slot)
                        (let ((name (zen:source-code (%%cadr form))))
                          (let ((getter (%%string->symbol (zen:system-format "get-{a}" name)))
                                (setter (%%string->symbol (zen:system-format "set-{a}" name))))
                            (zen:enqueue hubs getter)
                            (zen:enqueue hubs setter))))))))
              (%%cddr (zen:source-code form-src))))
  
  (define (shape-slot form-src hubs)
    (zen:with-expand-slot-form walker resume declaration form-src
      (lambda (name specifier access compatibility modifiers initialize
               getter-access getter-propagation getter-abstraction getter-expansion getter-generation getter-name
               setter-access setter-propagation setter-abstraction setter-expansion setter-generation setter-name
               name-self generate-getter? generate-setter? specifier-list)
        (%%when generate-getter?
          (zen:enqueue hubs getter-name))
        (%%when generate-setter?
          (zen:enqueue hubs setter-name)))))
  
  (define (shape-method form hubs)
    (let ((meta? (%%eq? (zen:source-code (%%cadr form)) 'meta)))
      (let ((rest (if meta? (%%cddr form) (%%cdr form))))
        (receive (access compatibility propagation abstraction expansion remote synchronized modifiers rest) (zen:parse-modifiers walker resume declaration zen:method-modifiers rest)
          (let ((name (zen:source-code (%%car (zen:source-code (%%car rest))))))
            (%%when (or (%%eq? access 'public)
                        (%%eq? access 'package))
              (zen:enqueue hubs (if meta? (%%list name) name))))))))
  
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
            (raise (zen:new-walk-source-not-found (zen:format "Unable to locate unit source: {s}" unit-name) unit-name))
          (zen:with-verbose (zen:shape-verbose?) "shaping" (zen:resource-pathname src)
            (lambda ()
              ;; not reading the literals is necessary as reading a literal will load units
              (let ((form (zen:read-toplevel-form src read-literals?: #f)))
                (let ((module (zen:source-code form)))
                  (zen:collect (lambda (src)
                                  (let ((toplevel (zen:source-code src)))
                                    (and (%%pair? toplevel)
                                         (let ((declaration-name (zen:source-code (%%car toplevel))))
                                           (cond ((%%eq? declaration-name 'class)
                                                  (shape-class toplevel))
                                                 ((%%eq? declaration-name 'interface)
                                                  (shape-interface toplevel))
                                                 ((%%eq? declaration-name 'remotable-stub)
                                                  (shape-remotable-stub toplevel))
                                                 (else
                                                  #f))))))
                                module))))))))))


;;;
;;;; Definition
;;;


(define zen:definition-modifiers
  '(((private protected package public) . private)
    ((deprecated undocumented uptodate) . uptodate)
    ((inline onsite) . onsite)))


(define (zen:parse-definition walker resume declaration form-src)
  (define (ill-formed)
    (zen:walk-error walker resume declaration form-src "Ill-formed definition"))
  
  (let ((rest (%%cdr (zen:source-code form-src))))
    (receive (access compatibility expansion modifiers rest) (zen:parse-modifiers walker resume declaration zen:definition-modifiers rest)
      (cond ((%%null? rest)
             (ill-formed))
            ((%%symbol? (zen:unwrap-syntactic-closure (%%car rest)))
             (let ((name (zen:unwrap-syntactic-closure (%%car rest)))
                   (name-src (%%car rest)))
               (zen:parse-specifier (%%cdr rest)
                 (lambda (specifier specifier-source rest)
                   (values name name-src specifier specifier-source access compatibility expansion modifiers (if (%%null? rest) (%%list 'unspecified) (%%car rest)) #f)))))
            ((%%pair? (zen:unwrap-syntactic-closure (%%car rest)))
             (let* ((name (zen:source-code (%%car (zen:unwrap-syntactic-closure (%%car rest)))))
                    (name-src (%%car (zen:unwrap-syntactic-closure (%%car rest))))
                    (parameters (%%cdr (zen:source-code (%%car rest)))))
               (if (%%symbol? name)
                   (zen:parse-specifier (%%cdr rest)
                     (lambda (specifier specifier-source body)
                       (let ((effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body))
                             (specifier-list (if specifier (%%list specifier) '())))
                         (let ((value
                                 `(lambda ,parameters ,@specifier-list
                                          ,@effective-body)))
                           (values name name-src specifier specifier-source access compatibility expansion modifiers value parameters)))))
                 (ill-formed))))
            (else
             (ill-formed))))))


(define (zen:walk-extended-definition-declaration walker resume declaration environment form-src new-definition-declaration)
  (receive (name name-src specifier specifier-source access compatibility expansion modifiers value parameters) (zen:parse-definition walker resume declaration form-src)
    (%%assertion (%%class-is? declaration zen:Namespace-Declaration) (zen:walk-error walker resume declaration form-src "Definitions can only be defined inside namespaces: {s}" name)
      (let ((type (zen:specifier->type walker resume declaration environment specifier)))
        (let ((signature (and parameters (zen:walk-parameters walker resume declaration environment parameters #t #f))))
          (let ((effective-type (if signature (zen:signature->function-type signature type) type)))
            (let ((new-declaration (or (zen:find-declaration-child-of-type zen:Definition-Declaration declaration name)
                                       (new-definition-declaration name effective-type access compatibility modifiers '() declaration expansion signature specifier-source))))
              (zen:set-declaration-source new-declaration form-src)
              (zen:set-declaration-name-source new-declaration name-src)
              (let ((effective-declaration (zen:add-declaration-child walker resume declaration new-declaration)))
                (%%when (%%eq? expansion 'inline)
                  (let ((new-environment (%%cons effective-declaration environment)))
                    (zen:set-definition-declaration-signature effective-declaration signature)
                    (zen:set-definition-declaration-value effective-declaration
                                                           (zen:walk walker resume effective-declaration new-environment value))))
                effective-declaration))))))))


(define (zen:walk-extended-definition walker resume declaration environment form-src)
  (receive (name name-src specifier specifier-source access compatibility expansion modifiers value parameters) (zen:parse-definition walker resume declaration form-src)
    (%%assertion (%%class-is? declaration zen:Namespace-Declaration) (zen:walk-error walker resume declaration form-src "Definitions can only be defined inside namespaces: {s}" name)
      (let ((new-declaration (zen:require-declaration-of-type zen:Definition-Declaration declaration name)))
        (%%when (%%neq? expansion 'inline)
          #; ;; wait buggy if file is both loaded interpreted and compiled
          (%%when (and (%%neq? (zen:walk-for) 'eval) (zen:get-definition-declaration-value new-declaration))
            (zen:walk-error walker resume declaration form-src "Cannot redefine definition: {s}" name))
          ;; adding source information for parameters (default for optional and keyword may be source code)
          ;; zen:find-annotated fails on first keyword at zen.language.runtime:minimum
          ;; because (eq? variable annotated-variable) -> one points to a stale value
          (let ((new-environment (if #f #; parameters
                                     (receive (signature augmented-environment) (zen:walk-parameters walker resume declaration environment parameters #t #t)
                                       (zen:set-definition-declaration-signature new-declaration signature)
                                       (%%cons new-declaration augmented-environment))
                                     (%%cons new-declaration environment))))
            (zen:set-definition-declaration-value new-declaration (zen:walk walker resume new-declaration new-environment value))))
        (zen:set-declaration-source new-declaration form-src)
        (zen:set-declaration-name-source new-declaration name-src)
        new-declaration))))


(define (zen:walk-definition-declaration walker resume declaration environment form-src)
  (zen:walk-extended-definition-declaration walker resume declaration environment form-src
    zen:new-definition-declaration))


(define (zen:walk-definition walker resume declaration environment form-src)
  (zen:walk-extended-definition walker resume declaration environment form-src))


;;;
;;;; Specialize Macro
;;;


(define zen:specialize-modifiers
  '(((inline onsite) . onsite)))


(define (zen:parse-specialize walker resume declaration rest)
  (receive (expansion modifiers rest) (zen:parse-modifiers walker resume declaration zen:specialize-modifiers rest)
    (if (%%eq? (%%car rest) 'as)
        (values expansion (%%cadr rest) modifiers (%%cddr rest))
      (values expansion #f modifiers rest))))


(define (zen:expand-specialize walker resume declaration environment . rest)
  (receive (expansion as modifiers rest) (zen:parse-specialize walker resume declaration rest)
    (let ((signature (%%car rest))
          (rest (%%cdr rest)))
      (let ((operator (%%car signature))
            (parameters (%%cdr signature)))
        (let ((name (or as (zen:compose-specializer-name operator parameters))))
          `(begin
             (definition public undocumented ,expansion (,name ,@parameters) ,@rest)
             (%specialize ,operator ,name)))))))


(define (zen:compose-specializer-name operator parameters)
  (%%string->symbol
    (%%string-append (%%symbol->string operator)
                     (%%apply string-append (%%apply append (map (lambda (parameter)
                                                                   (if (zen:specifier? parameter)
                                                                       (%%list (%%symbol->string (zen:specifier->name parameter)))
                                                                     '()))
                                                                 parameters))))))


;;;
;;;; Specialize
;;;


(define (zen:walk-%specialize-declaration walker resume declaration environment form-src)
  (let ((specialized (zen:source-code (%%cadr (zen:source-code form-src))))
        (specializer (zen:source-code (%%car (%%cddr (zen:source-code form-src))))))
    (let ((specialized-declaration (zen:lookup-reference walker resume declaration environment specialized))
          (specializer-declaration (zen:lookup-reference walker resume declaration environment specializer)))
      (zen:add-specializer specialized-declaration specializer-declaration)
      (zen:new-specialize))))


;; we should not even have to do this
(define (zen:walk-%specialize walker resume declaration environment form-src)
  (zen:new-specialize))


;;;
;;;; Generic
;;;


(define zen:generic-modifiers
  '(((private protected package public) . public)
    ((deprecated undocumented uptodate) . uptodate)))


(define (zen:parse-generic walker resume declaration rest)
  (receive (access compatibility modifiers rest) (zen:parse-modifiers walker resume declaration zen:generic-modifiers rest)
    (let ((signature (zen:source-code (%%car rest))))
      (let ((name (zen:source-code (%%car signature)))
            (name-src (%%car signature))
            (parameters (%%cdr signature)))
        (zen:parse-specifier (%%cdr rest)
          (lambda (specifier specifier-source body)
            (values name name-src specifier access compatibility modifiers parameters body)))))))


(define (zen:walk-generic-declaration walker resume declaration environment form-src)
  (receive (name name-src specifier access compatibility modifiers parameters body) (zen:parse-generic walker resume declaration (%%cdr (zen:source-code form-src)))
    (%%assertion (%%class-is? declaration zen:Module-Declaration) (zen:walk-error walker resume declaration form-src "Generics can only be defined at the module level: {s}" name)
      (let ((type (if specifier (zen:walk-specifier walker resume declaration environment specifier) zen:Any))
            (dispatch-type-declarations (map (lambda (dynamic-parameter-type)
                                               (zen:lookup-reference walker resume declaration environment dynamic-parameter-type))
                                             (zen:dynamic-parameter-types parameters)))
            (signature (zen:walk-parameters walker resume declaration environment parameters #t #f)))
        (let ((new-declaration (zen:new-generic-declaration name type access compatibility modifiers '() declaration dispatch-type-declarations signature)))
          (zen:set-declaration-source new-declaration form-src)
          (zen:set-declaration-name-source new-declaration name-src)
          (let ((effective-declaration (zen:add-declaration-child walker resume declaration new-declaration)))
            effective-declaration))))))


(define (zen:walk-generic walker resume declaration environment form-src)
  (receive (name name-src specifier access compatibility modifiers parameters body) (zen:parse-generic walker resume declaration (%%cdr (zen:source-code form-src)))
    (%%assertion (%%class-is? declaration zen:Module-Declaration) (zen:walk-error walker resume declaration form-src "Generics can only be defined at the module level: {s}" name)
      (receive (signature augmented-environment) (zen:walk-parameters walker resume declaration environment parameters #t #t)
        (let ((new-declaration (zen:require-declaration declaration name)))
          (zen:set-generic-declaration-signature new-declaration signature)
          (zen:set-generic-declaration-body new-declaration
                                          (zen:walk-body walker resume new-declaration augmented-environment body))
          (zen:set-declaration-source new-declaration form-src)
          (zen:set-declaration-name-source new-declaration name-src)
          new-declaration)))))


;;;
;;;; Specific
;;;


(define zen:specific-modifiers
  '())


(define (zen:parse-specific walker resume declaration rest)
  (receive (modifiers rest) (zen:parse-modifiers walker resume declaration zen:specific-modifiers rest)
    (let* ((signature (zen:source-code (%%car rest)))
           (name (zen:source-code (%%car signature)))
           (name-src (%%car signature))
           (parameters (%%cdr signature))
           (body (%%cdr rest))
           (effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body)))
      (values modifiers name name-src parameters effective-body))))


(define (zen:walk-specific walker resume declaration environment form-src)
  (define (root-dynamic-parameters? generic-declaration specific-signature name parameters)
    (let iter ((generic-parameters (zen:get-signature-positional (zen:get-generic-declaration-signature generic-declaration)))
               (specific-parameters (zen:get-signature-positional specific-signature))
               (root? #t))
      (let ((generic-parameter (and (%%pair? generic-parameters) (%%car generic-parameters)))
            (specific-parameter (and (%%pair? specific-parameters) (%%car specific-parameters))))
        (let ((generic-dynamic? (%%is? generic-parameter zen:Dynamic-Parameter))
              (specific-dynamic? (%%is? specific-parameter zen:Dynamic-Parameter)))
          (cond ((and generic-dynamic? specific-dynamic?)
                 (let ((generic-class (zen:resolve-binding (zen:get-binding-reference-binding (zen:get-dynamic-parameter-class generic-parameter))))
                       (specific-class (zen:resolve-binding (zen:get-binding-reference-binding (zen:get-dynamic-parameter-class specific-parameter)))))
                   (if (zen:of-subtype? generic-class specific-class)
                       (iter (%%cdr generic-parameters)
                             (%%cdr specific-parameters)
                             (%%eq? generic-class specific-class))
                     (zen:walk-error walker resume declaration form-src "Dynamic parameter {a} is not a subtype of {a}: {s}"
                       (zen:get-lexical-binding-name specific-parameter)
                       (zen:get-declaration-locator generic-class)
                       (%%cons name parameters)))))
                ((or generic-dynamic? specific-dynamic?)
                 (zen:walk-error walker resume declaration form-src "Specific must dispatch on the same number of dynamic parameters: {s}" (%%cons name parameters)))
                (else
                 root?))))))
  
  (receive (modifiers name name-src parameters body) (zen:parse-specific walker resume declaration (%%cdr (zen:source-code form-src)))
    (%%assertion (%%class-is? declaration zen:Module-Declaration) (zen:walk-error walker resume declaration form-src "Specifics can only be defined at the module level: {s}" name)
      (let ((generic-declaration (zen:lookup-declaration declaration name zen:private-access declaration)))
        (if (%%class-is? generic-declaration zen:Generic-Declaration)
            (receive (signature augmented-environment) (zen:walk-parameters walker resume declaration environment parameters #t #t)
              (let* ((root? (root-dynamic-parameters? generic-declaration signature name parameters))
                     (new-declaration (zen:new-specific-declaration name #f 'public 'uptodate modifiers '() declaration generic-declaration signature root?))
                     (body-environment (if root? augmented-environment (%%cons (zen:new-nextmethod-variable 'nextmethod #f #f #f) augmented-environment))))
                (zen:set-specific-declaration-body new-declaration
                                                 (zen:walk-body walker resume new-declaration body-environment body))
                (zen:set-declaration-source new-declaration form-src)
                (zen:set-declaration-name-source new-declaration name-src)
                new-declaration))
          (zen:walk-error walker resume declaration form-src "Cannot find generic declaration for {s}" (%%cons name parameters)))))))


;;;
;;;; Class
;;;


(define zen:class-modifiers
  '(((private protected package public) . public)
    ((abstract concrete) . concrete)
    ((deprecated undocumented uptodate) . uptodate)
    ((primitive native) . native)))

(define zen:class-keywords
  '(metaclass extends implements attributes))


(define (zen:parse-class walker resume declaration rest)
  (receive (access abstraction compatibility implementor modifiers rest) (zen:parse-modifiers walker resume declaration zen:class-modifiers rest)
    (let ((name (zen:source-code (%%car rest)))
          (type zen:Any)
          (rest (%%cdr rest)))
      (%%assert (%%symbol? name)
        (receive (metaclass-name ascendant-name interface-names attributes body) (zen:parse-keywords zen:class-keywords rest)
          (values name type access abstraction compatibility implementor modifiers metaclass-name ascendant-name interface-names attributes body))))))


(define (zen:expand-class walker resume declaration environment form-src)
  (define (preprocess-class name body)
    (let ((metaclass (zen:new-queue))
          (class (zen:new-queue))
          (nodes (zen:new-queue)))
      (define (preprocess expr)
        (let ((expr (parameterize ((zen:current-declaration-name name))
                      (zen:expand-macros walker resume declaration environment expr))))
          (cond ((and (%%pair? (zen:source-code expr))
                      (%%eq? (zen:source-code (%%car (zen:source-code expr))) 'begin))
                 (for-each preprocess (%%cdr (zen:source-code expr))))
                ((and (%%pair? (zen:source-code expr))
                      (%%pair? (%%cdr (zen:source-code expr)))
                      (%%eq? (zen:source-code (%%cadr (zen:source-code expr))) 'meta))
                 (zen:enqueue metaclass (zen:sourcify-deep-if (%%cons (%%car (zen:source-code expr)) (%%cddr (zen:source-code expr))) expr)))
                ;; first draft lets not worry about meta public methods generating nodes
                ((and (%%pair? (zen:source-code expr))
                      (%%eq? (zen:source-code (%%car (zen:source-code expr))) 'method))
                 (zen:enqueue class expr)
                 (receive (access compatibility propagation abstraction expansion remote synchronized modifiers rest) (zen:parse-modifiers walker resume declaration zen:method-modifiers (%%cdr (zen:source-code expr)))
                   (%%unless (or (%%eq? access 'hidden)
                                 (%%eq? propagation 'override))
                     ;; for protected we generate a private node so code in the same module can access the method
                     (let ((adjusted-access (if (%%eq? access 'protected) 'private access)))
                       (let ((signature (zen:source-code (%%car rest))))
                         (let ((method-name (zen:source-code (%%car signature))))
                           (zen:enqueue nodes `(node ,adjusted-access ,method-name ,name))))))))
                (else
                 (zen:enqueue class expr)))))
      
      (for-each preprocess body)
      (values (zen:queue-list metaclass)
              (zen:queue-list class)
              (zen:queue-list nodes))))
  
  (receive (name type access abstraction compatibility implementor modifiers metaclass-name ascendant-name interface-names attributes body) (zen:parse-class walker resume declaration (%%cdr (zen:source-code form-src)))
    (receive (metaclass-body class-body nodes) (preprocess-class name body)
      (cond ((and (%%not-null? metaclass-body)
                  (%%specified? metaclass-name))
             (zen:walk-error walker resume declaration form-src "Ambiguous use of both metaclass and meta keywords: {s}" name))
            ((or (%%specified? metaclass-name)
                 (zen:core-class? name)
                 (%%unspecified? ascendant-name))
             (zen:sourcify-deep-if
               `(begin
                  (%class ,@(%%cdr (zen:source-code form-src)))
                  ,@(map (lambda (node)
                           (zen:sourcify-deep-if
                             node
                             form-src))
                         nodes))
               form-src))
            (else
             (let ((metaclass-name (%%string->symbol (%%string-append (%%symbol->string name) "~Class"))))
               `(begin
                  ,(zen:sourcify-deep-if
                     `(%class ,metaclass-name extends (:class ,ascendant-name)
                        ,@metaclass-body)
                     form-src)
                  ,(zen:sourcify-deep-if
                     `(%class ,access ,abstraction ,compatibility ,implementor ,name metaclass (:generated ,metaclass-name) extends ,ascendant-name implements ,interface-names
                        ,@class-body)
                     form-src)
                  ,@(map (lambda (node)
                           (zen:sourcify-deep-if
                             node
                             form-src))
                         nodes))))))))


(define (zen:walk-%class-declaration walker resume declaration environment form-src)
  (define (lookup-metaclass walker resume declaration environment ascendant metaclass-name)
    (cond ((or (zen:unspecified? metaclass-name) (%%eq? metaclass-name 'Object-Class))
           (values #f #f))
          ((%%symbol? metaclass-name)
           (values (zen:lookup-reference walker resume declaration environment metaclass-name) #t))
          (else
           (values (zen:lookup-reference walker resume declaration environment (%%cadr metaclass-name)) #f))))
  
  (receive (name type access abstraction compatibility implementor modifiers metaclass-name ascendant-name interface-names attributes body) (zen:parse-class walker resume declaration (%%cdr (zen:source-code form-src)))
    (%%assertion (%%class-is? declaration zen:Module-Declaration) (zen:walk-error walker resume declaration form-src "Classes can only be defined at the module level: {s}" name)
      ;; explicit test on Object-Class is to break circularity
      (receive (ascendant ascendant-relation ascendant-base) (zen:lookup-ascendant walker resume declaration environment ascendant-name)
        (receive (metaclass metaclass-explicit?) (lookup-metaclass walker resume declaration environment ascendant metaclass-name)
          (let ((interfaces (if (zen:unspecified? interface-names) '() (map (lambda (interface-name) (zen:lookup-reference walker resume declaration environment interface-name)) (zen:listify interface-names)))))
            (let ((new-declaration (or (zen:find-declaration-child declaration name)
                                       (zen:new-class-declaration name type access compatibility modifiers attributes declaration implementor metaclass metaclass-explicit? ascendant ascendant-relation ascendant-base interfaces))))
              (zen:set-declaration-source new-declaration form-src)
              (let ((effective-declaration (zen:add-declaration-child walker resume declaration new-declaration)))
                (zen:setup-class-lookups effective-declaration)
                (let ((new-environment (%%cons effective-declaration environment)))
                  (zen:walk-declarations walker resume effective-declaration new-environment body)
                  effective-declaration)))))))))


(define (zen:walk-%class walker resume declaration environment form-src)
  (receive (name type access abstraction compatibility implementor modifiers metaclass-name ascendant-name interface-names attributes body) (zen:parse-class walker resume declaration (%%cdr (zen:source-code form-src)))
    (%%assertion (%%class-is? declaration zen:Module-Declaration) (zen:walk-error walker resume declaration form-src "Classes can only be defined at the module level: {s}" name)
      (let* ((new-declaration (zen:require-declaration declaration name))
             (new-environment (%%cons new-declaration environment))
             (ascendant-declaration (zen:get-class-declaration-ascendant new-declaration)))
        (if (and (%%not ascendant-declaration) (%%neq? name 'Object))
            (zen:walk-error walker resume declaration form-src "Class {s} does not specify an ascendant" name)
          (begin
            (zen:set-namespace-declaration-body new-declaration (zen:walk-namespace walker resume new-declaration new-environment body))
            (zen:set-declaration-source new-declaration form-src)
            new-declaration))))))


(define (zen:lookup-ascendant walker resume declaration environment ascendant-name)
  (cond ((zen:unspecified? ascendant-name)
         (values #f
                 #f
                 #f))
        ((and (%%pair? ascendant-name)
              (%%eq? (%%car ascendant-name) ':class))
         (let ((object-class (zen:lookup-reference walker resume declaration environment 'Object-Class)))
           (let rec ((ascendant-name ascendant-name))
                (if (%%pair? ascendant-name)
                    (receive (decl relation base) (rec (%%cadr ascendant-name))
                      (values (if (%%eq? decl object-class)
                                  object-class
                                (or (zen:effective-class-declaration-metaclass base) object-class))
                              (%%cons (%%car ascendant-name) relation)
                              base))
                  (let ((base (zen:lookup-reference walker resume declaration environment ascendant-name)))
                    (values base '() base))))))
        (else
         (values (zen:lookup-reference walker resume declaration environment ascendant-name)
                 #f
                 #f))))


(define (zen:effective-class-declaration-metaclass class-declaration)
  (if (%%not class-declaration)
      #f
    (let ((class-declaration (zen:resolve-binding class-declaration)))
      (or (zen:get-category-declaration-metaclass class-declaration)
          (let ((ascendant (zen:get-class-declaration-ascendant class-declaration)))
            (if (%%not ascendant)
                #f
              (zen:effective-class-declaration-metaclass ascendant)))))))


;;;
;;;; Interface
;;;


(define zen:interface-modifiers
  '(((private protected package public) . public)
    ((deprecated undocumented uptodate) . uptodate)
    ((primitive native) . native)))

(define zen:interface-keywords
  '(metaclass extends attributes))


(define (zen:parse-interface walker resume declaration rest)
  (receive (access compatibility implementor modifiers rest) (zen:parse-modifiers walker resume declaration zen:interface-modifiers rest)
    (let ((name (%%car rest))
          (type zen:Any)
          (rest (%%cdr rest)))
      (%%assert (%%symbol? name)
        (receive (metaclass-name ascendant-names attributes body) (zen:parse-keywords zen:interface-keywords rest)
          (values name type access compatibility implementor modifiers metaclass-name ascendant-names attributes body))))))


;; copy/paste of expand-class
(define (zen:expand-interface walker resume declaration environment form-src)
  (define (preprocess-interface name body)
    (let ((metaclass (zen:new-queue))
          (interface (zen:new-queue))
          (nodes (zen:new-queue)))
      (define (preprocess expr)
        (let ((expr (parameterize ((zen:current-declaration-name name))
                      (zen:expand-macros walker resume declaration environment expr))))
          (cond ((and (%%pair? (zen:source-code expr))
                      (%%eq? (zen:source-code (%%car (zen:source-code expr))) 'begin))
                 (for-each preprocess (%%cdr (zen:source-code expr))))
                ((and (%%pair? (zen:source-code expr))
                      (%%pair? (%%cdr (zen:source-code expr)))
                      (%%eq? (zen:source-code (%%cadr (zen:source-code expr))) 'meta))
                 (zen:enqueue metaclass (zen:sourcify-deep-if (%%cons (%%car (zen:source-code expr)) (%%cddr (zen:source-code expr))) expr)))
                ;; first draft lets not worry about meta public methods generating nodes
                ((and (%%pair? (zen:source-code expr))
                      (%%eq? (zen:source-code (%%car (zen:source-code expr))) 'method))
                 (zen:enqueue interface expr)
                 (receive (access compatibility propagation abstraction expansion remote synchronized modifiers rest) (zen:parse-modifiers walker resume declaration zen:method-modifiers (%%cdr (zen:source-code expr)))
                   (%%unless (or (%%eq? access 'hidden)
                                 (%%eq? propagation 'override))
                     ;; for protected we generate a private node so code in the same module can access the method
                     (let ((adjusted-access (if (%%eq? access 'protected) 'private access)))
                       (let ((signature (zen:source-code (%%car rest))))
                         (let ((method-name (zen:source-code (%%car signature))))
                           (zen:enqueue nodes `(node ,adjusted-access ,method-name ,name))))))))
                (else
                 (zen:enqueue interface expr)))))
      
      (for-each preprocess body)
      (values (zen:queue-list metaclass)
              (zen:queue-list interface)
              (zen:queue-list nodes))))
  
  (receive (name type access compatibility implementor modifiers metaclass-name ascendant-names attributes body) (zen:parse-interface walker resume declaration (%%cdr (zen:source-code (%%desourcify form-src))))
    (receive (metaclass-body interface-body nodes) (preprocess-interface name body)
      (cond ((and (%%not-null? metaclass-body)
                  (%%specified? metaclass-name))
             (zen:walk-error walker resume declaration form-src "Ambiguous use of both metaclass and meta keywords: {s}" name))
            (else
             (zen:sourcify-deep-if
               `(begin
                  (%interface ,@(%%cdr (zen:source-code (%%desourcify form-src))))
                  ,@(map (lambda (node)
                           (zen:sourcify-deep-if
                             node
                             form-src))
                         nodes))
               form-src))
            #; ;; convert from expand-class
            ((%%specified? metaclass-name)
             (zen:sourcify-deep-if
               `(begin
                  (%interface ,@(%%cdr (zen:source-code form-src)))
                  ,@(map (lambda (node)
                           (zen:sourcify-deep-if
                             node
                             form-src))
                         nodes))
               form-src))
            #; ;; convert from expand-class
            (else
             (let ((metaclass-name (%%string->symbol (%%string-append (%%symbol->string name) "~Class"))))
               `(begin
                  ,(zen:sourcify-deep-if
                     `(%interface ,metaclass-name extends (:interface ,ascendant-names)
                        ,@metaclass-body)
                     form-src)
                  ,(zen:sourcify-deep-if
                     `(%interface ,access ,compatibility ,implementor ,name metaclass (:generated ,metaclass-name) extends ,ascendant-names
                        ,@interface-body)
                     form-src)
                  ,@(map (lambda (node)
                           (zen:sourcify-deep-if
                             node
                             form-src))
                         nodes))))))))


(define (zen:walk-%interface-declaration walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (receive (name type access compatibility implementor modifiers metaclass-name ascendant-names attributes body) (zen:parse-interface walker resume declaration (%%cdr form))
      (%%assertion (%%class-is? declaration zen:Module-Declaration) (zen:walk-error walker resume declaration form-src "Interfaces can only be defined at the module level: {s}" name)
        (let ((metaclass (if (or (zen:unspecified? metaclass-name) (%%eq? metaclass-name 'Interface)) #f (zen:lookup-reference walker resume declaration environment metaclass-name)))
              (ascendants (if (zen:unspecified? ascendant-names) '() (map (lambda (ascendant-name) (zen:lookup-reference walker resume declaration environment ascendant-name)) (zen:listify ascendant-names)))))
          (let ((new-declaration (or (zen:find-declaration-child declaration name)
                                     (zen:new-interface-declaration name type access compatibility modifiers attributes declaration implementor metaclass (%%boolean metaclass) ascendants))))
            (zen:set-declaration-source new-declaration form-src)
            (let ((effective-declaration (zen:add-declaration-child walker resume declaration new-declaration)))
              (zen:setup-interface-lookups effective-declaration)
              (let ((new-environment (%%cons effective-declaration environment)))
                (zen:walk-declarations walker resume effective-declaration new-environment body)
                effective-declaration))))))))


(define (zen:walk-%interface walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (receive (name type access compatibility implementor modifiers metaclass-name ascendant-names attributes body) (zen:parse-interface walker resume declaration (%%cdr form))
      (%%assertion (%%class-is? declaration zen:Module-Declaration) (zen:walk-error walker resume declaration form-src "Interfaces can only be defined at the module level: {s}" name)
        (let* ((new-declaration (zen:require-declaration declaration name))
               (new-environment (%%cons new-declaration environment)))
          (zen:set-namespace-declaration-body new-declaration (zen:walk-namespace walker resume new-declaration new-environment body))
          (zen:set-declaration-source new-declaration form-src)
          new-declaration)))))


;;;
;;;; Slot
;;;


(define zen:slot-modifiers
  '(((private protected package public) . #f)
    ((deprecated undocumented uptodate) . uptodate)))

(define zen:slot-keywords
  '(initialize accessors getter setter))


(define zen:slot-accessors-modifiers
  '(((private protected package public) . #f)
    ((final virtual chained override) . final)
    ((abstract concrete) . concrete)
    ((inline onsite) . inline)
    ((none generate explicit) . none)))


(define zen:slot-accessor-modifiers
  '(((private protected package public) . #f)
    ((final virtual chained override) . #f)
    ((abstract concrete) . #f)
    ((inline onsite) . #f)
    ((none generate explicit) . #f)))


(define (zen:parse-slot walker resume declaration form-src)
  (receive (access compatibility modifiers rest) (zen:parse-modifiers walker resume declaration zen:slot-modifiers (%%cdr (zen:source-code form-src)))
    (let ((name (zen:source-code (%%car rest))))
      (if (%%not (%%symbol? name))
          (zen:walk-error walker resume declaration form-src "Invalid slot name: {s}" (zen:desourcify-all name))
        (zen:parse-specifier (%%cdr rest)
          (lambda (specifier specifier-source rest)
            (receive (initialize accessors getter setter rest) (zen:parse-keywords zen:slot-keywords rest)
              (if (%%not-null? rest)
                  (zen:walk-error walker resume declaration form-src "Invalid slot definition: {s}" name)
                (values name specifier access compatibility modifiers initialize accessors getter setter)))))))))


(define (zen:expand-slot walker resume declaration environment form-src)
  (zen:expand-slot-form walker resume declaration form-src '%slot))


(define (zen:with-expand-slot-form walker resume declaration form-src proc)
  (define (parse-accessors form slot-access)
    (receive (access propagation abstraction expansion generation modifiers rest) (zen:parse-modifiers walker resume declaration zen:slot-accessors-modifiers form)
      (if (%%not-null? rest)
          (zen:walk-error walker resume declaration form-src "Invalid slot accessors definition: {s}" form)
        (values (or access slot-access) propagation abstraction expansion generation))))
  
  (define (parse-accessor slot-name default-access default-propagation default-abstraction default-expansion default-generation form prefix)
    (receive (access propagation abstraction expansion generation modifiers rest) (zen:parse-modifiers walker resume declaration zen:slot-accessor-modifiers form)
      (let ((generation (or generation default-generation)))
        (let ((name (cond ((%%null? rest)
                           (and (%%neq? generation 'none)
                                (%%string->symbol (%%string-append prefix (%%symbol->string slot-name)))))
                          ((%%null? (%%cdr rest))
                           (%%car rest))
                          (else
                           (zen:walk-error walker resume declaration form-src "Invalid slot accessor definition: {s}" form)))))
          (values (or access default-access)
                  (or propagation default-propagation)
                  (or abstraction default-abstraction)
                  (or expansion default-expansion)
                  generation
                  name)))))
  
  (receive (name specifier access compatibility modifiers initialize accessors getter setter) (zen:parse-slot walker resume declaration form-src)
    (let ((standardize
            (lambda (info)
              (let ((info (zen:desourcify info)))
                (cond ((zen:unspecified? info)
                       '())
                      ((%%symbol? info)
                       (%%list info))
                      (else
                       info))))))
      (let ((accessors (standardize accessors))
            (getter (standardize getter))
            (setter (standardize setter)))
        (receive (default-access default-propagation default-abstraction default-expansion default-generation) (parse-accessors accessors access)
          (receive (getter-access getter-propagation getter-abstraction getter-expansion getter-generation getter-name) (parse-accessor name default-access default-propagation default-abstraction default-expansion default-generation getter "get-")
            (receive (setter-access setter-propagation setter-abstraction setter-expansion setter-generation setter-name) (parse-accessor name default-access default-propagation default-abstraction default-expansion default-generation setter "set-")
              (let ((name-self (%%string->symbol (%%string-append "self." (%%symbol->string name))))
                    (generate-getter? (%%eq? getter-generation 'generate))
                    (generate-setter? (%%eq? setter-generation 'generate))
                    (specifier-list (if specifier (%%list specifier) '())))
                (proc name specifier access compatibility modifiers initialize
                      getter-access getter-propagation getter-abstraction getter-expansion getter-generation getter-name
                      setter-access setter-propagation setter-abstraction setter-expansion setter-generation setter-name
                      name-self generate-getter? generate-setter? specifier-list)))))))))


(define (zen:expand-slot-form walker resume declaration form-src symbol)
  (zen:with-expand-slot-form walker resume declaration form-src
    (lambda (name specifier access compatibility modifiers initialize
             getter-access getter-propagation getter-abstraction getter-expansion getter-generation getter-name
             setter-access setter-propagation setter-abstraction setter-expansion setter-generation setter-name
             name-self generate-getter? generate-setter? specifier-list)
      `(begin
         ,(zen:sourcify-deep-if
            `(,symbol ,name ,specifier ,access ,compatibility ,modifiers ,(if (%%unspecified? initialize) initialize `(with-self ,initialize)) ,getter-name ,setter-name ,getter-generation ,setter-generation)
            form-src)
         ,@(if generate-getter?
               `((method ,(or getter-access 'public) ,getter-propagation ,getter-abstraction ,getter-expansion (,getter-name self) ,@specifier-list
                         ,name-self))
             '())
         ,@(if generate-setter?
               `((method ,(or setter-access 'package) ,setter-propagation ,setter-abstraction ,setter-expansion (,setter-name self value ,@specifier-list) <void>
                         (set! ,name-self value)))
             '())))))


(define (zen:walk-%slot-declaration walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (zen:bind (name specifier access compatibility modifiers initialize getter-name setter-name getter-generation setter-generation) (%%cdr form)
      (%%assertion (%%class-is? declaration zen:Class-Declaration) (zen:walk-error walker resume declaration form-src "Slots can only be defined inside classes: {s}" name)
        (let ((type (if specifier (zen:walk-specifier walker resume declaration environment specifier) zen:Any))
              (new (if (%%eq? (%%car form) '%property) zen:new-property-declaration zen:new-slot-declaration)))
          (let ((new-declaration (or (zen:find-declaration-child declaration name)
                                     (new name type access compatibility modifiers '() declaration specifier #f getter-name setter-name getter-generation setter-generation (%%eq? (zen:walk-for) 'eval)))))
            (zen:set-declaration-source new-declaration form-src)
            (let ((effective-declaration (zen:add-declaration-child walker resume declaration new-declaration)))
              effective-declaration)))))))


(define (zen:walk-%slot walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (zen:bind (name specifier access compatibility modifiers initialize getter-name setter-name) (%%cdr form)
      (%%assertion (%%class-is? declaration zen:Class-Declaration) (zen:walk-error walker resume declaration form-src "Slots can only be defined inside classes: {s}" name)
        (let ((new-declaration (zen:require-declaration declaration (%%cadr form))))
          #; ;; wait buggy if file is both loaded interpreted and compiled
          (let ((ascendant-declaration (let ((ascendant-declaration (zen:get-class-declaration-ascendant declaration)))
                                         (if (%%is? ascendant-declaration zen:Autoload-Declaration)
                                             (zen:get-autoload-declaration-declaration ascendant-declaration)
                                           ascendant-declaration))))
            (let ((duplicate-declaration (zen:lookup-declaration ascendant-declaration name zen:private-access declaration)))
              (%%when (and duplicate-declaration
                           ;; remove this test when methods duplicating slots are fixed
                           (%%is? duplicate-declaration zen:Slot-Declaration))
                (zen:walk-error walker resume declaration form-src "Cannot override slot: {s}" name))))
          (zen:set-slot-declaration-initialize new-declaration
            (and (%%specified? initialize) (zen:walk walker resume declaration environment initialize)))
          (%%when (%%class-is? new-declaration zen:Property-Declaration)
            (let ((allocate? (%%neq? (zen:get-lexical-binding-type new-declaration) zen:Void)))
              (zen:set-property-declaration-getter new-declaration
                (zen:walk walker resume declaration environment
                  (cond (getter-name
                         `(lambda (self)
                            (with-self
                              (,getter-name self))))
                        (allocate?
                         `(lambda (self)
                            (with-self ,name)))
                        (else
                         #f))))
              (zen:set-property-declaration-setter new-declaration
                (let ((value (zen:generate-symbol "val")))
                  (zen:walk walker resume declaration environment
                    (cond (setter-name
                           `(lambda (self ,value)
                              (with-self
                                (,setter-name self ,value))))
                          (allocate?
                           `(lambda (self ,value)
                              (with-self
                                (set! ,name ,value))))
                          (else
                           #f)))))))
          (zen:set-declaration-source new-declaration form-src)
          new-declaration)))))


;;;
;;;; Method
;;;


(define zen:method-modifiers
  '(((hidden private protected package public) . protected)
    ((deprecated undocumented uptodate) . uptodate)
    ((final virtual chained override) . final)
    ((abstract concrete core) . concrete)
    ((inline onsite) . onsite)
    ;; quicky
    ((remote notremote) . notremote)
    ((synchronized notsynchronized) . notsynchronized)))


(define (zen:parse-method walker resume declaration rest)
  (receive (access compatibility propagation abstraction expansion remote synchronized modifiers rest) (zen:parse-modifiers walker resume declaration zen:method-modifiers rest)
    (%%assertion (and (%%pair? rest) (%%pair? (zen:source-code (%%car rest)))) (zen:walk-error walker resume declaration #f "Ill-formed method in {a}: {s}" (zen:get-lexical-binding-name (zen:get-declaration-toplevel declaration)) (%%cons 'method (zen:desourcify-all rest)))
      (let ((name (zen:source-code (%%car (zen:source-code (%%car rest)))))
            (name-src (%%car (zen:source-code (%%car rest))))
            (parameters (zen:wrap-parameters declaration (%%cdr (zen:source-code (%%car rest))))))
        (zen:parse-specifier (%%cdr rest)
          (lambda (specifier specifier-source body)
            (let ((effective-body
                    (if (%%null? body)
                        (and (%%eq? abstraction 'concrete)
                             (%%list (%%list 'unspecified)))
                      body)))
              (values name name-src specifier specifier-source access compatibility propagation abstraction expansion remote synchronized modifiers parameters effective-body))))))))


(define (zen:walk-method-parameters walker resume declaration environment name parameters extended? walk? form-src)
  (define (first-positional-parameter parameter specifier)
    (%%assert (%%not specifier)
      (zen:new-self-parameter parameter declaration #f #f)))
  
  (cond ((%%null? parameters)
         (zen:walk-error walker resume declaration form-src "Missing self for method {a}" name))
        ((%%not (%%eq? (%%car parameters) 'self))
         (zen:walk-error walker resume declaration form-src "Missing self for method {a}" name))
        (else
         (zen:walk-parameters walker resume declaration environment parameters extended? walk? first-positional-parameter))))


(define (zen:walk-method-declaration walker resume declaration environment form-src)
  (define (find-root-declaration name)
    (let* ((next-declaration (zen:lookup-declaration declaration name zen:private-access declaration))
           (root-declaration (and next-declaration (or (zen:get-method-declaration-root next-declaration) next-declaration))))
      (if (and root-declaration (%%eq? declaration (zen:get-declaration-parent root-declaration)))
          #f
        root-declaration)))
  
  (receive (name name-src specifier specifier-source access compatibility propagation abstraction expansion remote synchronized modifiers parameters body) (zen:parse-method walker resume declaration (%%cdr (zen:source-code form-src)))
    (%%assertion (%%class-is? declaration zen:Category-Declaration) (zen:walk-error walker resume declaration form-src "Methods can only be defined inside categories: {s}" name)
      (let ((inline? (and (%%eq? expansion 'inline) body)))
        (receive (signature augmented-environment)
            ;; yuck. to clean
            (if inline?
                (zen:walk-method-parameters walker resume declaration environment name parameters #t #t form-src)
              (values
                (zen:walk-method-parameters walker resume declaration environment name parameters #t #f form-src)
                (zen:unspecified)))
          (let* ((type (zen:signature->function-type signature (if specifier (zen:walk-specifier walker resume declaration environment specifier) zen:Any)))
                 (root-declaration (find-root-declaration name))
                 (new-declaration (or (zen:find-declaration-child declaration name)
                                      (zen:new-method-declaration name type access compatibility modifiers '() declaration root-declaration propagation abstraction expansion remote synchronized signature specifier-source))))
            (zen:set-declaration-source new-declaration form-src)
            (zen:set-declaration-name-source new-declaration name-src)
            (let ((effective-declaration (zen:add-declaration-child walker resume declaration new-declaration)))
              (%%when inline?
                (zen:set-method-declaration-signature effective-declaration signature)
                (zen:set-method-declaration-body effective-declaration
                  (zen:walk walker resume effective-declaration augmented-environment
                    `(with-self ,@body))))
              effective-declaration)))))))


(define (zen:walk-method walker resume declaration environment form-src)
  (receive (name name-src specifier specifier-source access compatibility propagation abstraction expansion remote synchronized modifiers parameters body) (zen:parse-method walker resume declaration (%%cdr (zen:source-code form-src)))
    (%%assertion (%%class-is? declaration zen:Category-Declaration) (zen:walk-error walker resume declaration form-src "Methods can only be defined inside categories: {s}" name)
      (let* ((new-declaration (zen:lookup-declaration declaration name zen:private-access declaration))
             (category-declaration (zen:get-declaration-parent new-declaration))
             (root-method-declaration (zen:get-method-declaration-root new-declaration))
             (root-method-propagation (and root-method-declaration (zen:get-method-declaration-propagation root-method-declaration)))
             (root-category-declaration (and root-method-declaration (zen:get-declaration-parent root-method-declaration))))
        (cond ((and root-category-declaration (%%eq? root-method-propagation 'final))
               (zen:walk-error walker resume declaration form-src "Cannot redefine final method: {s}" name))
              ((and root-category-declaration (%%memq root-method-propagation '(virtual chained)) (%%neq? propagation 'override))
               (case propagation
                 ((virtual chained)
                  (zen:walk-error walker resume declaration form-src "Cannot redefine virtual method: {s}" name))
                 ((final)
                  (zen:walk-error walker resume declaration form-src "Cannot finalize virtual method: {s}" name))))
              ((and (%%not root-category-declaration) (%%eq? propagation 'override))
               (zen:walk-error walker resume declaration form-src "Cannot find root method: {s}" name))
              ((and (%%not root-category-declaration) (%%class-is? category-declaration zen:Interface-Declaration) (%%neq? propagation 'virtual))
               (zen:walk-error walker resume declaration form-src "Interface method must be virtual: {s}" name))
              (else
               (if (and (zen:analysis-mode?)
                        (%%eq? (zen:get-method-declaration-propagation new-declaration) 'final))
                   (let ((data (zen:get-analysis-data (zen:get-declaration-locator new-declaration))))
                     (zen:set-analysis-data-declaration-references data '())))
               (receive (signature augmented-environment) (zen:walk-method-parameters walker resume declaration environment name parameters #t #t form-src)
                 (let ((with-self-body
                         (if (%%class-is? category-declaration zen:Interface-Declaration)
                             body
                           `(with-self ,@body))))
                   (let ((body-expression
                           (cond (root-category-declaration
                                  (zen:walk walker resume new-declaration (%%cons (zen:new-nextmethod-variable 'nextmethod (zen:get-lexical-binding-type root-method-declaration) #f #f) augmented-environment) with-self-body))
                                 (body
                                  (zen:walk walker resume new-declaration augmented-environment with-self-body))
                                 (else
                                  #f))))
                     (%%when (%%not (and (%%eq? expansion 'inline) (%%neq? abstraction 'abstract)))
                       #; ;; wait buggy if file is both loaded interpreted and compiled
                       (%%when (and (%%neq? (zen:walk-for) 'eval) (zen:get-method-declaration-body new-declaration))
                         (zen:walk-error walker resume declaration form-src "Cannot redefine method: {s}" name))
                       (zen:set-method-declaration-signature new-declaration signature)
                       (zen:set-method-declaration-body new-declaration body-expression))
                     (zen:set-declaration-source new-declaration form-src)
                     (zen:set-declaration-name-source new-declaration name-src)
                     new-declaration)))))))))


;; quick not elegant solution to wrap with-self around optional and named parameter code
(define (zen:wrap-parameters declaration parameters)
  (if (%%class-is? declaration zen:Interface-Declaration)
      parameters
    (let ((queue (zen:new-queue)))
      (let iter ((scan parameters))
           (cond ((%%null? scan))
                 ((%%symbol? (zen:source-code scan))
                  (zen:enqueue-list queue scan))
                 (else
                  (let ((parameter (zen:source-code (%%car scan))))
                    (if (%%pair? parameter)
                        (if (zen:specifier? (zen:source-code (%%car parameter)))
                            (zen:enqueue queue parameter)
                          (if (%%keyword? (zen:source-code (%%car parameter)))
                              (zen:parse-specifier (%%cddr parameter)
                                (lambda (specifier specifier-source rest)
                                  (let ((specifier-list (if specifier (%%list specifier) '())))
                                    (zen:enqueue queue `(,(%%car parameter) ,(%%cadr parameter) ,@specifier-list (with-self ,(%%car rest)))))))
                            (zen:parse-specifier (%%cdr parameter)
                              (lambda (specifier specifier-source rest)
                                (let ((specifier-list (if specifier (%%list specifier) '())))
                                  (zen:enqueue queue `(,(%%car parameter) ,@specifier-list (with-self ,(%%car rest)))))))))
                      (zen:enqueue queue parameter)))
                  (iter (%%cdr scan)))))
      (zen:queue-list queue))))


;;;
;;;; NextMethod Variable
;;;


(zen:define-class zen:NextMethod-Variable zen:Variable (constructor: zen:allocate-nextmethod-variable)
  ())


(define (zen:new-nextmethod-variable name type source specifier-source)
  (%%assertion (zen:variable-name-valid? name) (zen:error "Invalid variable name: {s}" (zen:desourcify-all name))
    (zen:allocate-nextmethod-variable name type #f #f source specifier-source 0)))


(zen:define-method (zen:emit-binding-reference (zen:NextMethod-Variable binding) source-declaration walker resume environment)
  (zen:new-code
    (zen:get-lexical-binding-name binding)
    zen:Any
    #f))


(zen:define-method (zen:walk-binding-validate-call (zen:NextMethod-Variable declaration) walker resume source-declaration operator arguments form-src)
  (let ((signature (zen:get-nextmethod-signature source-declaration)))
    (if signature
        (zen:validate-arguments walker resume source-declaration declaration signature arguments form-src))))


(zen:define-method (zen:emit-binding-call (zen:NextMethod-Variable binding) binding-src arguments arguments-codes source-declaration walker resume environment)
  (let ((type (zen:get-lexical-binding-type binding))
        (self (zen:*self*)))
    (zen:new-code
      (let ((name (zen:get-lexical-binding-name binding)))
        `(,name
           ,@(zen:codes-forms arguments-codes)))
      (zen:call-return-type type)
      #f)))


;;;
;;;; Self Parameter
;;;


(zen:define-class zen:Self-Parameter zen:Parameter (constructor: zen:allocate-self-parameter)
  ())


(define (zen:new-self-parameter name type source specifier-source)
  (zen:allocate-self-parameter name type #f #f source specifier-source 0))


(zen:define-method (zen:emit-check? (zen:Self-Parameter parameter))
  #f)


;;;
;;;; Self Binding
;;;


(zen:define-class zen:Self-Binding zen:Lexical-Binding (constructor: zen:allocate-self-binding)
  ())


(define (zen:new-self-binding type)
  (zen:allocate-self-binding 'self type #f))


(zen:define-method (zen:emit-binding-reference (zen:Self-Binding declaration) source-declaration walker resume environment)
  (zen:new-code
    'self
    (zen:get-code-type (zen:*self*))
    #f))


;;;
;;;; Dynamic Self Binding
;;;


(zen:define-class zen:Dynamic-Self-Binding zen:Lexical-Binding (constructor: zen:allocate-dynamic-self-binding)
  ((code getter: generate)))


(define (zen:new-dynamic-self-binding type code)
  (zen:allocate-dynamic-self-binding 'self type #f code))


(zen:define-method (zen:emit-binding-reference (zen:Dynamic-Self-Binding declaration) source-declaration walker resume environment)
  (zen:new-code
    (zen:get-dynamic-self-binding-code declaration)
    (zen:get-declaration-parent source-declaration)
    #f))


;;;
;;;; With Self
;;;


(define (zen:walk-with-self walker resume declaration environment form-src)
  (let ((form (zen:source-code form-src)))
    (let ((new-environment (%%cons (zen:new-self-binding #f) environment)))
      (zen:new-with-self
        (zen:find-class-declaration declaration)
        (let ((body (%%cdr form)))
          (zen:walk-body walker resume declaration new-environment body))))))


(define zen:*self*
  (%%make-parameter #f))


;;;
;;;; With Local Variables
;;;


(define (zen:parse-with-local-variables form)
  (let ((variables (%%car form))
        (body (%%cdr form)))
    (values variables body)))


(define (zen:walk-with-local-variables-declaration walker resume declaration environment form-src)
  (receive (variables body) (zen:parse-with-local-variables (%%cdr (zen:source-code form-src)))
    (zen:walk-declarations walker resume declaration environment body)))


(define (zen:walk-with-local-variables walker resume declaration environment form-src)
  (define (make-bindings variables)
    (let ((table (%%make-table test: eq?)))
      (for-each (lambda (variable)
                  (%%table-set! table variable (zen:new-local-variable-binding #f variable)))
                variables)
      table))
  
  (receive (variables body) (zen:parse-with-local-variables (%%cdr (zen:source-code form-src)))
    (let ((new-environment (%%cons (zen:new-walk-frame (make-bindings variables)) environment)))
      (zen:new-begin form-src (zen:walk-list walker resume declaration new-environment body)))))


;;;
;;;; Proclaim
;;;


(zen:define-method (zen:validate-proclaim (zen:Zen-Walker walker) resume declaration form-src)
  (if (and (%%not (%%class-is? declaration zen:Module-Declaration))
           (%%not (%%class-is? declaration zen:Category-Declaration)))
      (zen:walk-error walker resume declaration form-src "For now, proclaim can only be used at the module or category level")))


;;;
;;;; Declare
;;;


(define (zen:walk-declare walker resume declaration environment form-src)
  (let ((form (%%desourcify form-src)))
    (let ((declarations (%%cdr form)))
      (zen:new-declare declarations))))


;;;
;;;; Parameterize
;;;


(define (zen:walk-parameterize walker resume declaration environment form-src)
  (let ((bindings (zen:source-code (%%cadr (zen:source-code form-src))))
        (body (%%cddr (zen:source-code form-src))))
    (let ((effective-body (if (%%null? body) (%%list (%%list 'unspecified)) body))
          (expanded-bindings (zen:new-queue)))
      (for-each (lambda (binding-form)
                  (continuation-capture
                    (lambda (resume)
                      (let ((variable (%%car (zen:source-code binding-form)))
                            (value (%%car (zen:source-code (%%cdr (zen:source-code binding-form))))))
                        (zen:enqueue expanded-bindings
                                      (%%cons (continuation-capture
                                                (lambda (resume)
                                                  (zen:walk walker resume declaration environment variable)))
                                              (continuation-capture
                                                (lambda (resume)
                                                  (zen:walk walker resume declaration environment value)))))))))
                bindings)
      (zen:new-parameterize (zen:queue-list expanded-bindings)
                             (zen:walk-body walker resume declaration environment effective-body)))))


;;;
;;;; With Slots
;;;


(define (zen:walk-with-slots walker resume declaration environment form-src)
  (let ((form (zen:source-code form-src)))
    (zen:bind (slot-names object . body) (%%cdr form)
      (let ((object-symbol (zen:generate-symbol "object")))
        (zen:walk walker resume declaration environment
          `(let ((,object-symbol ,object))
             (let-symbol ,(map (lambda (slot-name)
                                 (let* ((slot-declaration (zen:lookup-reference walker resume declaration environment slot-name))
                                        (getter-name (zen:get-slot-declaration-getter-name slot-declaration))
                                        (setter-name (zen:get-slot-declaration-setter-name slot-declaration)))
                                   (%%list slot-name `(lambda () (%%list ',getter-name ',object-symbol)) `(lambda (value) (%%list ',setter-name ',object-symbol value)))))
                               slot-names)
                         ,@body)))))))


;;;
;;;; Dialect
;;;


(zen:define-dialect zen
  (zen:new-zen-dialect 'zen))

(zen:define-dialect zen.dialect
  (zen:new-zen-dialect 'zen.dialect))


(zen:define-walker-declaration definition           zen zen:walk-definition-declaration zen:walk-definition)
(zen:define-walker-declaration generic              zen zen:walk-generic-declaration zen:walk-generic)
(zen:define-walker-special     specific             zen zen:walk-specific)
(zen:define-walker-syntax      class                zen zen:expand-class)
(zen:define-walker-declaration %class               zen zen:walk-%class-declaration zen:walk-%class)
(zen:define-walker-syntax      interface            zen zen:expand-interface)
(zen:define-walker-declaration %interface           zen zen:walk-%interface-declaration zen:walk-%interface)
(zen:define-walker-syntax      slot                 zen zen:expand-slot)
(zen:define-walker-syntax      property             zen zen:expand-property)
(zen:define-walker-declaration %slot                zen zen:walk-%slot-declaration zen:walk-%slot)
(zen:define-walker-declaration %property            zen zen:walk-%slot-declaration zen:walk-%slot)
(zen:define-walker-declaration method               zen zen:walk-method-declaration zen:walk-method)
(zen:define-walker-declaration node                 zen zen:walk-node-declaration zen:walk-node)
(zen:define-walker-special     declare              zen zen:walk-declare)
(zen:define-walker-macro       specialize           zen zen:expand-specialize)
(zen:define-walker-declaration %specialize          zen zen:walk-%specialize-declaration zen:walk-%specialize)
(zen:define-walker-special     parameterize         zen zen:walk-parameterize)
(zen:define-walker-special     with-slots           zen zen:walk-with-slots)
(zen:define-walker-special     with-self            zen zen:walk-with-self)
(zen:define-walker-declaration with-dynamic-self    zen zen:walk-with-dynamic-self-declaration zen:walk-with-dynamic-self)
(zen:define-walker-declaration with-local-variables zen zen:walk-with-local-variables-declaration zen:walk-with-local-variables)
(zen:define-walker-special     cast                 zen zen:walk-cast)
(zen:define-walker-special     %allege              zen zen:walk-%allege)
(zen:define-walker-special     allocate             zen zen:walk-allocate)
(zen:define-walker-special     static               zen zen:walk-static)
(zen:define-walker-special     stack                zen zen:walk-stack))
