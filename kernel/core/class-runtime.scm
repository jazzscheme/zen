;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Class Runtime
;;;


(block core.class-runtime


;;;
;;;; Runtime
;;;


(define zen:new-interface-rank
  0)


;;;
;;;; Identifier
;;;


(define (zen:dispatch? symbol)
  (and (%%symbol? symbol)
       (let ((name (%%symbol->string symbol)))
         (let ((len (%%string-length name)))
           (and (%%fx> len 1)
                (%%eqv? (%%string-ref name (%%fx- len 1))
                        #\~))))))


(define (zen:dispatch->symbol dispatch)
  (let ((name (%%symbol->string dispatch)))
    (%%string->symbol (%%substring name 0 (%%fx- (%%string-length name) 1)))))


(define (zen:composite-identifier? symbol)
  (and (%%symbol? symbol)
       (zen:memstring #\. (%%symbol->string symbol))))


(define (zen:composite-reference? symbol)
  (and (%%symbol? symbol)
       (zen:memstring #\: (%%symbol->string symbol))))


(define (zen:split-symbol symbol separator)
  (%%debug-assert (%%symbol? symbol)
    (map string->symbol (zen:split-string (%%symbol->string symbol) separator))))


(define (zen:split-identifier symbol)
  (zen:split-symbol symbol #\.))


(define (zen:split-reference symbol)
  (zen:split-symbol symbol #\:))


(define (zen:composite-namespace? symbol)
  (and (%%symbol? symbol)
       (zen:memstring #\# (%%symbol->string symbol))))


(define (zen:break-namespace symbol)
  (let ((str (%%symbol->string symbol)))
    (let ((n (zen:string-find-reversed str #\#)))
      (if (%%not n)
          (values #f symbol)
        (let ((next (%%fx+ n 1)))
          (values (%%string->symbol (%%substring str 0 next))
                  (%%string->symbol (%%substring str next (%%string-length str)))))))))


(define (zen:namespace-name symbol)
  (receive (namespace name) (zen:break-namespace symbol)
    name))


;;;
;;;; Mutation
;;;


(define zen:loading-module
  (%%make-parameter #f))


(define zen:Mutations
  (%%make-table test: eq?))


(define (zen:get-mutations)
  zen:Mutations)


(define (zen:reset-mutations)
  (set! zen:Mutations (%%make-table test: eq?)))


(define (zen:register-mutation obj)
  (%%table-set! zen:Mutations obj #t))


;;;
;;;; Object
;;;


(define (zen:inspect-object object)
  (%%assert (%%object? object)
    (let* ((size (%%object-length object))
           (content (%%make-vector size)))
      (let iter ((n 0))
        (if (%%fx< n size)
            (begin
              (%%vector-set! content n (%%object-ref object n))
              (iter (%%fx+ n 1)))))
      content)))


(define (zen:object->vector obj)
  (%%vector-copy obj))


(define (zen:vector->object vec)
  (%%zenify (%%vector-copy vec)))


(define (zen:object-copy obj)
  (%%zenify (%%vector-copy obj)))


;;;
;;;; Core
;;;


(define zen:Core-Classes
  (%%make-table test: eq?))


(define (zen:get-core-classes)
  zen:Core-Classes)


(define (zen:core-class? name)
  (%%boolean (%%table-ref zen:Core-Classes name #f)))


(define (zen:get-core-class name)
  (%%table-ref zen:Core-Classes name #f))


(define (zen:set-core-class name class)
  (%%table-set! zen:Core-Classes name class))


(define (zen:set-core-class-redefined name core-class-locator)
  (%%table-set! zen:Core-Classes name core-class-locator))


;;;
;;;; Category
;;;


(define (zen:object? expr)
  (%%object? expr))


(define (zen:primitive? expr)
  (%%not (%%object? expr)))


(define (zen:subtype? target type)
  (%%debug-assert (and (zen:type? target) (zen:type? type))
    (%%boolean (%%subtype? target type))))


(define (zen:subcategory? target category)
  (%%boolean (%%subcategory? target category)))


(define (zen:subclass? target class)
  (%%boolean (%%subclass? target class)))


(define (zen:get-category-descendants category)
  (%%get-category-descendants category))


(define (zen:get-class-ascendant class)
  (%%get-class-ascendant class))


(zen:define-variable-override zen:object-of-class?
  (lambda (object class)
    (%%subclass? (%%get-object-class object) class)))


(define (zen:collect-type type lst)
  (zen:collect-if (lambda (obj)
                     (%%is? obj type))
                   lst))


(define (zen:create-class-tables class)
  (zen:create-class-interface-table class)
  (zen:create-class-class-table class))


(define (zen:create-core-class-tables class)
  (zen:create-class-class-table class))


(define (zen:create-type-class-tables class)
  (zen:create-class-class-table class))


(define (zen:create-class-interface-table class)
  (%%when (%%not (%%get-class-interface-table class))
    (let ((vtable (%%make-vector zen:new-interface-rank #f))
          (ascendant (%%get-class-ascendant class)))
      (%%when ascendant
        (let ((ascendant-interface-table (%%get-class-interface-table ascendant)))
          (%%when ascendant-interface-table
            (let ((size (%%vector-length ascendant-interface-table)))
              (let iter ((i 0))
                   (%%when (%%fx< i size)
                     (let ((ascendant-vtable (%%vector-ref ascendant-interface-table i)))
                       (%%when ascendant-vtable
                         (%%vector-set! vtable i (%%vector-copy ascendant-vtable))))
                     (iter (%%fx+ i 1))))))))
      (zen:vector-for-each (lambda (category)
                              (%%when (%%class-is? category zen:Interface)
                                (let* ((rank (%%get-interface-rank category))
                                       (size (%%get-category-virtual-size category)))
                                  (%%when (%%not (%%vector-ref vtable rank))
                                    (let ((category-vtable (%%make-vector size zen:call-into-incoherent))
                                          (class-name (%%get-category-identifier class))
                                          (category-identifier (%%get-category-identifier category)))
                                      (zen:table-iterate (%%get-category-fields category)
                                        (lambda (field-name field)
                                          (%%when (%%is? field zen:Method)
                                            (%%vector-set! category-vtable
                                                           (%%get-method-implementation-rank field)
                                                           (lambda (object . rest)
                                                             (zen:call-into-abstract (%%get-category-identifier (zen:class-of object)) field-name object rest))))))
                                      (%%vector-set! vtable rank category-vtable))))))
                            (%%get-category-ancestors class))
      (%%set-class-interface-table class vtable))))


(define (zen:create-class-class-table class)
  (%%when (%%not (%%get-class-class-table class))
    (let ((ascendant (%%get-class-ascendant class)))
      (%%set-class-class-table class
        (if ascendant
            (let* ((ascendant-class-table (%%get-class-class-table ascendant))
                   (size (%%vector-length ascendant-class-table))
                   (vtable (%%make-vector (%%fx+ size 1) '#())))
              (let iter ((i 0))
                   (%%when (%%fx< i size)
                     (%%vector-set! vtable i (%%vector-copy (%%vector-ref ascendant-class-table i)))
                     (iter (%%fx+ i 1))))
              vtable)
          (%%make-vector 1 '#()))))))


(define (zen:update-class class)
  (zen:update-class-class-table class))


(define (zen:update-class-class-table class)
  (let ((class-table (%%get-class-class-table class))
        (class-level (%%get-class-level class)))
    (zen:table-iterate-safe (%%get-category-fields class)
      (lambda (key field)
        (%%when (zen:virtual-method? field)
          (if (%%not (%%get-method-category-rank field))
              (%%set-method-category-rank field class-level))
          (let ((class-virtual-size (%%get-category-virtual-size class)))
            (%%when (%%not (%%fx= class-virtual-size (%%vector-length (%%vector-ref class-table class-level))))
              (let iter ((class class))
                   (let ((class-table (%%get-class-class-table class)))
                     (let ((implementation-table (zen:resize-vector (%%vector-ref class-table class-level) class-virtual-size)))
                       (%%vector-set! class-table class-level implementation-table))
                     (for-each iter (%%get-category-descendants class))))))
          (let ((root-implementation-table (%%vector-ref class-table class-level)))
            (let ((implementation-rank (%%get-method-implementation-rank field)))
              (let ((old-implementation (%%vector-ref root-implementation-table implementation-rank))
                    (new-implementation (%%get-method-node-implementation (%%get-method-implementation-tree field))))
                (%%when (%%neq? old-implementation new-implementation)
                  ;; method exists and has changed - update vtable + propagate to descendants
                  (let iter ((class class))
                       (let* ((class-table (%%get-class-class-table class))
                              (implementation-table (%%vector-ref class-table class-level)))
                         (%%when (%%eq? old-implementation (%%vector-ref implementation-table implementation-rank))
                           (%%vector-set! implementation-table implementation-rank new-implementation)
                           (for-each iter (%%get-category-descendants class))))))))))))))


;;;
;;;; Core
;;;


(define (zen:add-slot class slot-name slot-initialize slot-allocate?)
  (let ((actual (%%get-category-field class slot-name))
        (eval? (%%eq? (zen:walk-for) 'eval)))
    (cond (actual
           (if eval?
               (%%set-slot-initialize actual slot-initialize))
           actual)
          (else
           (let* ((instance-size (%%get-class-instance-size class))
                  (slot-offset (and slot-allocate? (if eval? #f instance-size)))
                  (slot (%%allocate-slot zen:Slot slot-name slot-offset slot-initialize)))
             (zen:add-field class slot)
             (%%set-class-slots class (%%append (%%get-class-slots class) (%%list slot)))
             (let ((class-instance-slots (%%get-class-instance-slots class)))
               (%%when (zen:find-in (lambda (instance-slot)
                                       (%%eq? slot-name (%%get-field-name instance-slot)))
                                     class-instance-slots)
                 (zen:error "Redefinition of slot {s} in {s}" slot-name (%%get-category-identifier class)))
               (%%set-class-instance-slots class (%%append class-instance-slots (%%list slot))))
             (%%when slot-allocate?
               (%%set-class-instance-size class (%%fx+ instance-size 1)))
             slot)))))


(define (zen:add-core-virtual-method class method-name)
  (%%set-class-virtual-names class
    (%%append (%%get-class-virtual-names class)
              (%%list method-name)))
  (let ((method-rank (%%get-category-virtual-size class)))
    (%%set-category-virtual-size class (%%fx+ method-rank 1))
    method-rank))


(define (zen:add-core-method-node class method-name implementation)
  (receive (root-class method-rank) (zen:require-core-level/rank class method-name)
    (let ((root-level (%%get-class-level root-class))
          (root-size (%%length (%%get-class-virtual-names root-class))))
      (let iter ((class class))
           (let* ((class-table (%%get-class-class-table class))
                  (implementation-table (zen:resize-vector (%%vector-ref class-table root-level) root-size)))
             (%%vector-set! implementation-table method-rank implementation)
             (%%vector-set! class-table root-level implementation-table))
           (for-each (lambda (descendant)
                       (iter descendant))
                     (%%get-category-descendants class))))))


(define (zen:find-nextmethod class method-name)
  (receive (root-class method-rank) (zen:require-core-level/rank class method-name)
    (let ((root-level (%%get-class-level root-class)))
      (if (eq? root-class class)
          (lambda (obj . rest)
            (zen:error "No nextmethod for {s} on {s}" obj method-name))
        (%%class-dispatch (%%get-class-ascendant class) root-level method-rank)))))


(define (zen:find-core-level/rank class method-name)
  (let iter ((class class))
       (if class
           (let ((method-rank (zen:find-rank method-name (%%get-class-virtual-names class))))
             (if (%%not method-rank)
                 (iter (%%get-class-ascendant class))
               (values class method-rank)))
         #f)))


(define (zen:require-core-level/rank class method-name)
  (or (zen:find-core-level/rank class method-name)
      (zen:error "Invalid core method: {s}" method-name)))


;;;
;;;; Define Class
;;;


(define (zen:new-core-class class name fields ascendant)
  (define (compute-core-class-ancestors class ascendant)
    (if (%%not ascendant)
        (%%list class)
      (%%append (%%vector->list (%%get-category-ancestors ascendant)) (%%list class))))
  
  (let ((core-class
         (%%allocate-class
          ; Object
          class
          ; Category
          name
          fields
          0
          #f
          '()
          ; Class
          ascendant
          '()
          '()
          (if ascendant (%%get-class-instance-slots ascendant) '())
          (if ascendant (%%get-class-instance-size ascendant) 1)
          (if ascendant (%%fx+ (%%get-class-level ascendant) 1) 0)
          '()
          #f
          #f
          #f)))
    (%%set-category-ancestors core-class (%%list->vector (compute-core-class-ancestors core-class ascendant)))
    (%%when ascendant
      (%%set-category-descendants ascendant (%%cons core-class (%%get-category-descendants ascendant))))
    (zen:create-core-class-tables core-class)
    core-class))


(define (zen:new-type-class class name ascendant user-data)
  (define (compute-type-class-ancestors class ascendant)
    (%%append (%%vector->list (%%get-category-ancestors ascendant)) (%%list class)))
  
  (let ((type-class
         (%%allocate-class
          ; Object
          class
          ; Category
          name
          (%%make-table test: eq?)
          0
          #f
          '()
          ; Class
          ascendant
          '()
          '()
          (%%get-class-instance-slots ascendant)
          (%%get-class-instance-size ascendant)
          (%%fx+ (%%get-class-level ascendant) 1)
          '()
          #f
          #f
          user-data)))
    (%%set-category-ancestors type-class (%%list->vector (compute-type-class-ancestors type-class ascendant)))
    (zen:create-type-class-tables type-class)
    type-class))


;;;
;;;; Object
;;;


(zen:define-class-runtime zen:Object #t)


(define (zen:get-object-slot object slot-offset)
  (%%debug-assert (%%object? object)
    (if (%%integer? slot-offset)
        (%%object-ref object slot-offset)
      (zen:unspecified))))


(define (zen:set-object-slot object slot-offset value)
  (%%debug-assert (%%object? object)
    (%%debug-assert (%%integer? slot-offset)
      (%%object-set! object slot-offset value))))


(define (zen:classname->string class)
  (if (%%null? class)
      "()"
    (%%symbol->string (%%get-category-identifier class))))


(zen:define-virtual (zen:initialize (zen:Object object)))
(zen:define-virtual (zen:destroy (zen:Object object)))
(zen:define-virtual (zen:call-print (zen:Object object) output detail))
(zen:define-virtual (zen:print-object (zen:Object object) output detail))
(zen:define-virtual (zen:tree-fold (zen:Object expression) down up here seed environment))


(zen:define-method (zen:initialize (zen:Object object))
  #f)


(zen:define-method (zen:destroy (zen:Object object))
  #f)


(zen:define-method (zen:call-print (zen:Object object) output detail)
  (zen:print-object object output detail))


(zen:define-method (zen:print-object (zen:Object object) output detail)
  (zen:print-serial object output))


(define (zen:print-serial object output)
  (let ((serial (object->serial-number object)))
    (display "#<zen #" output)
    (display serial output)
    (display ">" output)))


(zen:define-method (zen:tree-fold (zen:Object expression) down up here seed environment)
  (here expression seed environment))


;;;
;;;; Type
;;;


(zen:define-class-runtime zen:Type #t)


(zen:define-virtual (zen:of-type? (zen:Type type) object))
(zen:define-virtual (zen:of-subtype? (zen:Type type) subtype))
(zen:define-virtual (zen:specifiable? (zen:Type type)))
(zen:define-virtual (zen:category-type? (zen:Type type)))
(zen:define-virtual (zen:resolve-type (zen:Type type)))
(zen:define-virtual (zen:emit-specifier (zen:Type type)))
(zen:define-virtual (zen:emit-type (zen:Type type) source-declaration walker resume environment))
(zen:define-virtual (zen:emit-test (zen:Type type) value source-declaration walker resume environment))
(zen:define-virtual (zen:emit-cast (zen:Type type) value source-declaration walker resume environment))


(zen:define-method (zen:of-type? (zen:Type type) object)
  (zen:of-subtype? type (zen:class-of object)))


(zen:define-method (zen:of-subtype? (zen:Type type) subtype)
  (zen:error "Unable to test type on: {s}" type))


(zen:define-method (zen:specifiable? (zen:Type type))
  #t)


(zen:define-method (zen:category-type? (zen:Type type))
  #f)


(zen:define-method (zen:resolve-type (zen:Type type))
  type)


(zen:define-method (zen:emit-specifier (zen:Type type))
  (zen:error "Unable to emit specifier for: {s}" type))


(zen:define-method (zen:emit-type (zen:Type type) source-declaration walker resume environment)
  (zen:error "Unable to emit type for: {s}" type))


(zen:define-method (zen:emit-test (zen:Type type) value source-declaration walker resume environment)
  (let ((locator (zen:emit-type type source-declaration walker resume environment)))
    `(%%is? ,value ,locator)))


(zen:define-method (zen:emit-cast (zen:Type type) value source-declaration walker resume environment)
  `(if ,(zen:emit-test type value source-declaration walker resume environment)
       ,value
     (zen:type-error ,value ',(zen:emit-specifier type))))


;; for bootstrapping the core methods of type
(define (zen:bootstrap-type? object type-class)
  ;; should add a low-level test here if possible
  #t)


(define (zen:type? object)
  (and (%%object? object)
       (%%is? object zen:Type)))


;;;
;;;; Structure
;;;


(define zen:structure-types
  '())


(define (zen:register-structure-type test type)
  (set! zen:structure-types (%%cons (%%cons test type) zen:structure-types)))


(define (zen:structure-type object)
  (let iter ((scan zen:structure-types))
    (if (%%null? scan)
        zen:Structure
      (if ((%%caar scan) object)
          (%%cdar scan)
        (iter (%%cdr scan))))))


;;;
;;;; Category
;;;


(zen:define-class-runtime zen:Category #t)


(define (zen:category? object)
  (and (zen:type? object)
       (zen:category-type? object)))


(zen:define-method (zen:of-subtype? (zen:Category type) subtype)
  (and (zen:category-type? subtype)
       (zen:vector-memq? type (%%get-category-ancestors subtype))))


(zen:define-method (zen:category-type? (zen:Category type))
  #t)


(zen:define-method (zen:emit-type (zen:Category type) source-declaration walker resume environment)
  (%%get-category-identifier type))


(zen:define-virtual (zen:update-category (zen:Category category)))


(zen:define-method (zen:update-category (zen:Category category))
  #f)


(define (zen:is? object category)
  (%%boolean (%%is? object category)))


(define (zen:is-not? object category)
  (%%boolean (%%not (%%is? object category))))


(define (zen:get-category-identifier category)
  (%%debug-assert (zen:category? category)
    (%%get-category-identifier category)))


(define (zen:add-field category field)
  (%%set-category-field category (%%get-field-name field) field)
  (%%unspecified))


;;;
;;;; Class
;;;


(zen:define-class-runtime zen:Class #t)


(zen:define-virtual (zen:write-object (zen:Class class) we obj))


(zen:define-method (zen:write-object (zen:Class class) we obj)
  (%%default-wr we obj))


(define (zen:new-class class-of-class identifier ascendant interfaces)
  (define (compute-class-ancestors class ascendant interfaces)
    (let ((ancestors '()))
      (let add-interfaces ((category class))
           (cond ((%%class? category)
                  (let ((ascendant (%%get-class-ascendant category)))
                    (%%when ascendant
                      (add-interfaces ascendant)))
                  (for-each add-interfaces (%%get-class-interfaces category)))
                 (else
                  (%%when (%%not (%%memq category ancestors))
                    (set! ancestors (%%cons category ancestors))
                    (for-each add-interfaces (%%get-interface-ascendants category))))))
      (let add-classes ((class class))
           (%%when class
             (set! ancestors (%%cons class ancestors))
             (add-classes (%%get-class-ascendant class))))
      ancestors))

  ;; this should be made into a call to zen:new somehow
  (let ((class (%%zenify (zen:alloc-still-class (%%get-class-instance-size class-of-class)))))
    (%%set-object-class class class-of-class)
    (%%set-category-identifier class identifier)
    (%%set-category-fields class (%%make-table test: eq?))
    (%%set-category-virtual-size class 0)
    (%%set-category-ancestors class #f)
    (%%set-category-descendants class '())
    (%%set-class-ascendant class ascendant)
    (%%set-class-interfaces class interfaces)
    (%%set-class-slots class '())
    (%%set-class-instance-slots class (if ascendant (%%get-class-instance-slots ascendant) '()))
    (%%set-class-instance-size class (if ascendant (%%get-class-instance-size ascendant) zen:object-size))
    (%%set-class-level class (if ascendant (%%fx+ (%%get-class-level ascendant) 1) 0))
    (%%set-class-virtual-names class '())
    (%%set-class-class-table class #f)
    (%%set-class-interface-table class #f)
    (%%set-category-ancestors class (%%list->vector (compute-class-ancestors class ascendant interfaces)))
    (%%when ascendant
      (zen:register-mutation ascendant)
      (%%set-category-descendants ascendant (%%cons class (%%get-category-descendants ascendant))))
    (zen:create-class-tables class)
    (zen:initialize-slots class)
    ((%%class-dispatch class-of-class 0 0) class)
    class))


(define (zen:class? object)
  (%%class-is? object zen:Class))


(define (zen:class-of expr)
  (%%class-of-impl expr))


(define (zen:class-subtype? target class)
  (%%class-subtype? target class))


(define (zen:set-object-class object class)
  (%%set-object-class object class))


(zen:define-method (zen:of-type? (zen:Class class) object)
  (%%class-subtype? (zen:class-of object) class))


(zen:define-method (zen:update-category (zen:Class class))
  (zen:update-class class))


(define (zen:get-class-level class)
  (%%get-class-level class))


(define (zen:slot-form? form)
  (and (%%pair? form)
       (%%eq? (%%car form) 'slot)))


(define (zen:keyword-constructor class)
  (%%debug-assert (%%class? class)
    (lambda rest
      (let ((object (%%make-object class (%%get-class-instance-size class))))
        (zen:initialize-object object rest)
        object))))


(define (zen:new-instance class . rest)
  (%%debug-assert (%%class? class)
    (let ((object (%%make-object class (%%get-class-instance-size class))))
      (zen:initialize-object object rest)
      object)))


(define (zen:initialize-object object initargs)
  (let ((class (%%get-object-class object)))
    (zen:initialize-slots object)
    (let iter ((scan initargs))
         (if (%%not (%%null? scan))
             (let ((key (%%car scan))
                   (remain (%%cdr scan)))
               (%%debug-assert (and (%%keyword? key) (%%not (%%null? remain)))
                 (let ((value (%%car remain))
                       (remain (%%cdr remain)))
                   (let ((field (zen:category-field class (%%string->symbol (%%keyword->string key)))))
                     (%%debug-assert (%%class-is? field zen:Slot)
                       (let ((offset (%%get-slot-offset field)))
                         (if offset
                             (zen:set-object-slot object offset value)))))
                   (iter remain))))))
    (zen:initialize object)
    object))


(define (zen:new class . rest)
  (%%debug-assert (%%class? class)
    (let ((object (%%make-object class (%%get-class-instance-size class))))
      (zen:initialize-slots object)
      (apply (%%class-dispatch class 0 0) object rest)
      object)))


(define (zen:nascent-new class . rest)
  (%%debug-assert (%%class? class)
    (let ((object (%%make-object class (%%get-class-instance-size class))))
      (zen:initialize-slots object)
      object)))


(define (zen:iterate-descendants-tree class proc)
  (let iter ((class class))
    (proc class)
    (for-each iter (%%get-category-descendants class))))


(define (zen:$$object$$ class . rest)
  (%%zenify (apply vector class rest)))


;;;
;;;; Object-Class
;;;


(zen:define-class-runtime zen:Object-Class #t)


(zen:define-method (zen:of-subtype? (zen:Object-Class class) subtype)
  (if (%%object-class? class)
      #t
    (nextmethod class subtype)))


(zen:define-method (zen:emit-specifier (zen:Object-Class class))
  'Object)


;;;
;;;; Field
;;;


(zen:define-class-runtime zen:Field #t)


(define (zen:field? object)
  (%%class-is? object zen:Field))


(define (zen:field-name field)
  (%%get-field-name field))


(define (zen:category-field category field-name)
  (or (%%get-category-field category field-name)
      (let ((ascendant (%%get-class-ascendant category)))
        (and ascendant
             (zen:category-field ascendant field-name)))))


(define (zen:require-object-field object name)
  (let* ((class (%%get-object-class object))
         (field (zen:category-field class name)))
    (if (%%not field)
        (zen:error "Unknown field '{s} of {s}" name (%%get-category-identifier (%%get-object-class object)))
      field)))


;;;
;;;; Slot
;;;


(zen:define-class-runtime zen:Slot #t)


(define (zen:new-slot slot-name slot-offset slot-initialize)
  (zen:allocate-slot slot-name slot-offset slot-initialize))


(define (zen:slot? object)
  (%%class-is? object zen:Slot))


(define (zen:slot-ref object slot)
  (let ((offset (%%get-slot-offset slot)))
    (if offset
        (zen:get-object-slot object offset)
      (zen:get-dynamic object slot (%%get-slot-initialize slot)))))


(define (zen:slot-set! object slot value)
  (let ((offset (%%get-slot-offset slot)))
    (if offset
        (zen:set-object-slot object offset value)
      (zen:set-dynamic object slot value))))


(define (zen:slot-value object slot-name)
  (zen:slot-ref object (zen:require-object-field object slot-name)))


(define (zen:set-slot-value object slot-name value)
  (zen:slot-set! object (zen:require-object-field object slot-name) value))


(define (zen:initialize-slots object)
  (let ((slots (%%get-class-instance-slots (%%get-object-class object))))
    (for-each (lambda (slot)
                (let ((initialize (%%get-slot-initialize slot)))
                  (%%when initialize
                    (let ((offset (%%get-slot-offset slot)))
                      (%%when offset
                        (%%object-set! object offset (initialize object)))))))
              slots)))


(define zen:dynamics-mutex
  (make-mutex 'dynamics))


(define zen:dynamics
  (%%make-table test: eq? weak-keys: #t))


(define (zen:require-dynamics object)
  (or (%%table-ref zen:dynamics object #f)
      (let ((dynamics (%%make-table test: eq?)))
        (%%table-set! zen:dynamics object dynamics)
        dynamics)))


(define (zen:get-dynamic object slot initialize)
  (mutex-lock! zen:dynamics-mutex)
  (let ((dynamics (zen:require-dynamics object)))
    (let ((value (%%table-ref dynamics slot slot)))
      (let ((effective-value
              (if (%%eq? value slot)
                  (let ((initialize-value (if initialize (initialize object) (%%unspecified))))
                    (%%table-set! dynamics slot initialize-value)
                    initialize-value)
                value)))
        (mutex-unlock! zen:dynamics-mutex)
        effective-value))))


(define (zen:set-dynamic object slot value)
  (mutex-lock! zen:dynamics-mutex)
  (%%table-set! (zen:require-dynamics object) slot value)
  (mutex-unlock! zen:dynamics-mutex)
  (%%unspecified))


;;;
;;;; Interface
;;;


(zen:define-class-runtime zen:Interface #t)


(define (zen:new-interface class identifier ascendants)
  (define (compute-interface-ancestors interface ascendants)
    (zen:remove-duplicates
      (%%apply append (%%cons (map (lambda (ascendant)
                                     (%%vector->list (%%get-category-ancestors ascendant)))
                                   ascendants)
                              (%%list (%%list interface))))))
  
  (let ((interface (%%allocate-interface class identifier (%%make-table test: eq?) 0 #f '() ascendants zen:new-interface-rank)))
    (set! zen:new-interface-rank (%%fx+ zen:new-interface-rank 1))
    (%%set-category-ancestors interface (%%list->vector (compute-interface-ancestors interface ascendants)))
    (for-each (lambda (ascendant)
                (%%set-category-descendants ascendant (%%cons class (%%get-category-descendants ascendant))))
              ascendants)
    interface))


(define (zen:interface? object)
  (%%class-is? object zen:Interface))


;;tBool is_interface_subtype(jType target, jType type)
;;{
;;  jTypePtr ptr_start = target->ancestorsGet() + target->class_ancestors_sizeGet();
;;  jTypePtr ptr = target->ancestorsGet() + target->ancestors_sizeGet();
;;
;;  while (--ptr >= ptr_start)
;;      if (*ptr == type)
;;          return true;
;;
;;  return false;
;;}
(zen:define-method (zen:of-type? (zen:Interface interface) object)
  (zen:of-subtype? interface (zen:class-of object)))


(zen:define-method (zen:update-category (zen:Interface interface))
  (zen:update-interface interface))


(define (zen:update-interface interface)
  (define (update-interface-root-methods interface)
    (let* ((interface-rank (%%get-interface-rank interface))
           (added-methods '()))
      (zen:table-iterate (%%get-category-fields interface)
        (lambda (key field)
          (%%when (and (zen:virtual-method? field)
                       (%%not (%%get-method-category-rank field)))
            (%%set-method-category-rank field interface-rank)
            (set! added-methods (%%cons field added-methods)))))
      added-methods))

  (let ((added-methods (update-interface-root-methods interface)))
    (%%when (%%not-null? added-methods)
      (let ((interface-rank (%%get-interface-rank interface)))
        (let iter ((category interface))
             (%%when (%%class-is? category zen:Class)
               (let* ((interface-table (%%get-class-interface-table category))
                      (implementation-table (zen:resize-vector (%%vector-ref interface-table interface-rank) (%%get-category-virtual-size interface))))
                 (for-each (lambda (field)
                             (let ((implementation-rank (%%get-method-implementation-rank field))
                                   (implementation (%%get-method-node-implementation (%%get-method-implementation-tree field))))
                               (%%vector-set! implementation-table implementation-rank implementation)))
                           added-methods)
                 (%%vector-set! interface-table interface-rank implementation-table)))
             (for-each (lambda (descendant)
                         (iter descendant))
                       (%%get-category-descendants category)))))))


;;;
;;;; Class Bootstrap
;;;


(zen:define-class-bootstrap zen:Object)
(zen:define-class-bootstrap zen:Type)
(zen:define-class-bootstrap zen:Category)
(zen:define-class-bootstrap zen:Class)
(zen:define-class-bootstrap zen:Object-Class)
(zen:define-class-bootstrap zen:Field)
(zen:define-class-bootstrap zen:Slot)
(zen:define-class-bootstrap zen:Interface)


;;;
;;;; Bool
;;;


(zen:define-class-runtime zen:Bool-Class)


(zen:define-method (zen:of-type? (zen:Bool-Class class) object)
  #t)


(zen:define-method (zen:emit-specifier (zen:Bool-Class class))
  'bool)


(zen:define-method (zen:emit-test (zen:Bool-Class type) value source-declaration walker resume environment)
  #t)


(zen:define-method (zen:emit-cast (zen:Bool-Class type) value source-declaration walker resume environment)
  value)


(zen:define-class-runtime zen:Bool)


;;;
;;;; Boolean
;;;


(zen:define-class-runtime zen:Boolean-Class)


(zen:define-method (zen:of-type? (zen:Boolean-Class class) object)
  (%%boolean? object))


(zen:define-method (zen:emit-specifier (zen:Boolean-Class class))
  'boolean)


(zen:define-method (zen:emit-test (zen:Boolean-Class type) value source-declaration walker resume environment)
  `(%%boolean? ,value))


(zen:define-class-runtime zen:Boolean)


;;;
;;;; Char
;;;


(zen:define-class-runtime zen:Char-Class)


(zen:define-method (zen:of-type? (zen:Char-Class class) object)
  (%%char? object))


(zen:define-method (zen:emit-specifier (zen:Char-Class class))
  'char)


(zen:define-method (zen:emit-test (zen:Char-Class type) value source-declaration walker resume environment)
  `(%%char? ,value))


(zen:define-class-runtime zen:Char)


;;;
;;;; Numeric
;;;


(zen:define-class-runtime zen:Numeric-Class)


(zen:define-method (zen:emit-specifier (zen:Numeric-Class class))
  'numeric)


(zen:define-class-runtime zen:Numeric)


;;;
;;;; Number
;;;


(zen:define-class-runtime zen:Number-Class)


(zen:define-method (zen:of-type? (zen:Number-Class class) object)
  (%%number? object))


(zen:define-method (zen:emit-specifier (zen:Number-Class class))
  'number)


(zen:define-method (zen:emit-test (zen:Number-Class type) value source-declaration walker resume environment)
  `(%%number? ,value))


(zen:define-class-runtime zen:Number)


;;;
;;;; Complex
;;;


(zen:define-class-runtime zen:Complex-Class)


(zen:define-method (zen:of-type? (zen:Complex-Class class) object)
  (%%complex? object))


(zen:define-method (zen:emit-specifier (zen:Complex-Class class))
  'complex)


(zen:define-method (zen:emit-test (zen:Complex-Class type) value source-declaration walker resume environment)
  `(%%complex? ,value))


(zen:define-class-runtime zen:Complex)


;;;
;;;; Real
;;;


(zen:define-class-runtime zen:Real-Class)


(zen:define-method (zen:of-type? (zen:Real-Class class) object)
  (%%real? object))


(zen:define-method (zen:emit-specifier (zen:Real-Class class))
  'real)


(zen:define-method (zen:emit-test (zen:Real-Class type) value source-declaration walker resume environment)
  `(%%real? ,value))


(zen:define-class-runtime zen:Real)


;;;
;;;; Rational
;;;


(zen:define-class-runtime zen:Rational-Class)


(zen:define-method (zen:of-type? (zen:Rational-Class class) object)
  (%%rational? object))


(zen:define-method (zen:emit-specifier (zen:Rational-Class class))
  'rational)


(zen:define-method (zen:emit-test (zen:Rational-Class type) value source-declaration walker resume environment)
  `(%%rational? ,value))


(zen:define-class-runtime zen:Rational)


;;;
;;;; Integer
;;;


(zen:define-class-runtime zen:Integer-Class)


(zen:define-method (zen:of-type? (zen:Integer-Class class) object)
  (%%integer? object))


(zen:define-method (zen:emit-specifier (zen:Integer-Class class))
  'int)


(zen:define-method (zen:emit-test (zen:Integer-Class type) value source-declaration walker resume environment)
  `(%%integer? ,value))


(zen:define-class-runtime zen:Integer)


;;;
;;;; Fixnum
;;;


(zen:define-class-runtime zen:Fixnum-Class)


(zen:define-method (zen:of-type? (zen:Fixnum-Class class) object)
  (%%fixnum? object))


(zen:define-method (zen:emit-specifier (zen:Fixnum-Class class))
  'fx)


(zen:define-method (zen:emit-test (zen:Fixnum-Class type) value source-declaration walker resume environment)
  `(%%fixnum? ,value))


(zen:define-class-runtime zen:Fixnum)


;;;
;;;; Ratnum
;;;


(zen:define-class-runtime zen:Ratnum-Class)


(zen:define-method (zen:of-type? (zen:Ratnum-Class class) object)
  (%%ratnum? object))


(zen:define-method (zen:emit-specifier (zen:Ratnum-Class class))
  'rt)


(zen:define-method (zen:emit-test (zen:Ratnum-Class type) value source-declaration walker resume environment)
  `(%%ratnum? ,value))


(zen:define-class-runtime zen:Ratnum)


;;;
;;;; Flonum
;;;


(zen:define-class-runtime zen:Flonum-Class)


(zen:define-method (zen:of-type? (zen:Flonum-Class class) object)
  (%%flonum? object))


(zen:define-method (zen:emit-specifier (zen:Flonum-Class class))
  'fl)


(zen:define-method (zen:emit-test (zen:Flonum-Class type) value source-declaration walker resume environment)
  `(%%flonum? ,value))


(zen:define-method (zen:emit-cast (zen:Flonum-Class type) value source-declaration walker resume environment)
  `(cond ((%%flonum? ,value)
          ,value)
         ((%%fixnum? ,value)
          (%%fixnum->flonum ,value))
         ((%%ratnum? ,value)
          (%%ratnum->flonum ,value))
         (else
          (zen:type-error ,value ',(zen:emit-specifier type)))))


(zen:define-class-runtime zen:Flonum)


;;;
;;;; Flovec
;;;


(zen:define-class-runtime zen:Flovec-Class)


(zen:define-method (zen:of-type? (zen:Flovec-Class class) object)
  (%%flonum? object))


(zen:define-method (zen:emit-specifier (zen:Flovec-Class class))
  'fv)


(zen:define-method (zen:emit-test (zen:Flovec-Class type) value source-declaration walker resume environment)
  `(or (%%flonum? ,value)
       (%%f64vector? ,value)))


(zen:define-method (zen:emit-cast (zen:Flovec-Class type) value source-declaration walker resume environment)
  `(if (or (%%flonum? ,value)
           (%%f64vector? ,value))
       ,value
     (if (%%fixnum? ,value)
         (%%fixnum->flonum ,value)
       (zen:type-error ,value ',(zen:emit-specifier type)))))


(zen:define-class-runtime zen:Flovec)


;;;
;;;; S64
;;;


(zen:define-class-runtime zen:S64-Class)


(zen:define-method (zen:of-type? (zen:S64-Class class) object)
  (%%integer? object))


(zen:define-method (zen:emit-specifier (zen:S64-Class class))
  's64)


(zen:define-method (zen:emit-test (zen:S64-Class type) value source-declaration walker resume environment)
  `(%%integer? ,value))


(zen:define-class-runtime zen:S64)


;;;
;;;; Sequence
;;;


(zen:define-class-runtime zen:Sequence-Class)


(zen:define-method (zen:emit-specifier (zen:Sequence-Class class))
  'sequence)


(zen:define-class-runtime zen:Sequence)


;;;
;;;; List
;;;


(zen:define-class-runtime zen:List-Class)


(zen:define-method (zen:emit-specifier (zen:List-Class class))
  'list)


(zen:define-method (zen:emit-test (zen:List-Class type) value source-declaration walker resume environment)
  `(or (%%null? ,value)
       (%%pair? ,value)))


(zen:define-class-runtime zen:List)


;;;
;;;; Null
;;;


(zen:define-class-runtime zen:Null-Class)


(zen:define-method (zen:of-type? (zen:Null-Class class) object)
  (%%null? object))


(zen:define-method (zen:emit-specifier (zen:Null-Class class))
  'null)


(zen:define-method (zen:emit-test (zen:Null-Class type) value source-declaration walker resume environment)
  `(%%null? ,value))


(zen:define-class-runtime zen:Null)


;;;
;;;; Pair
;;;


(zen:define-class-runtime zen:Pair-Class)


(zen:define-method (zen:of-type? (zen:Pair-Class class) object)
  (%%pair? object))


(zen:define-method (zen:emit-specifier (zen:Pair-Class class))
  'pair)


(zen:define-method (zen:emit-test (zen:Pair-Class type) value source-declaration walker resume environment)
  `(%%pair? ,value))


(zen:define-class-runtime zen:Pair)


;;;
;;;; String
;;;


(zen:define-class-runtime zen:String-Class)


(zen:define-method (zen:of-type? (zen:String-Class class) object)
  (%%string? object))


(zen:define-method (zen:emit-specifier (zen:String-Class class))
  'string)


(zen:define-method (zen:emit-test (zen:String-Class type) value source-declaration walker resume environment)
  `(%%string? ,value))


(zen:define-class-runtime zen:String)


;;;
;;;; Vector
;;;


(zen:define-class-runtime zen:Vector-Class)


(zen:define-method (zen:of-type? (zen:Vector-Class class) object)
  (%%vector? object))


(zen:define-method (zen:emit-specifier (zen:Vector-Class class))
  'vector)


(zen:define-method (zen:emit-test (zen:Vector-Class type) value source-declaration walker resume environment)
  `(%%vector? ,value))


(zen:define-class-runtime zen:Vector)


;;;
;;;; S8Vector
;;;


(zen:define-class-runtime zen:S8Vector-Class)


(zen:define-method (zen:of-type? (zen:S8Vector-Class class) object)
  (%%s8vector? object))


(zen:define-method (zen:emit-specifier (zen:S8Vector-Class class))
  's8vector)


(zen:define-method (zen:emit-test (zen:S8Vector-Class type) value source-declaration walker resume environment)
  `(%%s8vector? ,value))


(zen:define-class-runtime zen:S8Vector)


;;;
;;;; U8Vector
;;;


(zen:define-class-runtime zen:U8Vector-Class)


(zen:define-method (zen:of-type? (zen:U8Vector-Class class) object)
  (%%u8vector? object))


(zen:define-method (zen:emit-specifier (zen:U8Vector-Class class))
  'u8vector)


(zen:define-method (zen:emit-test (zen:U8Vector-Class type) value source-declaration walker resume environment)
  `(%%u8vector? ,value))


(zen:define-class-runtime zen:U8Vector)


;;;
;;;; S16Vector
;;;


(zen:define-class-runtime zen:S16Vector-Class)


(zen:define-method (zen:of-type? (zen:S16Vector-Class class) object)
  (%%s16vector? object))


(zen:define-method (zen:emit-specifier (zen:S16Vector-Class class))
  's16vector)


(zen:define-method (zen:emit-test (zen:S16Vector-Class type) value source-declaration walker resume environment)
  `(%%s16vector? ,value))


(zen:define-class-runtime zen:S16Vector)


;;;
;;;; U16Vector
;;;


(zen:define-class-runtime zen:U16Vector-Class)


(zen:define-method (zen:of-type? (zen:U16Vector-Class class) object)
  (%%u16vector? object))


(zen:define-method (zen:emit-specifier (zen:U16Vector-Class class))
  'u16vector)


(zen:define-method (zen:emit-test (zen:U16Vector-Class type) value source-declaration walker resume environment)
  `(%%u16vector? ,value))


(zen:define-class-runtime zen:U16Vector)


;;;
;;;; S32Vector
;;;


(zen:define-class-runtime zen:S32Vector-Class)


(zen:define-method (zen:of-type? (zen:S32Vector-Class class) object)
  (%%s32vector? object))


(zen:define-method (zen:emit-specifier (zen:S32Vector-Class class))
  's32vector)


(zen:define-method (zen:emit-test (zen:S32Vector-Class type) value source-declaration walker resume environment)
  `(%%s32vector? ,value))


(zen:define-class-runtime zen:S32Vector)


;;;
;;;; U32Vector
;;;


(zen:define-class-runtime zen:U32Vector-Class)


(zen:define-method (zen:of-type? (zen:U32Vector-Class class) object)
  (%%u32vector? object))


(zen:define-method (zen:emit-specifier (zen:U32Vector-Class class))
  'u32vector)


(zen:define-method (zen:emit-test (zen:U32Vector-Class type) value source-declaration walker resume environment)
  `(%%u32vector? ,value))


(zen:define-class-runtime zen:U32Vector)


;;;
;;;; S64Vector
;;;


(zen:define-class-runtime zen:S64Vector-Class)


(zen:define-method (zen:of-type? (zen:S64Vector-Class class) object)
  (%%s64vector? object))


(zen:define-method (zen:emit-specifier (zen:S64Vector-Class class))
  's64vector)


(zen:define-method (zen:emit-test (zen:S64Vector-Class type) value source-declaration walker resume environment)
  `(%%s64vector? ,value))


(zen:define-class-runtime zen:S64Vector)


;;;
;;;; U64Vector
;;;


(zen:define-class-runtime zen:U64Vector-Class)


(zen:define-method (zen:of-type? (zen:U64Vector-Class class) object)
  (%%u64vector? object))


(zen:define-method (zen:emit-specifier (zen:U64Vector-Class class))
  'u64vector)


(zen:define-method (zen:emit-test (zen:U64Vector-Class type) value source-declaration walker resume environment)
  `(%%u64vector? ,value))


(zen:define-class-runtime zen:U64Vector)


;;;
;;;; F32Vector
;;;


(zen:define-class-runtime zen:F32Vector-Class)


(zen:define-method (zen:of-type? (zen:F32Vector-Class class) object)
  (%%f32vector? object))


(zen:define-method (zen:emit-specifier (zen:F32Vector-Class class))
  'f32vector)


(zen:define-method (zen:emit-test (zen:F32Vector-Class type) value source-declaration walker resume environment)
  `(%%f32vector? ,value))


(zen:define-class-runtime zen:F32Vector)


;;;
;;;; F64Vector
;;;


(zen:define-class-runtime zen:F64Vector-Class)


(zen:define-method (zen:of-type? (zen:F64Vector-Class class) object)
  (%%f64vector? object))


(zen:define-method (zen:emit-specifier (zen:F64Vector-Class class))
  'f64vector)


(zen:define-method (zen:emit-test (zen:F64Vector-Class type) value source-declaration walker resume environment)
  `(%%f64vector? ,value))


(zen:define-class-runtime zen:F64Vector)


;;;
;;;; FixedVector
;;;


(zen:define-class-runtime zen:FixedVector-Class)


(zen:define-method (zen:of-type? (zen:FixedVector-Class class) object)
  (and (%%vector? object)
       (%%fx= (%%vector-length object) (%%get-class-user-data class))))


(zen:define-method (zen:emit-specifier (zen:FixedVector-Class class))
  (%%string->symbol (%%string-append "vector#" (%%number->string (%%get-class-user-data class)))))


(zen:define-method (zen:emit-test (zen:FixedVector-Class class) value source-declaration walker resume environment)
  `(and (%%vector? ,value)
        (%%fx= (%%vector-length ,value) ,(%%get-class-user-data class))))


(zen:define-class-runtime zen:FixedVector)


;;;
;;;; FixedS8Vector
;;;


(zen:define-class-runtime zen:FixedS8Vector-Class)


(zen:define-method (zen:of-type? (zen:FixedS8Vector-Class class) object)
  (and (%%s8vector? object)
       (%%fx= (%%s8vector-length object) (%%get-class-user-data class))))


(zen:define-method (zen:emit-specifier (zen:FixedS8Vector-Class class))
  (%%string->symbol (%%string-append "s8vector#" (%%number->string (%%get-class-user-data class)))))


(zen:define-method (zen:emit-test (zen:FixedS8Vector-Class class) value source-declaration walker resume environment)
  `(and (%%s8vector? ,value)
        (%%fx= (%%s8vector-length ,value) ,(%%get-class-user-data class))))


(zen:define-class-runtime zen:FixedS8Vector)


;;;
;;;; FixedU8Vector
;;;


(zen:define-class-runtime zen:FixedU8Vector-Class)


(zen:define-method (zen:of-type? (zen:FixedU8Vector-Class class) object)
  (and (%%u8vector? object)
       (%%fx= (%%u8vector-length object) (%%get-class-user-data class))))


(zen:define-method (zen:emit-specifier (zen:FixedU8Vector-Class class))
  (%%string->symbol (%%string-append "u8vector#" (%%number->string (%%get-class-user-data class)))))


(zen:define-method (zen:emit-test (zen:FixedU8Vector-Class class) value source-declaration walker resume environment)
  `(and (%%u8vector? ,value)
        (%%fx= (%%u8vector-length ,value) ,(%%get-class-user-data class))))


(zen:define-class-runtime zen:FixedU8Vector)


;;;
;;;; FixedS16Vector
;;;


(zen:define-class-runtime zen:FixedS16Vector-Class)


(zen:define-method (zen:of-type? (zen:FixedS16Vector-Class class) object)
  (and (%%s16vector? object)
       (%%fx= (%%s16vector-length object) (%%get-class-user-data class))))


(zen:define-method (zen:emit-specifier (zen:FixedS16Vector-Class class))
  (%%string->symbol (%%string-append "s16vector#" (%%number->string (%%get-class-user-data class)))))


(zen:define-method (zen:emit-test (zen:FixedS16Vector-Class class) value source-declaration walker resume environment)
  `(and (%%s16vector? ,value)
        (%%fx= (%%s16vector-length ,value) ,(%%get-class-user-data class))))


(zen:define-class-runtime zen:FixedS16Vector)


;;;
;;;; FixedU16Vector
;;;


(zen:define-class-runtime zen:FixedU16Vector-Class)


(zen:define-method (zen:of-type? (zen:FixedU16Vector-Class class) object)
  (and (%%u16vector? object)
       (%%fx= (%%u16vector-length object) (%%get-class-user-data class))))


(zen:define-method (zen:emit-specifier (zen:FixedU16Vector-Class class))
  (%%string->symbol (%%string-append "u16vector#" (%%number->string (%%get-class-user-data class)))))


(zen:define-method (zen:emit-test (zen:FixedU16Vector-Class class) value source-declaration walker resume environment)
  `(and (%%u16vector? ,value)
        (%%fx= (%%u16vector-length ,value) ,(%%get-class-user-data class))))


(zen:define-class-runtime zen:FixedU16Vector)


;;;
;;;; FixedS32Vector
;;;


(zen:define-class-runtime zen:FixedS32Vector-Class)


(zen:define-method (zen:of-type? (zen:FixedS32Vector-Class class) object)
  (and (%%s32vector? object)
       (%%fx= (%%s32vector-length object) (%%get-class-user-data class))))


(zen:define-method (zen:emit-specifier (zen:FixedS32Vector-Class class))
  (%%string->symbol (%%string-append "s32vector#" (%%number->string (%%get-class-user-data class)))))


(zen:define-method (zen:emit-test (zen:FixedS32Vector-Class class) value source-declaration walker resume environment)
  `(and (%%s32vector? ,value)
        (%%fx= (%%s32vector-length ,value) ,(%%get-class-user-data class))))


(zen:define-class-runtime zen:FixedS32Vector)


;;;
;;;; FixedU32Vector
;;;


(zen:define-class-runtime zen:FixedU32Vector-Class)


(zen:define-method (zen:of-type? (zen:FixedU32Vector-Class class) object)
  (and (%%u32vector? object)
       (%%fx= (%%u32vector-length object) (%%get-class-user-data class))))


(zen:define-method (zen:emit-specifier (zen:FixedU32Vector-Class class))
  (%%string->symbol (%%string-append "u32vector#" (%%number->string (%%get-class-user-data class)))))


(zen:define-method (zen:emit-test (zen:FixedU32Vector-Class class) value source-declaration walker resume environment)
  `(and (%%u32vector? ,value)
        (%%fx= (%%u32vector-length ,value) ,(%%get-class-user-data class))))


(zen:define-class-runtime zen:FixedU32Vector)


;;;
;;;; FixedS64Vector
;;;


(zen:define-class-runtime zen:FixedS64Vector-Class)


(zen:define-method (zen:of-type? (zen:FixedS64Vector-Class class) object)
  (and (%%s64vector? object)
       (%%fx= (%%s64vector-length object) (%%get-class-user-data class))))


(zen:define-method (zen:emit-specifier (zen:FixedS64Vector-Class class))
  (%%string->symbol (%%string-append "s64vector#" (%%number->string (%%get-class-user-data class)))))


(zen:define-method (zen:emit-test (zen:FixedS64Vector-Class class) value source-declaration walker resume environment)
  `(and (%%s64vector? ,value)
        (%%fx= (%%s64vector-length ,value) ,(%%get-class-user-data class))))


(zen:define-class-runtime zen:FixedS64Vector)


;;;
;;;; FixedU64Vector
;;;


(zen:define-class-runtime zen:FixedU64Vector-Class)


(zen:define-method (zen:of-type? (zen:FixedU64Vector-Class class) object)
  (and (%%u64vector? object)
       (%%fx= (%%u64vector-length object) (%%get-class-user-data class))))


(zen:define-method (zen:emit-specifier (zen:FixedU64Vector-Class class))
  (%%string->symbol (%%string-append "u64vector#" (%%number->string (%%get-class-user-data class)))))


(zen:define-method (zen:emit-test (zen:FixedU64Vector-Class class) value source-declaration walker resume environment)
  `(and (%%u64vector? ,value)
        (%%fx= (%%u64vector-length ,value) ,(%%get-class-user-data class))))


(zen:define-class-runtime zen:FixedU64Vector)


;;;
;;;; FixedF32Vector
;;;


(zen:define-class-runtime zen:FixedF32Vector-Class)


(zen:define-method (zen:of-type? (zen:FixedF32Vector-Class class) object)
  (and (%%f32vector? object)
       (%%fx= (%%f32vector-length object) (%%get-class-user-data class))))


(zen:define-method (zen:emit-specifier (zen:FixedF32Vector-Class class))
  (%%string->symbol (%%string-append "f32vector#" (%%number->string (%%get-class-user-data class)))))


(zen:define-method (zen:emit-test (zen:FixedF32Vector-Class class) value source-declaration walker resume environment)
  `(and (%%f32vector? ,value)
        (%%fx= (%%f32vector-length ,value) ,(%%get-class-user-data class))))


(zen:define-class-runtime zen:FixedF32Vector)


;;;
;;;; FixedF64Vector
;;;


(zen:define-class-runtime zen:FixedF64Vector-Class)


(zen:define-method (zen:of-type? (zen:FixedF64Vector-Class class) object)
  (and (%%f64vector? object)
       (%%fx= (%%f64vector-length object) (%%get-class-user-data class))))


(zen:define-method (zen:emit-specifier (zen:FixedF64Vector-Class class))
  (%%string->symbol (%%string-append "f64vector#" (%%number->string (%%get-class-user-data class)))))


(zen:define-method (zen:emit-test (zen:FixedF64Vector-Class class) value source-declaration walker resume environment)
  `(and (%%f64vector? ,value)
        (%%fx= (%%f64vector-length ,value) ,(%%get-class-user-data class))))


(zen:define-class-runtime zen:FixedF64Vector)


;;;
;;;; Structure
;;;


(zen:define-class-runtime zen:Structure-Class)


(zen:define-method (zen:of-type? (zen:Structure-Class class) object)
  (%%structure? object))


(zen:define-method (zen:emit-specifier (zen:Structure-Class class))
  'structure)


(zen:define-method (zen:emit-test (zen:Structure-Class type) value source-declaration walker resume environment)
  `(%%structure? ,value))


(zen:define-class-runtime zen:Structure)


;;;
;;;; Port
;;;


(zen:define-class-runtime zen:Port-Class)


(zen:define-method (zen:of-type? (zen:Port-Class class) object)
  (%%port? object))


(zen:define-method (zen:emit-specifier (zen:Port-Class class))
  'port)


(zen:define-method (zen:emit-test (zen:Port-Class type) value source-declaration walker resume environment)
  `(%%port? ,value))


(zen:define-class-runtime zen:Port)


(zen:register-structure-type port? zen:Port)


;;;
;;;; Continuation
;;;


(zen:define-class-runtime zen:Continuation-Class)


(zen:define-method (zen:of-type? (zen:Continuation-Class class) object)
  (%%continuation? object))


(zen:define-method (zen:emit-specifier (zen:Continuation-Class class))
  'continuation)


(zen:define-method (zen:emit-test (zen:Continuation-Class type) value source-declaration walker resume environment)
  `(%%continuation? ,value))


(zen:define-class-runtime zen:Continuation)


;;;
;;;; Procedure
;;;


(zen:define-class-runtime zen:Procedure-Class)


(zen:define-method (zen:of-type? (zen:Procedure-Class class) object)
  (%%procedure? object))


(zen:define-method (zen:of-subtype? (zen:Procedure-Class class) subtype)
  (or (nextmethod class subtype)
      #; ;; fix because it is defined only is a later unit
      (%%is? subtype zen:Function-Type)))


(zen:define-method (zen:emit-specifier (zen:Procedure-Class class))
  'procedure)


(zen:define-method (zen:emit-test (zen:Procedure-Class type) value source-declaration walker resume environment)
  `(%%procedure? ,value))


(zen:define-class-runtime zen:Procedure)


;;;
;;;; Symbol
;;;


(zen:define-class-runtime zen:Symbol-Class)


(zen:define-method (zen:of-type? (zen:Symbol-Class class) object)
  (%%symbol? object))


(zen:define-method (zen:emit-specifier (zen:Symbol-Class class))
  'symbol)


(zen:define-method (zen:emit-test (zen:Symbol-Class type) value source-declaration walker resume environment)
  `(%%symbol? ,value))


(zen:define-class-runtime zen:Symbol)


;;;
;;;; Keyword
;;;


(zen:define-class-runtime zen:Keyword-Class)


(zen:define-method (zen:of-type? (zen:Keyword-Class class) object)
  (%%keyword? object))


(zen:define-method (zen:emit-specifier (zen:Keyword-Class class))
  'keyword)


(zen:define-method (zen:emit-test (zen:Keyword-Class type) value source-declaration walker resume environment)
  `(%%keyword? ,value))


(zen:define-class-runtime zen:Keyword)


;;;
;;;; Table
;;;


(zen:define-class-runtime zen:Table-Class)


(zen:define-method (zen:of-type? (zen:Table-Class class) object)
  (%%table? object))


(zen:define-method (zen:emit-specifier (zen:Table-Class class))
  'table)


(zen:define-method (zen:emit-test (zen:Table-Class type) value source-declaration walker resume environment)
  `(%%table? ,value))


(zen:define-class-runtime zen:Table)


(zen:register-structure-type table? zen:Table)


;;;
;;;; Thread
;;;


(zen:define-class-runtime zen:Thread-Class)


(zen:define-method (zen:of-type? (zen:Thread-Class class) object)
  (%%thread? object))


(zen:define-method (zen:emit-specifier (zen:Thread-Class class))
  'thread)


(zen:define-method (zen:emit-test (zen:Thread-Class type) value source-declaration walker resume environment)
  `(%%thread? ,value))


(zen:define-class-runtime zen:Thread)


(zen:register-structure-type thread? zen:Thread)


;;;
;;;; Promise
;;;


(zen:define-class-runtime zen:Promise-Class)


(zen:define-method (zen:emit-specifier (zen:Promise-Class class))
  'promise)


(zen:define-class-runtime zen:Promise)


;;;
;;;; Foreign
;;;


(zen:define-class-runtime zen:Foreign-Class)


(zen:define-method (zen:of-type? (zen:Foreign-Class class) object)
  (%%foreign? object))


(zen:define-method (zen:emit-specifier (zen:Foreign-Class class))
  'foreign)


(zen:define-method (zen:emit-test (zen:Foreign-Class type) value source-declaration walker resume environment)
  `(%%foreign? ,value))


(zen:define-class-runtime zen:Foreign)


;;;
;;;; Values
;;;


(zen:define-class-runtime zen:Values-Class)


(zen:define-method (zen:of-type? (zen:Values-Class class) object)
  (%%values? object))


(zen:define-method (zen:emit-specifier (zen:Values-Class class))
  'values)


(zen:define-method (zen:emit-test (zen:Values-Class type) value source-declaration walker resume environment)
  `(%%values? ,value))


(zen:define-class-runtime zen:Values)


;;;
;;;; Box
;;;


(zen:define-class-runtime zen:Box-Class)


(zen:define-method (zen:of-type? (zen:Box-Class class) object)
  (%%box? object))


(zen:define-method (zen:emit-specifier (zen:Box-Class class))
  'box)


(zen:define-method (zen:emit-test (zen:Box-Class type) value source-declaration walker resume environment)
  `(%%box? ,value))


(zen:define-class-runtime zen:Box)


;;;
;;;; EOF
;;;


(zen:define-class-runtime zen:EOF-Class)


(zen:define-method (zen:of-type? (zen:EOF-Class class) object)
  (%%eof-object? object))


(zen:define-method (zen:emit-specifier (zen:EOF-Class class))
  'eof)


(zen:define-method (zen:emit-test (zen:EOF-Class type) value source-declaration walker resume environment)
  `(%%eof-object? ,value))


(zen:define-class-runtime zen:EOF)


;;;
;;;; Unspecified
;;;


(zen:define-class-runtime zen:Unspecified-Class)


(zen:define-method (zen:of-type? (zen:Unspecified-Class class) object)
  (%%unspecified? object))


(zen:define-method (zen:emit-specifier (zen:Unspecified-Class class))
  'unspecified)


(zen:define-method (zen:emit-test (zen:Unspecified-Class type) value source-declaration walker resume environment)
  `(%%unspecified? ,value))


(zen:define-class-runtime zen:Unspecified)


;;;
;;;; Marker
;;;


(zen:define-class-runtime zen:Marker-Class)


(zen:define-method (zen:of-type? (zen:Marker-Class class) object)
  (zen:marker? object))


(zen:define-method (zen:emit-specifier (zen:Marker-Class class))
  'marker)


(zen:define-method (zen:emit-test (zen:Marker-Class type) value source-declaration walker resume environment)
  `(zen:marker? ,value))


(zen:define-class-runtime zen:Marker)


;;;
;;;; Types
;;;


(define zen:subtypes
  (%%make-vector 32 #f))

(define zen:specialtypes
  (%%make-vector 16 #f))

;; quicky until we find a clean solution with Marc
(define zen:table-type
  (%%structure-type (make-table)))

;; quicky until we find a clean solution with Marc
(define zen:port-type
  (%%structure-type (open-output-string)))

;; quicky until we find a clean solution with Marc
(define zen:thread-type
  (%%structure-type (current-thread)))

(%%vector-set! zen:subtypes zen:subtype-vector       zen:Vector)
(%%vector-set! zen:subtypes zen:subtype-pair         zen:Pair)
(%%vector-set! zen:subtypes zen:subtype-ratnum       zen:Rational)
(%%vector-set! zen:subtypes zen:subtype-cpxnum       zen:Complex)
(%%vector-set! zen:subtypes zen:subtype-symbol       zen:Symbol)
(%%vector-set! zen:subtypes zen:subtype-keyword      zen:Keyword)
(%%vector-set! zen:subtypes zen:subtype-continuation zen:Continuation)
(%%vector-set! zen:subtypes zen:subtype-procedure    zen:Procedure)
(%%vector-set! zen:subtypes zen:subtype-string       zen:String)
(%%vector-set! zen:subtypes zen:subtype-flonum       zen:Flonum)
(%%vector-set! zen:subtypes zen:subtype-bignum       zen:Rational)
(%%vector-set! zen:subtypes zen:subtype-foreign      zen:Foreign)
(%%vector-set! zen:subtypes zen:subtype-s8vector     zen:S8Vector)
(%%vector-set! zen:subtypes zen:subtype-u8vector     zen:U8Vector)
(%%vector-set! zen:subtypes zen:subtype-s16vector    zen:S16Vector)
(%%vector-set! zen:subtypes zen:subtype-u16vector    zen:U16Vector)
(%%vector-set! zen:subtypes zen:subtype-s32vector    zen:S32Vector)
(%%vector-set! zen:subtypes zen:subtype-u32vector    zen:U32Vector)
(%%vector-set! zen:subtypes zen:subtype-s64vector    zen:S64Vector)
(%%vector-set! zen:subtypes zen:subtype-u64vector    zen:U64Vector)
(%%vector-set! zen:subtypes zen:subtype-f32vector    zen:F32Vector)
(%%vector-set! zen:subtypes zen:subtype-f64vector    zen:F64Vector)
(%%vector-set! zen:subtypes zen:subtype-boxvalues    zen:Values)

(%%vector-set! zen:specialtypes #x0 zen:Boolean)
(%%vector-set! zen:specialtypes #x1 zen:Boolean)
(%%vector-set! zen:specialtypes #x2 zen:Null)
(%%vector-set! zen:specialtypes #x3 zen:EOF)
(%%vector-set! zen:specialtypes #x4 zen:Unspecified)
(%%vector-set! zen:specialtypes #x5 zen:Marker)   ;; absent
(%%vector-set! zen:specialtypes #x6 zen:Marker)   ;; unbound
(%%vector-set! zen:specialtypes #x7 zen:Marker)   ;; unbound2
(%%vector-set! zen:specialtypes #x8 zen:Marker)   ;; optional
(%%vector-set! zen:specialtypes #x9 zen:Marker)   ;; key
(%%vector-set! zen:specialtypes #xA zen:Marker)   ;; rest
(%%vector-set! zen:specialtypes #xB zen:Marker)   ;; unused
(%%vector-set! zen:specialtypes #xC zen:Marker)   ;; deleted
(%%vector-set! zen:specialtypes #xD zen:Marker)   ;; promise
(%%vector-set! zen:specialtypes #xE zen:Marker)   ;; unassigned1
(%%vector-set! zen:specialtypes #xF zen:Marker)   ;; unassigned2


;;;
;;;; Property
;;;


(zen:define-class-runtime zen:Property)


(define (zen:new-property slot-name slot-offset slot-initialize slot-getter slot-setter)
  (zen:allocate-property slot-name slot-offset slot-initialize slot-getter slot-setter))


(define (zen:property? object)
  (%%class-is? object zen:Property))


(define (zen:property-getter property)
  (%%get-property-getter property))


(define (zen:property-setter property)
  (%%get-property-setter property))


(define (zen:all-properties category)
  (let iter ((slots (%%get-class-instance-slots category)))
     (cond ((%%null? slots) '())
           ((zen:property? (%%car slots)) (%%cons (%%car slots) (iter (%%cdr slots))))
           (else (iter (%%cdr slots))))))


(define (zen:add-property class slot-name slot-initialize slot-allocate? slot-getter slot-setter)
  (let ((actual (%%get-category-field class slot-name)))
    (cond (actual
           (%%set-slot-initialize actual slot-initialize)
           (%%set-property-getter actual slot-getter)
           (%%set-property-setter actual slot-setter)
           actual)
          (else
           (let* ((instance-size (%%get-class-instance-size class))
                  (slot-offset (and slot-allocate? instance-size))
                  (slot (zen:new-property slot-name slot-offset slot-initialize slot-getter slot-setter)))
             (zen:add-field class slot)
             (%%set-class-slots class (%%append (%%get-class-slots class) (%%list slot)))
             (let ((class-instance-slots (%%get-class-instance-slots class)))
               (%%when (zen:find-in (lambda (instance-slot)
                                       (%%eq? slot-name (%%get-field-name instance-slot)))
                                     class-instance-slots)
                 (zen:error "Redefinition of property {s} in {s}" slot-name (%%get-category-identifier class)))
               (%%set-class-instance-slots class (%%append class-instance-slots (%%list slot))))
             (%%when slot-allocate?
               (%%set-class-instance-size class (%%fx+ instance-size 1)))
             slot)))))


;;;
;;;; Method
;;;


(zen:define-class-runtime zen:Method)


(define (zen:method? object)
  (%%class-is? object zen:Method))


(define (zen:final-method? field)
  (and (%%class-is? field zen:Method)
       (%%eq? (%%get-method-dispatch-type field) 'final)))


(define (zen:virtual-method? field)
  (and (%%class-is? field zen:Method)
       (%%neq? (%%get-method-dispatch-type field) 'final)))


(define (zen:locate-method-owner category method-name)
  (let iter ((category category))
    (cond ((%%not category)
           #f)
          ((%%get-category-field category method-name)
           category)
          ((%%class-is? category zen:Class)
           (or (iter (%%get-class-ascendant category))
               (zen:find-in iter (%%get-class-interfaces category))))
          ((%%class-is? category zen:Interface)
           (zen:find-in iter (%%get-interface-ascendants category))))))


(define (zen:iterate-class-overrides class proc)
  (let ((ascendant (%%get-class-ascendant class)))
    (let iter ((ancestor ascendant))
         (if ancestor
             (begin
               (zen:table-iterate-safe (%%get-category-fields ancestor)
                 (lambda (name field)
                   (if (and (%%is? field zen:Method)
                            (let ((dispatch-type (%%get-method-dispatch-type field)))
                              (or (%%eq? dispatch-type 'class)
                                  (%%eq? dispatch-type 'interface)))
                            (let ((category-rank (%%get-method-category-rank field))
                                  (implementation-rank (%%get-method-implementation-rank field)))
                              (%%neq? (%%class-dispatch class category-rank implementation-rank)
                                      (%%class-dispatch ascendant category-rank implementation-rank))))
                       (proc field))))
               (iter (%%get-class-ascendant ancestor)))))))


(define (zen:update-method class method-name method-implementation)
  (let ((owner (zen:locate-method-owner class method-name)))
    (let ((method (%%get-category-field owner method-name))
          (method-locator (%%compose-reference (%%get-category-identifier class) method-name)))
      (zen:global-set! method-locator method-implementation)
      (if (%%eq? owner class)
          (case (%%get-method-dispatch-type method)
            ((class interface) (zen:add-virtual-method class method-name method-implementation))
            ((final) (zen:add-final-method class method-name method-implementation)))
        (zen:add-method-node class method-name method-implementation)))))

  
;;;
;;;; Final Method
;;;


(define (zen:new-final-method name implementation)
  (zen:allocate-method name 'final implementation #f #f #f))


(define (zen:add-final-method class method-name method-implementation)
  (define (create-final-method class method-name method-implementation)
    (let ((method (zen:new-final-method method-name method-implementation)))
      (zen:add-field class method)
      method))
  
  (define (update-final-method class method-name method-implementation)
    (let ((field (%%get-category-field class method-name)))
      (if (zen:final-method? field)
          (%%set-method-implementation field method-implementation)
        (zen:error "Cannot change method propagation to final: {a}" method-implementation))
      field))
  
  (let ((owner (zen:locate-method-owner class method-name)))
    (cond ((%%not owner)
           (create-final-method class method-name method-implementation))
          ((%%eq? owner class)
           (update-final-method class method-name method-implementation))
          (else
           (zen:error "Cannot redefine virtual method: {a}" method-implementation)))))


;;;
;;;; Virtual Method
;;;


(define (zen:new-virtual-method name dispatch-type implementation-tree category-rank implementation-rank)
  (zen:allocate-method name dispatch-type #f implementation-tree category-rank implementation-rank))


(define (zen:add-virtual-method category method-name method-implementation)
  (define (create-virtual-method category method-name method-implementation)
    (let* ((dispatch-type (if (%%class-is? category zen:Class) 'class 'interface))
           (node (zen:new-method-node category method-implementation #f '()))
           (method (zen:new-virtual-method method-name dispatch-type node #f #f))
           (virtual-size (%%get-category-virtual-size category))
           (core-level/rank (and (%%class-is? category zen:Class) (zen:find-core-level/rank category method-name))))
      (if core-level/rank
          (receive (level rank) core-level/rank
            (%%set-method-implementation-rank method rank))
        (begin
          (%%set-method-implementation-rank method virtual-size)
          (%%set-category-virtual-size category (%%fx+ virtual-size 1))))
      (zen:add-field category method)
      (zen:update-category category)
      (%%get-method-implementation-rank method)))
  
  (define (update-virtual-method category method-name method-implementation)
    (let ((field (%%get-category-field category method-name)))
      (if (zen:virtual-method? field)
          (let ((node (%%get-method-implementation-tree field)))
            (%%set-method-node-implementation node method-implementation)
            (for-each (lambda (child)
                        (%%set-method-node-next-implementation child method-implementation))
                      (%%get-method-node-children node)))
        (zen:error "Cannot virtualize final method: {a}" method-implementation))
      (zen:update-category category)
      (%%get-method-implementation-rank field)))
  
  (let ((owner (zen:locate-method-owner category method-name)))
    (cond ((%%not owner)
           (create-virtual-method category method-name method-implementation))
          ((%%eq? owner category)
           (update-virtual-method category method-name method-implementation))
          (else
           (zen:error "Cannot rebase virtual method: {a}" method-implementation)))))


;;;
;;;; Method Dispatch
;;;


(define (zen:add-method-node class method-name method-implementation)
  (define (create/update-method-node root-node class method-implementation)
    (let ((node (locate-most-specific-method-node root-node class)))
      (if (%%eq? class (%%get-method-node-category node)) ; exact match
          (update-method-node node class method-implementation)
        (create-method-node node class method-implementation))))
  
  (define (locate-most-specific-method-node node category)
    (let iter ((node node))
         (if (%%eq? category (%%get-method-node-category node))
             node ; exact match
           (let sub-iter ((children (%%get-method-node-children node)))
                (if (%%null? children)
                    node ; inherited match
                  (let* ((child (%%car children))
                         (child-category (%%get-method-node-category child)))
                    (if (%%subtype? category child-category)
                        (iter child)
                      (sub-iter (%%cdr children)))))))))
  
  (define (create-method-node node class method-implementation)
    (let* ((partition (zen:partition (%%get-method-node-children node)
                                      (lambda (child)
                                        (let ((child-class (%%get-method-node-category child)))
                                          (%%subtype? class child-class)))
                                      assv))
           (new-children (%%cdr (or (%%assq #t partition) '(#t))))
           (old-children (%%cdr (or (%%assq #f partition) '(#f))))
           (new-node (zen:new-method-node class method-implementation node new-children)))
      (for-each (lambda (child)
                  (%%set-method-node-next-node child new-node)
                  (%%set-method-node-next-implementation child method-implementation))
                new-children)
      (%%set-method-node-children node (%%cons new-node old-children))
      (values new-node new-children)))
  
  (define (update-method-node node class method-implementation)
    (%%set-method-node-implementation node method-implementation)
    (for-each (lambda (child)
                (%%set-method-node-next-implementation child method-implementation))
              (%%get-method-node-children node))
    (values node (%%get-method-node-children node)))

  (define (update-method-tree proc start-node end-nodes)
    (let ((end-categories (map (lambda (node)
                                 (%%get-method-node-category node))
                               end-nodes)))
      (let iter ((category (%%get-method-node-category start-node)))
           (%%when (%%not (%%memq category end-categories))
             (proc category)
             (for-each (lambda (descendant)
                         (iter descendant))
                       (%%get-category-descendants category))))))

  (let ((owner (zen:locate-method-owner class method-name)))
    (cond ((%%not owner)
           (zen:error "Cannot locate root method: {a}" method-implementation))
          ((%%eq? owner class)
           (zen:error "Cannot remove root method: {a}" method-implementation))
          (else
           (let ((field (%%get-category-field owner method-name)))
             (cond ((zen:virtual-method? field)
                    (let ((root-node (%%get-method-implementation-tree field)))
                      (receive (start-node end-nodes) (create/update-method-node root-node class method-implementation)
                        (let ((category-rank (%%get-method-category-rank field))
                              (implementation-rank (%%get-method-implementation-rank field)))
                          (update-method-tree (lambda (class)
                                                (let* ((dispatch-table (case (%%get-method-dispatch-type field)
                                                                         ((class)     (%%get-class-class-table class))
                                                                         ((interface) (%%get-class-interface-table class))))
                                                       (method-table (%%vector-ref dispatch-table category-rank)))
                                                  (%%vector-set! method-table implementation-rank method-implementation)))
                                                   start-node end-nodes)
                          start-node))))
                   ((zen:final-method? field)
                    (zen:error "Overriding final method: {a}" method-implementation))
                   (else
                    (error "Method zen:add-method-node unimplemented for Interface"))))))))


;;;
;;;; Method Node
;;;


(zen:define-class-runtime zen:Method-Node)


(define (zen:new-method-node category implementation next-node children)
  (let ((next-implementation (if next-node (%%get-method-node-implementation next-node) #f)))
    (zen:allocate-method-node category implementation next-node next-implementation children)))


(define (zen:call-into-incoherent . rest)
  (zen:error "Dispatch table contains non-method, parameters are {l}" rest))


(define (zen:call-into-abstract class method object arguments)
  (zen:error "Cannot call abstract method {s} on a {s}" method class))


;;;
;;;; Call-Site
;;;


(zen:define-class-runtime zen:Call-Site)


(define (zen:new-call-site name id properties procedure)
  (zen:allocate-call-site name id properties procedure #f))


;;;
;;;; Queue
;;;


(zen:define-class-runtime zen:Queue)


(define (zen:new-queue)
  (zen:allocate-queue '() #f #f))


(define (zen:queue-empty? queue)
  (%%null? (%%get-queue-head queue)))


(define (zen:queue-length queue)
  (%%length (%%get-queue-head queue)))


(define (zen:enqueue queue object)
  (let ((added (%%cons object '())))
    (zen:enqueue-impl queue added
      (lambda (tail)
        (%%set-cdr! tail added)))
    (%%set-queue-tail queue added)
    (%%set-queue-shared queue #f)))


(define (zen:enqueue-list queue lst)
  (%%when (%%not-null? lst)
    (zen:enqueue-impl queue lst
      (lambda (tail)
        (%%set-cdr! tail lst)
        (%%set-queue-tail queue tail)))
    (%%set-queue-shared queue lst)))


(define (zen:enqueue-impl queue added proc)
  (define (stitch-head added)
    (let ((tail (%%get-queue-tail queue)))
      (if tail
          (%%set-cdr! tail added)
        (%%set-queue-head queue added))))
  
  (let ((shared (%%get-queue-shared queue)))
    (if shared
        (let ((copy (zen:list-copy (%%get-queue-shared queue))))
          (stitch-head copy)
          (proc (zen:last-pair copy)))
      (stitch-head added))))


(define (zen:dequeue queue)
  (let ((head (%%get-queue-head queue)))
    (if (%%null? head)
        (error "Queue is empty")
      (let ((next (%%cdr head)))
        (cond ((%%eq? head (%%get-queue-tail queue))
               (%%set-queue-tail queue #f))
              ((%%eq? head (%%get-queue-shared queue))
               (%%set-queue-shared queue (if (%%null? next) #f next))))
        (%%set-queue-head queue next)
        (%%car head)))))


(define (zen:queue-list queue)
  (%%get-queue-head queue))


;; should be made to work with shared content
(define (zen:trim-queue queue keep)
  (define (tail lst n)
    (if (or (%%null? lst) (%%fx= n 0))
        lst
      (tail (%%cdr lst) (%%fx- n 1))))
  
  (if (%%fx= keep 0)
      (let ((trimmed (%%length (%%get-queue-head queue))))
        (zen:reset-queue queue)
        trimmed)
    (let ((tail (tail (%%get-queue-head queue) (%%fx- keep 1))))
      (if (and (%%not-null? tail) (%%not-null? (%%cdr tail)))
          (let ((trimmed (%%length (%%cdr tail))))
            (%%set-cdr! tail '())
            (%%set-queue-tail queue tail)
            trimmed)
        0))))


(define (zen:reset-queue queue)
  (%%set-queue-head queue '())
  (%%set-queue-tail queue #f)
  (%%set-queue-shared queue #f))


;;;
;;;; Output Hook
;;;


(zen:define-variable zen:print-hook
  #f)


(%%wr-set!
  (lambda (we obj)
    (cond ;; bongo
          ((%%eq? (zen:writeenv-style we) 'mark)
           (%%default-wr we obj))
          ((and (%%not (%%zen? obj)) (%%record? obj))
           (%%default-wr we (zen:record->vector obj)))
          ((and (%%zen? obj) zen:print-hook)
           (zen:print-hook obj (zen:writeenv-port we) (zen:writeenv-style we)))
          (else
           (zen:write-object (zen:class-of obj) we obj)))))


;;;
;;;; Output
;;;


(define zen:output-mode
  ':reader)


(zen:define-variable-override zen:display
  (lambda (value output)
    (zen:output-value value output ':human)))


(zen:define-variable-override zen:write
  (lambda (value output)
    (zen:output-value value output ':reader)))


(define (zen:print-detail value output detail)
  (case detail
    ((:human) (display value output))
    ((:reader :text :describe) (write value output))
    (else (zen:error "Unknown print detail: {s}" detail))))


(define (zen:->string value)
  (cond ((%%unspecified? value)
         "<unspecified>")
        ((%%values? value)
         "<values>")
        (else
         (let ((output (open-output-string)))
           (zen:output-value value output zen:output-mode)
           (get-output-string output)))))


(define (zen:output-value value output detail)
  (cond ((or (%%null? value) (%%pair? value))
         (zen:output-list value output detail))
        ((zen:primitive? value)
         (zen:print-detail value output detail))
        (else
         (zen:print-zen value output detail))))


(define (zen:output-list lst output detail)
  (define (output-list-content lst output detail)
    (if (%%not (%%null? lst))
        (let ((scan lst)
              (done? #f))
          (%%while (and (%%not done?) (%%not (%%null? scan)))
            (zen:output-value (%%car scan) output detail)
            (set! scan (%%cdr scan))
            (if (%%not (%%null? scan))
                (if (%%pair? scan)
                    (display " " output)
                  (begin
                    (display " . " output)
                    (zen:output-value scan output detail)
                    (set! done? #t))))))))
  
  (display "(" output)
  (output-list-content lst output detail)
  (display ")" output))


(define (zen:debug . rest)
  (let ((port (console-port)))
    (%%when (%%not-null? rest)
      (display (zen:->string (%%car rest)) port)
      (for-each (lambda (expr)
                  (display " " port)
                  (display (zen:->string expr) port))
                (%%cdr rest)))
    (newline port)
    (force-output port)))


(define (zen:debug-string str)
  (let ((port (console-port)))
    (display str port)
    (force-output port)))


(define (zen:debug-line str)
  (let ((port (console-port)))
    (display str port)
    (newline port)
    (force-output port)))


(define (zen:debug-newline)
  (let ((port (console-port)))
    (newline port)
    (force-output port)))


(define zen:terminal
  zen:debug)


(define zen:terminal-string
  zen:debug-string)


(define zen:terminal-line
  zen:debug-line)


(define zen:terminal-newline
  zen:debug-newline)


(define (zen:terminal-port)
  (console-port))


(define (zen:bootstrap-output-value value output)
  (display (zen:->string value) output))


(define (zen:pretty-print expr . rest)
  (apply pretty-print expr rest))


(define (zen:print-value call-print we object)
  (if (zen:use-print?)
      (let ((detail (if (eq? (zen:writeenv-style we) 'display) ':human ':reader)))
        (call-print object (zen:writeenv-port we) detail))
    (%%default-wr we object)))


(define (zen:print-zen object output detail)
  (if (zen:use-print?)
      (zen:call-print object output detail)
    (zen:print-serial object output)))


(zen:define-variable-override zen:print-hook
  (lambda (object port style)
    (let ((detail (if (%%eq? style 'display) ':human ':reader)))
      (zen:print-zen object port detail)))))
