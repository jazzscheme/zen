;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Class Syntax
;;;


(block core.class-syntax


;;;
;;;; Errors
;;;


(zen:define-variable zen:object-of-class?)


(define (zen:expected-error class obj)
  (zen:error "{a} expected: {s}" class obj))


;;;
;;;; Object
;;;


(define %%object-content
  0)


(define zen:object-class
  %%object-content)


(define zen:object-size
  (%%fx+ zen:object-class 1))


(zen:define-macro (%%assert-class object class . body)
  `(%%core-assertion (zen:object-of-class? ,object ,class) (zen:expected-error ,class ,object)
     ,@body))


(zen:define-macro (%%get-object-class object)
  `(%%object-ref ,object ,zen:object-class))


(zen:define-macro (%%set-object-class object class)
  `(%%object-set! ,object ,zen:object-class ,class))


(zen:define-macro (%%new class . rest)
  (zen:with-uniqueness class
    (lambda (cls)
      (let ((obj (zen:generate-symbol "obj")))
        `(%%debug-assert (%%class? ,cls)
           (let ((,obj (%%make-object ,cls (%%get-class-instance-size ,cls))))
             (zen:initialize-slots ,obj)
             ((%%class-dispatch ,cls 0 0) ,obj ,@rest)
             ,obj))))))


;;;
;;;; Define Class
;;;


(zen:define-structure Class-Info () (constructor: zen:make-class-info)
  ((metaclass-accessor getter: generate)
   (ascendant-accessor getter: generate)
   (constructor        getter: generate)
   (constructor-type   getter: generate)
   (accessors-type     getter: generate)
   (slots              getter: generate)
   (slot-names         getter: generate)
   (all-slot-names     getter: generate)
   (instance-size      getter: generate)))


(define zen:class-info
  (%%make-table test: eq?))


(zen:define-macro (zen:define-class name ascendant-name options slots)
  `(begin
     (zen:define-class-syntax ,name ,ascendant-name ,options ,slots)
     (zen:define-class-runtime ,name)))


(zen:define-macro (zen:define-class-syntax name ascendant-name options slots)
  (define (downcase str)
    (let ((down (list->string (map char-downcase (string->list str)))))
      (if (zen:string-starts-with? down "zen:")
          (%%substring down 4 (%%string-length down))
        down)))
  
  (define (standardize-slot slot)
    (if (%%symbol? slot)
        (%%list slot)
      slot))
  
  (define (parse-slot slot prefix downcase-name)
    (define (slot-initialize slot)
      (zen:getf (%%cdr slot) initialize: (zen:unspecified)))
    
    (define (slot-getter slot)
      (parse-accessor slot getter: "get-"))
    
    (define (slot-setter slot)
      (parse-accessor slot setter: "set-"))
    
    (define (parse-accessor slot accessor-key accessor-prefix)
      (define (generate-accessor)
        (%%string->symbol (%%string-append prefix accessor-prefix downcase-name "-" (%%symbol->string (%%car slot)))))
      
      (let ((accessor (or (zen:getf (%%cdr slot) accessor-key #f)
                          (zen:getf (%%cdr slot) accessors: #f))))
        (cond ((%%eq? accessor #t)
               #f)
              ((%%eq? accessor 'generate)
               (generate-accessor))
              (else
               accessor))))
    
    (let ((slot (standardize-slot slot)))
      (%%list (slot-name slot)
              (slot-initialize slot)
              (slot-getter slot)
              (slot-setter slot))))
  
  (define (slot-name slot)
    (%%car slot))
  
  (let* ((downcase-name (downcase (%%symbol->string name)))
         (metaclass-name (zen:getf options metaclass: #f))
         (constructor (zen:getf options constructor: #f))
         (constructor-type (zen:getf options constructor-type: #f))
         (accessors-type (zen:getf options accessors-type: 'function))
         (accessors-prefix (case accessors-type ((macro) "%%") (else "zen:")))
         (metaclass-accessor (if (%%not metaclass-name) 'zen:Object-Class metaclass-name))
         (ascendant-accessor (if (%%null? ascendant-name) #f ascendant-name))
         (ascendant-info (%%table-ref zen:class-info ascendant-name #f))
         (ascendant-slot-names (if (%%null? ascendant-name) '() (zen:get-class-info-all-slot-names ascendant-info)))
         (ascendant-size (%%length ascendant-slot-names))
         (slots (map (lambda (slot) (parse-slot slot accessors-prefix downcase-name)) slots))
         (slot-names (map slot-name slots))
         (all-slot-names (%%append ascendant-slot-names slot-names))
         (all-length (%%length all-slot-names))
         (instance-size (%%fx+ zen:object-size all-length)))
    (%%table-set! zen:class-info name (zen:make-class-info metaclass-accessor ascendant-accessor constructor constructor-type accessors-type slots slot-names all-slot-names instance-size))
    `(begin
       ,@(map (lambda (slot rank)
                (zen:bind (slot-name slot-initialize slot-getter slot-setter) slot
                  `(begin
                     ,@(cond ((%%not slot-getter)
                              '())
                             (else
                              (case accessors-type
                                ((macro)
                                 `((zen:define-macro (,slot-getter object)
                                     (zen:with-uniqueness object
                                       (lambda (obj)
                                         `(%%assert-class ,obj ,',name
                                            (%%object-ref ,obj ,,rank)))))))
                                (else
                                 '()))))
                     ,@(cond ((%%not slot-setter)
                              '())
                             (else
                              (case accessors-type
                                ((macro)
                                 `((zen:define-macro (,slot-setter object value)
                                     (zen:with-uniqueness object
                                       (lambda (obj)
                                         `(%%assert-class ,obj ,',name
                                            (%%object-set! ,obj ,,rank ,value)))))))
                                (else
                                 '())))))))
              slots
              (zen:naturals (%%fx+ zen:object-size ascendant-size) instance-size))
       (%%table-set! zen:class-info ',name (zen:make-class-info ',metaclass-accessor ',ascendant-accessor ',constructor ',constructor-type ',accessors-type ',slots ',slot-names ',all-slot-names ',instance-size)))))


(zen:define-macro (zen:define-class-runtime name #!optional (bootstrap-type? #f))
  (let ((class-info (%%table-ref zen:class-info name))
        (class-level-name (%%compose-helper name 'core-level)))
    (let ((metaclass-accessor (and (%%not bootstrap-type?) (zen:get-class-info-metaclass-accessor class-info)))
          (ascendant-accessor (zen:get-class-info-ascendant-accessor class-info)))
      (let ((ascendant-info (if (%%not ascendant-accessor) #f (%%table-ref zen:class-info ascendant-accessor))))
        (let ((ascendant-slot-names (if (%%not ascendant-info) '() (zen:get-class-info-all-slot-names ascendant-info)))
              (constructor (zen:get-class-info-constructor class-info))
              (constructor-type (zen:get-class-info-constructor-type class-info))
              (accessors-type (zen:get-class-info-accessors-type class-info))
              (slots (zen:get-class-info-slots class-info))
              (slot-names (zen:get-class-info-slot-names class-info))
              (all-slot-names (zen:get-class-info-all-slot-names class-info))
              (instance-size (zen:get-class-info-instance-size class-info)))
          (let ((ascendant-size (%%length ascendant-slot-names))
                (all-variables (map (lambda (slot-name) (zen:generate-symbol (%%symbol->string slot-name))) all-slot-names))
                (class-symbol (zen:generate-symbol "class"))
                (obj-symbol (zen:generate-symbol "obj"))
                (rest-symbol (zen:generate-symbol "rest")))
            `(begin
               ,@(map (lambda (slot rank)
                        (zen:bind (slot-name slot-initialize slot-getter slot-setter) slot
                          `(begin
                             ,@(cond ((%%not slot-getter)
                                      '())
                                     (else
                                      (case accessors-type
                                        ((macro)
                                         '())
                                        (else
                                         `((define (,slot-getter object)
                                             (%%assert-class object ,name
                                               (%%object-ref object ,rank))))))))
                             ,@(cond ((%%not slot-setter)
                                      '())
                                     (else
                                      (case accessors-type
                                        ((macro)
                                         '())
                                        (else
                                         `((define (,slot-setter object value)
                                             (%%assert-class object ,name
                                               (%%object-set! object ,rank value)))))))))))
                      slots
                      (zen:naturals (%%fx+ zen:object-size ascendant-size) instance-size))
               (define ,name
                 (zen:new-core-class ,metaclass-accessor ',name (%%make-table test: eq?) ,ascendant-accessor))
               (define ,class-level-name
                 (%%get-class-level ,name))
               ,@(if (%%not constructor)
                     '()
                   (case constructor-type
                     ((keyword)
                      `((define ,constructor
                          (zen:new-constructor ,name))))
                     (else
                      `((define (,constructor ,@all-variables)
                          (%%object ,name ,@all-variables))))))
               ,@(if bootstrap-type?
                     '()
                   (zen:expand-slots name slots))
               (zen:set-core-class ',(zen:reference-name name) ,name))))))))


(zen:define-macro (zen:define-class-bootstrap name)
  (let ((class-info (%%table-ref zen:class-info name)))
    (let ((metaclass-accessor (zen:get-class-info-metaclass-accessor class-info))
          (ascendant-accessor (zen:get-class-info-ascendant-accessor class-info))
          (slots (zen:get-class-info-slots class-info)))
      `(begin
         (%%set-object-class ,name ,metaclass-accessor)
         ,@(if (%%not ascendant-accessor)
               '()
             `((%%set-class-instance-slots ,name (%%get-class-instance-slots ,ascendant-accessor))
               (%%set-class-instance-size ,name (%%get-class-instance-size ,ascendant-accessor))))
         ,@(zen:expand-slots name slots)))))


(define (zen:expand-slots name slots)
  (let ((obj-symbol (zen:generate-symbol "obj")))
    (map (lambda (slot)
           (zen:bind (slot-name slot-initialize slot-getter slot-setter) slot
             (if (zen:unspecified? slot-initialize)
                 `(zen:add-slot ,name ',slot-name #f #t)
               `(zen:add-slot ,name ',slot-name (lambda (,obj-symbol) ,slot-initialize) #t))))
         slots)))


;;;
;;;; Object
;;;


(zen:define-structure zen:Object () ()
  ())


;;;
;;;; Type
;;;


(zen:define-structure zen:Type zen:Object ()
  ())


;;;
;;;; Category
;;;


(zen:define-structure zen:Category zen:Type (accessors-type: macro)
  ((identifier   accessors: generate)
   (fields       accessors: generate)
   (virtual-size accessors: generate)
   (ancestors    accessors: generate)
   (descendants  accessors: generate)))


;;;
;;;; Class
;;;


(zen:define-structure zen:Class zen:Category (constructor: %%allocate-class constructor-class?: #t accessors-type: macro)
  ((ascendant       accessors: generate)
   (interfaces      accessors: generate)
   (slots           accessors: generate)
   (instance-slots  accessors: generate)
   (instance-size   accessors: generate)
   (level           accessors: generate)
   (virtual-names   accessors: generate)
   (class-table     accessors: generate)
   (interface-table accessors: generate)
   (user-data       accessors: generate)))


;;;
;;;; Interface
;;;


(zen:define-structure zen:Interface zen:Category (constructor: %%allocate-interface constructor-class?: #t accessors-type: macro)
  ((ascendants getter: generate)
   (rank       getter: generate)))


;;;
;;;; Field
;;;


(zen:define-structure zen:Field zen:Object (accessors-type: macro)
  ((name getter: generate)))


;;;
;;;; Slot
;;;


(zen:define-structure zen:Slot zen:Field (constructor: %%allocate-slot constructor-class?: #t accessors-type: macro)
  ((offset     getter: generate)
   (initialize getter: generate setter: generate)))


;;;
;;;; Dispatch
;;;


(zen:define-macro (%%final-dispatch class implementation)
  implementation)


(zen:define-macro (%%class-dispatch class class-level implementation-rank)
  `(%%vector-ref (%%vector-ref (%%get-class-class-table ,class) ,class-level) ,implementation-rank))


(zen:define-macro (%%interface-dispatch class interface-rank implementation-rank)
  `(%%vector-ref (%%vector-ref (%%get-class-interface-table ,class) ,interface-rank) ,implementation-rank))


;; This macro enables zen to bootstrap fully interpreted
;; Note that we need in the pure scheme version to implement every type that can potentially be used by
;; Zen code so that really means every type if we want to be a superset of the underlying scheme system
(zen:define-macro (%%class-of-impl obj)
  (case (zen:walk-for)
    ((compile)
     `(%%c-class-of-impl ,obj))
    (else
     `(%%scheme-class-of-impl ,obj))))


(zen:define-macro (%%c-class-of-impl obj)
  `(or (c-code ,(%%string-append #<<end-of-c-code
{
    ___SCMOBJ obj = ___ARG1;
    if (___MEM_ALLOCATED(obj))
    {
        int subtype = (*___UNTAG(obj) & ___SMASK) >> ___HTB;
end-of-c-code

    #<<end-of-c-code
        if (subtype == ___sJAZZ)
end-of-c-code

#<<end-of-c-code
            ___RESULT = ___VECTORREF(obj,0);
        else if (subtype == ___sSTRUCTURE)
            ___RESULT = ___FAL;
        else
            ___RESULT = ___BODY_AS(___ARG2,___tSUBTYPED)[subtype];
    }
    else if (___FIXNUMP(obj))
        ___RESULT = ___ARG3;
    else if (obj >= 0)
        ___RESULT = ___ARG4;
    else
        ___RESULT = ___BODY_AS(___ARG5,___tSUBTYPED)[___INT(___FAL - obj)];
}
end-of-c-code
)
    ,obj                   ;; ___ARG1
    zen:subtypes           ;; ___ARG2
    zen:Fixnum             ;; ___ARG3
    zen:Char               ;; ___ARG4
    zen:specialtypes       ;; ___ARG5
    )
       (zen:structure-type ,obj)))


(zen:define-macro (%%scheme-class-of-impl obj)
  `(cond ((%%object? ,obj)       (%%get-object-class ,obj))
         ((%%boolean? ,obj)      zen:Boolean)
         ((%%char? ,obj)         zen:Char)
         ((%%fixnum? ,obj)       zen:Fixnum)
         ((%%ratnum? ,obj)       zen:Ratnum)
         ((%%flonum? ,obj)       zen:Flonum)
         ((%%integer? ,obj)      zen:Integer)
         ((%%rational? ,obj)     zen:Rational)
         ((%%real? ,obj)         zen:Real)
         ((%%complex? ,obj)      zen:Complex)
         ((%%number? ,obj)       zen:Number)
         ((%%null? ,obj)         zen:Null)
         ((%%pair? ,obj)         zen:Pair)
         ((%%string? ,obj)       zen:String)
         ((%%vector? ,obj)       zen:Vector)
         ((%%s8vector? ,obj)     zen:S8Vector)
         ((%%u8vector? ,obj)     zen:U8Vector)
         ((%%s16vector? ,obj)    zen:S16Vector)
         ((%%u16vector? ,obj)    zen:U16Vector)
         ((%%s32vector? ,obj)    zen:S32Vector)
         ((%%u32vector? ,obj)    zen:U32Vector)
         ((%%s64vector? ,obj)    zen:S64Vector)
         ((%%u64vector? ,obj)    zen:U64Vector)
         ((%%f32vector? ,obj)    zen:F32Vector)
         ((%%f64vector? ,obj)    zen:F64Vector)
         ((%%symbol? ,obj)       zen:Symbol)
         ((%%keyword? ,obj)      zen:Keyword)
         ((%%port? ,obj)         zen:Port)
         ((%%continuation? ,obj) zen:Continuation)
         ((%%procedure? ,obj)    zen:Procedure)
         ((%%foreign? ,obj)      zen:Foreign)
         ((%%values? ,obj)       zen:Values)
         ((%%eof-object? ,obj)   zen:EOF)
         ((%%unspecified? ,obj)  zen:Unspecified)
         ((zen:marker? ,obj)    zen:Marker)
         (else
          (or (zen:structure-type ,obj)
              (zen:error "Unable to get class of {s}" ,obj)))))


;;;
;;;; Define Method
;;;


(define (zen:expand-define-virtual-syntax signature bootstrap-type?)
  (let* ((name (zen:reference-name (%%car signature)))
         (macro-name (%%car signature))
         (parameters (%%cdr signature))
         (class-name (%%caar parameters))
         (object-parameter (%%cadr (%%car parameters)))
         (extra-parameters (%%cdr parameters))
         (class-level-name (%%compose-helper class-name 'core-level))
         (implementation-name (zen:method-implementation-name class-name name))
         (method-rank-name (zen:method-rank-name implementation-name))
         (is-test (if bootstrap-type? 'zen:bootstrap-type? '%%class-is?))
         (object (zen:generate-symbol "object")))
    `(zen:define-macro (,macro-name ,object-parameter ,@extra-parameters)
       (if (%%symbol? ,object-parameter)
           (%%list '%%core-assertion (%%list ',is-test ,object-parameter ',class-name) (%%list 'zen:error (zen:format "{s} expected in calling {s}: {s}" ',class-name ',name ,object-parameter))
             (%%list (%%list '%%class-dispatch (%%list '%%get-object-class ,object-parameter) ',class-level-name ',method-rank-name)
                     ,object-parameter
                     ,@extra-parameters))
         (zen:with-uniqueness ,object-parameter
           (lambda (,object)
             (%%list '%%core-assertion (%%list ',is-test ,object ',class-name) (%%list 'zen:error (zen:format "{s} expected in calling {s}: {s}" ',class-name ',name ,object-parameter))
               (%%list (%%list '%%class-dispatch (%%list '%%get-object-class ,object) ',class-level-name ',method-rank-name)
                       ,object
                       ,@extra-parameters))))))))


(define (zen:expand-define-virtual signature)
  (let* ((name (zen:reference-name (%%car signature)))
         (parameters (%%cdr signature))
         (class-name (%%caar parameters))
         (implementation-name (zen:method-implementation-name class-name name))
         (rank-name (zen:method-rank-name implementation-name)))
    `(define ,rank-name
       (zen:add-core-virtual-method ,class-name ',name))))


(define (zen:expand-define-method signature body)
  (let* ((name (zen:reference-name (%%car signature)))
         (parameters (%%cdr signature))
         (class-name (%%caar parameters))
         (object-parameter (%%cadr (%%car parameters)))
         (extra-parameters (%%cdr parameters))
         (implementation-name (zen:method-implementation-name class-name name)))
    `(begin
       (define ,implementation-name
         (let ((nextmethod (zen:find-nextmethod ,class-name ',name)))
           (lambda (,object-parameter ,@extra-parameters)
             ,@body)))
       (zen:add-core-method-node ,class-name ',name ,implementation-name))))


(define (zen:method-implementation-name class-name name)
  (let ((name (zen:last (zen:split-string (%%symbol->string name) #\/))))
    (%%string->symbol (%%string-append (%%symbol->string class-name) "^" name))))


(define (zen:method-rank-name implementation-name)
  (%%compose-helper implementation-name 'rank))


(zen:define-macro (zen:define-virtual-syntax signature . rest)
  (let ((bootstrap-type? (if (%%null? rest) #f (%%car rest))))
    (zen:expand-define-virtual-syntax signature bootstrap-type?)))


(zen:define-macro (zen:define-virtual signature)
  (zen:expand-define-virtual signature))


(zen:define-syntax zen:define-method
  (lambda (src)
    (zen:bind (signature . body) (%%cdr (%%source-code src))
      (zen:expand-define-method (%%desourcify signature) body)))))
