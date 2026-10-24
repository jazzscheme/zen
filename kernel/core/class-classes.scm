;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Classes
;;;


(block core.class-classes


;;;
;;;; Object
;;;


(zen:define-class-syntax zen:Object () (metaclass: zen:Object-Class)
  ())


(zen:define-virtual-syntax (zen:initialize (zen:Object object)))
(zen:define-virtual-syntax (zen:destroy (zen:Object object)))
(zen:define-virtual-syntax (zen:call-print (zen:Object object) output detail))
(zen:define-virtual-syntax (zen:print-object (zen:Object object) output detail))
(zen:define-virtual-syntax (zen:tree-fold (zen:Object object) down up here seed environment))


;;;
;;;; Type
;;;


(zen:define-class-syntax zen:Type zen:Object (metaclass: zen:Class)
  ())


(zen:define-virtual-syntax (zen:of-type? (zen:Type type) object) #t)
(zen:define-virtual-syntax (zen:of-subtype? (zen:Type type) subtype) #t)
(zen:define-virtual-syntax (zen:specifiable? (zen:Type type)) #t)
(zen:define-virtual-syntax (zen:category-type? (zen:Type type)) #t)
(zen:define-virtual-syntax (zen:resolve-type (zen:Type type)) #t)
(zen:define-virtual-syntax (zen:emit-specifier (zen:Type type)) #t)
(zen:define-virtual-syntax (zen:emit-type (zen:Type type) source-declaration walker resume environment) #t)
(zen:define-virtual-syntax (zen:emit-test (zen:Type type) value source-declaration walker resume environment) #t)
(zen:define-virtual-syntax (zen:emit-cast (zen:Type type) value source-declaration walker resume environment) #t)


(zen:define-macro (%%subtype? target type)
  `(zen:of-subtype? ,type ,target))


(zen:define-macro (%%subcategory? target category)
  `(zen:vector-memq? ,category (%%get-category-ancestors ,target)))


(zen:define-macro (%%subclass? target class)
  `(zen:vector-memq? ,class (%%get-category-ancestors ,target)))


(zen:define-macro (%%is? object type)
  `(zen:of-type? ,type ,object))


(zen:define-macro (%%is-not? object type)
  `(%%not (%%is? ,object ,type)))


;;;
;;;; Category
;;;


(zen:define-class-syntax zen:Category zen:Type (metaclass: zen:Class)
  ((identifier   accessors: #t)
   (fields       accessors: #t)
   (virtual-size accessors: #t)
   (ancestors    accessors: #t)
   (descendants  accessors: #t)))


(zen:define-virtual-syntax (zen:update-category (zen:Category category)))


;;;
;;;; Class
;;;


(zen:define-class-syntax zen:Class zen:Category (metaclass: zen:Class)
  ((ascendant       accessors: #t)
   (interfaces      accessors: #t)
   (slots           accessors: #t)
   (instance-slots  accessors: #t)
   (instance-size   accessors: #t)
   (level           accessors: #t)
   (virtual-names   accessors: #t)
   (class-table     accessors: #t)
   (interface-table accessors: #t)
   ;; quick hack around types being classes
   ;; and needing a place to store information
   (user-data       accessors: #t)))


(zen:define-virtual-syntax (zen:write-object (zen:Class class) we obj))


(zen:define-macro (%%class-subtype? target class)
  (zen:with-uniqueness target
    (lambda (trg)
      (zen:with-uniqueness class
        (lambda (cls)
          `(let ((class-level (%%get-class-level ,cls)))
             (and (%%fx>= (%%get-class-level ,trg) class-level)
                  (%%eq? (%%vector-ref (%%get-category-ancestors ,trg) class-level) ,cls))))))))


(zen:define-macro (%%class-is? object class)
  `(%%class-subtype? (zen:class-of ,object) ,class))


(zen:define-macro (%%category-is? object category)
  `(%%is? ,object ,category))


(zen:define-macro (%%class? object)
  `(%%class-is? ,object zen:Class))


(zen:define-macro (%%object-class? object)
  `(%%eq? ,object zen:Object))


;;;
;;;; Field
;;;


(zen:define-class-syntax zen:Field zen:Object (accessors-type: macro)
  ((name getter: #t)))


(zen:define-macro (%%get-category-field category field-name)
  `(%%table-ref (%%get-category-fields ,category) ,field-name #f))


(zen:define-macro (%%set-category-field category field-name field)
  `(%%table-set! (%%get-category-fields ,category) ,field-name ,field))


;;;
;;;; Slot
;;;


(zen:define-class-syntax zen:Slot zen:Field (constructor: zen:allocate-slot accessors-type: macro)
  ((offset     getter: #t)
   (initialize getter: #t setter: #t)))


;;;
;;;; Object-Class
;;;


(zen:define-class-syntax zen:Object-Class zen:Class (metaclass: zen:Class)
  ())


;;;
;;;; Primitive Classes
;;;


(zen:define-class-syntax zen:Bool-Class         zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Boolean-Class      zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Char-Class         zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Numeric-Class      zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Number-Class       zen:Numeric-Class  (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Complex-Class      zen:Number-Class   (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Real-Class         zen:Complex-Class  (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Rational-Class     zen:Real-Class     (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Integer-Class      zen:Rational-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Fixnum-Class       zen:Integer-Class  (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Ratnum-Class       zen:Rational-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Flonum-Class       zen:Real-Class     (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Flovec-Class       zen:Flonum-Class   (metaclass: zen:Class) ())
(zen:define-class-syntax zen:S64-Class          zen:Integer-Class  (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Sequence-Class     zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:List-Class         zen:Sequence-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Null-Class         zen:List-Class     (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Pair-Class         zen:List-Class     (metaclass: zen:Class) ())
(zen:define-class-syntax zen:String-Class       zen:Sequence-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Vector-Class       zen:Sequence-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:S8Vector-Class     zen:Sequence-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:U8Vector-Class     zen:Sequence-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:S16Vector-Class    zen:Sequence-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:U16Vector-Class    zen:Sequence-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:S32Vector-Class    zen:Sequence-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:U32Vector-Class    zen:Sequence-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:S64Vector-Class    zen:Sequence-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:U64Vector-Class    zen:Sequence-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:F32Vector-Class    zen:Sequence-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:F64Vector-Class    zen:Sequence-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Structure-Class    zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Port-Class         zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Continuation-Class zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Procedure-Class    zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Symbol-Class       zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Keyword-Class      zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Table-Class        zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Thread-Class       zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Promise-Class      zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Foreign-Class      zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Values-Class       zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Box-Class          zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:EOF-Class          zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Unspecified-Class  zen:Class          (metaclass: zen:Class) ())
(zen:define-class-syntax zen:Marker-Class       zen:Class          (metaclass: zen:Class) ())


(zen:define-class-syntax zen:Bool         zen:Object   (metaclass: zen:Bool-Class)         ())
(zen:define-class-syntax zen:Boolean      zen:Object   (metaclass: zen:Boolean-Class)      ())
(zen:define-class-syntax zen:Char         zen:Object   (metaclass: zen:Char-Class)         ())
(zen:define-class-syntax zen:Numeric      zen:Object   (metaclass: zen:Numeric-Class)      ())
(zen:define-class-syntax zen:Number       zen:Numeric  (metaclass: zen:Number-Class)       ())
(zen:define-class-syntax zen:Complex      zen:Number   (metaclass: zen:Complex-Class)      ())
(zen:define-class-syntax zen:Real         zen:Complex  (metaclass: zen:Real-Class)         ())
(zen:define-class-syntax zen:Rational     zen:Real     (metaclass: zen:Rational-Class)     ())
(zen:define-class-syntax zen:Integer      zen:Rational (metaclass: zen:Integer-Class)      ())
(zen:define-class-syntax zen:Fixnum       zen:Integer  (metaclass: zen:Fixnum-Class)       ())
(zen:define-class-syntax zen:Ratnum       zen:Rational (metaclass: zen:Ratnum-Class)       ())
(zen:define-class-syntax zen:Flonum       zen:Real     (metaclass: zen:Flonum-Class)       ())
(zen:define-class-syntax zen:Flovec       zen:Flonum   (metaclass: zen:Flovec-Class)       ())
(zen:define-class-syntax zen:S64          zen:Integer  (metaclass: zen:S64-Class)          ())
(zen:define-class-syntax zen:Sequence     zen:Object   (metaclass: zen:Sequence-Class)     ())
(zen:define-class-syntax zen:List         zen:Sequence (metaclass: zen:List-Class)         ())
(zen:define-class-syntax zen:Null         zen:List     (metaclass: zen:Null-Class)         ())
(zen:define-class-syntax zen:Pair         zen:List     (metaclass: zen:Pair-Class)         ())
(zen:define-class-syntax zen:String       zen:Sequence (metaclass: zen:String-Class)       ())
(zen:define-class-syntax zen:Vector       zen:Sequence (metaclass: zen:Vector-Class)       ())
(zen:define-class-syntax zen:S8Vector     zen:Sequence (metaclass: zen:S8Vector-Class)     ())
(zen:define-class-syntax zen:U8Vector     zen:Sequence (metaclass: zen:U8Vector-Class)     ())
(zen:define-class-syntax zen:S16Vector    zen:Sequence (metaclass: zen:S16Vector-Class)    ())
(zen:define-class-syntax zen:U16Vector    zen:Sequence (metaclass: zen:U16Vector-Class)    ())
(zen:define-class-syntax zen:S32Vector    zen:Sequence (metaclass: zen:S32Vector-Class)    ())
(zen:define-class-syntax zen:U32Vector    zen:Sequence (metaclass: zen:U32Vector-Class)    ())
(zen:define-class-syntax zen:S64Vector    zen:Sequence (metaclass: zen:S64Vector-Class)    ())
(zen:define-class-syntax zen:U64Vector    zen:Sequence (metaclass: zen:U64Vector-Class)    ())
(zen:define-class-syntax zen:F32Vector    zen:Sequence (metaclass: zen:F32Vector-Class)    ())
(zen:define-class-syntax zen:F64Vector    zen:Sequence (metaclass: zen:F64Vector-Class)    ())
(zen:define-class-syntax zen:Structure    zen:Object   (metaclass: zen:Structure-Class)    ())
(zen:define-class-syntax zen:Port         zen:Object   (metaclass: zen:Port-Class)         ())
(zen:define-class-syntax zen:Continuation zen:Object   (metaclass: zen:Continuation-Class) ())
(zen:define-class-syntax zen:Procedure    zen:Object   (metaclass: zen:Procedure-Class)    ())
(zen:define-class-syntax zen:Symbol       zen:Object   (metaclass: zen:Symbol-Class)       ())
(zen:define-class-syntax zen:Keyword      zen:Object   (metaclass: zen:Keyword-Class)      ())
(zen:define-class-syntax zen:Table        zen:Object   (metaclass: zen:Table-Class)        ())
(zen:define-class-syntax zen:Thread       zen:Object   (metaclass: zen:Thread-Class)       ())
(zen:define-class-syntax zen:Promise      zen:Object   (metaclass: zen:Promise-Class)      ())
(zen:define-class-syntax zen:Foreign      zen:Object   (metaclass: zen:Foreign-Class)      ())
(zen:define-class-syntax zen:Values       zen:Object   (metaclass: zen:Values-Class)       ())
(zen:define-class-syntax zen:Box          zen:Object   (metaclass: zen:Box-Class)          ())
(zen:define-class-syntax zen:EOF          zen:Object   (metaclass: zen:EOF-Class)          ())
(zen:define-class-syntax zen:Unspecified  zen:Object   (metaclass: zen:Unspecified-Class)  ())
(zen:define-class-syntax zen:Marker       zen:Object   (metaclass: zen:Marker-Class)       ())


;;;
;;;; Fixed Types
;;;


(zen:define-class-syntax zen:FixedVector-Class    zen:Vector-Class    (metaclass: zen:Class) ())
(zen:define-class-syntax zen:FixedS8Vector-Class  zen:S8Vector-Class  (metaclass: zen:Class) ())
(zen:define-class-syntax zen:FixedU8Vector-Class  zen:U8Vector-Class  (metaclass: zen:Class) ())
(zen:define-class-syntax zen:FixedS16Vector-Class zen:S16Vector-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:FixedU16Vector-Class zen:U16Vector-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:FixedS32Vector-Class zen:S32Vector-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:FixedU32Vector-Class zen:U32Vector-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:FixedS64Vector-Class zen:S64Vector-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:FixedU64Vector-Class zen:U64Vector-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:FixedF32Vector-Class zen:F32Vector-Class (metaclass: zen:Class) ())
(zen:define-class-syntax zen:FixedF64Vector-Class zen:F64Vector-Class (metaclass: zen:Class) ())


(zen:define-class-syntax zen:FixedVector          zen:Vector    (metaclass: zen:FixedVector-Class)    ())
(zen:define-class-syntax zen:FixedS8Vector        zen:S8Vector  (metaclass: zen:FixedS8Vector-Class)  ())
(zen:define-class-syntax zen:FixedU8Vector        zen:U8Vector  (metaclass: zen:FixedU8Vector-Class)  ())
(zen:define-class-syntax zen:FixedS16Vector       zen:S16Vector (metaclass: zen:FixedS16Vector-Class) ())
(zen:define-class-syntax zen:FixedU16Vector       zen:U16Vector (metaclass: zen:FixedU16Vector-Class) ())
(zen:define-class-syntax zen:FixedS32Vector       zen:S32Vector (metaclass: zen:FixedS32Vector-Class) ())
(zen:define-class-syntax zen:FixedU32Vector       zen:U32Vector (metaclass: zen:FixedU32Vector-Class) ())
(zen:define-class-syntax zen:FixedS64Vector       zen:S64Vector (metaclass: zen:FixedS64Vector-Class) ())
(zen:define-class-syntax zen:FixedU64Vector       zen:U64Vector (metaclass: zen:FixedU64Vector-Class) ())
(zen:define-class-syntax zen:FixedF32Vector       zen:F32Vector (metaclass: zen:FixedF32Vector-Class) ())
(zen:define-class-syntax zen:FixedF64Vector       zen:F64Vector (metaclass: zen:FixedF64Vector-Class) ())


;;;
;;;; Property
;;;


(zen:define-class-syntax zen:Property zen:Slot (constructor: zen:allocate-property accessors-type: macro)
  ((getter getter: generate setter: generate)
   (setter getter: generate setter: generate)))


;;;
;;;; Method
;;;


(zen:define-class-syntax zen:Method zen:Field (constructor: zen:allocate-method accessors-type: macro)
  ((dispatch-type        getter: generate setter: generate)
   (implementation       getter: generate setter: generate)
   (implementation-tree  getter: generate setter: generate)
   (category-rank        getter: generate setter: generate)
   (implementation-rank  getter: generate setter: generate)))


;;;
;;;; Method-Node
;;;


(zen:define-class-syntax zen:Method-Node zen:Object (constructor: zen:allocate-method-node accessors-type: macro)
  ((category            getter: generate setter: generate)
   (implementation      getter: generate setter: generate)
   (next-node           getter: generate setter: generate)
   (next-implementation getter: generate setter: generate)
   (children            getter: generate setter: generate)))


;;;
;;;; Interface
;;;


(zen:define-class-syntax zen:Interface zen:Category (accessors-type: macro)
  ((ascendants getter: generate)
   (rank       getter: generate)))


;;;
;;;; Call-Site
;;;


(zen:define-class-syntax zen:Call-Site zen:Object (constructor: zen:allocate-call-site accessors-type: macro)
  ((name       getter: generate setter: generate)
   (id         getter: generate setter: generate)
   (properties getter: generate setter: generate)
   (procedure  getter: generate setter: generate)
   (monitor    getter: generate setter: generate)))


;;;
;;;; Queue
;;;


(zen:define-class-syntax zen:Queue zen:Object (constructor: zen:allocate-queue accessors-type: macro)
  ((head   getter: generate setter: generate)
   (tail   getter: generate setter: generate)
   (shared getter: generate setter: generate))))
