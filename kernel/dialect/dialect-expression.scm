;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Dialect Expressions
;;;


(block dialect.dialect-expression


;;;
;;;; Proclaim
;;;


(zen:define-class zen:Proclaim zen:Expression (constructor: zen:allocate-proclaim)
  ((clauses getter: generate)))


(define (zen:new-proclaim clauses)
  (zen:allocate-proclaim #f #f clauses))


(zen:define-method (zen:emit-expression (zen:Proclaim expression) declaration walker resume environment)
  (let ((clauses (zen:get-proclaim-clauses expression)))
    (for-each zen:proclaim clauses))
  #f)


(zen:define-method (zen:validate-proclaim (zen:Walker walker) resume declaration form-src)
  (%%unless (%%class-is? declaration zen:Module-Declaration)
    (zen:walk-error walker resume declaration form-src "For now, proclaim can only be used at the module level")))


(define (zen:validate-proclaim-clauses walker resume declaration form-src clauses)
  (define (ill-formed clause)
    (zen:walk-error walker resume declaration form-src "Ill-formed proclaim: {s}" clause))
  
  (define (parse-clause clause)
    (or (zen:parse-proclaim-clause clause)
        (ill-formed clause)))
  
  (define (validate-clause clause)
    (receive (value kind arguments) (parse-clause clause)
      (case kind
        ((warn)
         (let ((warnings (if (%%null? arguments) zen:all-warnings arguments)))
           (for-each (lambda (warning)
                       (%%unless (%%memq warning zen:all-warnings)
                         (zen:walk-error walker resume declaration form-src "Unknown proclaim warning: {s}" warning)))
                     warnings)))
        ((check)
         (let ((checks (if (%%null? arguments) zen:all-checks arguments)))
           (for-each (lambda (check)
                       (%%unless (%%memq check zen:all-checks)
                         (zen:walk-error walker resume declaration form-src "Unknown proclaim check: {s}" check)))
                     checks)))
        ((generate)
         (let ((generates (if (%%null? arguments) zen:all-generates arguments)))
           (for-each (lambda (generate)
                       (%%unless (%%memq generate zen:all-generates)
                         (zen:walk-error walker resume declaration form-src "Unknown proclaim generate: {s}" generate)))
                     generates)))
        (else
         (ill-formed clause)))))
  
  (for-each validate-clause clauses))


(define (zen:walk-proclaim walker resume declaration environment form-src)
  (zen:validate-proclaim walker resume declaration form-src)
  (let ((form (%%desourcify form-src)))
    (let ((clauses (%%cdr form)))
      (zen:validate-proclaim-clauses walker resume declaration form-src clauses)
      (zen:new-proclaim clauses))))


;;;
;;;; Delay
;;;


(zen:define-class zen:Delay zen:Expression (constructor: zen:allocate-delay)
  ((expression getter: generate)))


(define (zen:new-delay expression)
  (zen:allocate-delay #f #f expression))


(zen:define-method (zen:emit-expression (zen:Delay expression) declaration walker resume environment)
  (let ((expr (zen:emit-expression (zen:get-delay-expression expression) declaration walker resume environment)))
    (zen:new-code
      `(delay ,(zen:sourcified-form expr))
      zen:Any
      #f)))


(zen:define-method (zen:tree-fold (zen:Delay expression) down up here seed environment)
  (up expression
      seed
      (zen:tree-fold (zen:get-delay-expression expression) down up here (down expression seed environment) environment)
      environment))


;;;
;;;; Specialized Call
;;;


(define zen:*specializers*
  (%%make-table test: eq?))


(define (zen:add-specializer specialized-declaration specializer)
  (%%table-set! zen:*specializers* specialized-declaration
    (%%append (%%table-ref zen:*specializers* specialized-declaration '())
              (%%list specializer))))


(define (zen:get-specializers binding)
  (%%table-ref zen:*specializers* binding '()))


(zen:define-variable-override zen:emit-specialized-call
  (lambda (operator locator arguments arguments-codes call declaration walker resume environment)
    (if (%%not locator)
        #f
      (or (zen:emit-specialized-locator locator arguments-codes environment)
          (if (%%class-is? operator zen:Binding-Reference)
              (let ((binding (zen:get-binding-reference-binding operator)))
                (let ((specializers (zen:get-specializers binding)))
                  (let ((types (zen:codes-types arguments-codes)))
                    (let iter ((scan specializers) (least-mismatch #f))
                         (if (%%null? scan)
                             (begin
                               ;; a bit too much for now
                               #;
                               (let ((expression (and (%%pair? least-mismatch) (%%car least-mismatch))))
                                 (%%when (and (or (zen:reporting?) (zen:warnings?)) (%%not (%%null? specializers)) (zen:get-warn? 'optimizations)
                                           ;; quicky to suppress duplicate warnings as for the moment those are both primitive and specialize
                                           (%%not (%%memq locator '(scheme.runtime:=
                                                                    scheme.runtime:<
                                                                    scheme.runtime:<=
                                                                    scheme.runtime:>
                                                                    scheme.runtime:>=
                                                                    scheme.runtime:+
                                                                    scheme.runtime:-
                                                                    scheme.runtime:*
                                                                    scheme.runtime:/))))
                                   (zen:warning "Warning: In {a}{a}: Unable to match call to specialized {a}"
                                                 (zen:get-declaration-locator declaration)
                                                 (zen:present-expression-location (and expression (zen:get-expression-source expression)) (zen:get-expression-source operator))
                                                 (zen:get-lexical-binding-name binding)))
                                 ;; for debugging
                                 (%%when (%%memq (zen:get-lexical-binding-name binding) (zen:debug-specializers))
                                   (zen:warning "Warning: In {a}{a}: Unable to match call to specialized {a} on {a}"
                                                 (zen:get-declaration-locator declaration)
                                                 (zen:present-expression-location (and expression (zen:get-expression-source expression)) (zen:get-expression-source operator))
                                                 (zen:get-lexical-binding-name binding)
                                                 types)))
                               #f)
                           (let ((specializer (%%car scan)))
                             (let ((function-type (zen:get-lexical-binding-type specializer)))
                               (let ((mismatch (zen:signature-mismatch arguments types function-type)))
                                 (if (%%not mismatch)
                                     (or (zen:emit-inlined-binding-call specializer arguments-codes call declaration walker resume environment)
                                         (begin
                                           (zen:add-to-module-references declaration specializer)
                                           (zen:new-code
                                             (let ((locator (zen:get-declaration-locator specializer)))
                                               `(,locator ,@(zen:codes-forms arguments-codes)))
                                             (zen:get-function-type-result function-type)
                                             #f)))
                                   (iter (%%cdr scan) (if (or (%%not least-mismatch)
                                                              (%%symbol? least-mismatch)
                                                              (and (%%pair? mismatch)
                                                                   (%%fx< (%%length mismatch) (%%length least-mismatch))))
                                                          mismatch
                                                        least-mismatch)))))))))))
            #f)))))


;; quicky because classes are not yet defined at this point
(zen:define-variable zen:emit-specialized-locator)

(zen:define-variable-override zen:emit-specialized-locator
  (lambda (locator arguments-codes environment)
    #f))


;;;
;;;; New Call
;;;


(zen:define-variable-override zen:emit-new-call
  (lambda (operator locator arguments arguments-codes declaration walker resume environment)
    #f))


;;;
;;;; Primitive Call
;;;


(zen:define-variable-override zen:emit-primitive-call
  (lambda (operator locator arguments arguments-codes declaration walker resume environment)
    (zen:primitive-call-emit operator locator arguments arguments-codes declaration walker resume environment)))


;; To make this a lot more clean would necessitate moving the specializer into the walk phase so that the
;; result of inlining can be Zen code. With this we could specialize for instance (##length x) and ##length
;; would simply be an external typed as <list:int> which would do all type propagation automatically.
;; This is really difficult to achieve because as inlining can impact type inference it also needs
;; to be done at emit phase... Even better, all those should be specialized definitions in Zen with support
;; for specializing based on static types...


(define zen:*primitive-patterns*
  (%%make-table test: eq?))


(define (zen:primitive-patterns-get)
  zen:*primitive-patterns*)


(define (zen:add-primitive-patterns operator patterns)
  (%%table-set! zen:*primitive-patterns* operator
    (map (lambda (pattern)
           (let ((safety (%%car pattern))
                 (primitive (%%cadr pattern))
                 (specifier (%%car (%%cddr pattern))))
             (%%list safety primitive (zen:walk-specifier #f #f #f '() specifier))))
         patterns)))


(define (zen:get-primitive-patterns locator)
  (%%table-ref zen:*primitive-patterns* locator #f))


(zen:add-primitive-patterns     'scheme/runtime:=                                    '((safe %%fx=  <fx*:bool>)  (safe %%fl=  <fv*:bool>)  (safe %%= <number^number:bool>)))
(zen:add-primitive-patterns     'scheme/runtime:<                                    '((safe %%fx<  <fx*:bool>)  (safe %%fl<  <fv*:bool>)))
(zen:add-primitive-patterns     'scheme/runtime:<=                                   '((safe %%fx<= <fx*:bool>)  (safe %%fl<= <fv*:bool>)))
(zen:add-primitive-patterns     'scheme/runtime:>                                    '((safe %%fx>  <fx*:bool>)  (safe %%fl>  <fv*:bool>)))
(zen:add-primitive-patterns     'scheme/runtime:>=                                   '((safe %%fx>= <fx*:bool>)  (safe %%fl>= <fv*:bool>)))

(zen:add-primitive-patterns     'scheme/runtime:+                                    '((safe %%fx+  <fx*:fx>)    (safe %%fl+  <fv*:fl>)    (safe %%+ <int^int:int>) (safe %%+ <number^number:number>)))
(zen:add-primitive-patterns     'scheme/runtime:-                                    '((safe %%fx-  <fx^fx*:fx>) (safe %%fl-  <fv^fv*:fl>) (safe %%- <int^int:int>) (safe %%- <number^number:number>)))
(zen:add-primitive-patterns     'scheme/runtime:*                                    '((safe %%fx*  <fx*:fx>)    (safe %%fl*  <fv*:fl>)    (safe %%* <int^int:int>) (safe %%* <number^number:number>)))

(zen:add-primitive-patterns     'scheme/runtime:/                                    '(                          (safe %%fl/  <fv^fv*:fl>)                   (zero-unsafe %%/ <number^number:number>)))
(zen:add-primitive-patterns     'scheme/runtime:quotient                             '((zero-unsafe %%fxquotient <fx^fx:fx>)))

(zen:add-primitive-patterns     'scheme/runtime:floor                                '(                          (safe %%flfloor    <fv:fl>)))
(zen:add-primitive-patterns     'scheme/runtime:ceiling                              '(                          (safe %%flceiling  <fv:fl>)))
(zen:add-primitive-patterns     'scheme/runtime:truncate                             '(                          (safe %%fltruncate <fv:fl>)))
(zen:add-primitive-patterns     'scheme/runtime:round                                '(                          (safe %%flround    <fv:fl>)))

(zen:add-primitive-patterns     'scheme/runtime:abs                                  '((safe %%fxabs <fx:fx>)    (safe %%flabs      <fv:fl>)))
(zen:add-primitive-patterns     'scheme/runtime:sqrt                                 '(                          (safe %%flsqrt     <fv:fl>)))
(zen:add-primitive-patterns     'scheme/runtime:expt                                 '(                          (safe %%flexpt     <fv^fv:fl>)))
(zen:add-primitive-patterns     'scheme/runtime:square                               '(                          (safe %%flsquare   <fv:fl>)))

(zen:add-primitive-patterns     'scheme/runtime:sin                                  '(                          (safe %%flsin      <fv:fl>)))
(zen:add-primitive-patterns     'scheme/runtime:cos                                  '(                          (safe %%flcos      <fv:fl>)))
(zen:add-primitive-patterns     'scheme/runtime:tan                                  '(                          (safe %%fltan      <fv:fl>)))
(zen:add-primitive-patterns     'scheme/runtime:asin                                 '(                          (safe %%flasin     <fv:fl>)))
(zen:add-primitive-patterns     'scheme/runtime:acos                                 '(                          (safe %%flacos     <fv:fl>)))
(zen:add-primitive-patterns     'scheme/runtime:atan                                 '(                          (safe %%flatan     <fv:fl>) (safe %%flatan <fv^fv:fl>)))

(zen:add-primitive-patterns     'scheme/runtime:not                                  '((safe %%not  <any:bool>)))
(zen:add-primitive-patterns     'scheme/runtime:eq?                                  '((safe %%eq?  <any^any:bool>)))
(zen:add-primitive-patterns     'scheme/runtime:eqv?                                 '((safe %%eqv? <any^any:bool>)))

(zen:add-primitive-patterns     'scheme/runtime:car                                  '((safe %%car    <pair:any>)))
(zen:add-primitive-patterns     'scheme/runtime:cdr                                  '((safe %%cdr    <pair:any>)))
(zen:add-primitive-patterns     'scheme/runtime:cons                                 '((safe %%cons   <any^any:pair>)))
(zen:add-primitive-patterns     'scheme/runtime:list                                 '((safe %%list   <any*:list>)))
(zen:add-primitive-patterns     'scheme/runtime:length                               '((safe %%length <list:int>) (safe %%vector-length <vector:int>) (safe %%string-length <string:int>)))
(zen:add-primitive-patterns     'scheme/runtime:null?                                '((safe %%null?  <any:bool>)))

(zen:add-primitive-patterns     'scheme/runtime:string                               '((safe          %%string        <char*:string>)))
(zen:add-primitive-patterns     'scheme/runtime:string-length                        '((safe          %%string-length <string:fx>)))
(zen:add-primitive-patterns     'scheme/runtime:string-ref                           '((bounds-unsafe %%string-ref    <string^fx:char>)))

(zen:add-primitive-patterns     'scheme/runtime:vector                               '((safe          %%vector        <any*:vector>)))
(zen:add-primitive-patterns     'scheme/runtime:vector-length                        '((safe          %%vector-length <vector:fx>)))
(zen:add-primitive-patterns     'scheme/runtime:vector-ref                           '((bounds-unsafe %%vector-ref    <vector^fx:any>)))
(zen:add-primitive-patterns     'scheme/runtime:vector-set!                          '((bounds-unsafe %%vector-set!   <vector^fx^any:void>)))

(zen:add-primitive-patterns     'zen/dialect/kernel:values-ref                       '((bounds-unsafe %%values-ref  <values^fx:any>)))
(zen:add-primitive-patterns     'zen/dialect/kernel:values-set!                      '((bounds-unsafe %%values-set! <values^fx^any:void>)))

(zen:add-primitive-patterns     'gambit:table-ref                                    '((safe %%table-ref  <table^any:any>) (safe %%table-ref <table^any^any:any>)))
(zen:add-primitive-patterns     'gambit:table-set!                                   '((safe %%table-set! <table^any^any:void>)))

(zen:add-primitive-patterns     'scheme/runtime:min                                  '((safe %%fxmin <fx^fx:fx>) (safe %%flmin <fv^fv:fl>) (safe %%flmin <fv^fv^fv:fl>)))
(zen:add-primitive-patterns     'scheme/runtime:max                                  '((safe %%fxmax <fx^fx:fx>) (safe %%flmax <fv^fv:fl>) (safe %%flmax <fv^fv^fv:fl>)))
(zen:add-primitive-patterns     'scheme/runtime:modulo                               '((zero-unsafe %%fxmodulo <fx^fx:fx>)))
(zen:add-primitive-patterns     'scheme/runtime:even?                                '((safe %%fxeven? <fx:bool>)))
(zen:add-primitive-patterns     'scheme/runtime:odd?                                 '((safe %%fxodd? <fx:bool>)))

(zen:add-primitive-patterns     'gambit:fx+                                          '((safe %%fx+ <fx^fx:fx>)))
(zen:add-primitive-patterns     'gambit:fx-                                          '((safe %%fx- <fx^fx:fx>)))
(zen:add-primitive-patterns     'gambit:fx*                                          '((safe %%fx* <fx^fx:fx>)))

(zen:add-primitive-patterns     'gambit:fl+                                          '(                     (safe %%fl+ <fv^fv:fl>)))
(zen:add-primitive-patterns     'gambit:fl-                                          '(                     (safe %%fl- <fv^fv:fl>)))
(zen:add-primitive-patterns     'gambit:fl*                                          '(                     (safe %%fl* <fv^fv:fl>)))
(zen:add-primitive-patterns     'gambit:fl/                                          '(                     (safe %%fl/ <fv^fv:fl>)))

(zen:add-primitive-patterns     'gambit:nan?                                         '(                     (safe %%flnan? <fv:bool>)))

(zen:add-primitive-patterns     'gambit:bitwise-not                                  '((safe %%fxnot <fx:fx>)))
(zen:add-primitive-patterns     'gambit:bitwise-and                                  '((safe %%fxand <fx^fx:fx>)))
(zen:add-primitive-patterns     'gambit:bitwise-ior                                  '((safe %%fxior <fx^fx:fx>)))
(zen:add-primitive-patterns     'gambit:bitwise-xor                                  '((safe %%fxxor <fx^fx:fx>)))
(zen:add-primitive-patterns     'gambit:arithmetic-shift                             '((safe %%fxarithmetic-shift <fx^fx:fx>)))
(zen:add-primitive-patterns     'zen/dialect/kernel:arithmetic-shift-left            '((safe %%fxarithmetic-shift-left <fx^fx:fx>)))
(zen:add-primitive-patterns     'zen/dialect/kernel:arithmetic-shift-right           '((safe %%fxarithmetic-shift-right <fx^fx:fx>)))

(zen:add-primitive-patterns     'zen/dialect/kernel:fixnum->flonum                   '((safe %%fixnum->flonum <fx:fl>)))
(zen:add-primitive-patterns     'zen/dialect/kernel:flonum->fixnum                   '((safe %%flonum->fixnum <fv:fx>)))

(zen:add-primitive-patterns     'gambit:s8vector                                     '((safe          %%s8vector         <fx*:s8vector>)))
(zen:add-primitive-patterns     'gambit:s8vector-length                              '((safe          %%s8vector-length  <s8vector:fx>)))
(zen:add-primitive-patterns     'gambit:s8vector-ref                                 '((bounds-unsafe %%s8vector-ref     <s8vector^fx:fx>)))
(zen:add-primitive-patterns     'gambit:s8vector-set!                                '((bounds-unsafe %%s8vector-set!    <s8vector^fx^any:void>)))

(zen:add-primitive-patterns     'gambit:u8vector                                     '((safe          %%u8vector         <fx*:u8vector>)))
(zen:add-primitive-patterns     'gambit:u8vector-length                              '((safe          %%u8vector-length  <u8vector:fx>)))
(zen:add-primitive-patterns     'gambit:u8vector-ref                                 '((bounds-unsafe %%u8vector-ref     <u8vector^fx:fx>)))
(zen:add-primitive-patterns     'gambit:u8vector-set!                                '((bounds-unsafe %%u8vector-set!    <u8vector^fx^any:void>)))

(zen:add-primitive-patterns     'gambit:s16vector                                    '((safe          %%s16vector        <fx*:s16vector>)))
(zen:add-primitive-patterns     'gambit:s16vector-length                             '((safe          %%s16vector-length <s16vector:fx>)))
(zen:add-primitive-patterns     'gambit:s16vector-ref                                '((bounds-unsafe %%s16vector-ref    <s16vector^fx:fx>)))
(zen:add-primitive-patterns     'gambit:s16vector-set!                               '((bounds-unsafe %%s16vector-set!   <s16vector^fx^any:void>)))

(zen:add-primitive-patterns     'gambit:u16vector                                    '((safe          %%u16vector        <fx*:u16vector>)))
(zen:add-primitive-patterns     'gambit:u16vector-length                             '((safe          %%u16vector-length <u16vector:fx>)))
(zen:add-primitive-patterns     'gambit:u16vector-ref                                '((bounds-unsafe %%u16vector-ref    <u16vector^fx:fx>)))
(zen:add-primitive-patterns     'gambit:u16vector-set!                               '((bounds-unsafe %%u16vector-set!   <u16vector^fx^any:void>)))

(zen:add-primitive-patterns     'gambit:s32vector                                    '((safe          %%s32vector        <fx*:s32vector>)))
(zen:add-primitive-patterns     'gambit:s32vector-length                             '((safe          %%s32vector-length <s32vector:fx>)))
(zen:add-primitive-patterns     'gambit:s32vector-ref                                '((bounds-unsafe %%s32vector-ref    <s32vector^fx:fx>)))
(zen:add-primitive-patterns     'gambit:s32vector-set!                               '((bounds-unsafe %%s32vector-set!   <s32vector^fx^any:void>)))

(zen:add-primitive-patterns     'gambit:u32vector                                    '((safe          %%u32vector        <fx*:u32vector>)))
(zen:add-primitive-patterns     'gambit:u32vector-length                             '((safe          %%u32vector-length <u32vector:fx>)))
(zen:add-primitive-patterns     'gambit:u32vector-ref                                '((bounds-unsafe %%u32vector-ref    <u32vector^fx:fx>)))
(zen:add-primitive-patterns     'gambit:u32vector-set!                               '((bounds-unsafe %%u32vector-set!   <u32vector^fx^any:void>)))

(zen:add-primitive-patterns     'gambit:s64vector                                    '((safe          %%s64vector        <fx*:s64vector>)))
(zen:add-primitive-patterns     'gambit:s64vector-length                             '((safe          %%s64vector-length <s64vector:fx>)))
(zen:add-primitive-patterns     'gambit:s64vector-ref                                '((bounds-unsafe %%s64vector-ref    <s64vector^fx:fx>)))
(zen:add-primitive-patterns     'gambit:s64vector-set!                               '((bounds-unsafe %%s64vector-set!   <s64vector^fx^any:void>)))

(zen:add-primitive-patterns     'gambit:u64vector                                    '((safe          %%u64vector        <fx*:u64vector>)))
(zen:add-primitive-patterns     'gambit:u64vector-length                             '((safe          %%u64vector-length <u64vector:fx>)))
(zen:add-primitive-patterns     'gambit:u64vector-ref                                '((bounds-unsafe %%u64vector-ref    <u64vector^fx:fx>)))
(zen:add-primitive-patterns     'gambit:u64vector-set!                               '((bounds-unsafe %%u64vector-set!   <u64vector^fx^any:void>)))

(zen:add-primitive-patterns     'gambit:f32vector                                    '((safe          %%f32vector        <fl*:f32vector>)))
(zen:add-primitive-patterns     'gambit:f32vector-length                             '((safe          %%f32vector-length <f32vector:fx>)))
(zen:add-primitive-patterns     'gambit:f32vector-ref                                '((bounds-unsafe %%f32vector-ref    <f32vector^fx:fl>)))
(zen:add-primitive-patterns     'gambit:f32vector-set!                               '((bounds-unsafe %%f32vector-set!   <f32vector^fx^any:void>)))

(zen:add-primitive-patterns     'gambit:f64vector                                    '((safe          %%f64vector        <fl*:f64vector>)))
(zen:add-primitive-patterns     'gambit:f64vector-length                             '((safe          %%f64vector-length <f64vector:fx>)))
(zen:add-primitive-patterns     'gambit:f64vector-ref                                '((bounds-unsafe %%f64vector-ref    <f64vector^fx:fl>)))
(zen:add-primitive-patterns     'gambit:f64vector-set!                               '((bounds-unsafe %%f64vector-set!   <f64vector^fx^any:void>)))

;; use at your own risk versions that do not initialize memory
(zen:add-primitive-patterns     'zen/language/runtime:allocate-s8vector              '((safe %%allocate-s8vector  <any*:s8vector>)))
(zen:add-primitive-patterns     'zen/language/runtime:allocate-u8vector              '((safe %%allocate-u8vector  <any*:u8vector>)))
(zen:add-primitive-patterns     'zen/language/runtime:allocate-s16vector             '((safe %%allocate-s16vector <any*:s16vector>)))
(zen:add-primitive-patterns     'zen/language/runtime:allocate-u16vector             '((safe %%allocate-u16vector <any*:u16vector>)))
(zen:add-primitive-patterns     'zen/language/runtime:allocate-s32vector             '((safe %%allocate-s32vector <any*:s32vector>)))
(zen:add-primitive-patterns     'zen/language/runtime:allocate-u32vector             '((safe %%allocate-u32vector <any*:u32vector>)))
(zen:add-primitive-patterns     'zen/language/runtime:allocate-s64vector             '((safe %%allocate-s64vector <any*:s64vector>)))
(zen:add-primitive-patterns     'zen/language/runtime:allocate-u64vector             '((safe %%allocate-u64vector <any*:u64vector>)))
(zen:add-primitive-patterns     'zen/language/runtime:allocate-f32vector             '((safe %%allocate-f32vector <any*:f32vector>)))
(zen:add-primitive-patterns     'zen/language/runtime:allocate-f64vector             '((safe %%allocate-f64vector <any*:f64vector>)))
(zen:add-primitive-patterns     'zen/language/runtime:allocate-vector                '((safe %%allocate-vector    <any*:vector>)))

;; tracking allocations
(zen:add-primitive-patterns     'scheme/runtime:make-string                          '((safe %%make-string    <any*:string>)))
(zen:add-primitive-patterns     'scheme/runtime:make-vector                          '((safe %%make-vector    <any*:vector>)))
(zen:add-primitive-patterns     'gambit:make-s8vector                                '((safe %%make-s8vector  <any*:s8vector>)))
(zen:add-primitive-patterns     'gambit:make-u8vector                                '((safe %%make-u8vector  <any*:u8vector>)))
(zen:add-primitive-patterns     'gambit:make-s16vector                               '((safe %%make-s16vector <any*:s16vector>)))
(zen:add-primitive-patterns     'gambit:make-u16vector                               '((safe %%make-u16vector <any*:u16vector>)))
(zen:add-primitive-patterns     'gambit:make-s32vector                               '((safe %%make-s32vector <any*:s32vector>)))
(zen:add-primitive-patterns     'gambit:make-u32vector                               '((safe %%make-u32vector <any*:u32vector>)))
(zen:add-primitive-patterns     'gambit:make-s64vector                               '((safe %%make-s64vector <any*:s64vector>)))
(zen:add-primitive-patterns     'gambit:make-u64vector                               '((safe %%make-u64vector <any*:u64vector>)))
(zen:add-primitive-patterns     'gambit:make-f32vector                               '((safe %%make-f32vector <any*:f32vector>)))
(zen:add-primitive-patterns     'gambit:make-f64vector                               '((safe %%make-f64vector <any*:f64vector>)))
(zen:add-primitive-patterns     'gambit:make-table                                   '((safe %%make-table     <any*:table>)))
(zen:add-primitive-patterns     'gambit:make-condition-variable                      '((safe %%make-condition-variable <any*:any>)))
(zen:add-primitive-patterns     'gambit:make-mutex                                   '((safe %%make-mutex     <any*:any>)))
(zen:add-primitive-patterns     'gambit:make-thread                                  '((safe %%make-thread    <any*:any>)))
(zen:add-primitive-patterns     'gambit:make-will                                    '((safe %%make-will      <any*:any>)))
(zen:add-primitive-patterns     'gambit:make-parameter                               '((safe %%make-parameter <any*:any>)))

(zen:add-primitive-patterns     'gambit:subs8vector                                  '((safe %%subs8vector <any*:s8vector>)))
(zen:add-primitive-patterns     'gambit:subu8vector                                  '((safe %%subu8vector <any*:u8vector>)))
(zen:add-primitive-patterns     'gambit:subs16vector                                 '((safe %%subs16vector <any*:s16vector>)))
(zen:add-primitive-patterns     'gambit:subu16vector                                 '((safe %%subu16vector <any*:u16vector>)))
(zen:add-primitive-patterns     'gambit:subs32vector                                 '((safe %%subs32vector <any*:s32vector>)))
(zen:add-primitive-patterns     'gambit:subu32vector                                 '((safe %%subu32vector <any*:u32vector>)))
(zen:add-primitive-patterns     'gambit:subs64vector                                 '((safe %%subs64vector <any*:s64vector>)))
(zen:add-primitive-patterns     'gambit:subu64vector                                 '((safe %%subu64vector <any*:u64vector>)))
(zen:add-primitive-patterns     'gambit:subf32vector                                 '((safe %%subf32vector <any*:f32vector>)))
(zen:add-primitive-patterns     'gambit:subf64vector                                 '((safe %%subf64vector <any*:f64vector>)))

(zen:add-primitive-patterns     'zen/dialect/kernel:flref                           '((safe %%flref <fv^fx:fl>)))
(zen:add-primitive-patterns     'zen/dialect/kernel:flset!                          '((safe %%flset! <fv^fx^fv:void>)))

(%%when (%%not zen:debug-user?)
  (zen:add-primitive-patterns   'zen/language/runtime:element                       '((safe list-ref <list^int:any>) (bounds-unsafe %%vector-ref    <vector^int:any>)      (bounds-unsafe %%string-ref    <string^int:char>)))
  (zen:add-primitive-patterns   'zen/language/runtime:set-element!                  '(                               (bounds-unsafe %%vector-set!   <vector^int^any:void>) (bounds-unsafe %%string-set!   <string^int^char:void>))))

(zen:add-primitive-patterns     'zen/language/runtime:between?                      '((safe %%fxbetween? <fx^fx^fx:bool>) (safe %%flbetween? <fl^fl^fl:bool>)))

(zen:add-primitive-patterns     'zen/dialect/kernel:current-seconds!                '((safe %%get-current-time! <f64vector^fx:void>)))
(zen:add-primitive-patterns     'zen/dialect/kernel:bytes-allocated!                '((safe %%get-bytes-allocated! <f64vector^fx:void>)))


(define (zen:primitive-call-emit operator locator arguments arguments-codes declaration walker resume environment)
  (if (%%not locator)
      #f
    (let ((patterns (zen:get-primitive-patterns locator)))
      (if (%%not patterns)
          #f
        (let ((zero-unsafe? (%%not (zen:get-check? 'zero)))
              (bounds-unsafe? (%%not (zen:get-check? 'bounds)))
              (types (zen:codes-types arguments-codes)))
          (let iter ((scan patterns) (least-mismatch #f))
               (if (%%null? scan)
                   (begin
                     (%%when (and (or (zen:reporting?) (zen:warnings?)) (zen:get-warn? 'optimizations)
                                  ;; a bit extreme for now
                                  (%%not (%%memq locator '(scheme/runtime:car
                                                           scheme/runtime:cdr))))
                       (let ((expression (and (%%pair? least-mismatch) (%%car least-mismatch))))
                         (zen:warning "Warning: In {a}{a}: Unmatched call to primitive {a}"
                                       (zen:get-declaration-locator declaration)
                                       (zen:present-expression-location (and expression (zen:get-expression-source expression)) (zen:get-expression-source operator))
                                       (zen:reference-name locator))))
                     #f)
                 (zen:bind (safety primitive function-type) (%%car scan)
                   (let ((mismatch (zen:signature-mismatch arguments types function-type)))
                     (if (and (%%not mismatch)
                              (or (%%eq? safety 'safe)
                                  (and (%%eq? safety 'zero-unsafe) (or zero-unsafe?
                                                                       ;; at the moment the divisor is always the last argument
                                                                       (let ((divisor (zen:last arguments)))
                                                                         (and (%%class-is? divisor zen:Constant)
                                                                              (let ((value (zen:source-code (zen:get-constant-expansion divisor))))
                                                                                (and (%%fixnum? value)
                                                                                     (%%not (%%fx= value 0))))))))
                                  (and (%%eq? safety 'bounds-unsafe)
                                       (or bounds-unsafe?
                                           (let ((fixed-safe?
                                                   (let ((fixed-metaclass (zen:class->fixed-metaclass (%%car (zen:get-function-type-positional function-type)))))
                                                     (and fixed-metaclass
                                                          (let ((type (%%car types))
                                                                (arg (%%cadr arguments)))
                                                            (and (%%is? type fixed-metaclass)
                                                                 (let ((size (%%get-class-user-data type)))
                                                                   (and (%%class-is? arg zen:Constant)
                                                                        (let ((index (zen:source-code (zen:get-constant-expansion arg))))
                                                                          (and (%%fixnum? index)
                                                                               (%%fx< index size)))))))))))
                                             (%%when (and (%%not fixed-safe?) (or (zen:reporting?) (zen:warnings?)) (zen:get-warn? 'optimizations))
                                               (let ((expression (or (zen:call-being-inlined) (and (%%pair? least-mismatch) (%%car least-mismatch)))))
                                                 (zen:warning "Warning: In {a}{a}: Bounds checked call to primitive {a}"
                                                               (zen:get-declaration-locator declaration)
                                                               (zen:present-expression-location (and expression (zen:get-expression-source expression)) (zen:get-expression-source operator))
                                                               (zen:reference-name locator))))
                                             fixed-safe?)))))
                         (zen:new-code
                           `(,primitive ,@(zen:codes-forms arguments-codes))
                           (let ((class (zen:constructor->fixed-class locator)))
                             (if class
                                 (zen:new-fixed-type class (%%length arguments))
                               (let ((class (zen:maker->fixed-class locator)))
                                 (if class
                                     (let ((arg (%%car arguments)))
                                       (if (%%class-is? arg zen:Constant)
                                           (let ((card (zen:source-code (zen:get-constant-expansion arg))))
                                             (if (%%fixnum? card)
                                                 (zen:new-fixed-type class card)
                                               (zen:get-function-type-result function-type)))
                                         (zen:get-function-type-result function-type)))
                                   (zen:get-function-type-result function-type)))))
                           #f)
                       (iter (%%cdr scan) (if (or (%%not least-mismatch)
                                                  (%%symbol? least-mismatch)
                                                  (and (%%pair? mismatch)
                                                       (%%fx< (%%length mismatch) (%%length least-mismatch))))
                                              mismatch
                                            least-mismatch))))))))))))


;;;
;;;; Inlined Call
;;;


(zen:define-variable-override zen:emit-inlined-call
  (lambda (operator arguments call declaration walker resume environment)
    (if (%%class-is? operator zen:Binding-Reference)
        (let ((binding (zen:get-binding-reference-binding operator)))
          (zen:emit-inlined-binding-call binding arguments call declaration walker resume environment))
      #f)))


;;;
;;;; Signature
;;;


;; #f -> no mismatch
;; #t -> mismatch at the call level
;; (arg ...) -> mismatched arguments
(define (zen:signature-mismatch arguments argument-types function-type #!optional (implicit? #f))
  (let ((argcount (%%length argument-types))
        (mandatory (zen:get-function-type-mandatory function-type))
        (positional (zen:get-function-type-positional function-type))
        (optional (zen:get-function-type-optional function-type))
        (named (zen:get-function-type-named function-type))
        (rest (zen:get-function-type-rest function-type)))
    (define (match? arg type expect)
      (let ((type (zen:resolve-type-safe type)))
        (if (%%class-is? expect zen:Category-Type)
            (or (and (%%class-is? arg zen:Binding-Reference)
                     (%%eq? (zen:get-binding-reference-binding arg) (zen:get-category-type-declaration expect)))
                (and (%%class-is? type zen:Category-Type)
                     (%%eq? (zen:get-category-type-declaration type) (zen:get-category-type-declaration expect))))
          (let ((type (or type zen:Any)))
            (cond ;; bool
                  ((%%eq? expect zen:Bool)
                   #t)
                  ;; flonum
                  ((%%eq? expect zen:Flonum)
                   (or (%%subtype? type zen:Flonum)
                       (and implicit? (%%subtype? type zen:Fixnum))
                       (and implicit? (%%subtype? type zen:Ratnum))))
                  ;; flovec
                  ((%%eq? expect zen:Flovec)
                   (or (%%subtype? type zen:Flonum)
                       (%%subtype? type zen:F64Vector)
                       (and implicit? (%%subtype? type zen:Fixnum))))
                  (else
                   (%%subtype? type expect)))))))
    
    (define (positional-mismatch)
      (let iter ((args arguments)
                 (types argument-types)
                 (expected positional)
                 (mismatched '()))
           (if (%%null? expected)
               (if (%%null? mismatched)
                   #f
                 (zen:reverse! mismatched))
             (let ((match? (match? (%%car args) (%%car types) (%%car expected))))
               (iter (%%cdr args)
                     (%%cdr types)
                     (%%cdr expected)
                     (if match? mismatched (%%cons (%%car args) mismatched)))))))
    
    (define (rest-mismatch)
      (let iter ((args (list-tail arguments mandatory))
                 (types (list-tail argument-types mandatory))
                 (expected (zen:get-rest-type-type rest))
                 (mismatched '()))
           (if (%%null? args)
               (if (%%null? mismatched)
                   #f
                 (zen:reverse! mismatched))
             (let ((match? (match? (%%car args) (%%car types) expected)))
               (iter (%%cdr args)
                     (%%cdr types)
                     expected
                     (if match? mismatched (%%cons (%%car args) mismatched)))))))
    
    (cond ((%%fx< argcount mandatory)
           'not-enough-arguments)
          ((%%fx= argcount mandatory)
           (positional-mismatch))
          ((and (%%null? optional)
                (%%null? named)
                rest)
           (let ((positional-mismatch (positional-mismatch))
                 (rest-mismatch (rest-mismatch)))
             (if (and (%%not positional-mismatch)
                      (%%not rest-mismatch))
                 #f
               (%%append (or positional-mismatch '())
                         (or rest-mismatch '())))))
          (else
           'not-only-positional))))


;;;
;;;; If
;;;


(zen:define-class zen:If zen:Expression (constructor: zen:allocate-if)
  ((test getter: generate)
   (yes  getter: generate)
   (no   getter: generate)))


(define (zen:new-if source test yes no)
  (zen:allocate-if #f source test yes no))


(define zen:primitive-predicates
  (%%make-table test: eq?))


(define (zen:primitive-predicates-get)
  zen:primitive-predicates)


(define (zen:add-primitive-predicate name class)
  (%%table-set! zen:primitive-predicates name class))


(zen:add-primitive-predicate 'scheme/runtime:number?                      zen:Number)
(zen:add-primitive-predicate 'scheme/runtime:complex?                     zen:Complex)
(zen:add-primitive-predicate 'scheme/runtime:real?                        zen:Real)
(zen:add-primitive-predicate 'scheme/runtime:rational?                    zen:Rational)
(zen:add-primitive-predicate 'scheme/runtime:integer?                     zen:Integer)
(zen:add-primitive-predicate 'scheme/runtime:number?                      zen:Number)
(zen:add-primitive-predicate 'scheme/runtime:boolean?                     zen:Boolean)
;; not 100% correct because of Scheme's semantic for list? that is costly
(zen:add-primitive-predicate 'scheme/runtime:list?                        zen:List)
(zen:add-primitive-predicate 'scheme/runtime:null?                        zen:Null)
(zen:add-primitive-predicate 'scheme/runtime:pair?                        zen:Pair)
(zen:add-primitive-predicate 'scheme/runtime:symbol?                      zen:Symbol)
(zen:add-primitive-predicate 'scheme/runtime:char?                        zen:Char)
(zen:add-primitive-predicate 'scheme/runtime:string?                      zen:String)
(zen:add-primitive-predicate 'scheme/runtime:vector?                      zen:Vector)
(zen:add-primitive-predicate 'gambit:s8vector?                            zen:S8Vector)
(zen:add-primitive-predicate 'gambit:u8vector?                            zen:U8Vector)
(zen:add-primitive-predicate 'gambit:s16vector?                           zen:S16Vector)
(zen:add-primitive-predicate 'gambit:u16vector?                           zen:U16Vector)
(zen:add-primitive-predicate 'gambit:s32vector?                           zen:S32Vector)
(zen:add-primitive-predicate 'gambit:u32vector?                           zen:U32Vector)
(zen:add-primitive-predicate 'gambit:s64vector?                           zen:S64Vector)
(zen:add-primitive-predicate 'gambit:u64vector?                           zen:U64Vector)
(zen:add-primitive-predicate 'gambit:f32vector?                           zen:F32Vector)
(zen:add-primitive-predicate 'gambit:f64vector?                           zen:F64Vector)
(zen:add-primitive-predicate 'gambit:continuation?                        zen:Continuation)
(zen:add-primitive-predicate 'scheme/runtime:procedure?                   zen:Procedure)
(zen:add-primitive-predicate 'scheme/runtime:input-port?                  zen:Port)
(zen:add-primitive-predicate 'scheme/runtime:output-port?                 zen:Port)
(zen:add-primitive-predicate 'scheme/runtime:eof-object?                  zen:EOF)
(zen:add-primitive-predicate 'gambit:fixnum?                              zen:Fixnum)
(zen:add-primitive-predicate 'gambit:flonum?                              zen:Flonum)
(zen:add-primitive-predicate 'gambit:keyword?                             zen:Keyword)
(zen:add-primitive-predicate 'zen/dialect/kernel:object?                  zen:Object)
(zen:add-primitive-predicate 'zen/dialect/kernel:category?                zen:Category)
(zen:add-primitive-predicate 'zen/language/runtime:class?                 zen:Class)
(zen:add-primitive-predicate 'zen/dialect/kernel:interface?               zen:Interface)
(zen:add-primitive-predicate 'zen/dialect/kernel:field?                   zen:Field)
(zen:add-primitive-predicate 'zen/dialect/kernel:slot?                    zen:Slot)
(zen:add-primitive-predicate 'zen/dialect/kernel:method?                  zen:Method)
(zen:add-primitive-predicate 'gambit:table?                               zen:Table)
(zen:add-primitive-predicate 'gambit:thread?                              zen:Thread)
(zen:add-primitive-predicate 'gambit:foreign?                             zen:Foreign)
(zen:add-primitive-predicate 'zen/dialect/kernel:values?                  zen:Values)
(zen:add-primitive-predicate 'zen/dialect/kernel:unspecified?             zen:Unspecified)


(define zen:not-primitive-predicates
  (%%make-table test: eq?))


(%%table-set! zen:not-primitive-predicates 'zen/dialect/kernel:not-null? zen:Null)


(define (zen:restrict-type base type)
  (zen:new-restriction-type base type))


(define (zen:restriction-of? type class)
  (and (%%class-is? type zen:Restriction-Type)
       (%%class-is? (zen:get-restriction-type-type type) class)))


(define (zen:complement-type base type)
  (if (and (zen:restriction-of? type zen:Complement-Type)
           (%%eq? (zen:get-restriction-type-base type) base))
      (zen:get-complement-type-type (zen:get-restriction-type-type type))
    (zen:new-restriction-type base (zen:new-complement-type type))))


(define (zen:branch-types test environment)
  (define (process-not expr env)
    (revenv (process-expr expr env)))
  
  (define (process-and expr-list env)
    (if (and (%%not-null? expr-list)
             (%%null? (%%cdr expr-list)))
        (process-expr (%%car expr-list) env)
      (let iter ((scan expr-list) (augmented env))
        (if (%%null? scan)
            (%%cons (%%car augmented) (%%cdr env))
          (let ((expr (%%car scan)))
            (let ((expr-env (process-expr expr augmented)))
              (let ((yes (%%car expr-env)))
                (iter (%%cdr scan) (%%cons yes yes)))))))))
  
  (define (process-or expr-list env)
    (if (and (%%not-null? expr-list)
             (%%null? (%%cdr expr-list)))
        (process-expr (%%car expr-list) env)
      (let iter ((scan expr-list) (augmented env))
        (if (%%null? scan)
            (%%cons (%%car env) (%%cdr augmented))
          (let ((expr (%%car scan)))
            (let ((expr-env (process-expr expr augmented)))
              (let ((no (%%cdr expr-env)))
                (iter (%%cdr scan) (%%cons no no)))))))))
  
  (define (process-is expr type-expr env)
    (receive (origin actual-type) (extract-binding expr env)
      (if origin
          (let ((yes-type (cond ((zen:type? type-expr)
                                 type-expr)
                                ((%%class-is? type-expr zen:Binding-Reference)
                                 (let ((binding (zen:get-binding-reference-binding type-expr)))
                                   (if (%%class-is? binding zen:Declaration)
                                       (zen:resolve-binding binding)
                                     #f)))
                                (else
                                 #f))))
            ;; quick try for fun
            (let ((no-type
                    (if (%%eq? actual-type zen:List)
                        (cond ((%%eq? yes-type zen:Null)
                               zen:Pair)
                              ((%%eq? yes-type zen:Pair)
                               zen:Null)
                              (else
                               #f))
                      #f)))
              (let ((yes
                      (if yes-type
                          (%%cons (zen:new-annotated-frame (%%list (zen:new-restricted-binding origin yes-type)) #f) (%%car env))
                        (%%car env)))
                    (no
                      (if no-type
                          (%%cons (zen:new-annotated-frame (%%list (zen:new-restricted-binding origin no-type)) #f) (%%cdr env))
                        (%%cdr env))))
                (%%cons yes no))))
        env)))
  
  (define (extract-binding expr env)
    (if (%%class-is? expr zen:Binding-Reference)
        (let ((binding (zen:get-binding-reference-binding expr)))
          (cond ((%%class-is? binding zen:Variable)
                 (receive (frame actual-variable actual-type) (zen:find-annotated binding (%%car env))
                   (let ((origin (zen:get-annotated-variable-variable actual-variable)))
                     (values origin actual-type))))
                ;; this is really for slots so i need to think about this
                ((%%class-is? binding zen:Declaration)
                 (values binding (zen:get-lexical-binding-type binding)))
                (else
                 (values #f #f))))
      (values #f #f)))
  
  (define (revenv env)
    (%%cons (%%cdr env) (%%car env)))
  
  (define (process-expr expr env)
    (cond ((%%class-is? expr zen:And)
           (process-and (zen:get-and-expressions expr) env))
          ((%%class-is? expr zen:Or)
           (process-or (zen:get-or-expressions expr) env))
          ((%%class-is? expr zen:Call)
           (let ((operator (zen:get-call-operator expr)))
             (if (%%class-is? operator zen:Binding-Reference)
                 (let ((operator-binding (zen:get-binding-reference-binding operator)))
                   (if (%%class-is? operator-binding zen:Declaration)
                       (let ((operator-locator (zen:get-declaration-locator operator-binding))
                             (arguments (zen:get-call-arguments expr)))
                         (let ((count (%%length arguments)))
                           (case operator-locator
                             ((scheme/runtime:not)
                              (if (%%fx= count 1)
                                  (process-not (%%car arguments) env)
                                env))
                             ((zen/dialect/kernel:is?)
                              (if (%%fx= count 2)
                                  (process-is (%%car arguments) (%%cadr arguments) env)
                                env))
                             ((zen/language/runtime:is-not?)
                              (if (%%fx= count 2)
                                  (revenv (process-is (%%car arguments) (%%cadr arguments) env))
                                env))
                             (else
                              (if (%%fx= count 1)
                                  (let ((class (%%table-ref zen:primitive-predicates operator-locator #f)))
                                    (if class
                                        (process-is (%%car arguments) class env)
                                      (let ((class (%%table-ref zen:not-primitive-predicates operator-locator #f)))
                                        (if class
                                            (revenv (process-is (%%car arguments) class env))
                                          env))))
                                env)))))
                     env))
               env)))
          (else
           (receive (origin actual-type) (extract-binding expr env)
             (if origin
                 (if (%%class-is? actual-type zen:Nillable-Type)
                     (let ((yes (%%cons (zen:new-annotated-frame (%%list (zen:new-restricted-binding origin (zen:get-nillable-type-type actual-type))) #f) (%%car env)))
                           (no (%%cdr env)))
                       (%%cons yes no))
                   env)
               env)))))
  
  (process-expr test (%%cons environment environment)))


(zen:define-method (zen:emit-expression (zen:If expression) declaration walker resume environment)
  (let ((test (zen:get-if-test expression))
        (yes (zen:get-if-yes expression))
        (no (zen:get-if-no expression)))
    (zen:bind (yes-environment . no-environment) (zen:branch-types test environment)
      (let ((test (zen:emit-expression test declaration walker resume environment))
            (yes (zen:emit-expression yes declaration walker resume yes-environment))
            (no (and no (zen:emit-expression no declaration walker resume no-environment))))
        (zen:new-code
          `(if ,(zen:sourcified-form test)
               ,(zen:sourcified-form yes)
             ,@(if no
                   (%%list (zen:simplify-begin (zen:sourcified-form no)))
                 '()))
          (zen:extend-type (zen:get-code-type yes) (and no (zen:get-code-type no)))
          (zen:get-expression-source expression))))))


(zen:define-method (zen:tree-fold (zen:If expression) down up here seed environment)
  (let ((test (zen:get-if-test expression))
        (yes (zen:get-if-yes expression))
        (no (zen:get-if-no expression)))
    (up expression
        seed
        (let ((fold (zen:tree-fold
                      yes down up here
                      (zen:tree-fold
                        test down up here (down expression seed environment) environment)
                      environment)))
          (if (%%not no)
              fold
            (zen:tree-fold
              no down up here
              fold
              environment)))
        environment)))


;;;
;;;; Cond
;;;


(zen:define-class zen:Cond zen:Expression (constructor: zen:allocate-cond)
  ((clauses getter: generate)))


(define (zen:new-cond source clauses)
  (zen:allocate-cond #f source clauses))


(zen:define-method (zen:emit-expression (zen:Cond expression) declaration walker resume environment)
  (let ((clauses (zen:get-cond-clauses expression)))
    (let ((clauses
            (let recurse ((clauses clauses)
                          (environment environment))
                 (if (%%null? clauses)
                     '()
                   (let ((clause (%%car clauses)))
                     (let ((test (%%car clause))
                           (arrow? (%%cadr clause))
                           (body (%%cddr clause)))
                       (zen:bind (yes-environment . no-environment) (zen:branch-types test environment)
                         (let ((test (and test (zen:emit-expression test declaration walker resume environment)))
                               (body (and body (zen:emit-expression body declaration walker resume yes-environment))))
                           (let ((output
                                   `(,(if (%%not test)
                                          'else
                                        (zen:sourcified-form test))
                                     ,@(if arrow?
                                           `(=>)
                                         '())
                                     ,@(if body
                                           (%%list (zen:sourcified-form body))
                                         '())))
                                 (type
                                   (cond (arrow? zen:Any)
                                         (body (zen:get-code-type body))
                                         (test (zen:get-code-type test))
                                         (else zen:Any))))
                             (%%cons (%%cons output type) (recurse (%%cdr clauses) no-environment)))))))))))
      (zen:new-code
        `(cond ,@(map car clauses))
        (zen:extend-types (map cdr clauses))
        (zen:get-expression-source expression)))))


(zen:define-method (zen:tree-fold (zen:Cond expression) down up here seed environment)
  (up expression
      seed
      (let fold ((ls (zen:get-cond-clauses expression))
                 (seed (down expression seed environment)))
           (if (null? ls)
               seed
             (let* ((clause (%%car ls))
                    (test (%%car clause))
                    (body (%%cddr clause))
                    (seed (if body (zen:tree-fold body down up here seed environment) seed)))
               (fold (%%cdr ls)
                     (if (%%not test)
                         body
                       (zen:tree-fold test down up here seed environment))))))
      environment))


;;;
;;;; Case
;;;


(zen:define-class zen:Case zen:Expression (constructor: zen:allocate-case)
  ((target  getter: generate)
   (clauses getter: generate)))


(define (zen:new-case source target clauses)
  (zen:allocate-case #f source target clauses))


(zen:define-method (zen:emit-expression (zen:Case expression) declaration walker resume environment)
  (let ((target (zen:get-case-target expression))
        (clauses (zen:get-case-clauses expression)))
    (let ((target-emit (zen:emit-expression target declaration walker resume environment))
          (clauses-emit (map (lambda (clause)
                               (let ((body (%%cdr clause)))
                                 (zen:emit-expression body declaration walker resume environment)))
                             clauses)))
      (zen:new-code
        `(case ,(zen:sourcified-form target-emit)
           ,@(map (lambda (clause emited-clause)
                    (let ((tries (%%car clause)))
                      `(,tries ,(zen:sourcified-form emited-clause))))
                  clauses
                  clauses-emit))
        (zen:extend-types (map (lambda (emited-clause)
                                  (zen:get-code-type emited-clause))
                                clauses-emit))
        (zen:get-expression-source expression)))))


(zen:define-method (zen:tree-fold (zen:Case expression) down up here seed environment)
  (up expression
      seed
      (zen:tree-fold-list
        (map cdr (zen:get-case-clauses expression)) down up here
        (zen:tree-fold (zen:get-case-target expression) down up here (down expression seed environment) environment)
        environment)
      environment))


;;;
;;;; And
;;;


(zen:define-class zen:And zen:Expression (constructor: zen:allocate-and)
  ((expressions getter: generate)))


(define (zen:new-and source expressions)
  (zen:allocate-and #f source expressions))


(zen:define-method (zen:emit-expression (zen:And expression) declaration walker resume environment)
  (let ((expressions
          (let recurse ((expressions (zen:get-and-expressions expression))
                        (environment environment))
               (if (%%null? expressions)
                   '()
                 (let ((expression (%%car expressions)))
                   (zen:bind (yes-environment . no-environment) (zen:branch-types expression environment)
                     (let ((code (zen:emit-expression expression declaration walker resume environment)))
                       (%%cons code (recurse (%%cdr expressions) yes-environment)))))))))
    (let ((type (cond ((%%null? expressions) zen:Any)
                      ((%%null? (%%cdr expressions)) (zen:get-code-type (%%car expressions)))
                      (else (let ((last-type (zen:get-code-type (zen:last expressions))))
                              (if (%%eq? last-type zen:Any)
                                  zen:Any
                                (zen:new-nillable-type last-type)))))))
      (zen:new-code
        `(and ,@(zen:codes-forms expressions))
        type
        (zen:get-expression-source expression)))))


(zen:define-method (zen:tree-fold (zen:And expression) down up here seed environment)
  (up expression
      seed
      (zen:tree-fold-list
        (zen:get-and-expressions expression) down up here (down expression seed environment) environment)
      environment))


;;;
;;;; Or
;;;


(zen:define-class zen:Or zen:Expression (constructor: zen:allocate-or)
  ((expressions getter: generate)))


(define (zen:new-or source expressions)
  (zen:allocate-or #f source expressions))


(zen:define-method (zen:emit-expression (zen:Or expression) declaration walker resume environment)
  (let ((expressions
          (let recurse ((expressions (zen:get-or-expressions expression))
                        (environment environment))
               (if (%%null? expressions)
                   '()
                 (let ((expression (%%car expressions)))
                   (zen:bind (yes-environment . no-environment) (zen:branch-types expression environment)
                     (let ((code (zen:emit-expression expression declaration walker resume environment)))
                       (%%cons code (recurse (%%cdr expressions) no-environment)))))))))
    (let ((type (cond ((%%null? expressions) zen:Any)
                      ((%%null? (%%cdr expressions)) (zen:get-code-type (%%car expressions)))
                      (else (let ((last-type (zen:get-code-type (zen:last expressions))))
                              (if (zen:every? (lambda (code)
                                                 (let ((type (zen:get-code-type code)))
                                                   (or (%%eq? type last-type)
                                                       (and (%%is? type zen:Nillable-Type)
                                                            (%%eq? (zen:get-nillable-type-type type) last-type)))))
                                               (zen:butlast expressions))
                                  last-type
                                zen:Any))))))
      (zen:new-code
        `(or ,@(zen:codes-forms expressions))
        type
        (zen:get-expression-source expression)))))


(zen:define-method (zen:tree-fold (zen:Or expression) down up here seed environment)
  (up expression
      seed
      (zen:tree-fold-list
        (zen:get-or-expressions expression) down up here (down expression seed environment) environment)
      environment))


;;;
;;;; Declare
;;;


(zen:define-class zen:Declare zen:Expression (constructor: zen:allocate-declare)
  ((declarations getter: generate)))


(define (zen:new-declare declarations)
  (zen:allocate-declare #f #f declarations))


(zen:define-method (zen:emit-expression (zen:Declare expression) declaration walker resume environment)
  (zen:new-code
    (let ((declarations (zen:get-declare-declarations expression)))
      `(declare ,@declarations))
    zen:Any
    #f))


(zen:define-variable-override zen:declare-class
  zen:Declare)


;;;
;;;; Parameterize
;;;


(zen:define-class zen:Parameterize zen:Expression (constructor: zen:allocate-parameterize)
  ((bindings getter: generate)
   (body     getter: generate)))


(define (zen:new-parameterize bindings body)
  (zen:allocate-parameterize #f #f bindings body))


(zen:define-method (zen:emit-expression (zen:Parameterize expression) declaration walker resume environment)
  (let ((body (zen:get-parameterize-body expression)))
    (let ((body-code (zen:emit-expression body declaration walker resume environment)))
      (zen:new-code
        (let ((bindings (zen:get-parameterize-bindings expression)))
          `(parameterize ,(map (lambda (binding)
                                 (let ((variable (%%car binding))
                                       (value (%%cdr binding)))
                                   `(,(zen:sourcified-form (zen:emit-expression variable declaration walker resume environment))
                                     ,(zen:sourcified-form (zen:emit-expression value declaration walker resume environment)))))
                               bindings)
             ,@(zen:sourcified-form body-code)))
        (zen:get-code-type body-code)
        #f))))


(zen:define-method (zen:tree-fold (zen:Parameterize expression) down up here seed environment)
  (let ((seed2 (zen:tree-fold-list (map cdr (zen:get-parameterize-bindings expression)) down up here (down expression seed environment) environment)))
    (up expression
        seed
        (zen:tree-fold (zen:get-parameterize-body expression) down up here seed2 environment)
        environment))))
