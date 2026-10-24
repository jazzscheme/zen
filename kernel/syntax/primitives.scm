;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Primitives
;;;


(block kernel.primitives


(declare (not safe))


;;;
;;;; Boolean
;;;


(zen:define-synto (%%boolean? obj)
  (if zen:debug-core?
      `(boolean? ,obj)
    `(^#boolean? ,obj)))

(zen:define-synto (%%not expr)
  (if zen:debug-core?
      `(not ,expr)
    `(^#not ,expr)))


;;;
;;;; Box
;;;


(zen:define-synto (%%box? obj)
  (if zen:debug-core?
      `(box? ,obj)
    `(^#box? ,obj)))

(zen:define-synto (%%box obj)
  (if zen:debug-core?
      `(box ,obj)
    `(^#box ,obj)))

(zen:define-synto (%%unbox box)
  (if zen:debug-core?
      `(unbox ,box)
    `(^#unbox ,box)))

(zen:define-synto (%%set-box! box val)
  (if zen:debug-core?
      `(set-box! ,box ,val)
    `(^#set-box! ,box ,val)))


;;;
;;;; Char
;;;


(zen:define-synto (%%char? obj)
  (if zen:debug-core?
      `(char? ,obj)
    `(^#char? ,obj)))

(zen:define-synto (%%char=? c1 c2)
  (if zen:debug-core?
      `(char=? ,c1 ,c2)
    `(^#char=? ,c1 ,c2)))

(zen:define-synto (%%char<=? c1 c2)
  (if zen:debug-core?
      `(char<=? ,c1 ,c2)
    `(^#char<=? ,c1 ,c2)))


;;;
;;;; Closure
;;;


(zen:define-synto (%%closure? obj)
  `(^#closure? ,obj))

(zen:define-synto (%%closure-code closure)
  (%%force-uniqueness (closure)
    `(%%check-closure ,closure 1 (%%closure-code ,closure)
       (^#closure-code ,closure))))

(zen:define-synto (%%closure-length closure)
  (%%force-uniqueness (closure)
    `(%%check-closure ,closure 1 (%%closure-length ,closure)
       (^#closure-length ,closure))))

(zen:define-synto (%%closure-ref closure n)
  (%%force-uniqueness (closure)
    `(%%check-closure ,closure 1 (%%closure-ref ,closure ,n)
       (%%check-fixnum ,n 2 (%%closure-ref ,closure ,n)
         (^#closure-ref ,closure ,n)))))


;;;
;;;; Complex
;;;


(zen:define-synto (%%complex? obj)
  (if zen:debug-core?
      `(complex? ,obj)
    `(^#complex? ,obj)))


;;;
;;;; Container
;;;


(zen:define-synto (%%path->container-hook-set! hook)
  (%%force-uniqueness (hook)
    `(%%check-procedure ,hook 1 (%%path->container-hook-set! ,hook)
       (^#path->container-hook-set! ,hook))))

(zen:define-synto (%%container->path-hook-set! hook)
  (%%force-uniqueness (hook)
    `(%%check-procedure ,hook 1 (%%container->path-hook-set! ,hook)
       (^#container->path-hook-set! ,hook))))

(zen:define-synto (%%container->id-hook-set! hook)
  (%%force-uniqueness (hook)
    `(%%check-procedure ,hook 1 (%%container->id-hook-set! ,hook)
       (^#container->id-hook-set! ,hook))))


;;;
;;;; Continuation
;;;


(zen:define-synto (%%continuation? obj)
  (if zen:debug-core?
      `(continuation? ,obj)
    `(^#continuation? ,obj)))

(zen:define-synto (%%continuation-capture proc)
  (if zen:debug-core?
      `(continuation-capture ,proc)
    `(^#continuation-capture ,proc)))

(zen:define-synto (%%continuation-graft cont proc)
  (if zen:debug-core?
      `(continuation-graft ,cont ,proc)
    `(^#continuation-graft ,cont ,proc)))

(zen:define-synto (%%continuation-return cont . values)
  (if zen:debug-core?
      `(continuation-return ,cont ,@values)
    `(^#continuation-return ,cont ,@values)))

(zen:define-synto (%%continuation-graft-no-winding cont values)
  (%%force-uniqueness (cont values)
    `(%%check-continuation ,cont 1 (%%continuation-graft-no-winding ,cont ,values)
       (^#continuation-graft-no-winding ,cont ,values))))

(zen:define-synto (%%continuation-return-no-winding cont values)
  (%%force-uniqueness (cont values)
    `(%%check-continuation ,cont 1 (%%continuation-return-no-winding ,cont ,values)
       (^#continuation-return-no-winding ,cont ,values))))

(zen:define-synto (%%continuation-parent cont)
  (%%force-uniqueness (cont)
    `(%%check-continuation ,cont 1 (%%continuation-parent ,cont)
       (^#continuation-parent ,cont))))

(zen:define-synto (%%continuation-creator cont)
  (%%force-uniqueness (cont)
    `(%%check-continuation ,cont 1 (%%continuation-creator ,cont)
       (^#continuation-creator ,cont))))

(zen:define-synto (%%continuation-locat cont)
  (%%force-uniqueness (cont)
    `(%%check-continuation ,cont 1 (%%continuation-locat ,cont)
       (^#continuation-locat ,cont))))

(zen:define-synto (%%continuation-locals cont)
  (%%force-uniqueness (cont)
    `(%%check-continuation ,cont 1 (%%continuation-locals ,cont)
       (^#continuation-locals ,cont))))

(zen:define-synto (%%continuation-next cont)
  (%%force-uniqueness (cont)
    `(%%check-continuation ,cont 1 (%%continuation-next ,cont)
       (^#continuation-next ,cont))))

(zen:define-synto (%%continuation-first-frame cont all-frames?)
  (%%force-uniqueness (cont all-frames?)
    `(%%check-continuation ,cont 1 (%%continuation-first-frame ,cont ,all-frames?)
       (^#continuation-first-frame ,cont ,all-frames?))))

(zen:define-synto (%%continuation-next-frame cont all-frames?)
  (%%force-uniqueness (cont all-frames?)
    `(%%check-continuation ,cont 1 (%%continuation-next-frame ,cont ,all-frames?)
       (^#continuation-next-frame ,cont ,all-frames?))))

(zen:define-synto (%%interp-continuation? cont)
  (%%force-uniqueness (cont)
    `(%%check-continuation ,cont 1 (%%interp-continuation? ,cont)
       (^#interp-continuation? ,cont))))


;;;
;;;; Control
;;;


(zen:define-synto (%%apply proc lst)
  (if zen:debug-core?
      `(apply ,proc ,lst)
    `(^#apply ,proc ,lst)))


;;;
;;;; Equality
;;;


(zen:define-synto (%%eq? x y)
  (if zen:debug-core?
      `(eq? ,x ,y)
    `(^#eq? ,x ,y)))

(zen:define-synto (%%neq? x y)
  `(%%not (%%eq? ,x ,y)))

(zen:define-synto (%%eqv? x y)
  (if zen:debug-core?
      `(eqv? ,x ,y)
    `(^#eqv? ,x ,y)))

(zen:define-synto (%%equal? x y)
  (if zen:debug-core?
      `(equal? ,x ,y)
    `(^#equal? ,x ,y)))


;;;
;;;; Eval
;;;


(zen:define-synto (%%load path script-callback clone-cte? raise-os-exception? linker-name quiet?)
  ;; DANGER : temporary hack until proper primitive exists
  `(^#load ,path ,script-callback ,clone-cte? ,raise-os-exception? ,linker-name ,quiet?))


;;;
;;;; Exactness
;;;


(zen:define-synto (%%exact->inexact obj)
  (if zen:debug-core?
      `(exact->inexact ,obj)
    `(^#exact->inexact ,obj)))


(zen:define-synto (%%inexact->exact obj)
  (if zen:debug-core?
      `(inexact->exact ,obj)
    `(^#inexact->exact ,obj)))


;;;
;;;; Exception
;;;


(zen:define-synto (%%exception->locat exc cont)
  (%%force-uniqueness (exc cont)
    `(%%check-continuation ,cont 2 (%%exception->locat ,exc ,cont)
       (^#exception->locat ,exc ,cont))))


(zen:define-synto (%%display-exception-hook-ref)
  `##display-exception-hook)

(zen:define-synto (%%display-exception-hook-set! hook)
  (%%force-uniqueness (hook)
    `(%%check-procedure ,hook 1 (%%display-exception-hook-set! ,hook)
       (^#display-exception-hook-set! ,hook))))


(zen:define-synto (%%raise-heap-overflow-exception)
  `(##raise-heap-overflow-exception))


;;;
;;;; Features
;;;


(zen:define-synto (%%cond-expand-features . rest)
  `(^#cond-expand-features ,@rest))


;;;
;;;; Fixnum
;;;


(zen:define-synto (%%fixnum? obj)
  (if zen:debug-core?
      `(fixnum? ,obj)
    `(^#fixnum? ,obj)))

(zen:define-synto (%%fixnum->flonum x)
  (%%force-uniqueness (x)
    `(%%check-fixnum ,x 1 (%%fixnum->flonum ,x)
       (^#fixnum->flonum ,x))))

(zen:define-synto (%%fx= . rest)
  (if zen:debug-core?
      `(= ,@rest)
    `(^#fx= ,@rest)))

(zen:define-synto (%%fx< . rest)
  (if zen:debug-core?
      `(< ,@rest)
    `(^#fx< ,@rest)))

(zen:define-synto (%%fx<= . rest)
  (if zen:debug-core?
      `(<= ,@rest)
    `(^#fx<= ,@rest)))

(zen:define-synto (%%fx> . rest)
  (if zen:debug-core?
      `(> ,@rest)
    `(^#fx> ,@rest)))

(zen:define-synto (%%fx>= . rest)
  (if zen:debug-core?
      `(>= ,@rest)
    `(^#fx>= ,@rest)))

(zen:define-synto (%%fx+ . rest)
  (if zen:debug-core?
      `(+ ,@rest)
    `(^#fx+ ,@rest)))

(zen:define-synto (%%fx- . rest)
  (if zen:debug-core?
      `(- ,@rest)
    `(^#fx- ,@rest)))

(zen:define-synto (%%fx* . rest)
  (if zen:debug-core?
      `(* ,@rest)
    `(^#fx* ,@rest)))

(zen:define-synto (%%fxabs x)
  (if zen:debug-core?
      `(fxabs ,x)
    `(^#fxabs ,x)))

(zen:define-synto (%%fxquotient x y)
  (if zen:debug-core?
      `(quotient ,x ,y)
    `(^#fxquotient ,x ,y)))

(zen:define-synto (%%fxremainder x y)
  (if zen:debug-core?
      `(remainder ,x ,y)
    `(^#fxremainder ,x ,y)))

(zen:define-synto (%%fxmin . rest)
  (if zen:debug-core?
      `(fxmin ,@rest)
    `(^#fxmin ,@rest)))

(zen:define-synto (%%fxmax . rest)
  (if zen:debug-core?
      `(fxmax ,@rest)
    `(^#fxmax ,@rest)))

(zen:define-synto (%%fxmodulo . rest)
  (if zen:debug-core?
      `(fxmodulo ,@rest)
    `(^#fxmodulo ,@rest)))

(zen:define-synto (%%fxeven? . rest)
  (if zen:debug-core?
      `(fxeven? ,@rest)
    `(^#fxeven? ,@rest)))

(zen:define-synto (%%fxodd? . rest)
  (if zen:debug-core?
      `(fxodd? ,@rest)
    `(^#fxodd? ,@rest)))

(zen:define-synto (%%fxnot . rest)
  (if zen:debug-core?
      `(fxnot ,@rest)
    `(^#fxnot ,@rest)))

(zen:define-synto (%%fxand . rest)
  (if zen:debug-core?
      `(fxand ,@rest)
    `(^#fxand ,@rest)))

(zen:define-synto (%%fxior . rest)
  (if zen:debug-core?
      `(fxior ,@rest)
    `(^#fxior ,@rest)))

(zen:define-synto (%%fxxor . rest)
  (if zen:debug-core?
      `(fxxor ,@rest)
    `(^#fxxor ,@rest)))

(zen:define-synto (%%fxarithmetic-shift . rest)
  (if zen:debug-core?
      `(fxarithmetic-shift ,@rest)
    `(^#fxarithmetic-shift ,@rest)))

(zen:define-synto (%%fxarithmetic-shift-left . rest)
  (if zen:debug-core?
      `(fxarithmetic-shift-left ,@rest)
    `(^#fxarithmetic-shift-left ,@rest)))

(zen:define-synto (%%fxarithmetic-shift-right . rest)
  (if zen:debug-core?
      `(fxarithmetic-shift-right ,@rest)
    `(^#fxarithmetic-shift-right ,@rest)))

(zen:define-synto (%%fxbetween? n lower upper)
  (%%force-uniqueness (n)
    (if zen:debug-core?
        `(and (>= ,n ,lower)
              (<= ,n ,upper))
      `(and (^#fx>= ,n ,lower)
            (^#fx<= ,n ,upper)))))


;;;
;;;; Flonum
;;;


(zen:define-synto (%%flonum? obj)
  (if zen:debug-flonum?
      `(flonum? ,obj)
    `(^#flonum? ,obj)))

(zen:define-synto (%%flnan? obj)
  (if zen:debug-flonum?
      `(flnan? ,obj)
    `(^#flnan? ,obj)))

(zen:define-synto (%%flonum->fixnum x)
  (%%force-uniqueness (x)
    `(%%check-flonum ,x 1 (%%flonum->fixnum ,x)
       (^#flonum->fixnum ,x))))

(zen:define-synto (%%fl= . rest)
  (if zen:debug-flonum?
      `(= ,@rest)
    `(^#fl= ,@rest)))

(zen:define-synto (%%fl< . rest)
  (if zen:debug-flonum?
      `(< ,@rest)
    `(^#fl< ,@rest)))

(zen:define-synto (%%fl<= . rest)
  (if zen:debug-flonum?
      `(<= ,@rest)
    `(^#fl<= ,@rest)))

(zen:define-synto (%%fl> . rest)
  (if zen:debug-flonum?
      `(> ,@rest)
    `(^#fl> ,@rest)))

(zen:define-synto (%%fl>= . rest)
  (if zen:debug-flonum?
      `(>= ,@rest)
    `(^#fl>= ,@rest)))

(zen:define-synto (%%fl+ . rest)
  (if zen:debug-flonum?
      `(+ ,@rest)
    `(^#fl+ ,@rest)))

(zen:define-synto (%%fl- . rest)
  (if zen:debug-flonum?
      `(- ,@rest)
    `(^#fl- ,@rest)))

(zen:define-synto (%%fl* . rest)
  (if zen:debug-flonum?
      `(* ,@rest)
    `(^#fl* ,@rest)))

(zen:define-synto (%%fl/ . rest)
  (if zen:debug-flonum?
      `(/ ,@rest)
    `(^#fl/ ,@rest)))

(zen:define-synto (%%flfloor x)
  (if zen:debug-flonum?
      `(flfloor ,x)
    `(^#flfloor ,x)))

(zen:define-synto (%%flceiling x)
  (if zen:debug-flonum?
      `(flceiling ,x)
    `(^#flceiling ,x)))

(zen:define-synto (%%fltruncate x)
  (if zen:debug-flonum?
      `(fltruncate ,x)
    `(^#fltruncate ,x)))

(zen:define-synto (%%flround x)
  (if zen:debug-flonum?
      `(flround ,x)
    `(^#flround ,x)))

(zen:define-synto (%%flabs x)
  (if zen:debug-flonum?
      `(flabs ,x)
    `(^#flabs ,x)))

(zen:define-synto (%%flsqrt x)
  (if zen:debug-flonum?
      `(flsqrt ,x)
    `(^#flsqrt ,x)))

(zen:define-synto (%%flexpt x y)
  (if zen:debug-flonum?
      `(flexpt ,x ,y)
    `(^#flexpt ,x ,y)))

(zen:define-synto (%%flsquare x)
  (if zen:debug-flonum?
      `(flsquare ,x)
    `(^#flsquare ,x)))

(zen:define-synto (%%flsin x)
  (if zen:debug-flonum?
      `(flsin ,x)
    `(^#flsin ,x)))

(zen:define-synto (%%flcos x)
  (if zen:debug-flonum?
      `(flcos ,x)
    `(^#flcos ,x)))

(zen:define-synto (%%fltan x)
  (if zen:debug-flonum?
      `(fltan ,x)
    `(^#fltan ,x)))

(zen:define-synto (%%flasin x)
  (if zen:debug-flonum?
      `(flasin ,x)
    `(^#flasin ,x)))

(zen:define-synto (%%flacos x)
  (if zen:debug-flonum?
      `(flacos ,x)
    `(^#flacos ,x)))

(zen:define-synto (%%flatan . rest)
  (if zen:debug-flonum?
      `(flatan ,@rest)
    `(^#flatan ,@rest)))

(zen:define-synto (%%flmin . rest)
  (if zen:debug-flonum?
      `(flmin ,@rest)
    `(^#flmin ,@rest)))

(zen:define-synto (%%flmax . rest)
  (if zen:debug-flonum?
      `(flmax ,@rest)
    `(^#flmax ,@rest)))

(zen:define-synto (%%flalloc)
  `(^#subtype-set! (^#f64vector 0.) zen:subtype-flonum))

(zen:define-synto (%%flref fl ignore)
  `(^#f64vector-ref ,fl 0))

(zen:define-synto (%%flset! fl ignore val)
  `(^#f64vector-set! ,fl 0 ,val))

(zen:define-synto (%%flonum->exact-int fl)
  (%%force-uniqueness (fl)
    `(%%check-flonum ,fl 1 (%%flonum->exact-int ,fl)
       (^#flonum->exact-int ,fl))))

(zen:define-synto (%%flbetween? n lower upper)
  (%%force-uniqueness (n)
    (if zen:debug-core?
        `(and (>= ,n ,lower)
              (<= ,n ,upper))
      `(and (^#fl>= ,n ,lower)
            (^#fl<= ,n ,upper)))))


;;;
;;;; Foreign
;;;


(zen:define-synto (%%foreign? obj)
  `(^#foreign? ,obj))

;(zen:define-synto (%%still-obj-refcount foreign)
;  )

(zen:define-synto (%%still-obj-refcount-dec! foreign)
  (%%force-uniqueness (foreign)
    `(%%check-foreign ,foreign 1 (%%still-obj-refcount-dec! ,foreign)
       (^#still-obj-refcount-dec! ,foreign))))

(zen:define-synto (%%still-obj-refcount-inc! foreign)
  (%%force-uniqueness (foreign)
    `(%%check-foreign ,foreign 1 (%%still-obj-refcount-inc! ,foreign)
       (^#still-obj-refcount-inc! ,foreign))))

(zen:define-synto (%%still-copy obj)
  `(^#still-copy ,obj))


;;;
;;;; GC
;;;


(zen:define-synto (%%add-gc-interrupt-job! thunk)
  (%%force-uniqueness (thunk)
    `(%%check-procedure ,thunk 1 (%%add-gc-interrupt-job! ,thunk)
       (^#add-gc-interrupt-job! ,thunk))))

(zen:define-synto (%%clear-gc-interrupt-jobs!)
  `(^#clear-gc-interrupt-jobs!))


;;;
;;;; Heartbeat
;;;


(zen:define-synto (%%get-heartbeat-interval! f64vect i)
  (%%force-uniqueness (f64vect i)
    `(%%check-f64vector ,f64vect 1 (%%get-heartbeat-interval! ,f64vect ,i)
       (%%check-fixnum ,i 2 (%%get-heartbeat-interval! ,f64vect ,i)
         (^#get-heartbeat-interval! ,f64vect ,i)))))

(zen:define-synto (%%set-heartbeat-interval! seconds)
  (%%force-uniqueness (seconds)
    `(%%check-flonum ,seconds 1 (%%set-heartbeat-interval! ,seconds)
       (^#set-heartbeat-interval! ,seconds))))


;;;
;;;; Homogeneous
;;;


;; use at your own risk versions that do not initialize memory
(zen:define-synto (%%allocate-vector    size . rest) `(%%tracking (^#make-vector    ,size ,@rest)))
(zen:define-synto (%%allocate-s8vector  size . rest) `(%%tracking (^#make-s8vector  ,size ,@rest)))
(zen:define-synto (%%allocate-u8vector  size . rest) `(%%tracking (^#make-u8vector  ,size ,@rest)))
(zen:define-synto (%%allocate-s16vector size . rest) `(%%tracking (^#make-s16vector ,size ,@rest)))
(zen:define-synto (%%allocate-u16vector size . rest) `(%%tracking (^#make-u16vector ,size ,@rest)))
(zen:define-synto (%%allocate-s32vector size . rest) `(%%tracking (^#make-s32vector ,size ,@rest)))
(zen:define-synto (%%allocate-u32vector size . rest) `(%%tracking (^#make-u32vector ,size ,@rest)))
(zen:define-synto (%%allocate-s64vector size . rest) `(%%tracking (^#make-s64vector ,size ,@rest)))
(zen:define-synto (%%allocate-u64vector size . rest) `(%%tracking (^#make-u64vector ,size ,@rest)))
(zen:define-synto (%%allocate-f32vector size . rest) `(%%tracking (^#make-f32vector ,size ,@rest)))
(zen:define-synto (%%allocate-f64vector size . rest) `(%%tracking (^#make-f64vector ,size ,@rest)))


;; these versions initialize memory
(zen:define-synto (%%make-vector    size . rest) `(%%tracking (make-vector    ,size ,@rest)))
(zen:define-synto (%%make-s8vector  size . rest) `(%%tracking (make-s8vector  ,size ,@rest)))
(zen:define-synto (%%make-u8vector  size . rest) `(%%tracking (make-u8vector  ,size ,@rest)))
(zen:define-synto (%%make-s16vector size . rest) `(%%tracking (make-s16vector ,size ,@rest)))
(zen:define-synto (%%make-u16vector size . rest) `(%%tracking (make-u16vector ,size ,@rest)))
(zen:define-synto (%%make-s32vector size . rest) `(%%tracking (make-s32vector ,size ,@rest)))
(zen:define-synto (%%make-u32vector size . rest) `(%%tracking (make-u32vector ,size ,@rest)))
(zen:define-synto (%%make-s64vector size . rest) `(%%tracking (make-s64vector ,size ,@rest)))
(zen:define-synto (%%make-u64vector size . rest) `(%%tracking (make-u64vector ,size ,@rest)))
(zen:define-synto (%%make-f32vector size . rest) `(%%tracking (make-f32vector ,size ,@rest)))
(zen:define-synto (%%make-f64vector size . rest) `(%%tracking (make-f64vector ,size ,@rest)))


(zen:define-synto (%%s8vector . rest) (if zen:debug-core? `(s8vector ,@rest) `(^#s8vector ,@rest)))
(zen:define-synto (%%subs8vector . rest) (if zen:debug-core? `(subs8vector ,@rest) `(%%tracking (^#subs8vector ,@rest))))
(zen:define-synto (%%s8vector-length vec) (if zen:debug-core? `(s8vector-length ,vec) `(^#s8vector-length ,vec)))
(zen:define-synto (%%s8vector-ref vec n) (if zen:debug-core? `(s8vector-ref ,vec ,n) `(^#s8vector-ref ,vec ,n)))
(zen:define-synto (%%s8vector-set! vec n value) (if zen:debug-core? `(s8vector-set! ,vec ,n ,value) `(^#s8vector-set! ,vec ,n ,value)))


(zen:define-synto (%%u8vector . rest) (if zen:debug-core? `(u8vector ,@rest) `(^#u8vector ,@rest)))
(zen:define-synto (%%subu8vector . rest) (if zen:debug-core? `(subu8vector ,@rest) `(%%tracking (^#subu8vector ,@rest))))
(zen:define-synto (%%u8vector-length vec) (if zen:debug-core? `(u8vector-length ,vec) `(^#u8vector-length ,vec)))
(zen:define-synto (%%u8vector-ref vec n) (if zen:debug-core? `(u8vector-ref ,vec ,n) `(^#u8vector-ref ,vec ,n)))
(zen:define-synto (%%u8vector-set! vec n value) (if zen:debug-core? `(u8vector-set! ,vec ,n ,value) `(^#u8vector-set! ,vec ,n ,value)))


(zen:define-synto (%%s16vector . rest) (if zen:debug-core? `(s16vector ,@rest) `(^#s16vector ,@rest)))
(zen:define-synto (%%subs16vector . rest) (if zen:debug-core? `(subs16vector ,@rest) `(%%tracking (^#subs16vector ,@rest))))
(zen:define-synto (%%s16vector-length vec) (if zen:debug-core? `(s16vector-length ,vec) `(^#s16vector-length ,vec)))
(zen:define-synto (%%s16vector-ref vec n) (if zen:debug-core? `(s16vector-ref ,vec ,n) `(^#s16vector-ref ,vec ,n)))
(zen:define-synto (%%s16vector-set! vec n value) (if zen:debug-core? `(s16vector-set! ,vec ,n ,value) `(^#s16vector-set! ,vec ,n ,value)))


(zen:define-synto (%%u16vector . rest) (if zen:debug-core? `(u16vector ,@rest) `(^#u16vector ,@rest)))
(zen:define-synto (%%subu16vector . rest) (if zen:debug-core? `(subu16vector ,@rest) `(%%tracking (^#subu16vector ,@rest))))
(zen:define-synto (%%u16vector-length vec) (if zen:debug-core? `(u16vector-length ,vec) `(^#u16vector-length ,vec)))
(zen:define-synto (%%u16vector-ref vec n) (if zen:debug-core? `(u16vector-ref ,vec ,n) `(^#u16vector-ref ,vec ,n)))
(zen:define-synto (%%u16vector-set! vec n value) (if zen:debug-core? `(u16vector-set! ,vec ,n ,value) `(^#u16vector-set! ,vec ,n ,value)))


(zen:define-synto (%%s32vector . rest) (if zen:debug-core? `(s32vector ,@rest) `(^#s32vector ,@rest)))
(zen:define-synto (%%subs32vector . rest) (if zen:debug-core? `(subs32vector ,@rest) `(%%tracking (^#subs32vector ,@rest))))
(zen:define-synto (%%s32vector-length vec) (if zen:debug-core? `(s32vector-length ,vec) `(^#s32vector-length ,vec)))
(zen:define-synto (%%s32vector-ref vec n) (if zen:debug-core? `(s32vector-ref ,vec ,n) `(^#s32vector-ref ,vec ,n)))
(zen:define-synto (%%s32vector-set! vec n value) (if zen:debug-core? `(s32vector-set! ,vec ,n ,value) `(^#s32vector-set! ,vec ,n ,value)))


(zen:define-synto (%%u32vector . rest) (if zen:debug-core? `(u32vector ,@rest) `(^#u32vector ,@rest)))
(zen:define-synto (%%subu32vector . rest) (if zen:debug-core? `(subu32vector ,@rest) `(%%tracking (^#subu32vector ,@rest))))
(zen:define-synto (%%u32vector-length vec) (if zen:debug-core? `(u32vector-length ,vec) `(^#u32vector-length ,vec)))
(zen:define-synto (%%u32vector-ref vec n) (if zen:debug-core? `(u32vector-ref ,vec ,n) `(^#u32vector-ref ,vec ,n)))
(zen:define-synto (%%u32vector-set! vec n value) (if zen:debug-core? `(u32vector-set! ,vec ,n ,value) `(^#u32vector-set! ,vec ,n ,value)))


(zen:define-synto (%%s64vector . rest) (if zen:debug-core? `(s64vector ,@rest) `(^#s64vector ,@rest)))
(zen:define-synto (%%subs64vector . rest) (if zen:debug-core? `(subs64vector ,@rest) `(%%tracking (^#subs64vector ,@rest))))
(zen:define-synto (%%s64vector-length vec) (if zen:debug-core? `(s64vector-length ,vec) `(^#s64vector-length ,vec)))
(zen:define-synto (%%s64vector-ref vec n) (if zen:debug-core? `(s64vector-ref ,vec ,n) `(^#s64vector-ref ,vec ,n)))
(zen:define-synto (%%s64vector-set! vec n value) (if zen:debug-core? `(s64vector-set! ,vec ,n ,value) `(^#s64vector-set! ,vec ,n ,value)))


(zen:define-synto (%%u64vector . rest) (if zen:debug-core? `(u64vector ,@rest) `(^#u64vector ,@rest)))
(zen:define-synto (%%subu64vector . rest) (if zen:debug-core? `(subu64vector ,@rest) `(%%tracking (^#subu64vector ,@rest))))
(zen:define-synto (%%u64vector-length vec) (if zen:debug-core? `(u64vector-length ,vec) `(^#u64vector-length ,vec)))
(zen:define-synto (%%u64vector-ref vec n) (if zen:debug-core? `(u64vector-ref ,vec ,n) `(^#u64vector-ref ,vec ,n)))
(zen:define-synto (%%u64vector-set! vec n value) (if zen:debug-core? `(u64vector-set! ,vec ,n ,value) `(^#u64vector-set! ,vec ,n ,value)))


(zen:define-synto (%%f32vector . rest) (if zen:debug-core? `(f32vector ,@rest) `(^#f32vector ,@rest)))
(zen:define-synto (%%subf32vector . rest) (if zen:debug-core? `(subf32vector ,@rest) `(%%tracking (^#subf32vector ,@rest))))
(zen:define-synto (%%f32vector-length vec) (if zen:debug-core? `(f32vector-length ,vec) `(^#f32vector-length ,vec)))
(zen:define-synto (%%f32vector-ref vec n) (if zen:debug-core? `(f32vector-ref ,vec ,n) `(^#f32vector-ref ,vec ,n)))
(zen:define-synto (%%f32vector-set! vec n value) (if zen:debug-core? `(f32vector-set! ,vec ,n ,value) `(^#f32vector-set! ,vec ,n ,value)))


(zen:define-synto (%%f64vector . rest) (if zen:debug-core? `(f64vector ,@rest) `(^#f64vector ,@rest)))
(zen:define-synto (%%subf64vector . rest) (if zen:debug-core? `(subf64vector ,@rest) `(%%tracking (^#subf64vector ,@rest))))
(zen:define-synto (%%f64vector-length vec) (if zen:debug-core? `(f64vector-length ,vec) `(^#f64vector-length ,vec)))
(zen:define-synto (%%f64vector-ref vec n) (if zen:debug-core? `(f64vector-ref ,vec ,n) `(^#f64vector-ref ,vec ,n)))
(zen:define-synto (%%f64vector-set! vec n value) (if zen:debug-core? `(f64vector-set! ,vec ,n ,value) `(^#f64vector-set! ,vec ,n ,value)))


;;;
;;;; Interrupts
;;;


(define TERMINATE-INTERRUPT  1)
(define HEARTBEAT-INTERRUPT  2)
(define USER-INTERRUPT       3)
(define GC-INTERRUPT         4)
(define HIGH-LEVEL-INTERRUPT 5)


(zen:define-synto (%%interrupt-handler code)
  `(%%danger %%interrupt-handler
     (^#interrupt-handler ,code)))

(zen:define-synto (%%interrupt-vector-set! code handler)
  `(%%danger %%interrupt-vector-set!
     (^#interrupt-vector-set! ,code ,handler)))


(zen:define-synto (%%disable-interrupts!)
  `(^#disable-interrupts!))

(zen:define-synto (%%enable-interrupts!)
  `(^#enable-interrupts!))


;;;
;;;; Kernel
;;;


(zen:define-synto (%%subtype obj)
  `(%%danger %%subtype
     (^#subtype ,obj)))


;;;
;;;; Keyword
;;;


(zen:define-synto (%%keyword? obj)
  (if zen:debug-core?
      `(keyword? ,obj)
    `(^#keyword? ,obj)))

(zen:define-synto (%%string->keyword str)
  (if zen:debug-core?
      `(string->keyword ,str)
    `(^#string->keyword ,str)))

(zen:define-synto (%%keyword->string keyword)
  (if zen:debug-core?
      `(keyword->string ,keyword)
    `(^#keyword->string ,keyword)))

(zen:define-synto (%%keyword-table)
  `(^#keyword-table))


;;;
;;;; List
;;;


(zen:define-synto (%%null? obj)
  (if zen:debug-core?
      `(null? ,obj)
    `(^#null? ,obj)))

(zen:define-synto (%%pair? obj)
  (if zen:debug-core?
      `(pair? ,obj)
    `(^#pair? ,obj)))

(zen:define-synto (%%car pair)
  (if zen:debug-core?
      `(car ,pair)
    `(^#car ,pair)))

(zen:define-synto (%%cdr pair)
  (if zen:debug-core?
      `(cdr ,pair)
    `(^#cdr ,pair)))

(zen:define-synto (%%set-car! pair val)
  (if zen:debug-core?
      `(set-car! ,pair ,val)
    `(^#set-car! ,pair ,val)))

(zen:define-synto (%%set-cdr! pair val)
  (if zen:debug-core?
      `(set-cdr! ,pair ,val)
    `(^#set-cdr! ,pair ,val)))

(zen:define-synto (%%caar pair)
  (if zen:debug-core?
      `(caar ,pair)
    `(^#caar ,pair)))

(zen:define-synto (%%cadr pair)
  (if zen:debug-core?
      `(cadr ,pair)
    `(^#cadr ,pair)))

(zen:define-synto (%%cdar pair)
  (if zen:debug-core?
      `(cdar ,pair)
    `(^#cdar ,pair)))

(zen:define-synto (%%cddr pair)
  (if zen:debug-core?
      `(cddr ,pair)
    `(^#cddr ,pair)))

(zen:define-synto (%%length lst)
  (if zen:debug-core?
      `(length ,lst)
    `(^#length ,lst)))

(zen:define-synto (%%memq obj lst)
  (if zen:debug-core?
      `(memq ,obj ,lst)
    `(^#memq ,obj ,lst)))

(zen:define-synto (%%memv obj lst)
  `(memv ,obj ,lst))

(zen:define-synto (%%member obj lst)
  (if zen:debug-core?
      `(member ,obj ,lst)
    `(^#member ,obj ,lst)))

(zen:define-synto (%%assq obj alist)
  (if zen:debug-core?
      `(assq ,obj ,alist)
    `(^#assq ,obj ,alist)))

(zen:define-synto (%%assv obj alist)
  (if zen:debug-core?
      `(assv ,obj ,alist)
    `(^#assv ,obj ,alist)))

(zen:define-synto (%%assoc obj alist)
  (if zen:debug-core?
      `(assoc ,obj ,alist)
    `(^#assoc ,obj ,alist)))

(zen:define-synto (%%cons x y)
  (if zen:debug-core?
      `(cons ,x ,y)
    `(^#cons ,x ,y)))

(zen:define-synto (%%list . rest)
  (if zen:debug-core?
      `(list ,@rest)
    `(^#list ,@rest)))

(zen:define-synto (%%append x y)
  (if zen:debug-core?
      `(append ,x ,y)
    `(^#append ,x ,y)))

(zen:define-synto (%%remove elem lst)
  (%%force-uniqueness (elem lst)
    `(%%check-list ,lst 2 (%%remove ,elem ,lst)
       ;; bongo
       (^#remq ,elem ,lst))))

(zen:define-synto (%%reverse lst)
  (if zen:debug-core?
      `(reverse ,lst)
    `(^#reverse ,lst)))

(zen:define-synto (%%list->vector lst)
  (if zen:debug-core?
      `(list->vector ,lst)
    `(^#list->vector ,lst)))


;;;
;;;; Memory
;;;


(zen:define-synto (%%gc)
  `(^#gc))

(zen:define-synto (%%get-bytes-allocated! floats i)
  (%%force-uniqueness (floats i)
    `(%%check-f64vector ,floats 1 (%%get-bytes-allocated! ,floats ,i)
       (%%check-fixnum ,i 2 (%%get-bytes-allocated! ,floats ,i)
         (^#get-bytes-allocated! ,floats ,i)))))

(zen:define-synto (%%get-live-percent)
  `(^#get-live-percent))


;;;
;;;; Number
;;;


(zen:define-synto (%%number? obj)
  (if zen:debug-core?
      `(number? ,obj)
    `(^#number? ,obj)))

(zen:define-synto (%%integer? obj)
  (if zen:debug-core?
      `(integer? ,obj)
    `(^#integer? ,obj)))

(zen:define-synto (%%real? obj)
  (if zen:debug-core?
      `(real? ,obj)
    `(^#real? ,obj)))

(zen:define-synto (%%= x y)
  (if zen:debug-core?
      `(= ,x ,y)
    `(^#= ,x ,y)))

(zen:define-synto (%%+ x y)
  (if zen:debug-core?
      `(+ ,x ,y)
    `(^#+ ,x ,y)))

(zen:define-synto (%%- . rest)
  (if zen:debug-core?
      `(- ,@rest)
    `(^#- ,@rest)))

(zen:define-synto (%%* x y)
  (if zen:debug-core?
      `(* ,x ,y)
    `(^#* ,x ,y)))

(zen:define-synto (%%/ x y)
  (if zen:debug-core?
      `(/ ,x ,y)
    `(^#/ ,x ,y)))

(zen:define-synto (%%number->string n)
  (if zen:debug-core?
      `(number->string ,n)
    `(^#number->string ,n)))

(zen:define-synto (%%string->number str)
  (if zen:debug-core?
      `(string->number ,str)
    `(^#string->number ,str)))


;;;
;;;; Object
;;;


(zen:define-synto (%%object->string n)
  (if zen:debug-core?
      `(object->string ,n)
    `(^#object->string ,n)))


;;;
;;;; Parameter
;;;


(zen:define-synto (%%make-parameter . rest)
  `(%%tracking
     (make-parameter ,@rest)))


;;;
;;;; Path
;;;


(zen:define-synto (%%path-expand path)
  (%%force-uniqueness (path)
    `(%%check-string ,path 1 (%%path-expand ,path)
       (^#path-expand ,path))))


;;;
;;;; Port
;;;


(zen:define-synto (%%port? obj)
  (if zen:debug-core?
      `(port? ,obj)
    `(^#port? ,obj)))

(zen:define-synto (%%port-name port)
  (%%force-uniqueness (port)
    `(%%check-port ,port 1 (%%port-name ,port)
       (^#port-name ,port))))

(zen:define-synto (%%port-name->container obj)
  `(^#port-name->container ,obj))

(zen:define-synto (%%eof-object? obj)
  (if zen:debug-core?
      `(eof-object? ,obj)
    `(^#eof-object? ,obj)))

(zen:define-synto (%%input-port-names-set! port names)
  (%%force-uniqueness (port names)
    `(%%check-port ,port 1 (%%input-port-names-set! ,port ,names)
       (%%danger %%input-port-names-set!
         (^#vector-set! ,port 4 ,names)))))

(zen:define-synto (%%input-port-line-set! port line)
  (%%force-uniqueness (port line)
    `(%%check-port ,port 1 (%%input-port-line-set! ,port ,line)
       `(%%check-fixnum ,line 2 (%%input-port-line-set! ,port ,line)
          (^#input-port-line-set! ,port ,line)))))

(zen:define-synto (%%input-port-column-set! port col)
  (%%force-uniqueness (port col)
    `(%%check-port ,port 1 (%%input-port-column-set! ,port ,col)
       `(%%check-fixnum ,col 2 (%%input-port-column-set! ,port ,col)
          (^#input-port-column-set! ,port ,col)))))

(zen:define-synto (%%read-all-as-a-begin-expr-from-port port readtable wrap unwrap start-syntax close-port?)
  (%%force-uniqueness (port readtable wrap unwrap start-syntax close-port?)
    `(%%check-port ,port 1 (%%read-all-as-a-begin-expr-from-port ,port ,readtable ,wrap ,unwrap ,start-syntax ,close-port?)
       (%%check-readtable ,readtable 2 (%%read-all-as-a-begin-expr-from-port ,port ,readtable ,wrap ,unwrap ,start-syntax ,close-port?)
         (^#read-all-as-a-begin-expr-from-port ,port ,readtable ,wrap ,unwrap ,start-syntax ,close-port?)))))

(zen:define-synto (%%write obj port)
  (%%force-uniqueness (obj port)
    `(%%check-port ,port 2 (%%write ,obj ,port)
       (^#write ,obj ,port))))

(zen:define-synto (%%write-string str port)
  (%%force-uniqueness (str port)
    `(%%check-string ,str 1 (%%write-string ,str ,port)
       (%%check-port ,port 2 (%%write-string ,str ,port)
         (^#write-string ,str ,port)))))

(zen:define-synto (%%newline port)
  (%%force-uniqueness (port)
    `(%%check-port ,port 2 (%%newline ,port)
       (^#newline ,port))))

(zen:define-synto (%%default-wr we obj)
  (%%force-uniqueness (we obj)
    `(%%check-writeenv ,we 1 (%%default-wr ,we ,obj)
       (^#default-wr ,we ,obj))))

(zen:define-synto (%%wr-set! proc)
  (%%force-uniqueness (proc)
    `(%%check-procedure ,proc 1 (%%wr-set! ,proc)
       (^#wr-set! ,proc))))


;;;
;;;; Procedure
;;;


(zen:define-synto (%%procedure? obj)
  (if zen:debug-core?
      `(procedure? ,obj)
    `(^#procedure? ,obj)))

(zen:define-synto (%%procedure-name procedure)
  (%%force-uniqueness (procedure)
    `(%%check-procedure ,procedure 1 (%%procedure-name ,procedure)
       (^#procedure-name ,procedure))))

(zen:define-synto (%%procedure-locat procedure)
  (%%force-uniqueness (procedure)
    `(%%check-procedure ,procedure 1 (%%procedure-locat ,procedure)
       (^#procedure-locat ,procedure))))

(zen:define-synto (%%interp-procedure? obj)
  `(^#interp-procedure? ,obj))


;;;
;;;; Process
;;;


(zen:define-synto (%%process-statistics)
  `(^#process-statistics))

(zen:define-synto (%%get-min-heap)
  `(^#get-min-heap))

(zen:define-synto (%%set-min-heap! bytes)
  (%%force-uniqueness (bytes)
    `(%%check-fixnum ,bytes 1 (%%set-min-heap! ,bytes)
       (^#set-min-heap! ,bytes))))

(zen:define-synto (%%get-max-heap)
  `(^#get-max-heap))

(zen:define-synto (%%set-max-heap! bytes)
  (%%force-uniqueness (bytes)
    `(%%check-fixnum ,bytes 1 (%%set-max-heap! ,bytes)
       (^#set-max-heap! ,bytes))))


;;;
;;;; Rational
;;;


(zen:define-synto (%%rational? obj)
  (if zen:debug-core?
      `(rational? ,obj)
    `(^#rational? ,obj)))

(zen:define-synto (%%ratnum? obj)
  `(^#ratnum? ,obj))

(zen:define-synto (%%ratnum->flonum x)
  (%%force-uniqueness (x)
    `(%%check-ratnum ,x 1 (%%ratnum->flonum ,x)
       (^#ratnum->flonum ,x))))


;;;
;;;; Readenv
;;;


(zen:define-synto (%%readenv? obj)
  `(macro-readenv? ,obj))

(zen:define-synto (%%readenv-current-filepos readenv)
  (%%force-uniqueness (readenv)
    `(%%check-readenv ,readenv 1 (%%readenv-current-filepos ,readenv)
       (^#readenv-current-filepos ,readenv))))

(zen:define-synto (%%build-list readenv allow-improper? start-pos close)
  (%%force-uniqueness (readenv allow-improper? start-pos close)
    `(%%check-readenv ,readenv 1 (%%build-list ,readenv ,allow-improper? ,start-pos ,close)
       (^#build-list ,readenv ,allow-improper? ,start-pos ,close))))

(zen:define-synto (%%read-datum-or-label-or-none-or-dot readenv)
  (%%force-uniqueness (readenv)
    `(%%check-readenv ,readenv 1 (%%read-datum-or-label-or-none-or-dot ,readenv)
       (^#read-datum-or-label-or-none-or-dot ,readenv))))


;;;
;;;; Readtable
;;;


(zen:define-synto (%%readtable? obj)
  (if zen:debug-core?
      `(readtable? ,obj)
    `(^#readtable? ,obj)))

(zen:define-synto (%%current-readtable)
  `(^#current-readtable))

(zen:define-synto (%%make-standard-readtable)
  `(^#make-standard-readtable))

(zen:define-synto (%%readtable-copy readtable)
  (%%force-uniqueness (readtable)
    `(%%check-readtable ,readtable 1 (%%readtable-copy ,readtable)
       (^#readtable-copy ,readtable))))

(zen:define-synto (%%readtable-char-delimiter? readtable c)
  (%%force-uniqueness (readtable c)
    `(%%check-readtable ,readtable 1 (%%readtable-char-delimiter? ,readtable ,c)
       (%%check-char ,c 2 (%%readtable-char-delimiter? ,readtable ,c)
         (^#readtable-char-delimiter? ,readtable ,c)))))

(zen:define-synto (%%readtable-char-delimiter?-set! readtable c delimiter?)
  (%%force-uniqueness (readtable c delimiter?)
    `(%%check-readtable ,readtable 1 (%%readtable-char-delimiter?-set! ,readtable ,c ,delimiter?)
       (%%check-char ,c 2 (%%readtable-char-delimiter?-set! ,readtable ,c ,delimiter?)
         (^#readtable-char-delimiter?-set! ,readtable ,c ,delimiter?)))))

(zen:define-synto (%%readtable-char-handler readtable c)
  (%%force-uniqueness (readtable c)
    `(%%check-readtable ,readtable 1 (%%readtable-char-handler ,readtable ,c)
       (%%check-char ,c 2 (%%readtable-char-handler ,readtable ,c)
         (^#readtable-char-handler ,readtable ,c)))))

(zen:define-synto (%%readtable-char-handler-set! readtable c handler)
  (%%force-uniqueness (readtable c handler)
    `(%%check-readtable ,readtable 1 (%%readtable-char-handler-set! ,readtable ,c ,handler)
       (%%check-char ,c 2 (%%readtable-char-handler-set! ,readtable ,c ,handler)
         (^#readtable-char-handler-set! ,readtable ,c ,handler)))))

(zen:define-synto (%%readtable-char-sharp-handler readtable c)
  (%%force-uniqueness (readtable c)
    `(%%check-readtable ,readtable 1 (%%readtable-char-sharp-handler ,readtable ,c)
       (%%check-char ,c 2 (%%readtable-char-sharp-handler ,readtable ,c)
         (^#readtable-char-sharp-handler ,readtable ,c)))))

(zen:define-synto (%%readtable-char-sharp-handler-set! readtable c handler)
  (%%force-uniqueness (readtable c handler)
    `(%%check-readtable ,readtable 1 (%%readtable-char-sharp-handler-set! ,readtable ,c ,handler)
       (%%check-char ,c 2 (%%readtable-char-sharp-handler-set! ,readtable ,c ,handler)
         (^#readtable-char-sharp-handler-set! ,readtable ,c ,handler)))))

(zen:define-synto (%%readtable-char-class-set! readtable c delimiter? handler)
  (%%force-uniqueness (readtable c delimiter? handler)
    `(%%check-readtable ,readtable 1 (%%readtable-char-class-set! ,readtable ,c ,delimiter? ,handler)
       (%%check-char ,c 2 (%%readtable-char-class-set! ,readtable ,c ,delimiter? ,handler)
         (^#readtable-char-class-set! ,readtable ,c ,delimiter? ,handler)))))

(zen:define-synto (%%readtable-escaped-char-table readtable)
  (%%force-uniqueness (readtable)
    `(%%check-readtable ,readtable 1 (%%readtable-escaped-char-table ,readtable)
       (%%danger %%readtable-escaped-char-table
         (^#vector-ref ,readtable 3)))))

(zen:define-synto (%%readtable-escaped-char-table-set! readtable table)
  (%%force-uniqueness (readtable table)
    `(%%check-readtable ,readtable 1 (%%readtable-escaped-char-table-set! ,readtable ,table)
       (%%danger %%readtable-escaped-char-table-set!
         (^#vector-set! ,readtable 3 ,table)))))


;;;
;;;; Repl
;;;


(zen:define-synto (%%repl . rest)
  (let ((write-reason (if (##null? rest) #f (##car rest))))
    `(^#repl ,write-reason)))

(zen:define-synto (%%repl-debug . rest)
  (let ((write-reason (if (##null? rest) #f (##car rest)))
        (reason (if (or (##null? rest) (##null? (##cdr rest))) #f (##cadr rest)))
        (toplevel? (if (or (##null? rest) (##null? (##cdr rest)) (##null? (##cddr rest))) #f (##caddr rest))))
    `(^#repl-debug ,write-reason ,reason ,toplevel?)))

(zen:define-synto (%%thread-repl-context-get!)
  `(^#thread-repl-context-get!))

(zen:define-synto (%%thread-repl-channel-get! thread)
  (%%force-uniqueness (thread)
    `(%%check-thread ,thread 1 (%%thread-repl-channel-get! ,thread)
       (^#thread-repl-channel-get! ,thread))))

(zen:define-synto (%%repl-channel-result-history-add channel result)
  `(^#repl-channel-result-history-add ,channel ,result))


;;;
;;;; Six
;;;


(zen:define-synto (%%six-types-ref)
  `##six-types)


(zen:define-synto (%%six-types-set! lst)
  (%%force-uniqueness (lst)
    `(%%check-list ,lst 1 (%%six-types-set! ,lst)
       (^#six-types-set! ,lst))))


;;;
;;;; String
;;;


(zen:define-synto (%%string? obj)
  (if zen:debug-core?
      `(string? ,obj)
    `(^#string? ,obj)))

(zen:define-synto (%%string . rest)
  (if zen:debug-core?
      `(string ,@rest)
    `(^#string ,@rest)))

(zen:define-synto (%%make-string size . rest)
  `(%%tracking
     (make-string ,size ,@rest)))

(zen:define-synto (%%string=? str1 str2)
  (if zen:debug-core?
      `(string=? ,str1 ,str2)
    `(^#string=? ,str1 ,str2)))

(zen:define-synto (%%string-ci=? str1 str2)
  (if zen:debug-core?
      `(string-ci=? ,str1 ,str2)
    `(^#string-ci=? ,str1 ,str2)))

(zen:define-synto (%%string<? str1 str2)
  (if zen:debug-core?
      `(string<? ,str1 ,str2)
    `(^#string<? ,str1 ,str2)))

(zen:define-synto (%%string-length str)
  (if zen:debug-core?
      `(string-length ,str)
    `(^#string-length ,str)))

(zen:define-synto (%%string-ref str pos)
  (if zen:debug-core?
      `(string-ref ,str ,pos)
    `(^#string-ref ,str ,pos)))

(zen:define-synto (%%string-set! str pos val)
  (if zen:debug-core?
      `(string-set! ,str ,pos ,val)
    `(^#string-set! ,str ,pos ,val)))

(zen:define-synto (%%substring str start end)
  (if zen:debug-core?
      `(substring ,str ,start ,end)
    `(^#substring ,str ,start ,end)))

(zen:define-synto (%%string-append . rest)
  (if zen:debug-core?
      `(string-append ,@rest)
    `(^#string-append ,@rest)))

(zen:define-synto (%%string-shrink! str len)
  (%%force-uniqueness (str len)
    `(%%check-string ,str 1 (%%string-shrink! ,str ,len)
       (%%check-fixnum ,len 2 (%%string-shrink! ,str ,len)
         (^#string-shrink! ,str ,len)))))


;;;
;;;; Structure
;;;


(zen:define-synto (%%structure? obj)
  `(^#structure? ,obj))

(zen:define-synto (%%structure-type structure)
  (%%force-uniqueness (structure)
    `(%%check-structure ,structure 1 (%%structure-type ,structure)
       (^#structure-type ,structure))))

(zen:define-synto (%%structure-ref structure i type proc)
  (%%force-uniqueness (structure)
    `(%%check-structure ,structure 1 (%%structure-ref ,structure ,i ,type ,proc)
       (^#structure-ref ,structure ,i ,type ,proc))))

(zen:define-synto (%%structure-set! structure val i type proc)
  (%%force-uniqueness (structure)
    `(%%check-structure ,structure 1 (%%structure-set! ,structure ,val ,i ,type ,proc)
       (^#structure-set! ,structure ,val ,i ,type ,proc))))


;;;
;;;; Symbol
;;;


(zen:define-synto (%%symbol? obj)
  (if zen:debug-core?
      `(symbol? ,obj)
    `(^#symbol? ,obj)))

(zen:define-synto (%%string->symbol str)
  (if zen:debug-core?
      `(string->symbol ,str)
    `(^#string->symbol ,str)))

(zen:define-synto (%%symbol->string symbol)
  (if zen:debug-core?
      `(symbol->string ,symbol)
    `(^#symbol->string ,symbol)))

(zen:define-synto (%%unbound? obj)
  `(^#unbound? ,obj))

(zen:define-synto (%%global-var? symbol)
  (%%force-uniqueness (symbol)
    `(%%check-symbol ,symbol 1 (%%global-var? ,symbol)
       (^#global-var? ,symbol))))

(zen:define-synto (%%global-var-ref symbol)
  (%%force-uniqueness (symbol)
    `(%%check-symbol ,symbol 1 (%%global-var-ref ,symbol)
       (^#global-var-ref ,symbol))))

(zen:define-synto (%%global-var-set! symbol value)
  (%%force-uniqueness (symbol)
    `(%%check-symbol ,symbol 1 (%%global-var-set! ,symbol ,value)
       (^#global-var-set! ,symbol ,value))))

(zen:define-synto (%%global-var-unbind! symbol)
  (%%force-uniqueness (symbol)
    `(%%check-symbol ,symbol 1 (%%global-var-unbind! ,symbol)
       (^#global-var-set! ,symbol #!unbound))))

(zen:define-synto (%%symbol-table)
  `(^#symbol-table))


;;;
;;;; Source
;;;


(zen:define-synto (%%source? expr)
  `(^#source? ,expr))

(zen:define-synto (%%source-code src)
  (%%force-uniqueness (src)
    `(%%check-source ,src 1 (%%source-code ,src)
       (^#source-code ,src))))

(zen:define-synto (%%source-locat src)
  (%%force-uniqueness (src)
    `(%%check-source ,src 1 (%%source-locat ,src)
       (^#source-locat ,src))))

(zen:define-synto (%%desourcify expr)
  `(^#desourcify ,expr))

(zen:define-synto (%%make-source code locat)
  (%%force-uniqueness (code locat)
    `(%%check-locat ,locat 2 (%%make-source ,code ,locat)
       (^#make-source ,code ,locat))))

(zen:define-synto (%%sourcify expr src)
  (%%force-uniqueness (expr src)
    `(%%check-source ,src 2 (%%sourcify ,expr ,src)
       (^#sourcify ,expr ,src))))

(zen:define-synto (%%sourcify-deep expr src)
  (%%force-uniqueness (expr src)
    `(%%check-source ,src 2 (%%sourcify-deep ,expr ,src)
       (^#sourcify-deep ,expr ,src))))

(zen:define-synto (%%locat? expr)
  `(^#locat? ,expr))

(zen:define-synto (%%make-locat container position)
  (%%force-uniqueness (position)
    `(%%check-fixnum ,position 2 (%%make-locat ,container ,position)
       (^#make-locat ,container ,position))))

(zen:define-synto (%%locat-container locat)
  (%%force-uniqueness (locat)
    `(%%check-locat ,locat 1 (%%locat-container ,locat)
       (^#locat-container ,locat))))

(zen:define-synto (%%locat-start-position locat)
  (%%force-uniqueness (locat)
    `(%%check-locat ,locat 1 (%%locat-start-position ,locat)
       (^#locat-start-position ,locat))))

(zen:define-synto (%%container->path container)
  `(^#container->path ,container))

(zen:define-synto (%%position->filepos position)
  (%%force-uniqueness (position)
    `(%%check-fixnum ,position 1 (%%position->filepos ,position)
       (^#position->filepos ,position))))

(zen:define-synto (%%filepos->position filepos)
  (%%force-uniqueness (filepos)
    `(%%check-fixnum ,filepos 1 (%%filepos->position ,filepos)
       (^#filepos->position ,filepos))))

(zen:define-synto (%%filepos-line filepos)
  (%%force-uniqueness (filepos)
    `(%%check-fixnum ,filepos 1 (%%filepos-line ,filepos)
       (^#filepos-line ,filepos))))

(zen:define-synto (%%filepos-col filepos)
  (%%force-uniqueness (filepos)
    `(%%check-fixnum ,filepos 1 (%%filepos-col ,filepos)
       (^#filepos-col ,filepos))))


;;;
;;;; Table
;;;


(zen:define-synto (%%table? obj)
  `(table? ,obj))

(zen:define-synto (%%make-table . rest)
  `(%%tracking
     (make-table ,@rest)))

(zen:define-synto (%%table-ref table key . rest)
  (if zen:debug-core?
      `(table-ref ,table ,key ,@rest)
    `(^#table-ref ,table ,key ,@rest)))

(zen:define-synto (%%table-set! table key value)
  (if zen:debug-core?
      `(table-set! ,table ,key ,value)
    `(^#table-set! ,table ,key ,value)))

(zen:define-synto (%%table-clear table key)
  `(table-set! ,table ,key))

(zen:define-synto (%%table-length table)
  `(table-length ,table))

(zen:define-synto (%%table-for-each proc table)
  `(table-for-each ,proc ,table))

(zen:define-synto (%%table-merge! table additions . rest)
  (let ((additions-takes-precedence? (if (##null? rest) #f (##car rest))))
    (if zen:debug-core?
        `(table-merge! ,table ,additions ,additions-takes-precedence?)
      `(^#table-merge! ,table ,additions ,additions-takes-precedence?))))

(zen:define-synto (%%list->table alist . rest)
  `(list->table ,alist ,@rest))

(zen:define-synto (%%table->list table)
  `(table->list ,table))

(zen:define-synto (%%copy-table table)
  `(table-copy ,table))


(zen:define-synto (%%gc-hash-table? obj)
  `(^#gc-hash-table? ,obj))


;;;
;;;; Thread
;;;


(zen:define-synto (%%thread? obj)
  `(thread? ,obj))

(zen:define-synto (%%make-thread . rest)
  `(%%tracking
     (make-thread ,@rest)))

(zen:define-synto (%%primordial-thread)
  `##primordial-thread)

(zen:define-synto (%%current-thread)
  `(^#current-thread))

(zen:define-synto (%%make-mutex . rest)
  `(%%tracking
     (make-mutex ,@rest)))

(zen:define-synto (%%make-condition-variable . rest)
  `(%%tracking
     (make-condition-variable ,@rest)))


;;;
;;;; Time
;;;


(zen:define-synto (%%get-current-time! floats i)
  (%%force-uniqueness (floats i)
    `(%%check-f64vector ,floats 1 (%%get-current-time! ,floats ,i)
       (%%check-fixnum ,i 2 (%%get-current-time! ,floats ,i)
         (^#get-current-time! ,floats ,i)))))


;;;
;;;; Type
;;;


(zen:define-synto (%%type? obj)
  `(^#type? ,obj))

(zen:define-synto (%%type-id type)
  (%%force-uniqueness (type)
    `(%%check-type ,type 1 (%%type-id ,type)
       (^#type-id type))))

(zen:define-synto (%%type-name type)
  (%%force-uniqueness (type)
    `(%%check-type ,type 1 (%%type-name ,type)
       (^#type-name type))))

(zen:define-synto (%%type-flags type)
  (%%force-uniqueness (type)
    `(%%check-type ,type 1 (%%type-flags ,type)
       (^#type-flags type))))

(zen:define-synto (%%type-super type)
  (%%force-uniqueness (type)
    `(%%check-type ,type 1 (%%type-super ,type)
       (^#type-super type))))

(zen:define-synto (%%type-field-count type)
  (%%force-uniqueness (type)
    `(%%check-type ,type 1 (%%type-field-count ,type)
       (^#type-field-count type))))

(zen:define-synto (%%type-all-fields type)
  (%%force-uniqueness (type)
    `(%%check-type ,type 1 (%%type-all-fields ,type)
       (^#type-all-fields type))))


;;;
;;;; Values
;;;


(zen:define-synto (%%values? obj)
  `(^#values? ,obj))


;;;
;;;; Vector
;;;


(zen:define-synto (%%vector? obj)
  (if zen:debug-core?
      `(vector? ,obj)
    `(^#vector? ,obj)))

(zen:define-synto (%%vector . rest)
  (if zen:debug-core?
      `(vector ,@rest)
    `(^#vector ,@rest)))

(zen:define-synto (%%vector-length vector)
  (if zen:debug-core?
      `(vector-length ,vector)
    `(^#vector-length ,vector)))

(zen:define-synto (%%vector-ref vector n)
  (if zen:debug-core?
      `(vector-ref ,vector ,n)
    `(^#vector-ref ,vector ,n)))

(zen:define-synto (%%vector-set! vector n value)
  (if zen:debug-core?
      `(vector-set! ,vector ,n ,value)
    `(^#vector-set! ,vector ,n ,value)))

(zen:define-synto (%%vector-copy vector . rest)
  (if zen:debug-core?
      `(vector-copy ,vector ,@rest)
    `(^#vector-copy ,vector ,@rest)))

(zen:define-synto (%%vector->list vector)
  (if zen:debug-core?
      `(vector->list ,vector)
    `(^#vector->list ,vector)))

(zen:define-synto (%%s8vector? obj)
  (if zen:debug-core?
      `(s8vector? ,obj)
    `(^#s8vector? ,obj)))

(zen:define-synto (%%u8vector? obj)
  (if zen:debug-core?
      `(u8vector? ,obj)
    `(^#u8vector? ,obj)))

(zen:define-synto (%%s16vector? obj)
  (if zen:debug-core?
      `(s16vector? ,obj)
    `(^#s16vector? ,obj)))

(zen:define-synto (%%u16vector? obj)
  (if zen:debug-core?
      `(u16vector? ,obj)
    `(^#u16vector? ,obj)))

(zen:define-synto (%%s32vector? obj)
  (if zen:debug-core?
      `(s32vector? ,obj)
    `(^#s32vector? ,obj)))

(zen:define-synto (%%u32vector? obj)
  (if zen:debug-core?
      `(u32vector? ,obj)
    `(^#u32vector? ,obj)))

(zen:define-synto (%%s64vector? obj)
  (if zen:debug-core?
      `(s64vector? ,obj)
    `(^#s64vector? ,obj)))

(zen:define-synto (%%u64vector? obj)
  (if zen:debug-core?
      `(u64vector? ,obj)
    `(^#u64vector? ,obj)))

(zen:define-synto (%%f32vector? obj)
  (if zen:debug-core?
      `(f32vector? ,obj)
    `(^#f32vector? ,obj)))

(zen:define-synto (%%f64vector? obj)
  (if zen:debug-core?
      `(f64vector? ,obj)
    `(^#f64vector? ,obj)))


;;;
;;;; Will
;;;


(zen:define-synto (%%make-will . rest)
  `(%%tracking
     (make-will ,@rest))))
