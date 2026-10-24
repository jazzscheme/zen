;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Macro Expansion
;;;


(block kernel.expansion


(declare (not safe))


;; bongo
(define ##gensym-interned-counter
  -1)

(define (##gensym-interned #!optional (p #f))
  (let ((prefix
         (if (##eq? p #f)
             'g
             p)))
    (let ((new-count
            (##fxmodulo
              (##fx+ ##gensym-interned-counter 1)
              1000000)))
      ;; Note: it is unimportant if the increment of ##gensym-interned-counter
      ;; is not atomic; it simply means a possible close repetition
      ;; of the same name
      (set! ##gensym-interned-counter new-count)
      (##string->symbol
        (if (##eq? prefix 'g)
            new-count ;; ##symbol->string will create the string
          (##string-append (##symbol->string prefix)
            (##number->string new-count 10)))))))


(define (zen:generate-symbol #!optional (prefix "sym"))
  (let ((for (zen:generate-symbol-for)))
    (let ((name (string-append prefix (or for "^"))))
      ;; bongo
      (##gensym-interned (##string->symbol name))
      #;
      (##gensym (##string->uninterned-symbol name)))))


(define (zen:generate-global-symbol #!optional (prefix "sym"))
  (let ((for (zen:generate-symbol-for))
        (context (zen:generate-symbol-context)))
    (cond ((%%not for)
           (error "Invalid call to generate-global-symbol without a for"))
          ((%%not context)
           (error "Invalid call to generate-global-symbol without a context"))
          (else
           (let ((module (zen:string-replace (symbol->string context) #\. #\/)))
             (let ((name (%%string-append module "_" prefix for)))
               ;; bongo
               (##gensym-interned (##string->symbol name))
               #;
               (##gensym (##string->uninterned-symbol name))))))))


(define (zen:with-uniqueness expr proc)
  (if (symbol? (zen:source-code expr))
      (proc expr)
    (let ((value (zen:generate-symbol "val")))
      `(let ((,value ,expr))
         ,(proc value)))))


(define (zen:with-uniqueness-typed expr specifier proc)
  (if (symbol? (zen:source-code expr))
      (proc expr)
    (let ((value (zen:generate-symbol "val")))
      `(let ((,value ,specifier ,expr))
         ,(proc value)))))


(zen:define-macro (%%force-uniqueness variables code)
  (if (null? variables)
      code
    (let ((variable (car variables)))
      `(zen:with-uniqueness ,variable
         (lambda (,variable)
           (%%force-uniqueness ,(cdr variables) ,code))))))


(define (zen:null/pair? obj)
  (or (%%null? obj) (%%pair? obj)))


;;;
;;;; Check
;;;


(zen:define-macro (zen:define-check-macro name test type)
  (let ((str (symbol->string name)))
    (let ((core (string->symbol (string-append "%%" str)))
          (debug (string->symbol (string-append "zen:" str))))
      `(begin
         (zen:define-macro (,core arg pos call code)
           (if zen:debug-core?
               `(if (,',test ,arg)
                    ,code
                  (zen:primitive-type-error ,pos ,',type ',(car call) (list ,@(cdr call))))
             code))
         (zen:define-macro (,debug arg pos call code)
           (if zen:debug-user?
               `(if (,',test ,arg)
                    ,code
                  (zen:primitive-type-error ,pos ,',type ',(car call) (list ,@(cdr call))))
             code))))))


(zen:define-check-macro check-char
  ^#char?
  "CHAR")

(zen:define-check-macro check-closure
  ^#closure?
  "CLOSURE")

(zen:define-check-macro check-continuation
  ^#continuation?
  "CONTINUATION")

(zen:define-check-macro check-fixnum
  ^#fixnum?
  "FIXNUM")

(zen:define-check-macro check-flonum
  ^#flonum?
  "FLONUM")

(zen:define-check-macro check-foreign
  ^#foreign?
  "FOREIGN")

(zen:define-check-macro check-f64vector
  ^#f64vector?
  "F64VECTOR")

;; not 100% correct because of Scheme's semantic for list? that is costly
(zen:define-check-macro check-list
  zen:null/pair?
  "LIST")

(zen:define-check-macro check-locat
  ^#locat?
  "LOCAT")

(zen:define-check-macro check-port
  ^#port?
  "PORT")

(zen:define-check-macro check-procedure
  ^#procedure?
  "PROCEDURE")

(zen:define-check-macro check-ratnum
  ^#ratnum?
  "RATNUM")

(zen:define-check-macro check-readenv
  zen:readenv?
  "READENV")

(zen:define-check-macro check-readtable
  ^#readtable?
  "READTABLE")

(zen:define-check-macro check-source
  ^#source?
  "SOURCE")

(zen:define-check-macro check-string
  ^#string?
  "STRING")

(zen:define-check-macro check-structure
  ^#structure?
  "STRUCTURE")

(zen:define-check-macro check-symbol
  ^#symbol?
  "SYMBOL")

(zen:define-check-macro check-type
  ^#type?
  "TYPE")

(zen:define-check-macro check-table
  table?
  "TABLE")

(zen:define-check-macro check-thread
  thread?
  "THREAD")

(zen:define-check-macro check-values
  ^#values?
  "VALUES")

(zen:define-check-macro check-vector
  ^#vector?
  "VECTOR")

(zen:define-check-macro check-writeenv
  zen:writeenv?
  "WRITEENV")


;;;
;;;; Code
;;;


(zen:define-macro (c-code code . arguments)
  `(let ()
     (declare (extended-bindings))
     (##c-code ,code ,@arguments)))


;;;
;;;; Memory
;;;


(define MOVABLE0 0)
(define MOVABLE1 1)
(define MOVABLE2 2)
(define STILL    5)
(define PERM     6)


(define zen:memory-allocated?
  ##mem-allocated?)


(define (zen:memory-permanent? obj)
  (and (##mem-allocated? obj)
       (let ((k (bitwise-and
                  (##u64vector-ref (##type-cast obj 1) -1)
                  7)))
         (##fx= k PERM))))


(define (zen:memory-allocated-kind obj)
  (let ((k (bitwise-and
             (##u64vector-ref (##type-cast obj 1) -1)
             7)))
    (cond ((##fx= k MOVABLE0) 'MOVABLE)
          ((##fx= k MOVABLE1) 'MOVABLE)
          ((##fx= k MOVABLE2) 'MOVABLE)
          ((##fx= k STILL)    'STILL)
          ((##fx= k PERM)     'PERM))))


(define (zen:memory-allocated-size obj)
  (let ((size (##fx+ (##u8vector-length '#(1))
                     (##u8vector-length obj))))
    (if (##eq? (zen:memory-allocated-kind obj) 'MOVABLE)
        (##fx* size 2) ;; two-space factor
      size)))


(define (zen:memory-size obj)
  (cond ((##pair? obj)
         48)
        ((##symbol? obj)
         40)
        ((##mem-allocated? obj)
         (zen:memory-allocated-size obj))
        (else
         ;; (pp obj) should not happen
         0)))


(define zen:gc-hash-table?
  ##gc-hash-table?)


;;;
;;;; Tracking
;;;


(define zen:tracking-enabled?
  (if zen:kernel-track-memory?
      (zen:global-ref '##tracking-enabled?)
    (lambda ()
      #f)))

(define zen:tracking-allocations?
  (if zen:kernel-track-memory?
      (zen:global-ref '##tracking-allocations?)
    (lambda ()
      #f)))

(define zen:track-allocations
  (if zen:kernel-track-memory?
      (zen:global-ref '##track-allocations)
    (lambda ()
      #f)))

(define zen:untrack-allocations
  (if zen:kernel-track-memory?
      (zen:global-ref '##untrack-allocations)
    (lambda ()
      #f)))

(define zen:record-tracked-hook-set!
  (if zen:kernel-track-memory?
      (zen:global-ref '##record-tracked-hook-set!)
    (lambda (hook)
      #f)))


;;;
;;;; Moving
;;;


(define (zen:make-moving)
  (let ((moving (##make-f64vector 4)))
    (zen:reset-moving moving)
    moving))

(define-macro (zen:moving-start moving)
  `(##f64vector-ref ,moving 0))

(define-macro (zen:moving-total moving)
  `(##f64vector-ref ,moving 1))

(define-macro (zen:moving-rate moving)
  `(##f64vector-ref ,moving 2))

(define-macro (zen:moving-average moving)
  `(##f64vector-ref ,moving 3))

(define-macro (zen:moving-start-set! moving val)
  `(##f64vector-set! ,moving 0 ,val))

(define-macro (zen:moving-total-set! moving val)
  `(##f64vector-set! ,moving 1 ,val))

(define-macro (zen:moving-rate-set! moving val)
  `(##f64vector-set! ,moving 2 ,val))

(define-macro (zen:moving-average-set! moving val)
  `(##f64vector-set! ,moving 3 ,val))


;; MSECTION / MSECTION - FUDGE * 2
(define zen:fudge-price
  1.142897)


(define zen:monotonic-time
  (##make-f64vector 1))

(define-macro (zen:monotonic-get!)
  `(##get-current-monotonic-time! zen:monotonic-time 0))

(define-macro (zen:monotonic-ref)
  `(##f64vector-ref zen:monotonic-time 0))


(define zen:moving-elapsed
  (##make-f64vector 1))

(define-macro (zen:elapsed-ref)
  `(##f64vector-ref zen:moving-elapsed 0))

(define-macro (zen:elapsed-set! val)
  `(##f64vector-set! zen:moving-elapsed 0 ,val))


;; <<<<<<<<<<<<<<<<<<
;; the code from here


(define (zen:update-moving moving size)
  (declare (not interrupts-enabled))
  (zen:monotonic-get!)
  (if (##fl= (zen:moving-start moving) -1.)
      (begin
        (zen:moving-start-set! moving (zen:monotonic-ref))
        (zen:moving-total-set! moving 0.)))
  (zen:moving-total-set! moving (##fl+ (zen:moving-total moving) (##fl* (##fixnum->flonum size) zen:fudge-price)))
  (zen:elapsed-set! (##fl- (zen:monotonic-ref) (zen:moving-start moving)))
  (if (##fl>= (zen:elapsed-ref) .1)
      (begin
        (zen:moving-rate-set! moving (##fl/ (zen:moving-total moving) (zen:elapsed-ref)))
        (if (##fl= (zen:moving-average moving) -1.)
            (zen:moving-average-set! moving (zen:moving-rate moving))
          (zen:moving-average-set! moving (##fl/ (##fl+ (##fl* 15. (zen:moving-average moving)) (zen:moving-rate moving)) 16.)))
        (zen:moving-start-set! moving (zen:monotonic-ref))
        (zen:moving-total-set! moving 0.))))


(define (zen:reset-moving moving)
  (zen:moving-start-set! moving -1.)
  (zen:moving-total-set! moving -1.)
  (zen:moving-rate-set! moving 0.)
  (zen:moving-average-set! moving -1.))


;;;
;;;; Monitor
;;;


(define zen:monitor-allocations?
  #f)


(define zen:monitor-allocations-count
  0)

(define zen:monitor-allocations-size
  0)


(define zen:save-moving
  (zen:make-moving))

(define zen:capture-moving
  (zen:make-moving))

(define zen:allocation-moving
  (zen:make-moving))


(define (zen:monitoring-allocations?)
  zen:monitor-allocations?)


(define (zen:monitor-allocations-reset)
  (set! zen:monitor-allocations-count 0)
  (set! zen:monitor-allocations-size 0)
  (zen:reset-moving zen:save-moving)
  (zen:reset-moving zen:capture-moving)
  (zen:reset-moving zen:allocation-moving))


(define (zen:save-rate-update size)
  (zen:update-moving zen:save-moving size))


(define (zen:capture-rate-update size)
  (zen:update-moving zen:capture-moving size))


(define (zen:allocation-rate-update size)
  (zen:update-moving zen:allocation-moving size))


(define (zen:monitor-allocations-start)
  (set! zen:monitor-allocations? #t)
  (zen:monitor-allocations-reset)
  (zen:track-allocations))

(define (zen:monitor-allocations-stop)
  (set! zen:monitor-allocations? #f)
  (zen:untrack-allocations))


(define (zen:monitor-tracked)
  #f #; ;; bongo
(
  (declare (not interrupts-enabled))
  (let ((save (##save-tracked)))
    (if (##fx> save 0)
        (zen:save-rate-update (##fx* save 8 2))))
  (let ((capture (##capture-tracked)))
    (if (##fx> capture 0)
        (zen:capture-rate-update (##fx* capture 8 2))))
  (let ((count (##count-tracked)))
    (let loop ((n 0) (size 0))
         (if (##fx>= n count)
             (zen:allocation-rate-update size)
           (let ((obj (##get-tracked-object n)))
             (let ((sz (zen:memory-size obj)))
               (set! zen:monitor-allocations-count (##fx+ zen:monitor-allocations-count 1))
               (set! zen:monitor-allocations-size (##fx+ zen:monitor-allocations-size sz))
               (loop (##fx+ n 1) (##fx+ size sz)))))))))


;; to here should not allocate memory so as to not impact
;; the gc period. note that opening the generated c file and
;; scanning for BOX is a good way to check for allocations
;; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


(define (zen:monitor-save-rate)
  (if (##fl= (zen:moving-average zen:save-moving) -1.)
      #f
    (zen:moving-average zen:save-moving)))

(define (zen:monitor-capture-rate)
  (if (##fl= (zen:moving-average zen:capture-moving) -1.)
      #f
    (zen:moving-average zen:capture-moving)))

(define (zen:monitor-allocation-rate)
  (if (##fl= (zen:moving-average zen:allocation-moving) -1.)
      #f
    (zen:moving-average zen:allocation-moving)))


;;;
;;;; Register
;;;


(define zen:register-allocations?
  #f)


(define zen:allocations
  (##make-table test: ##eq?))

(define zen:allocations-rank
  0)


(define (zen:register-allocations)
  (set! zen:register-allocations? #t)
  (zen:track-allocations))

(define (zen:unregister-allocations)
  (set! zen:register-allocations? #f)
  (zen:untrack-allocations))


(define (zen:reset-allocations)
  (set! zen:allocations (##make-table test: ##eq?))
  (set! zen:allocations-rank 0))


(define (zen:registered-allocations)
  zen:allocations)


(define (zen:ordered-allocations)
  (zen:sort-list < (##table->list (zen:registered-allocations)) key: (lambda (pair) (zen:allocation-rank (##cdr pair)))))


(define (zen:register-allocation obj allocation)
  (##table-set! zen:allocations obj allocation)
  (set! zen:allocations-rank (##fx+ zen:allocations-rank 1)))


(define (zen:register-tracked)
  #f #; ;; bongo
(
  (declare (not interrupts-enabled))
  (##continuation-capture
    (lambda (cont)
      (let ((thread (thread-name (##current-thread)))
            (stack (zen:track-continuation cont zen:*track-depth*))
            (count (##count-tracked)))
        (let loop ((n 0))
             (if (##fx< n count)
                 (let ((obj (##get-tracked-object n))
                       (file (##get-tracked-file n))
                       (line (##get-tracked-line n)))
                   (let ((size (zen:memory-size obj)))
                     (zen:register-allocation obj (##vector zen:allocations-rank size thread (zen:fix-tracked-file file) line stack))
                     (loop (##fx+ n 1)))))))))))


;;;
;;;; Persist
;;;


(define zen:persist-allocations?
  #f)


(define zen:persisted-allocations
  (##make-table test: ##eq? weak-keys: #t))


(define (zen:persist-allocations)
  (set! zen:persist-allocations? #t)
  (zen:track-allocations))

(define (zen:unpersist-allocations)
  (set! zen:persist-allocations? #f)
  (zen:untrack-allocations))


(define (zen:persist-allocation obj allocation)
  (##table-set! zen:persisted-allocations obj allocation))


(define (zen:persisted-allocations-table)
  zen:persisted-allocations)


(define (zen:persist-tracked)
  #f #; ;; bongo
(
  (declare (not interrupts-enabled))
  (let ((thread (thread-name (##current-thread)))
        (count (##count-tracked)))
    (let loop ((n 0))
         (if (##fx< n count)
             (let ((obj (##get-tracked-object n)))
               (if (##not (zen:memory-permanent? obj))
                   (let ((file (##get-tracked-file n))
                         (line (##get-tracked-line n)))
                     (let ((size (zen:memory-size obj)))
                       (let ((stack (and (>= size 500)
                                         (##continuation-capture
                                           (lambda (cont)
                                             (zen:track-continuation cont zen:*track-depth*))))))
                         (zen:persist-allocation obj (##vector zen:allocations-rank size thread (zen:fix-tracked-file file) line stack))
                         (loop (##fx+ n 1))))))))))))


;;;
;;;; Tracked
;;;


(define (zen:record-tracked)
  (if zen:monitor-allocations?
      (zen:monitor-tracked))
  (if zen:register-allocations?
      (zen:register-tracked))
  (if zen:persist-allocations?
      (zen:persist-tracked)))

(zen:record-tracked-hook-set! zen:record-tracked)


(define (zen:track-allocations)
  #f #; ;; bongo
(
  (##track-allocations)))


(define (zen:untrack-allocations)
  #f #; ;; bongo
(
  (if (and (##not zen:monitor-allocations?)
           (##not zen:register-allocations?)
           (##not zen:persist-allocations?))
      (##untrack-allocations))))


(define (zen:fix-tracked-file file)
  ;; hack around a gambit bug fixed in latest
  (let ((len (##string-length file)))
    (if (and (##fx> len 0)
             (##eq? (##string-ref file (##fx- len 1)) #\"))
        (##substring file 0 (##fx- len 1))
      file)))


(define (zen:allocation-rank allocation)
  (##vector-ref allocation 0))

(define (zen:allocation-size allocation)
  (##vector-ref allocation 1))

(define (zen:allocation-thread allocation)
  (##vector-ref allocation 2))

(define (zen:allocation-file allocation)
  (##vector-ref allocation 3))

(define (zen:allocation-line allocation)
  (##vector-ref allocation 4))

(define (zen:allocation-stack allocation)
  (##vector-ref allocation 5))


(zen:define-macro (%%tracking expr)
  (if zen:kernel-track-memory?
      (case (zen:walk-for)
        ((compile)
         `(c-code #<<end-of-code
___RESULT = ___UPDATE_TRACKED(___ARG1);
end-of-code

            ,expr))
        (else
         expr))
    expr))


(define zen:*track-depth*
  8)


(define (zen:track-depth)
  zen:*track-depth*)

(define (zen:track-depth-set! depth)
  (set! zen:*track-depth* depth))


(define (zen:track-continuation cont depth #!key (filter #f))
  (define (continuation-creator cont)
    (let ((proc (^#continuation-creator cont)))
      (if (and proc (^#closure? proc))
          (^#closure-code proc)
        proc)))
  
  (define (continuation-next-interesting cont)
    (let loop ((current-cont cont))
         (if current-cont
             (if (and (or (^#not filter) (filter current-cont))
                      (let ((creator (continuation-creator current-cont)))
                        (and ;; bongo (^#not (^#eq? creator ##record-tracked))
                             (^#not (^#eq? creator zen:record-tracked))
                             (^#not (^#eq? creator zen:persist-tracked)))))
                 current-cont
               (loop (^#continuation-next current-cont)))
           #f)))
  
  (define (continuation-next-distinct cont creator)
    (let loop ((current-cont (^#continuation-next cont)))
         (if current-cont
             (if (^#eq? creator (continuation-creator current-cont))
                 (loop (^#continuation-next current-cont))
               current-cont)
           #f)))
  
  (define (identify-location locat)
    (let ((container (and locat (^#locat-container locat))))
      (if container
          (let ((filepos (^#position->filepos (^#locat-start-position locat))))
            (let ((line (^#filepos-line filepos))
                  (col (^#filepos-col filepos)))
              (^#list container line col)))
        #f)))
  
  (define (identify cont d)
    (let ((cont (and cont (^#fx< d depth) (continuation-next-interesting cont))))
      (if (^#not cont)
          '()
        (let ((creator (continuation-creator cont))
              (location (identify-location (^#continuation-locat cont))))
          (^#cons (^#list creator location)
                  (identify (continuation-next-distinct cont creator)
                            (^#fx+ d 1)))))))
  
  (identify cont 0)))
