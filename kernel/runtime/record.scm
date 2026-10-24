;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Records
;;;


(block kernel.record


;; costly test for a low probability class of bugs
(define zen:debug-out-of-bounds?
  #f)


;;;
;;;; Structure
;;;


(define zen:structure-offset
  0)

(define zen:structure-marker
  (list 'structure))

(define zen:record-offset
  (%%fx+ zen:structure-offset 1))

(zen:define-macro (%%subtype-zen)
  7)

(zen:define-macro (%%subtype-zenstruct)
  (if (zen:gambitzen?)
      16
    `(error "Unimplemented")))

(zen:define-macro (%%zen-subtype? st)
  `(%%fx= ,st (%%subtype-zen)))

(zen:define-macro (%%zenstruct-subtype? st)
  (if (zen:gambitzen?)
      `(%%fx= ,st (%%subtype-zenstruct))
    #f))


;; this function is here for structure / class unification
(zen:define-macro (%%zen? expr)
  (if (zen:gambitzen?)
      (zen:with-uniqueness expr
        (lambda (expr)
          `(or (^#jazz? ,expr) (^#jazzstruct? ,expr))))
    `(^#jazz? ,expr)))

;; this function is here for structure / class unification
(zen:define-macro (%%zenstruct? expr)
  (if (zen:gambitzen?)
      `(^#jazzstruct? ,expr)
    #f))

;; this function is here for structure / class unification
(zen:define-macro (%%zenify expr)
  (%%force-uniqueness (expr)
    `(%%check-vector ,expr 1 (%%zenify ,expr)
       (^#subtype-set! ,expr (%%subtype-zen)))))

;; this function is here for structure / class unification
(zen:define-macro (%%zenstructify expr)
  (if (zen:gambitzen?)
      (%%force-uniqueness (expr)
        `(%%check-vector ,expr 1 (%%zenstructify ,expr)
           (^#subtype-set! ,expr (%%subtype-zenstruct))))
    `(error "Unimplemented")))


(define (zen:zen? obj)
  (%%zen? obj))

(define (zen:zenify obj)
  (%%zenify obj))

(define (zen:zenstruct? obj)
  (%%zenstruct? obj))

(define (zen:zenstructify obj)
  (%%zenstructify obj))


;;;
;;;; Record
;;;


#; ;; defined as functions until structure / class unification
(zen:define-macro (%%record? expr)
  `(or (%%zen? ,expr)
       (and (%%vector? ,expr)
            (%%fx> (%%vector-length ,expr) 0)
            (%%symbol? (%%vector-ref ,expr 0)))))

#; ;; defined as functions until structure / class unification
(zen:define-macro (%%record structure . rest)
  `(%%zenify (%%vector ,structure ,@rest)))

#; ;; defined as functions until structure / class unification
(zen:define-macro (%%make-record structure size)
  (let ((record (zen:generate-symbol "record")))
    `(let ((,record (%%zenify (%%make-vector ,size (%%unspecified)))))
       (%%set-record-structure ,record ,structure)
       ,record)))


(zen:define-macro (%%record-length record)
  `(%%vector-length ,record))


(zen:define-macro (%%record-ref record n)
  (if zen:debug-core?
      (zen:with-uniqueness record
        (lambda (rec)
          (zen:with-uniqueness n
            (lambda (rnk)
              (if zen:debug-out-of-bounds?
                  `(cond ((%%not (%%record? ,rec))
                          (zen:not-record-error ,rec))
                         ((or (%%not (%%fixnum? ,rnk))
                              (%%not (%%fx< ,rnk (^#vector-length ,rec))))
                          (zen:outside-record-error ,rec ,rnk))
                         (else
                          (^#vector-ref ,rec ,rnk)))
                `(if (%%not (%%record? ,rec))
                     (zen:not-record-error ,rec)
                   (^#vector-ref ,rec ,rnk)))))))
    `(^#vector-ref ,record ,n)))


(zen:define-macro (%%record-set! record n value)
  (if zen:debug-core?
      (zen:with-uniqueness record
        (lambda (rec)
          (zen:with-uniqueness n
            (lambda (rnk)
              (if zen:debug-out-of-bounds?
                  `(cond ((%%not (%%record? ,rec))
                          (zen:not-record-error ,rec))
                         ((or (%%not (%%fixnum? ,rnk))
                              (%%not (%%fx< ,rnk (^#vector-length ,rec))))
                          (zen:outside-record-error ,rec ,rnk))
                         (else
                          (^#vector-set! ,rec ,rnk ,value)))
                `(if (%%not (%%record? ,rec))
                     (zen:not-record-error ,rec)
                   (^#vector-set! ,rec ,rnk ,value)))))))
    `(^#vector-set! ,record ,n ,value)))


(zen:define-macro (%%get-record-structure record)
  `(%%record-ref ,record ,zen:structure-offset))


(zen:define-macro (%%set-record-structure record structure)
  `(%%record-set! ,record ,zen:structure-offset ,structure))


;; defined as functions until structure / class unification
(define (%%record-structure? expr)
  (and (%%pair? expr)
       ;; eq? doesn't work probably due to records read by the reader
       (%%equal? (%%cdr expr) zen:structure-marker)))


;; defined as functions until structure / class unification
(define (%%make-record-structure name)
  (%%cons name zen:structure-marker))


(zen:define-macro (%%record-structure-name expr)
  `(%%car (%%get-record-structure ,expr)))


;; defined as functions until structure / class unification
(define (%%record? expr)
  (or (%%zen? expr)
      (and (%%vector? expr)
           (%%fx> (%%vector-length expr) 0)
           (%%record-structure? (%%vector-ref expr 0)))))


(zen:define-macro (%%record name . rest)
  `(%%vector (%%make-record-structure ,name) ,@rest))


(zen:define-macro (%%class-record class . rest)
  `(%%zenify (%%vector ,class ,@rest)))


(zen:define-macro (%%make-record name size)
  (let ((record (zen:generate-symbol "record")))
    `(let ((,record (%%make-vector ,size (%%unspecified))))
       (%%set-record-structure ,record (%%make-record-structure ,name))
       ,record)))


(define (zen:not-record-error obj)
  (zen:error "Zen record expected: {s}" obj))


(define (zen:outside-record-error obj rnk)
  (zen:error "Invalid access to record outside its bounds: {s}" obj))


(define (zen:record->vector record)
  (let* ((size (%%record-length record))
         (content (%%make-vector size)))
    (let iter ((n 0))
         (if (%%fx< n size)
             (begin
               (%%vector-set! content n (%%record-ref record n))
               (iter (%%fx+ n 1)))))
    content))


;;;
;;;; Object
;;;


(zen:define-macro (%%object? expr)
  `(%%zen? ,expr))


(zen:define-macro (%%object class . rest)
  (zen:with-uniqueness class
    (lambda (cls)
      `(%%zenify (%%vector ,cls ,@rest)))))


(define (zen:new-object class . rest)
  (let ((obj (%%list->vector (%%cons class rest))))
    (%%zenify obj)
    obj))


(zen:define-macro (%%make-object class size)
  (zen:with-uniqueness class
    (lambda (cls)
      (let ((obj (zen:generate-symbol "obj")))
        `(let ((,obj (%%zenify (%%make-vector ,size (%%unspecified)))))
           (%%set-object-class ,obj ,cls)
           ,obj)))))


(zen:define-macro (%%object-length object)
  (if zen:debug-core?
      (zen:with-uniqueness object
        (lambda (obj)
          `(%%core-assertion (%%object? ,obj) (zen:not-object-error ,obj)
             (^#vector-length ,obj))))
    `(^#vector-length ,object)))


(zen:define-macro (%%object-ref object n)
  (if zen:debug-core?
      (zen:with-uniqueness object
        (lambda (obj)
          (zen:with-uniqueness n
            (lambda (rnk)
              (if zen:debug-out-of-bounds?
                  `(%%core-assertion (%%object? ,obj) (zen:not-object-error ,obj)
                     (%%core-assertion (and (%%fixnum? ,rnk) (%%fx< ,rnk (^#vector-length ,obj))) (zen:outside-object-error ,obj ,rnk)
                       (^#vector-ref ,obj ,rnk)))
                `(%%core-assertion (%%object? ,obj) (zen:not-object-error ,obj)
                   (^#vector-ref ,obj ,rnk)))))))
    `(^#vector-ref ,object ,n)))


(zen:define-syntax %%object-set!
  (lambda (src)
    (let ((object (%%cadr (%%source-code src)))
          (n (%%car (%%cddr (%%source-code src))))
          (value (%%cadr (%%cddr (%%source-code src)))))
      (if zen:debug-core?
          (zen:with-uniqueness object
            (lambda (obj)
              (zen:with-uniqueness n
                (lambda (rnk)
                  (if zen:debug-out-of-bounds?
                      `(%%core-assertion (%%object? ,obj) (zen:not-object-error ,obj)
                         (%%core-assertion (and (%%fixnum? ,rnk) (%%fx< ,rnk (^#vector-length ,obj))) (zen:outside-object-error ,obj ,rnk)
                           (^#vector-set! ,obj ,rnk ,value)))
                    `(%%core-assertion (%%object? ,obj) (zen:not-object-error ,obj)
                       (^#vector-set! ,obj ,rnk ,value)))))))
        `(^#vector-set! ,object ,n ,value)))))


(define (zen:not-object-error obj)
  (zen:error "Object expected: {s}" obj))


(define (zen:outside-object-error obj rnk)
  (zen:error "Invalid access to object outside its bounds: {s}" obj)))
