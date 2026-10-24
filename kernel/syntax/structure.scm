;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Structures
;;;


(block kernel.structure


(define zen:structure-info
  (%%make-table test: eq?))


(zen:define-macro (zen:define-structure name ascendant-name options slots)
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
      (values (slot-name slot)
              (slot-getter slot)
              (slot-setter slot))))
  
  (define (slot-name slot)
    (%%car slot))
  
  (let* ((downcase-name (downcase (%%symbol->string name)))
         (constructor (zen:getf options constructor: #f))
         (constructor-class? (zen:getf options constructor-class?: #f))
         (predicate (zen:getf options predicate: #f))
         (accessors-type (zen:getf options accessors-type: 'function))
         (accessors-prefix (case accessors-type ((macro) "%%") (else "zen:")))
         (ascendant-accessor (if (%%null? ascendant-name) #f ascendant-name))
         (inherited-slot-names (if (%%null? ascendant-name) '() (%%table-ref zen:structure-info ascendant-name)))
         (ascendant-size (%%length inherited-slot-names))
         (slot-names (map (lambda (slot) (slot-name (standardize-slot slot))) slots))
         (all-slot-names (%%append inherited-slot-names slot-names))
         (all-length (%%length all-slot-names))
         (record-size (%%fx+ zen:record-offset all-length)))
    (%%table-set! zen:structure-info name all-slot-names)
    `(begin
       ,@(if (%%not constructor)
             '()
           (if constructor-class?
               `((define (,constructor class ,@all-slot-names)
                   (%%class-record class ,@all-slot-names)))
             `((define (,constructor ,@all-slot-names)
                 (%%record ',name ,@all-slot-names)))))
       ,@(if (%%not predicate)
             '()
           `((define (,predicate obj)
               (and (%%record? obj) (%%eq? (%%record-structure-name obj) ',name)))))
       ,@(map (lambda (slot rank)
                (receive (slot-name slot-getter slot-setter) (parse-slot slot accessors-prefix downcase-name)
                  `(begin
                     ,@(cond ((%%not slot-getter)
                              '())
                             (else
                              (case accessors-type
                                ((macro)
                                 `((zen:define-macro (,slot-getter record)
                                     (%%list '%%record-ref record ,rank))))
                                (else
                                 `((define (,slot-getter record)
                                     (%%record-ref record ,rank)))))))
                     ,@(cond ((%%not slot-setter)
                              '())
                             (else
                              (case accessors-type
                                ((macro)
                                 `((zen:define-macro (,slot-setter record value)
                                     (%%list '%%record-set! record ,rank value))))
                                (else
                                 `((define (,slot-setter record value)
                                     (%%record-set! record ,rank value))))))))))
              slots
              (zen:naturals (%%fx+ zen:record-offset ascendant-size) record-size))
       (%%table-set! zen:structure-info ',name ',all-slot-names)))))
