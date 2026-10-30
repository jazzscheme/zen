;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Base Runtime
;;;


(block core.base-runtime


;;;
;;;; Boolean
;;;


(define (zen:boolean expr)
  (%%boolean expr))


(define (zen:not-null? expr)
  (%%not-null? expr))


;;;
;;;; Keyword
;;;


(define (zen:keyword? obj)
  (%%keyword? obj))


(define (zen:keyword->string keyword)
  (%%keyword->string keyword))


(define (zen:string->keyword string)
  (%%string->keyword string))


;;;
;;;; List
;;;


(define (zen:find-in proc lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (%%null? scan)
        #f
      (or (proc (%%car scan))
          (iter (%%cdr scan))))))


(define (zen:find-if predicate lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (%%null? scan)
        #f
      (let ((value (%%car scan)))
        (if (predicate value)
            value
          (iter (%%cdr scan)))))))


(define (zen:find-rank element lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst) (rank 0))
       (cond ((%%null? scan)
              #f)
             ((%%eq? (%%car scan) element)
              rank)
             (else
              (iter (%%cdr scan) (%%fx+ rank 1))))))


;; from srfi-1
(define (zen:reverse! lst)
  (let loop ((lst lst) (ans '()))
    (if (%%null? lst)
        ans
      (let ((tail (%%cdr lst)))
        (%%set-cdr! lst ans)
        (loop tail lst)))))


(define (zen:list-copy lst)
  (map (lambda (obj) obj) lst))


(define (zen:last-tail lst)
  (if (%%pair? lst)
      (let iter ((scan lst))
        (let ((tail (%%cdr scan)))
          (cond ((%%pair? tail)
                 (iter tail))
                ((%%null? tail)
                 scan)
                (else
                 tail))))
    lst))


(define (zen:last-pair lst)
  (if (%%pair? lst)
      (let iter ((scan lst))
        (let ((tail (%%cdr scan)))
          (if (%%pair? tail)
              (iter tail)
            scan)))
    lst))


(define (zen:last lst)
  (%%car (zen:last-pair lst)))


(define (zen:remove-duplicates lst)
  (let iter ((scan lst))
    (if (%%not-null? scan)
        (let ((value (%%car scan))
              (result (iter (%%cdr scan))))
          (if (%%memv value result)
              result
            (%%cons value result)))
      '())))


(define (zen:proper-list lst)
  (let iter ((scan lst))
    (if (pair? scan)
        (cons (car scan) (iter (cdr scan)))
      '())))


(define (zen:partition lst key associate)
  (let iter ((scan lst))
    (if (%%null? scan)
        '()
      (let* ((partition (iter (%%cdr scan)))
             (element (%%car scan))
             (category (key element))
             (set (associate category partition)))
        (if (%%not set)
            (%%cons (%%cons category (%%list element)) partition)
          (begin
            (%%set-cdr! set (%%cons element (%%cdr set)))
            partition))))))


(define (zen:rassq obj alist)
  (let iter ((rest alist))
    (cond ((%%null? rest)
           #f)
          ((%%eq? obj (%%cdar rest))
           (%%car rest))
          (else
           (iter (%%cdr rest))))))


(define (zen:butlast lst)
  (if (%%null? (%%cdr lst))
      '()
    (%%cons (%%car lst) (zen:butlast (%%cdr lst)))))


(define (zen:remove! target lst)
  (%%while (and (%%not-null? lst) (%%eqv? target (%%car lst)))
    (set! lst (%%cdr lst)))
  (if (%%null? lst)
      '()
    (begin
      (let ((previous lst)
            (scan (%%cdr lst)))
        (%%while (%%not-null? scan)
          (if (%%eqv? target (%%car scan))
              (begin
                (set! scan (%%cdr scan))
                (%%set-cdr! previous scan))
            (begin
              (set! previous scan)
              (set! scan (%%cdr scan))))))
      lst)))


;;;
;;;; Reader
;;;


(define (zen:read-source-all port #!optional (container #f) (line #f) (col #f))
  (if container
      (%%input-port-names-set! port (lambda (port) container)))
  (if line
      (%%input-port-line-set! port (%%fx+ line 1)))
  (if col
      (%%input-port-column-set! port (%%fx+ col 1)))
  
  (let ((begin-vector
          (%%read-all-as-a-begin-expr-from-port
            port
            (%%current-readtable)
            ##wrap-datum
            ##unwrap-datum
            (zen:readtable-start-syntax (%%current-readtable))
            #f)))
    (%%cdr (%%source-code (%%vector-ref begin-vector 1)))))


(define (zen:read-source-first port #!optional (container #f) (line #f) (col #f))
  (%%car (zen:read-source-all port container line col)))


;;;
;;;; Serial
;;;


(define (zen:object->serial obj)
  (object->serial-number obj))


(define (zen:serial->object number #!optional (default #!void))
  (if (%%eq? default #!void)
      (serial-number->object number)
    (serial-number->object number default)))


;; for debugging
(define (zen:object->serial-symbol obj)
  (%%string->symbol (%%string-append "#" (%%number->string (zen:object->serial obj)))))


;;;
;;;; String
;;;


(define (zen:memstring char string)
  (let ((len (%%string-length string)))
    (let iter ((n 0))
      (cond ((%%fx= n len)
             #f)
            ((%%eqv? (%%string-ref string n) char)
             #t)
            (else
             (iter (%%fx+ n 1)))))))


;;;
;;;; Identifier
;;;


(define (zen:identifier-name identifier)
  (%%assert (%%symbol? identifier)
    (let* ((str (%%symbol->string identifier))
           (pos (zen:string-find-reversed str #\/)))
      (if (%%not pos)
          identifier
        (%%string->symbol (%%substring str (%%fx+ pos 1) (%%string-length str)))))))


;;;
;;;; Reference
;;;


(define (zen:reference-unit reference)
  (%%assert (%%symbol? reference)
    (let* ((str (%%symbol->string reference))
           (pos (or (zen:string-find-reversed str #\:)
                    (zen:string-find-reversed str #\#))))
      (if (%%not pos)
          #f
        (%%string->symbol (%%substring str 0 pos))))))


(define (zen:reference-name reference)
  (%%assert (%%symbol? reference)
    (let* ((str (%%symbol->string reference))
           (pos (or (zen:string-find-reversed str #\:)
                    (zen:string-find-reversed str #\#))))
      (if (%%not pos)
          reference
        (%%string->symbol (%%substring str (%%fx+ pos 1) (%%string-length str)))))))


;;;
;;;; Helper
;;;


(define (zen:compose-helper s1 s2)
  (%%compose-helper s1 s2))


;;;
;;;; Specifier
;;;


(define (zen:specifier? expr)
  (and (%%symbol? expr)
       (let ((str (%%symbol->string expr)))
         (let ((len (%%string-length str)))
           (and (%%fx> len 2)
                (%%eqv? (%%string-ref str 0) #\<)
                (%%eqv? (%%string-ref str (%%fx- len 1)) #\>))))))


(define (zen:specifier->name specifier)
  (let ((extract
          (lambda (string)
            (%%substring string 1 (%%fx- (%%string-length string) 1)))))
    (%%string->symbol (extract (%%symbol->string specifier)))))


(define (zen:name->specifier name)
  (%%string->symbol (%%string-append "<" (%%symbol->string name) ">")))


(define (zen:binding-specifier binding)
  (let ((cdr-binding (%%cdr (zen:source-code binding))))
    (and (%%pair? cdr-binding)
         (zen:specifier? (zen:source-code (%%car cdr-binding)))
         (zen:source-code (%%car cdr-binding)))))


;;;
;;;; Enumerator
;;;


(define (zen:enumerator? obj)
  (and (%%symbol? obj)
       (%%eqv? (%%string-ref (%%symbol->string obj) 0) #\:)))


(define (zen:enumerator->symbol enumerator)
  (let ((name (%%symbol->string enumerator)))
    (%%string->symbol (%%substring name 1 (%%string-length name)))))


(define (zen:symbol->enumerator symbol)
  (%%string->symbol (%%string-append ":" (%%symbol->string symbol))))


;;;
;;;; Source
;;;


(define (zen:source? obj)
  (%%source? obj))


(define (zen:source-code expr)
  (if (%%source? expr)
      (%%source-code expr)
    expr))


(define (zen:source-locat expr)
  (if (%%source? expr)
      (%%source-locat expr)
    #f))


(define (zen:desourcify expr)
  (%%desourcify expr))


(define (zen:desourcify-all expr)
  (define (desourcify-source src)
    (desourcify-all (%%source-code src)))

  (define (desourcify-list lst)
    (cond ((%%pair? lst)
           (%%cons (desourcify-all (%%car lst))
                   (desourcify-list (%%cdr lst))))
          ((%%null? lst)
           '())
          (else
           (desourcify-all lst))))

  (define (desourcify-vector vect)
    (let* ((len (%%vector-length vect))
           (x (%%make-vector len 0)))
      (let loop ((i (%%fx- len 1)))
        (if (%%fx< i 0)
          x
          (begin
            (%%vector-set! x i (desourcify-all (%%vector-ref vect i)))
            (loop (%%fx- i 1)))))))
  
  (define (desourcify-box bx)
    (%%box (desourcify-all (%%unbox bx))))
  
  (define (desourcify-all expr)
    (cond ((%%source? expr)
           (desourcify-source expr))
          ((%%pair? expr)
           (desourcify-list expr))
          ((%%vector? expr)
           (desourcify-vector expr))
          ((%%box? expr)
           (desourcify-box expr))
          (else
           expr)))
  
  (desourcify-all expr))


(define (zen:sourcify expr src)
  (%%sourcify expr src))


(define (zen:sourcify-if expr src)
  (if (zen:source? src)
      (zen:sourcify expr src)
    expr))


(define (zen:sourcify-deep expr src)
  (%%sourcify-deep expr src))


(define (zen:sourcify-deep-if expr src)
  (if (zen:source? src)
      (zen:sourcify-deep expr src)
    expr))


(define (zen:sourcify-list lst src)
  (map (lambda (expr)
         (zen:sourcify-if (zen:desourcify-all expr) src))
       lst))


(define (zen:locat-container locat)
  (%%locat-container locat))

(define (zen:locat-start locat)
  (%%locat-start-position locat))

(define (zen:locat-end locat)
  (%%locat-end-position locat))


(define (zen:locat->container/line/col locat)
  (let ((container (and locat (%%locat-container locat))))
    (if container
        (let ((filepos (%%position->filepos (%%locat-start-position locat))))
          (let ((line (%%filepos-line filepos))
                (col (%%filepos-col filepos)))
            (%%list container line col)))
      #f)))


(define (zen:locat->path/container/start/end locat)
  (let ((container (%%locat-container locat)))
    (let ((path (%%container->path container))
          (startpos (%%position->filepos (%%locat-start-position locat)))
          (endpos (%%position->filepos (%%locat-end-position locat))))
      (%%list path
              container
              (%%filepos-line startpos)
              (%%filepos-col startpos)
              (%%filepos-line endpos)
              (%%filepos-col endpos)))))


(define (zen:container->path container)
  (%%container->path container))


(define (zen:position->filepos position)
  (%%position->filepos position))


(define (zen:filepos-line filepos)
  (%%filepos-line filepos))


(define (zen:filepos-col filepos)
  (%%filepos-col filepos))


;;;
;;;; Emit
;;;


(define zen:save-emit-to
  (make-parameter #f))


(define (zen:save-emit-if emit)
  (%%when (or (zen:save-emit-to) (and (zen:save-emit?) (zen:compiled-source)))
    (parameterize ((current-readtable zen:scheme-readtable))
      (let ((path (or (zen:save-emit-to) (zen:binary-with-extension (zen:compiled-source) ".scm"))))
        (call-with-output-file (list path: path eol-encoding: (zen:platform-eol-encoding zen:kernel-platform))
          (lambda (port)
            (pretty-print (zen:desourcify-all emit) port)))))))


;;;
;;;; Debug
;;;


(define (zen:extract-location src)
  (if (%%not src)
      src
    (let ((locat (%%source-locat src)))
      (let ((start (%%locat-start-position locat))
            (end (%%locat-end-position locat)))
        (%%vector (%%locat-container locat)
                  (%%fx+ (%%filepos-line start) 1)
                  (%%fx+ (%%filepos-col start) 1)
                  (%%fx+ (%%filepos-line end) 1)
                  (%%fx+ (%%filepos-col end) 1))))))


(define (zen:present-source obj)
  (define (present-src src)
    (let ((code (zen:source-code src)))
      (let ((locat (%%source-locat src)))
        (let ((start (%%locat-start-position locat))
              (end (%%locat-end-position locat)))
          (%%vector 'source
                    (zen:present-source code)
                    (%%locat-container locat)
                    (%%fx+ (%%filepos-line start) 1)
                    (%%fx+ (%%filepos-col start) 1)
                    (%%fx+ (%%filepos-line end) 1)
                    (%%fx+ (%%filepos-col end) 1))))))

  (define (present-list lst)
    (cond ((%%pair? lst)
           (%%cons (zen:present-source (%%car lst))
                   (present-list (%%cdr lst))))
          ((%%null? lst)
           '())
          (else
           (zen:present-source lst))))

  (define (present-vector vect)
    (let* ((len (%%vector-length vect))
           (x (%%make-vector len 0)))
      (let loop ((i (%%fx- len 1)))
        (if (%%fx< i 0)
            x
          (begin
            (%%vector-set! x i (zen:present-source (%%vector-ref vect i)))
            (loop (%%fx- i 1)))))))

  (cond ((%%source? obj)
         (present-src obj))
        ((%%pair? obj)
         (present-list obj))
        ((%%vector? obj)
         (present-vector obj))
        (else
         obj)))


;;;
;;;; Unspecified
;;;


(define (zen:unspecified)
  (%%unspecified))


(define (zen:unspecified? expr)
  (%%unspecified? expr))


(define (zen:specified? expr)
  (%%not (%%unspecified? expr)))


;;;
;;;; Values
;;;


(define (zen:values? obj)
  (%%values? obj))


(define (zen:values-ref values n)
  (%%values-ref values n))


(define (zen:values-set! values n obj)
  (%%values-set! values n obj))


;;;
;;;; Vector
;;;


(define (zen:vector-for-each proc vector)
  (let ((len (%%vector-length vector)))
    (let iter ((n 0))
      (if (%%fx< n len)
          (begin
            (proc (%%vector-ref vector n))
            (iter (%%fx+ n 1)))))))


(define (zen:vector-memq? obj vector)
  (let ((len (%%vector-length vector)))
    (let iter ((n 0))
      (if (%%fx< n len)
          (if (%%eq? (%%vector-ref vector n) obj)
              #t
            (iter (%%fx+ n 1)))
        #f))))


(define (zen:vector-memv? obj vector)
  (let ((len (%%vector-length vector)))
    (let iter ((n 0))
      (if (%%fx< n len)
          (if (%%eqv? (%%vector-ref vector n) obj)
              #t
            (iter (%%fx+ n 1)))
        #f))))


(define (zen:resize-vector vector size)
  (let ((new-vector (%%make-vector size #f)))
    (let iter ((offset (%%fx- (min size (%%vector-length vector)) 1)))
      (%%when (%%fx>= offset 0)
        (%%vector-set! new-vector offset (%%vector-ref vector offset))
        (iter (%%fx- offset 1))))
    new-vector)))
