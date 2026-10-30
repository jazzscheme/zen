;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Zen Core
;;;


(unit protected zen/backend/core


;;;
;;;; Continuation
;;;


(declare (proper-tail-calls))


(define (zen:continuation-graft-no-winding cont proc)
  (zen:check-continuation cont 1 (continuation-graft-no-winding cont proc)
    (%%continuation-graft-no-winding cont proc)))


(define (zen:continuation-parent cont)
  (zen:check-continuation cont 1 (continuation-parent cont)
    (%%continuation-parent cont)))


(define (zen:continuation-creator cont)
  (zen:check-continuation cont 1 (continuation-creator cont)
    (%%continuation-creator cont)))


(define (zen:continuation-locat cont)
  (zen:check-continuation cont 1 (continuation-locat cont)
    (%%continuation-locat cont)))


(define (zen:continuation-next cont)
  (zen:check-continuation cont 1 (continuation-next cont)
    (%%continuation-next cont)))


(define (zen:continuation-backtrace cont depth procedure-ignore? module-ignore?)
  (define (continuation-creator cont)
    (let ((proc (%%continuation-creator cont)))
      (if (and proc (%%closure? proc))
          (%%closure-code proc)
        proc)))
  
  (define (continuation-next-interesting cont)
    (let loop ((current-cont cont))
         (if current-cont
             (if (or (and procedure-ignore? (procedure-ignore? (continuation-creator current-cont)))
                     (let ((current-location (%%continuation-locat current-cont)))
                       (and current-location module-ignore? (module-ignore? (%%locat-container current-location)))))
                 (loop (%%continuation-next current-cont))
               current-cont)
           #f)))
  
  (define (continuation-next-distinct cont creator)
    (let loop ((current-cont (%%continuation-next cont)))
         (if current-cont
             (if (%%eq? creator (continuation-creator current-cont))
                 (loop (%%continuation-next current-cont))
               current-cont)
           #f)))
  
  (define (identify-location locat)
    (let ((container (and locat (%%locat-container locat))))
      (if container
          (let ((filepos (%%position->filepos (%%locat-start-position locat))))
            (let ((line (%%filepos-line filepos))
                  (col (%%filepos-col filepos)))
              (%%list container line col)))
        #f)))
  
  (define (identify cont d)
    (let ((cont (and cont (or (%%not depth) (%%fx< d depth)) (continuation-next-interesting cont))))
      (if (%%not cont)
          '()
        (let ((creator (continuation-creator cont))
              (location (identify-location (%%continuation-locat cont))))
          (%%cons (%%list creator location)
                  (identify (continuation-next-distinct cont creator)
                            (%%fx+ d 1)))))))
  
  (identify cont 0))


;;;
;;;; Debug
;;;


;; inspect a Zen object
(define (inspect obj)
  (zen:inspect-object (if (integer? obj) (zen:serial->object obj) obj)))


;; run the message loop
(define (run-loop)
  (let ((get-process (zen:global-ref 'zen.process:current-process))
        (run-loop (zen:global-ref 'zen.process.Process:Process:run-loop)))
    (run-loop (get-process))))


;; resume the message loop
(define (resume)
  (let ((get-process (zen:global-ref 'zen.process:current-process))
        (invoke-resume-loop (zen:global-ref 'zen.process.Process:Process:invoke-resume-loop)))
    (invoke-resume-loop (get-process))))


;; start a scheme repl
(define (start-scheme-repl #!key (select? #t))
  (zen:load-unit 'zen)
  (zen:load-unit 'zen.debuggee)
  ((zen:module-ref 'zen.debuggee 'set-default-context) #f)
  ((zen:module-ref 'zen 'start-repl) readtable: zen:scheme-readtable select?: select?))


;;;
;;;; Devel
;;;


(define (zen:subtype obj)
  (%%subtype obj))


;;;
;;;; Global
;;;


(zen:define-macro (zen:define-global name)
  (let ((getter (%%string->symbol (%%string-append "get-" (%%symbol->string name))))
        (setter (%%string->symbol (%%string-append "set-" (%%symbol->string name))))
        (value (zen:generate-symbol "value")))
    `(begin
       (define ,name
         #f)
       (define (,getter)
         ,name)
       (define (,setter ,value)
         (set! ,name ,value)))))


;;;
;;;; ? and %
;;;


(zen:define-global ?)
(zen:define-global ?a)
(zen:define-global ?b)
(zen:define-global ?c)
(zen:define-global ?d)
(zen:define-global ?e)
(zen:define-global ?f)
(zen:define-global ?g)
(zen:define-global ?h)
(zen:define-global ?i)
(zen:define-global ?j)
(zen:define-global ?k)
(zen:define-global ?l)
(zen:define-global ?m)
(zen:define-global ?n)
(zen:define-global ?o)
(zen:define-global ?p)
(zen:define-global ?q)
(zen:define-global ?r)
(zen:define-global ?s)
(zen:define-global ?t)
(zen:define-global ?u)
(zen:define-global ?v)
(zen:define-global ?w)
(zen:define-global ?x)
(zen:define-global ?y)
(zen:define-global ?z)
(zen:define-global ?0)
(zen:define-global ?1)
(zen:define-global ?2)
(zen:define-global ?3)
(zen:define-global ?4)
(zen:define-global ?5)
(zen:define-global ?6)
(zen:define-global ?7)
(zen:define-global ?8)
(zen:define-global ?9)


(zen:define-global %)
(zen:define-global %a)
(zen:define-global %b)
(zen:define-global %c)
(zen:define-global %d)
(zen:define-global %e)
(zen:define-global %f)
(zen:define-global %g)
(zen:define-global %h)
(zen:define-global %i)
(zen:define-global %j)
(zen:define-global %k)
(zen:define-global %l)
(zen:define-global %m)
(zen:define-global %n)
(zen:define-global %o)
(zen:define-global %p)
(zen:define-global %q)
(zen:define-global %r)
(zen:define-global %s)
(zen:define-global %t)
(zen:define-global %u)
(zen:define-global %v)
(zen:define-global %w)
(zen:define-global %x)
(zen:define-global %y)
(zen:define-global %z)
(zen:define-global %0)
(zen:define-global %1)
(zen:define-global %2)
(zen:define-global %3)
(zen:define-global %4)
(zen:define-global %5)
(zen:define-global %6)
(zen:define-global %7)
(zen:define-global %8)
(zen:define-global %9)


(zen:define-global $)
(zen:define-global $a)
(zen:define-global $b)
(zen:define-global $c)
(zen:define-global $d)
(zen:define-global $e)
(zen:define-global $f)
(zen:define-global $g)
(zen:define-global $h)
(zen:define-global $i)
(zen:define-global $j)
(zen:define-global $k)
(zen:define-global $l)
(zen:define-global $m)
(zen:define-global $n)
(zen:define-global $o)
(zen:define-global $p)
(zen:define-global $q)
(zen:define-global $r)
(zen:define-global $s)
(zen:define-global $t)
(zen:define-global $u)
(zen:define-global $v)
(zen:define-global $w)
(zen:define-global $x)
(zen:define-global $y)
(zen:define-global $z)
(zen:define-global $0)
(zen:define-global $1)
(zen:define-global $2)
(zen:define-global $3)
(zen:define-global $4)
(zen:define-global $5)
(zen:define-global $6)
(zen:define-global $7)
(zen:define-global $8)
(zen:define-global $9)


;;;
;;;; Hook
;;;


(define (zen:get-exception-hook)
  (zen:primordial-exception-handler-hook-ref))

(define (zen:set-exception-hook hook)
  (zen:primordial-exception-handler-hook-set! hook))


(define (zen:invoke-exception-hook hook exc)
  (hook exc zen:thread-end-with-uncaught-exception!))


;;;
;;;; System
;;;


(define (zen:system-exception-hook exc other)
  (zen:repl-exception-handler-hook exc other))


;;;
;;;; Terminal
;;;


(define (zen:set-terminal-title)
  (display "\033]0;Terminal\007" (repl-output-port)))

(define (zen:bring-terminal-to-front)
  (display "\033[5t" (repl-output-port)))

(define (zen:clear-terminal)
  (display "\033[H\033[J" (repl-output-port)))


;;;
;;;; Memory
;;;


(define (zen:gc)
  (%%gc))


(define (zen:gc-count)
  (%%flonum->fixnum (f64vector-ref (%%process-statistics) 6)))


(define (zen:gc-statistics)
  (let ((vec (%%process-statistics)))
    (values (f64vector-ref vec 3)
            (f64vector-ref vec 4)
            (f64vector-ref vec 5)
            (%%flonum->fixnum (f64vector-ref vec 6)))))


(define (zen:add-gc-interrupt-job! thunk)
  (%%add-gc-interrupt-job! thunk))


(define (zen:clear-gc-interrupt-jobs!)
  (%%clear-gc-interrupt-jobs!))


(define (zen:last-gc-real-time)
  (f64vector-ref (%%process-statistics) 14))


;;;
;;;; Heap
;;;


(define (zen:get-min-heap)
  (%%get-min-heap))

(define (zen:set-min-heap! bytes)
  (%%set-min-heap! bytes))

(define (zen:get-max-heap)
  (%%get-max-heap))

(define (zen:set-max-heap! bytes)
  (%%set-max-heap! bytes))


(define (zen:process-memory)
  (let ((vec (%%process-statistics)))
    (let ((last_gc_heap_size (f64vector-ref vec 15))
          (last_gc_live      (f64vector-ref vec 17))
          (last_gc_movable   (f64vector-ref vec 18))
          (last_gc_still     (f64vector-ref vec 19)))
      (values last_gc_heap_size
              last_gc_live
              last_gc_movable
              last_gc_still))))


(define (zen:symbols-memory)
  (let ((count 0)
        (chars 0))
    (for-each (lambda (lst)
                (set! count (%%fx+ count (%%length lst)))
                (for-each (lambda (s)
                            (set! chars (%%fx+ chars (%%string-length (%%symbol->string s)))))
                          lst))
              (map (lambda (s)
                     (let loop ((s s) (lst '()))
                          (if (%%symbol? s)
                              (loop (%%vector-ref s 2) (%%cons s lst))
                            (%%reverse lst))))
                   (%%vector->list (%%symbol-table))))
    (values count chars)))


(define (zen:symbol-table)
  (%%symbol-table))

(define (zen:keyword-table)
  (%%keyword-table))


(define (zen:bytes-allocated! floats i)
  (%%get-bytes-allocated! floats i))


(define (zen:get-live-percent)
  (%%get-live-percent))


(define (zen:raise-heap-overflow-exception)
  (%%raise-heap-overflow-exception))


;;;
;;;; Still
;;;


(define zen:still8-minimum
  2048)

(define zen:still16-minimum
  1024)

(define zen:still32-minimum
  512)

(define zen:still64-minimum
  256)


;;;
;;;; Classes
;;;


(define (zen:classes-statistics)
  (let ((nb-classes 0) (sz-classes 0)
        (nb-interfaces 0) (sz-interfaces 0)
        (nb-slots 0) (sz-slots 0)
        (nb-methods 0) (sz-methods 0))
    (define (process-class class)
      (set! nb-classes (%%fx+ nb-classes 1))
      (set! sz-classes (%%fx+ sz-classes 
                              (zen:vector-size class)
                              (zen:table-size (%%get-category-fields class))
                              (zen:vector-size (%%get-category-ancestors class))
                              (zen:list-size (%%get-category-descendants class))
                              (zen:list-size (%%get-class-interfaces class))
                              (zen:list-size (%%get-class-instance-slots class))
                              (zen:vector-vector-size (%%get-class-class-table class))
                              (zen:vector-vector-size (%%get-class-interface-table class))))
      (zen:table-iterate (%%get-category-fields class)
        (lambda (name field)
          (cond ((zen:is? field zen:Slot) (process-slot field))
                ((zen:is? field zen:Method) (process-method field)))))
      (for-each process-class (%%get-category-descendants class)))
    
    (define (process-slot slot)
      (set! nb-slots (%%fx+ nb-slots 1))
      (set! sz-slots (%%fx+ sz-slots (zen:vector-size slot))))
    
    (define (process-method method)
      (set! nb-methods (%%fx+ nb-methods 1))
      (set! sz-methods (%%fx+ sz-methods (zen:vector-size method))))
    
    (process-class zen:Object)
    
    (values
      nb-classes sz-classes
      nb-interfaces sz-interfaces
      nb-slots sz-slots
      nb-methods sz-methods)))


;;;
;;;; Sizes
;;;


(define zen:word-bytes   4)
(define zen:f64-bytes    8)
(define zen:pair-bytes  12)
(define zen:table-bytes 32)


(define (zen:vector-size v)
  (%%fx+ zen:word-bytes (%%fx* zen:word-bytes (%%vector-length v))))

(define (zen:safe-vector-size v)
  (if (%%vector? v)
      (zen:vector-size v)
    0))

(define (zen:f64vector-size v)
  (%%fx+ zen:word-bytes (%%fx* zen:f64-bytes (f64vector-length v))))

(define (zen:vector-vector-size v)
  (if (%%not v)
      0
    (%%fx+ (zen:vector-size v)
           (let iter ((n 0) (size 0))
                (if (%%fx< n (%%vector-length v))
                    (let ((v (%%vector-ref v n)))
                      (iter (%%fx+ n 1) (%%fx+ size (if v (zen:vector-size v) 0))))
                  size)))))

(define (zen:list-size l)
  (%%fx* zen:pair-bytes (%%length l)))

(define (zen:table-size t)
  (let ((gcht1 (%%vector-ref t 3))
        (gcht2 (%%vector-ref t 5))
        (floats (%%vector-ref t 4)))
    (%%fx* zen:word-bytes
           (%%fx+
             1 (%%vector-length t)
             ;; 1 (%%vector-length floats)
             (if (%%gc-hash-table? gcht1) (%%fx+ 1 (%%vector-length gcht1)) 0)
             (if (%%gc-hash-table? gcht2) (%%fx+ 1 (%%vector-length gcht2)) 0)))))


;;;
;;;; Features
;;;


(define (zen:cond-expand-features)
  (%%cond-expand-features))


(define (zen:cond-expand-features-set! features)
  (%%cond-expand-features features))


(define (zen:cond-expand-features-add! feature)
  (%%cond-expand-features (append (%%cond-expand-features) (list feature))))


;;;
;;;; Fixnum
;;;


;; not very efficient but have to test for now

(define (zen:fixnum->flonum n)
  (if (%%fixnum? n)
      (%%fixnum->flonum n)
    (zen:type-error n zen:Fixnum)))

(define (zen:flonum->fixnum n)
  (if (%%flonum? n)
      (%%flonum->fixnum n)
    (zen:type-error n zen:Flonum)))


(define (zen:->flonum value)
  (cond ((%%flonum? value)
         value)
        ((%%fixnum? value)
         (%%fixnum->flonum value))
        ((%%ratnum? value)
         (%%ratnum->flonum value))
        (else
         (zen:type-error value zen:Flonum))))


(define (zen:arithmetic-shift-left x y)
  (arithmetic-shift x y))

(define (zen:arithmetic-shift-right x y)
  (arithmetic-shift x (- y)))


;;;
;;;; Flonum
;;;


(define (zen:flalloc)
  (%%flalloc))

(define (zen:flref fl ignore)
  (%%flref fl ignore))

(define (zen:flset! fl ignore val)
  (%%flset! fl ignore val))


;;;
;;;; Number
;;;


(define zen:ratnum?
  ##ratnum?)

(define zen:bignum?
  ##bignum?)


;;;
;;;; Random
;;;


(define zen:random-integer-65536
  (let* ((rs (make-random-source))
         (ri (random-source-make-integers rs)))
    (random-source-randomize! rs)
    (lambda ()
      (ri 65536))))


;;;
;;;; Pathname
;;;


(define (zen:current-directory . rest)
  (if (%%null? rest)
      (let ((dir (current-directory)))
        (zen:pathname-normalize
          (if (and (zen:filesystem-allowed? dir)
                   (zen:pathname-exists? dir))
              dir
            zen:kernel-root)))
    (let ((dir (%%car rest)))
      (current-directory dir))))

(define (zen:with-current-directory dir thunk)
  (parameterize ((current-directory dir))
    (thunk)))


;;;
;;;; Port
;;;


(define (zen:standard-input-port)
  zen:stdin-port)

(define (zen:standard-output-port)
  zen:stdout-port)

(define (zen:standard-error-port)
  zen:stderr-port)


(define (zen:write-string str port)
  (%%write-string str port))


;;;
;;;; Terminal
;;;


(cond-expand
  (windows
   (define (zen:terminal-available?)
     ;; terminal is always available on windows
     #t)
   
   (define (zen:filesystem-authorize-terminal)
     #f))
  (else
   (define zen:cached-unix-controlling-terminal?
     (zen:unspecified))
   
   (define (zen:unix-controlling-terminal?)
     (if (zen:unspecified? zen:cached-unix-controlling-terminal?)
         (set! zen:cached-unix-controlling-terminal?
               (zen:catch-exception-filter
                 os-exception?
                 (lambda (exception)
                   #f)
                 (lambda ()
                   (close-port (open-file "/dev/tty"))
                   #t))))
     zen:cached-unix-controlling-terminal?)
   
   (define (zen:terminal-available?)
     (zen:unix-controlling-terminal?))
   
   (define (zen:filesystem-authorize-terminal)
     (zen:filesystem-authorize "/dev/tty"))))


;;;
;;;; Console
;;;


(define (zen:get-console-port)
  (if (zen:global-bound? 'zen.language.runtime:current-console-port)
      ((zen:global-ref 'zen.language.runtime:current-console-port))
    #f))


;;;
;;;; Readtable
;;;


(define (zen:make-standard-readtable)
  (%%make-standard-readtable))

(define (zen:readtable-copy readtable)
  (%%readtable-copy readtable))

(define (zen:readtable-char-delimiter? readtable c)
  (%%readtable-char-delimiter? readtable c))

(define (zen:readtable-char-delimiter?-set! readtable c delimiter?)
  (%%readtable-char-delimiter?-set! readtable c delimiter?))

(define (zen:readtable-char-handler readtable c)
  (%%readtable-char-handler readtable c))

(define (zen:readtable-char-handler-set! readtable c handler)
  (%%readtable-char-handler-set! readtable c handler))

(define (zen:readtable-char-sharp-handler readtable c)
  (%%readtable-char-sharp-handler readtable c))

(define (zen:readtable-char-sharp-handler-set! readtable c handler)
  (%%readtable-char-sharp-handler-set! readtable c handler))

(define (zen:readtable-char-class-set! readtable c delimiter? handler)
  (%%readtable-char-class-set! readtable c delimiter? handler))

(define (zen:readtable-escaped-char-table readtable)
  (%%readtable-escaped-char-table readtable))

(define (zen:readtable-escaped-char-table-set! readtable table)
  (%%readtable-escaped-char-table-set! readtable table))


(define (zen:char-symbol char)
  (let ((table (zen:readtable-named-char-table zen:zen-readtable)))
    (let ((res (zen:rassq char table)))
      (and res (%%car res)))))


;;;
;;;; General
;;;


(define (zen:eof-object)
  #!eof)

(define (zen:read-proper-line port)
  (let ((line (read-line port #\newline #t)))
    (if (eof-object? line)
        (values #f #f)
      (let ((len (%%string-length line)))
        (if (and (%%fx> len 0) (%%eqv? (%%string-ref line (%%fx- len 1)) #\newline))
            (values (%%string-shrink! line (%%fx- len 1)) #t)
          (values line #f))))))

(define (zen:with-readtable readtable thunk)
  (parameterize ((current-readtable readtable))
    (thunk)))


(define (zen:six-types)
  (%%six-types-ref))

(define (zen:six-types-set! lst)
  (%%six-types-set! lst))

(define (zen:print-marker)
  #f)


(define (zen:skip-whitespace port)
  (%%while (char-whitespace? (peek-char port))
    (read-char port)))


(define (zen:read-delimited port delimiter)
  (let ((queue (zen:new-queue)))
    (zen:skip-whitespace port)
    (%%while (%%not (%%eqv? (peek-char port) delimiter))
      (zen:enqueue queue (read port))
      (zen:skip-whitespace port))
    (read-char port)
    (zen:queue-list queue)))


(define (zen:read-until test port)
  (let ((expr '())
        (queue (zen:new-queue))
        (done? #f))
    (%%while (%%not done?)
      (let ((expr (read port)))
        (if (test expr)
            (set! done? #t)
          (zen:enqueue queue expr))))
    (zen:queue-list queue)))


(define (zen:read-content port)
  (zen:read-until eof-object? port))


;;;
;;;; Repository
;;;


(define (zen:repository-name repository)
  (%%get-repository-name repository))

(define (zen:repository-title repository)
  (%%symbol->string (%%get-repository-name repository)))

(define (zen:repository-directory repository)
  (%%get-repository-directory repository))

(define (zen:repository-binary? repository)
  (%%get-repository-binary? repository))


;;;
;;;; Package
;;;


(define (zen:package-repository package)
  (%%get-package-repository package))

(define (zen:package-name package)
  (%%get-package-name package))

(define (zen:package-products package)
  (%%get-package-products package))

(define (zen:package-profiles package)
  (%%get-package-profiles package))

(define (zen:package-profiles-set! package profiles)
  (%%set-package-profiles package profiles))

(define (zen:package-project package)
  (%%get-package-project package))

(define (zen:package-title package)
  (%%get-package-title package))

(define (zen:package-description package)
  (%%get-package-description package))

(define (zen:package-authors package)
  (%%get-package-authors package))

(define (zen:package-stage package)
  (%%get-package-stage package))


;;;
;;;; Resource
;;;


(define (zen:resource-repository resource)
  (%%get-resource-repository resource))

(define (zen:resource-path resource)
  (%%get-resource-path resource))

(define (zen:resource-extension resource)
  (%%get-resource-extension resource))


;;;
;;;; Stack
;;;


(define (zen:procedure-name procedure)
  (zen:check-procedure procedure 1 (procedure-name procedure)
    (%%procedure-name procedure)))

(define (zen:procedure-name-set! proc)
  (set! zen:procedure-name proc))

(define (zen:procedure-locat procedure)
  (zen:check-procedure procedure 1 (procedure-locat procedure)
    (%%procedure-locat procedure)))


(define (zen:closure? obj)
  (and (%%procedure? obj)
       (%%closure? obj)))

(define (zen:closure-code closure)
  (zen:check-closure closure 1 (closure-code closure)
    (%%closure-code closure)))

(define (zen:closure-length closure)
  (zen:check-closure closure 1 (closure-length closure)
    (%%closure-length closure)))

(define (zen:closure-ref closure n)
  (zen:check-closure closure 1 (closure-ref closure)
    (%%closure-ref closure n)))


(define (zen:get-continuation-location cont)
  (zen:locat->container/line/col (%%continuation-locat cont)))


(define (zen:interpreted-continuation? cont)
  (%%interp-continuation? cont))


(define (zen:get-continuation-stack cont depth)
  (let ((queue (zen:new-queue)))
    (zen:iterate-continuation-stack cont depth
      (lambda (cont)
        (zen:enqueue queue cont)))
    (zen:queue-list queue)))


(define (zen:get-continuation-dynamic-environment cont)
  (let ((queue (zen:new-queue)))
    (zen:iterate-continuation-dynamic-environment cont zen:reference-name
      (lambda (info)
        (zen:enqueue queue info)))
    (zen:queue-list queue)))


(define (zen:get-continuation-lexical-environment cont)
  (let ((queue (zen:new-queue)))
    (zen:iterate-continuation-lexical-environment cont
      (lambda (info)
        (zen:enqueue queue info)))
    (zen:queue-list queue)))


(define (zen:with-repl-context cont thunk)
  (let ((prev-context (%%thread-repl-context-get!)))
    (let ((context
            (zen:make-repl-context
              (%%fx+ (zen:repl-context-level prev-context) 1)
              0
              cont
              cont
              #f
              prev-context
              #f)))
      (zen:repl-context-bind
        context
        thunk))))


(define (zen:repl #!optional (write-reason #f))
  (begin
    (%%repl write-reason)
    #f))


(define (zen:repl-debug #!optional (write-reason #f) (reason #f) (toplevel? #f))
  (begin
    (%%repl-debug write-reason reason toplevel?)
    #f))


(define (zen:repl-result-history-add result)
  (let ((channel (%%thread-repl-channel-get! (%%current-thread))))
    (%%repl-channel-result-history-add channel result)))


;;;
;;;; Debugging
;;;


(define (zen:inspect-repl-context context)
  `(:repl-context
    ,(zen:repl-context-level context)
    ,(zen:repl-context-depth context)
    ,(zen:repl-context-cont context)
    ,(zen:repl-context-initial-cont context)
    ,(zen:repl-context-prev-level context)
    ,(zen:repl-context-prev-depth context)))


;;;
;;;; Structure
;;;


(define (zen:kind? obj)
  (%%type? obj))

(define (zen:kind-id type)
  (%%type-id type))

(define (zen:kind-name type)
  (%%type-name type))

(define (zen:kind-flags type)
  (%%type-flags type))

(define (zen:kind-super type)
  (%%type-super type))

(define (zen:kind-length type)
  (%%type-field-count type))

(define (zen:kind-fields type)
  (let loop ((index 1)
             (fields (%%type-all-fields type))
             (alist '()))
       (if (%%pair? fields)
           (let* ((name (%%car fields))
                  (rest (%%cdr fields))
                  (options (%%car rest))
                  (rest (%%cdr rest))
                  (val (%%car rest))
                  (rest (%%cdr rest)))
             (loop (%%fx+ index 1)
                   rest
                   (%%cons (%%list name index options val)
                           alist)))
         (zen:reverse! alist))))


(define (zen:structure? obj)
  (%%structure? obj))

(define (zen:structure-kind obj)
  (%%structure-type obj))

(define (zen:structure-ref obj i type)
  (%%structure-ref obj i type #f))

(define (zen:structure-set! obj val i type)
  (%%structure-set! obj val i type #f))


;;;
;;;; Process
;;;


(define (zen:switch? arg)
  (and (%%fx> (%%string-length arg) 0)
       (%%eqv? (%%string-ref arg 0) #\-)))


(define (zen:switch-name arg)
  (let ((len (%%string-length arg)))
    (let ((start (if (and (%%fx>= len 2) (%%equal? (%%substring arg 0 2) "--"))
                     2
                   1)))
      (%%substring arg start len))))


(define (zen:command-argument name #!key (error? #t))
  (define (skip-scripts arguments)
    (let iter ((arguments arguments))
      (if (or (%%null? arguments)
              (zen:switch? (%%car arguments)))
          arguments
        (iter (%%cdr arguments)))))
  
  (if (eq? zen:image 'executable)
      (let ((all (zen:command-arguments)))
        (let iter ((arguments (skip-scripts all)))
          (if (%%null? arguments)
              #f
            (let ((arg (%%car arguments)))
              (cond ((%%member (zen:switch-name arg) zen:kernel-runtime-switches)
                     (iter (%%cdr arguments)))
                    ((or (%%not (zen:switch? arg))
                         (%%null? (%%cdr arguments)))
                     (if error?
                         (zen:error "Unable to parse command line: {a}" all)
                       #f))
                    ((%%equal? name (zen:switch-name arg))
                     (%%cadr arguments))
                    (else
                     (iter (%%cddr arguments))))))))
    #f))


(define (zen:command-argument? name)
  (if (eq? zen:image 'executable)
      (let ((all (zen:command-arguments)))
        (let iter ((arguments all))
          (if (%%null? arguments)
              #f
            (let ((arg (%%car arguments)))
              (if (and (zen:switch? arg)
                       (%%equal? name (zen:switch-name arg)))
                  #t
                (iter (%%cdr arguments)))))))
    #f))


;;;
;;;; Serialized
;;;


(define-type serialized
  id: 16E8E4BA-FD59-4E7A-A219-DFE934B6CC18

  (class read-only:)
  (content read-only:))


;;;
;;;; Table
;;;


(define (zen:table-clear table key)
  (%%debug-assert (%%table? table)
    (%%table-clear table key)))


(define (zen:map-table proc table)
  (%%debug-assert (%%table? table)
    (let ((queue (zen:new-queue)))
      (zen:table-iterate table
        (lambda (key value)
          (zen:enqueue queue (proc key value))))
      (zen:queue-list queue))))


(define (zen:list->table alist #!key (test equal?))
  (%%list->table alist test: test))


(define (zen:table->list table)
  (%%debug-assert (%%table? table)
    (%%table->list table)))


;;;
;;;; Interrupts
;;;


(define (zen:interrupt-vector-set! code handler)
  (%%interrupt-vector-set! code handler))


(define zen:*interrupts-enabled?*
  #t)

(define (zen:interrupts-enabled?)
  zen:*interrupts-enabled?*)

(define (zen:disable-interrupts!)
  (%%disable-interrupts!)
  (set! zen:*interrupts-enabled?* #f))

(define (zen:enable-interrupts!)
  (%%enable-interrupts!)
  (set! zen:*interrupts-enabled?* #t))


;;;
;;;; Heartbeat
;;;


(define zen:get-heartbeat-interval
  (let ((vec (%%f64vector 0.)))
    (lambda ()
      (declare (not interrupts-enabled))
      (%%get-heartbeat-interval! vec 0)
      (%%f64vector-ref vec 0))))

(define (zen:set-heartbeat-interval! seconds)
  (%%set-heartbeat-interval! seconds))


;;;
;;;; Thread
;;;


(define (zen:thread-int! thread thunk)
  (if (%%not (zen:thread-active? thread))
      (zen:error "Thread is not active")
    (##thread-int! thread thunk)))


(define (zen:thread-state-active? state)
  (or (thread-state-running? state)
      (thread-state-waiting? state)))


(define (zen:thread-continuation thread)
  (declare (not interrupts-enabled))
  (if (zen:thread-state-active? (thread-state thread))
      (let ((cont (zen:thread-cont thread)))
        ;; hack - gambit thread init with dummy continuation
        (if (%%not (%%eq? (%%vector-ref cont 0) #!void))
            cont
          #f))
    #f))


(define (zen:thread-mutexes thread)
  (declare (not interrupts-enabled))
  (let ((mutexes '()))
    (let iter ((mutex (^#vector-ref thread 2)) (mutexes '()))
         (if (%%eq? mutex thread)
             mutexes
           (iter (^#vector-ref mutex 2) (cons mutex mutexes))))))


;;;
;;;; Time
;;;


(define zen:current-systime
  current-time)

(define (zen:current-seconds! floats i)
  (%%get-current-time! floats i))

(define zen:systime?
  time?)

(define zen:systime->seconds
  time->seconds)

(define zen:seconds->systime
  seconds->time)


;; adapted from ##display-gc-report
(define (zen:present-absolute-seconds secs)
  (declare (not safe))
  
  (define (scale x m)
    (##flonum->exact-int (##flround (##fl* x m))))
  
  (define (show x*1000 unit)
    (define (decimals d)
      (let* ((n (##round (##/ x*1000 (##expt 10 (##fx- 3 d)))))
             (n-str (##number->string n 10))
             (n-str-len (##string-length n-str))
             (str (if (##fx< n-str-len d)
                      (##string-append
                        (##make-string (##fx- d n-str-len) #\0)
                        n-str)
                    n-str))
             (len (##string-length str))
             (split (##fx- len d)))
        (##string-append
          (if (##fx= d 0)
              str
            (##string-append (##substring str 0 split)
              "."
              (##substring str split len)))
          unit)))
    
    (cond ((##< x*1000 10000)
           (decimals 1))
          (else
           (decimals 0))))
  
  (let ((us (scale secs 1.0e9)))
    (if (##< us 1000000)
        (show us "us")
      (let ((ms (scale secs 1.0e6)))
        (if (##< ms 1000000)
            (show ms "ms")
          (let ((s (scale secs 1.0e3)))
            (show s "s")))))))


(define (zen:present-seconds secs)
  (if (>= secs 0.)
      (zen:present-absolute-seconds secs)
    (%%string-append "-" (zen:present-absolute-seconds (- secs)))))


;; adapted from ##display-gc-report
(define (zen:present-bytes bytes)
  (declare (not safe))
  
  (define (scale x m)
    (##flonum->exact-int (##flround (##fl* x m))))
  
  (define (show x*1000 unit)
    (define (decimals d)
      (let* ((n (##round (##/ x*1000 (##expt 10 (##fx- 3 d)))))
             (n-str (##number->string n 10))
             (n-str-len (##string-length n-str))
             (str (if (##fx< n-str-len d)
                      (##string-append
                        (##make-string (##fx- d n-str-len) #\0)
                        n-str)
                    n-str))
             (len (##string-length str))
             (split (##fx- len d)))
        (##string-append
          (if (##fx= d 0)
              str
            (##string-append (##substring str 0 split)
              "."
              (##substring str split len)))
          unit)))
    
    (cond ((##< x*1000 10000)
           (decimals 1))
          (else
           (decimals 0))))
  
  (if (##< bytes 1024)
      (##string-append (##number->string bytes) "B")
    (let ((bytes (##exact->inexact bytes)))
      (let ((k (scale bytes 9.765625e-1)))
        (if (##< k 1024000)
            (show k "K")
          (let ((m (scale bytes 9.5367431640625e-4)))
            (if (##< m 1024000)
                (show m "M")
              (let ((g (scale bytes 9.313225746154785e-7)))
                (show g "G")))))))))


;;;
;;;; UDP
;;;


(define (zen:udp-socket-tos port)
  #f #; ;; bongo
  (##udp-socket-tos port))

(define (zen:udp-socket-tos-set! port size)
  #f #; ;; Bongo
  (##udp-socket-tos-set! port size))

(define (zen:udp-socket-receive-buffer-size port)
  #f #; ;; Bongo
  (##udp-socket-receive-buffer-size port))

(define (zen:udp-socket-receive-buffer-size-set! port size)
  #f #; ;; Bongo
  (##udp-socket-receive-buffer-size-set! port size))

(define (zen:udp-socket-send-buffer-size port)
  #f #; ;; Bongo
  (##udp-socket-send-buffer-size port))

(define (zen:udp-socket-send-buffer-size-set! port size)
  #f #; ;; Bongo
  (##udp-socket-send-buffer-size-set! port size))

(define (zen:udp-socket-send-again-count port)
  #f #; ;; Bongo
  (##udp-socket-send-again-count port)))
