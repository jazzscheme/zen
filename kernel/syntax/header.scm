;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Header
;;;


(include "~~lib/_gambit#.scm")


(declare
  (block)
  (standard-bindings)
  (extended-bindings)
  (not safe))


;;;
;;;; Macro
;;;


;; DANGER : temporary hack until proper primitive exists
(define-runtime-macro (zen:define-macro pattern . rest)
  (define (form-size parms)
    (let loop ((lst parms) (n 1))
      (cond ((pair? lst)
             (let ((parm (car lst)))
               (if (memq parm '(#!optional #!key #!rest))
                 (- n)
                 (loop (cdr lst)
                       (+ n 1)))))
            ((null? lst)
             n)
            (else
             (- n)))))

  (let ((src `(lambda ,(cdr pattern) ,@rest)))
    `(begin
       (##define-macro ,pattern
         ,@rest)
       (##top-cte-add-macro!
        ##interaction-cte
        ',(car pattern)
        (##make-macro-descr
          #f
          ',(form-size (cdr pattern))
          ,src
          #f))
       (zen:register-macro ',(car pattern)
         ,src))))


;;;
;;;; Syntax
;;;


;; DANGER : temporary hack until proper primitive exists
(define-runtime-macro (zen:define-syntax name expander)
  `(begin
     (##define-syntax ,name
       ,expander)
     (##top-cte-add-macro!
      ##interaction-cte
      ',name
      (##make-macro-descr
        #t
        -1
        ,expander
        #f))
     (zen:register-macro ',name
       ,expander)))


;;;
;;;; Synto
;;;


;; DANGER : temporary hack until proper primitive exists
(define-runtime-macro (zen:define-synto pattern . rest)
  (let ((name (##car pattern))
        (variables (##cdr pattern)))
    (let ((expander
            `(lambda (src)
               (##sourcify-deep
                 (let ((src (##source-code src)))
                   ,(let iter ((scan variables))
                         (cond ((##null? scan)
                                (if (##null? (cdr rest))
                                    (##car rest)
                                  (##cons 'begin rest)))
                               ((##symbol? scan)
                                `(let ((,scan (##cdr src)))
                                   ,(iter '())))
                               (else
                                `(let ((src (##cdr src)))
                                   (let ((,(##car scan) (##car src)))
                                     ,(iter (##cdr scan))))))))
                 src))))
      `(begin
         (##define-syntax ,name
                          ,expander)
         (##top-cte-add-macro!
           ##interaction-cte
           ',name
           (##make-macro-descr
             #t
             -1
             ,expander
             #f))
         (zen:register-macro ',name
                              ,expander)))))


;;;
;;;; Subtypes
;;;


(define zen:subtype-vector       (macro-subtype-vector))
(define zen:subtype-pair         (macro-subtype-pair))
(define zen:subtype-ratnum       (macro-subtype-ratnum))
(define zen:subtype-cpxnum       (macro-subtype-cpxnum))
(define zen:subtype-structure    (macro-subtype-structure))
(define zen:subtype-boxvalues    (macro-subtype-boxvalues))
(define zen:subtype-zen          (macro-subtype-jazz))
(define zen:subtype-symbol       (macro-subtype-symbol))
(define zen:subtype-keyword      (macro-subtype-keyword))
(define zen:subtype-continuation (macro-subtype-continuation))
(define zen:subtype-procedure    (macro-subtype-procedure))
(define zen:subtype-string       (macro-subtype-string))
(define zen:subtype-flonum       (macro-subtype-flonum))
(define zen:subtype-bignum       (macro-subtype-bignum))
(define zen:subtype-foreign      (macro-subtype-foreign))
(define zen:subtype-s8vector     (macro-subtype-s8vector))
(define zen:subtype-u8vector     (macro-subtype-u8vector))
(define zen:subtype-s16vector    (macro-subtype-s16vector))
(define zen:subtype-u16vector    (macro-subtype-u16vector))
(define zen:subtype-s32vector    (macro-subtype-s32vector))
(define zen:subtype-u32vector    (macro-subtype-u32vector))
(define zen:subtype-s64vector    (macro-subtype-s64vector))
(define zen:subtype-u64vector    (macro-subtype-u64vector))
(define zen:subtype-f32vector    (macro-subtype-f32vector))
(define zen:subtype-f64vector    (macro-subtype-f64vector))


;;;
;;;; Tags
;;;


(define zen:tag-still 5)
(define zen:tag-permanent 6)


;;;
;;;; Code
;;;


(define (zen:code-cte c)
  (macro-code-cte c))


;;;
;;;; Environment
;;;


(define (zen:rte-up r)
  (macro-rte-up r))


;;;
;;;; Binding
;;;


(define (zen:repl-context-bind val thunk)
  (macro-dynamic-bind repl-context val thunk))


;;;
;;;; Continuation
;;;


(define (zen:continuation-denv cont)
  (macro-continuation-denv cont))


(define (zen:continuation-needs-winding? cont)
  (let ((src (macro-denv-dynwind (macro-thread-denv (macro-current-thread))))
        (dst (macro-denv-dynwind (macro-continuation-denv cont))))
    (##not (or (##eq? src dst)
               (and (##fx= (macro-dynwind-level src) 0)
                    (##fx= (macro-dynwind-level dst) 0))))))


;;;
;;;; Fifo
;;;


(define (zen:fifo->list fifo)
  (macro-fifo->list fifo))


;;;
;;;; Repl
;;;


(define (zen:current-repl-context)
  (macro-current-repl-context))

(define (zen:repl-context-level context)
  (macro-repl-context-level context))

(define (zen:repl-context-depth context)
  (macro-repl-context-depth context))

(define (zen:repl-context-cont context)
  (macro-repl-context-cont context))

(define (zen:repl-context-initial-cont context)
  (macro-repl-context-initial-cont context))

(define (zen:repl-context-prev-level context)
  (macro-repl-context-prev-level context))

(define (zen:repl-context-prev-depth context)
  (macro-repl-context-prev-depth context))

(define (zen:make-repl-context level depth cont initial-cont reason prev-level prev-depth)
  (macro-make-repl-context level depth cont initial-cont reason prev-level prev-depth))


;;;
;;;; Readtable
;;;


(define (zen:readtable-named-char-table rt)
  (macro-readtable-named-char-table rt))

(define (zen:readtable-named-char-table-set! rt nc)
  (macro-readtable-named-char-table-set! rt nc))

(define (zen:readtable-paren-keyword-set! rt key)
  ;; bongo
  (macro-readtable-paren-handler-set! rt key))

(define (zen:readtable-bracket-keyword-set! rt key)
  ;; bongo
  (macro-readtable-bracket-handler-set! rt key))

(define (zen:readtable-brace-keyword-set! rt key)
  ;; bongo
  (macro-readtable-brace-handler-set! rt key))

(define (zen:readtable-start-syntax rt)
  (macro-readtable-start-syntax rt))


;;;
;;;; Readenv
;;;


(define (zen:readenv? obj)
  (macro-readenv? obj))

(define (zen:readenv-port re)
  (macro-readenv-port re))

(define (zen:readenv-wrap re x)
  (macro-readenv-wrap re x))

(define (zen:readenv-container re)
  (macro-readenv-container re))

(define (zen:readenv-container-set! re c)
  (macro-readenv-container-set! re c))

(define (zen:readenv-filepos re)
  (macro-readenv-filepos re))


;;;
;;;; Writeenv
;;;


(define (zen:writeenv? obj)
  (macro-writeenv? obj))

(define (zen:writeenv-port we)
  (macro-writeenv-port we))

(define (zen:writeenv-style we)
  (macro-writeenv-style we))


;;;
;;;; Port
;;;


(define (zen:output-port-width-set! port width)
  (macro-character-port-output-width-set! port (lambda (port) width)))


(define (zen:debug-port-setup-width port)
  (zen:output-port-width-set! port 512))


#;
;; wait as it makes repl commands display really bad
(zen:debug-port-setup-width (repl-output-port))


;;;
;;;; Thread
;;;


(define (zen:thread-active? thread)
  (and (macro-initialized-thread? thread)
       (##not (macro-terminated-thread-given-initialized? thread))
       (macro-started-thread-given-initialized? thread)))


(define (zen:thread-cont thread)
  (macro-thread-cont thread))


(define (zen:btq-owner mutex)
  (macro-btq-owner mutex))


;;;
;;;; Thread With Stack
;;;


(define (zen:header-get obj)
  (##u64vector-ref obj -1))

(define (zen:header-set! obj h)
  (##u64vector-set! obj -1 h))

(define (zen:header bytes subtype tag)
  (+ (* bytes 256) (* subtype 8) tag))


(define (zen:thread-add-stack thread stack-len)
  #f #; ;; bongo
(
  (declare (not interrupts-enabled))
  ;; ensure vector is a still
  (let ((total-len (max 256 stack-len)))
    (let ((stack (make-vector total-len #f)))
      ;; 0 bytes header so the gc doesn't collect the stack
      (zen:header-set! stack (zen:header 0 zen:subtype-vector zen:tag-still))
      (macro-thread-stack-set! thread stack)
      ;; skip 2 32 bit quads for header
      (macro-thread-frame-pointer-set! thread 2)
      (macro-thread-stack-limit-set! thread (##fx+ 2 (##fx* total-len 2)))))))


(define (zen:make-thread-with-stack stack-len thunk name)
  (let ((thread (make-root-thread thunk name)))
    (zen:thread-add-stack thread stack-len)
    thread))


;;;
;;;; Various
;;;


(define (zen:absent-object? obj)
  (##eq? obj (macro-absent-obj)))


(define (zen:unbound-object? obj)
  (##eq? obj #!unbound))
