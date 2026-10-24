;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Internal
;;;


(block kernel.internal


(declare (not safe))


;;;
;;;; Closure
;;;


(define (zen:closure-environment closure)
  ;; to do interpreted
  (if (%%interp-procedure? closure)
      '()
    (let ((len (%%closure-length closure)))
      (let iter ((n 1) (env '()))
           (if (%%fx>= n len)
               env
             (iter (%%fx+ n 1) (%%cons (%%closure-ref closure n) env)))))))


;;;
;;;; Code
;;;


(define-macro (macro-code-run-fixed c rte)
  `(%%danger macro-code-run-fixed
     (let (($$code ,c))
       ((##vector-ref $$code 1) $$code ,rte))))

(define (zen:code-run c rte)
  (macro-code-run-fixed c rte))


;;;
;;;; Libraries
;;;


(cond-expand
  (mac
   (define (zen:use-dlclose)
     #f
     ;; bongo
     #;
     (##use-dlclose)))
  (else))


;;;
;;;; Compiler
;;;


(define (zen:compiler-present?)
  ;; bongo
  (zen:global-bound? '##gambuild)
  #;
  (zen:global-bound? '##gambcomp))


;;;
;;;; Devel
;;;


(define (zen:set-gambitdir! dir)
  (zen:check-string dir 1 (zen:set-gambitdir! dir)
    (##set-gambitdir! dir)))


;;;
;;;; Eval
;;;


(define (zen:allow-inner-global-define?-set! val)
  (##allow-inner-global-define?-set! val))


;; copy-pasted from gambit's repl and changed to no-windind
(define (eval-within-no-winding runner src cont repl-context receiver)
  (define (run c rte)
    (%%continuation-graft-no-winding
      cont
      (lambda ()
        (zen:repl-context-bind
          repl-context
          (lambda ()
            (receiver
              (let ((rte rte))
                (runner
                  (lambda ()
                    (zen:code-run c rte))))))))))
  
  (##define-macro (macro-make-rte-from-list rte lst)
    `(%%list->vector (%%cons ,rte ,lst)))
  
  (%%danger eval-within-no-winding
    (let ((src2 (%%sourcify src (%%make-source #f #f))))
      (cond ((%%interp-continuation? cont)
             (let* (($code (##interp-continuation-code cont))
                    (cte (zen:code-cte $code))
                    (rte (##interp-continuation-rte cont)))
               (run (##compile-inner cte src2) rte)))
            ((##with-no-result-expected-toplevel-continuation? cont)
             (run (##compile-top ##interaction-cte src2) #f))
            (else
             (let* ((locals (##continuation-locals cont))
                    (cte (##cte-frame (##cte-top-cte ##interaction-cte)
                                      (##map ##car locals)))
                    (rte (macro-make-rte-from-list #f (##map ##cdr locals))))
               (run (##compile-inner cte src2) rte)))))))


(define (zen:eval-within-no-winding runner expr cont)
  (continuation-capture
    (lambda (return)
      (eval-within-no-winding
        runner
        expr
        cont
        (zen:current-repl-context)
        (lambda (results)
          (call-with-values
            (lambda ()
              results)
            (lambda results
              (%%continuation-return-no-winding return (%%car results)))))))))


;;;
;;;; Exception
;;;


(define (zen:primordial-exception-handler-hook-ref)
  ##primordial-exception-handler-hook)

(define (zen:primordial-exception-handler-hook-set! hook)
  (zen:check-procedure hook 1 (zen:primordial-exception-handler-hook-set! hook)
    (set! ##primordial-exception-handler-hook hook)))

(define (zen:thread-end-with-uncaught-exception! exc)
  (##thread-end-with-uncaught-exception! exc))


(define (zen:repl-exception-handler-hook exc other)
  (##repl-exception-handler-hook exc other))


;;;
;;;; Exit
;;;


(define (zen:exit #!optional (status 0))
  (zen:check-fixnum status 1 (zen:exit status)
    (##exit status)))

(define (zen:exit-no-jobs #!optional (status 1))
  (zen:check-fixnum status 1 (zen:exit-no-jobs status)
    (begin
      (##clear-exit-jobs!)
      (##exit status))))

(define (zen:exit-cleanup)
  (##exit-cleanup))


;;;
;;;; File
;;;


(define zen:os-bat-extension-string-saved
  ##os-bat-extension-string-saved)


;;;
;;;; Gambit
;;;


(define (zen:gambitzen?)
  (and (zen:global-bound? '##gambitzen?)
       (zen:global-ref '##gambitzen?)))


;;;
;;;; Heartbeat
;;;


(define zen:thread-heartbeat!
  ##thread-heartbeat!)


;;;
;;;; Job
;;;


(define (zen:exit-jobs)
  ##exit-jobs)

(define (zen:add-exit-job! thunk)
  (zen:check-procedure thunk 1 (zen:add-exit-job! thunk)
    (##add-exit-job! thunk)))

(define (zen:clear-exit-jobs!)
  (##clear-exit-jobs!))


;;;
;;;; Module
;;;


(define (zen:load-required-module module-ref)
  ;; bongo
  (##load-module module-ref)
  #;
  (##load-required-module module-ref))

(define (zen:load-object-file pathname quiet?)
  (zen:check-string pathname 1 (zen:load-object-file pathname quiet?)
    (let ((linker-name (##path-strip-directory pathname)))
      (##load-object-file pathname linker-name quiet?))))

(define (zen:object-file-module-descrs result rank)
  (%%danger zen:object-file-module-descrs
    (##vector-ref result rank)))

(define (zen:register-module-descrs-and-load! module-descrs)
  ;; bongo
  (zen:error "unimplemented")
  #;
  (##register-module-descrs-and-load! module-descrs))


(define (zen:main-set! proc)
  (zen:check-procedure proc 1 (zen:main-set! proc)
    (##main-set! proc)))


;;;
;;;; Port
;;;


(define zen:stdin-port
  ##stdin-port)

(define zen:stdout-port
  ##stdout-port)

(define zen:stderr-port
  ##stderr-port)


;;;
;;;; Repl
;;;


(define (zen:repl-debug-main)
  (##repl-debug-main))


;;;
;;;; Source
;;;


(define zen:source1-marker
  ##source1-marker)

(define zen:source2-marker
  ##source2-marker)


(define zen:wrap-datum
  ##wrap-datum)

(define zen:unwrap-datum
  ##unwrap-datum)


(define (zen:wrap-datum-set! proc)
  (zen:check-procedure proc 1 (zen:wrap-datum-set! proc)
    (set! ##wrap-datum proc)))

(define (zen:unwrap-datum-set! proc)
  (zen:check-procedure proc 1 (zen:unwrap-datum-set! proc)
    (set! ##unwrap-datum proc)))

(define (zen:sourcify-aux1-set! proc)
  (zen:check-procedure proc 1 (zen:sourcify-aux1-set! proc)
    (set! ##sourcify-aux1 proc)))

(define (zen:sourcify-aux2-set! proc)
  (zen:check-procedure proc 1 (zen:sourcify-aux2-set! proc)
    (set! ##sourcify-aux2 proc)))


(define (zen:expand-source-set! proc)
  (zen:check-procedure proc 1 (zen:expand-source-set! proc)
    (##expand-source-set! proc)))


;;;
;;;; Stack
;;;


(define zen:hidden-frames
  (%%list
    ##dynamic-env-bind))

(define (zen:hidden-frame? frame)
  (%%memq frame zen:hidden-frames))

(define (zen:hidden-frame?-set! predicate)
  (set! zen:hidden-frame? predicate))


(define (zen:iterate-continuation-stack cont depth proc)
  (let iter ((d 0)
             (cont cont))
       (if (or (%%not depth) (%%fx< d depth))
           (and cont
                (begin
                  (proc cont)
                  (iter (%%fx+ d 1)
                        (%%continuation-next-frame cont #f)))))))


(define (zen:iterate-var-val var val-or-box cte proc)
  (%%danger zen:iterate-var-val
    (cond ((##var-i? var)
           (zen:iterate-var-val-aux (##var-i-name var)
                                     val-or-box
                                     #t
                                     cte
                                     proc))
          ((##var-c-boxed? var)
           (zen:iterate-var-val-aux (##var-c-name var)
                                     (%%unbox val-or-box)
                                     #t
                                     cte
                                     proc))
          (else
           (zen:iterate-var-val-aux (##var-c-name var)
                                     val-or-box
                                     #f
                                     cte
                                     proc)))))


(define (zen:iterate-var-val-aux var val mutable? cte proc)
  (define (remove-quote val)
    (if (and (pair? val)
             (eq? (%%car val) 'quote)
             (%%not (%%null? (%%cdr val))))
        (%%cadr val)
      val))
  
  (%%danger zen:iterate-var-val-aux
    (proc
      (%%list (%%object->string var)
              (cond ((zen:absent-object? val)
                     '<absent>)
                    ((zen:unbound-object? val)
                     '<unbound>)
                    ((%%procedure? val)
                     (remove-quote
                       (if (##cte-top? cte)
                           (##inverse-eval-in-env val cte)
                         (##inverse-eval-in-env val (##cte-parent-cte cte)))))
                    (else
                     val))
              mutable?))))


(define (zen:iterate-continuation-dynamic-environment cont reference-name proc)
  (define (iterate-parameters lst cte proc)
    (let iter ((lst lst))
         (if (%%pair? lst)
             (let* ((param-val (%%car lst))
                    (param (%%car param-val))
                    (val (%%cdr param-val)))
               (if (%%not (##hidden-parameter? param))
                   (let ((name (reference-name (##inverse-eval-in-env param cte))))
                     (zen:iterate-var-val-aux (%%list name) val #t cte proc)))
               (iter (%%cdr lst))))))
  
  (%%danger zen:iterate-continuation-dynamic-environment
    (and cont
         (iterate-parameters
           (##dynamic-env->list (zen:continuation-denv cont))
           (if (%%interp-continuation? cont)
               (let (($code (##interp-continuation-code cont)))
                 (zen:code-cte $code))
             ##interaction-cte)
           proc))))


(define (zen:iterate-continuation-lexical-environment cont proc)
  (define (iterate-rte cte rte proc)
    (let loop1 ((c cte)
                (r rte))
         (cond ((##cte-top? c))
               ((##cte-frame? c)
                (let loop2 ((vars (##cte-frame-vars c))
                            (vals (%%cdr (%%vector->list r))))
                     (if (%%pair? vars)
                         (let ((var (%%car vars)))
                           (if (%%not (##hidden-local-var? var))
                               (zen:iterate-var-val var (%%car vals) c proc))
                           (loop2 (%%cdr vars)
                                  (%%cdr vals)))
                       (loop1 (##cte-parent-cte c)
                              (zen:rte-up r)))))
               (else
                (loop1 (##cte-parent-cte c)
                       r)))))
  
  (define (iterate-vars lst cte proc)
    (let iter ((lst lst))
         (if (%%pair? lst)
             (let* ((var-val (%%car lst))
                    (var (%%car var-val))
                    (val (%%cdr var-val)))
               (zen:iterate-var-val var val cte proc)
               (iter (%%cdr lst))))))
  
  (define (iterate-locals lst cte proc)
    (and lst
         (iterate-vars lst cte proc)))
  
  (%%danger zen:iterate-continuation-lexical-environment
    (and cont
         (if (%%interp-continuation? cont)
             (let (($code (##interp-continuation-code cont))
                   (rte (##interp-continuation-rte cont)))
               (iterate-rte (zen:code-cte $code) rte proc)
               (zen:code-cte $code))
           (begin
             (iterate-locals (%%continuation-locals cont) ##interaction-cte proc)
             ##interaction-cte)))))


;;;
;;;; Step
;;;


(define (zen:install-step-handler step-level proc)
  (define (handler leapable? $code rte execute-body . other)
    (##step-off)
    (zen:process-step
      proc
      $code
      (lambda ()
        (%%apply execute-body (%%cons $code (%%cons rte other))))))
  
  (%%danger zen:install-step-handler
    (let ((cs (##current-stepper))
          (handlers (vector #f #f #f #f #f #f #f)))
      (let loop ((n 0))
           (if (< n step-level)
               (begin
                 (vector-set! handlers n handler)
                 (loop (+ n 1)))))
      (vector-set! cs 0 handlers)
      (void))))


(define (zen:process-step proc $code execute)
  (%%danger zen:process-step
    (proc
      (##code-locat $code)
      (lambda (cmd)
        (case cmd
          ((step)
           (##step-on)
           (execute))
          ((leap) ;; does this really work????
           (let ((result (execute)))
             (##step-on)
             result))
          ((continue)
           (execute)))))))


;;;
;;;; Symbol
;;;


(define (zen:set-gensym-counter! val)
  (zen:check-fixnum val 1 (zen:set-gensym-counter! val)
    (set! ##gensym-counter val)))


;;;
;;;; Timeout
;;;


;; copied from _repl
(define (zen:write-timeout to moment port)
  (%%write-string " " port)
  (let* ((expiry (%%fl- to moment))
         (e (%%fl/ (%%flround (%%fl* 10.0 expiry)) 10.0)))
    (%%write (if (%%integer? e) (%%inexact->exact e) e) port))
  (%%write-string "s" port))


;;;
;;;; Unspecified
;;;


(define zen:Unspecified-Value
  (void))


(zen:define-macro (%%unspecified)
  'zen:Unspecified-Value)


(zen:define-macro (%%unspecified? value)
  `(%%eq? ,value zen:Unspecified-Value))


(zen:define-macro (%%specified? value)
  `(%%neq? ,value zen:Unspecified-Value))


;;;
;;;; Values
;;;


(zen:define-macro (%%values-ref values n)
  (if zen:debug-core?
      (%%force-uniqueness (values n)
        `(%%check-values ,values 1 (%%values-ref ,values ,n)
           (if (%%fx< ,n (^#vector-length ,values))
               (^#vector-ref ,values ,n)
             (error "Out of bounds"))))
    `(%%vector-ref ,values ,n)))

(zen:define-macro (%%values-set! values n obj)
  (if zen:debug-core?
      (%%force-uniqueness (values n)
        `(%%check-values ,values 1 (%%values-set! ,values ,n ,obj)
           (if (%%fx< ,n (^#vector-length ,values))
               (^#vector-set! ,values ,n ,obj)
             (error "Out of bounds"))))
    `(%%vector-set! ,values ,n ,obj))))
