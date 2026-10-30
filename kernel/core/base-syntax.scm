;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Base Syntax
;;;


(block core.base-syntax


;;;
;;;; Boolean
;;;


(zen:define-syntax %%boolean
  (lambda (src)
    (let ((expr (%%cadr (zen:source-code src))))
      `(if ,expr #t #f))))


(zen:define-syntax %%not-null?
  (lambda (src)
    (let ((expr (%%cadr (zen:source-code src))))
      `(%%not (%%null? ,expr)))))


;;;
;;;; When
;;;


(zen:define-syntax %%when
  (lambda (src)
    (let ((test (%%cadr (zen:source-code src)))
          (body (%%cddr (zen:source-code src))))
      `(if ,test
           (begin
             ,@body)
         #f))))


;;;
;;;; Unless
;;;


(zen:define-syntax %%unless
  (lambda (src)
    (let ((test (%%cadr (zen:source-code src)))
          (body (%%cddr (zen:source-code src))))
      `(if (%%not ,test)
           (begin
             ,@body)
         #f))))


;;;
;;;; While
;;;


(zen:define-syntax %%while
  (lambda (src)
    (let ((test (%%cadr (zen:source-code src)))
          (body (%%cddr (zen:source-code src)))
          (iter (zen:generate-symbol "iter")))
      `(let ,iter ()
         (if ,test
             (begin
               ,@body
               (,iter)))))))


;;;
;;;; Assert
;;;


(zen:define-syntax %%core-assert
  (lambda (src)
    (zen:expand-%%assert zen:debug-core? src)))


(zen:define-syntax %%core-assertion
  (lambda (src)
    (zen:expand-%%assertion zen:debug-core? src)))


(zen:define-syntax %%debug-assert
  (lambda (src)
    (zen:expand-%%assert zen:debug-user? src)))


(zen:define-syntax %%debug-assertion
  (lambda (src)
    (zen:expand-%%assertion zen:debug-user? src)))


(zen:define-syntax %%assert
  (lambda (src)
    (zen:expand-%%assert #t src)))


(zen:define-syntax %%assertion
  (lambda (src)
    (zen:expand-%%assertion #t src)))


(define (zen:expand-%%assert test? src)
  (let ((assertion (%%cadr (zen:source-code src)))
        (body (%%cddr (zen:source-code src))))
    (let ((message (let ((port (open-output-string)))
                     (display "Assertion " port)
                     (write (%%desourcify assertion) port)
                     (display " failed" port)
                     (get-output-string port)))
          (effective-body (if (%%null? body) (%%list (%%list '%%unspecified)) body)))
      (zen:expand-%%assertion-body test? assertion (%%list 'error message) effective-body))))


(define (zen:expand-%%assertion test? src)
  (let ((assertion (%%cadr (zen:source-code src)))
        (action (%%car (%%cddr (zen:source-code src))))
        (body (%%cdr (%%cddr (zen:source-code src)))))
    (let ((effective-body (if (%%null? body) (%%list (%%list '%%unspecified)) body)))
      (zen:expand-%%assertion-body test? assertion action effective-body))))


(define (zen:expand-%%assertion-body test? assertion action body)
  (if test?
      `(if (%%not ,assertion)
           ,action
         ,(zen:simplify-begin
            `(begin
               ,@body)))
    (zen:simplify-begin `(begin ,@body))))


;;;
;;;; Bind
;;;


; @macro (bind ((a . r) b . c) tree (%%list a b c r))
; @expansion
; (let ((tree0 tree))
;   (let ((car1 (%%car tree0))
;         (cdr2 (%%cdr tree0)))
;     (let ((a (%%car car1))
;           (r (%%cdr car1)))
;       (let ((b (%%car cdr2))
;             (c (%%cdr cdr2)))
;         (%%list a b c r)))))


(zen:define-syntax zen:bind
  (lambda (src)
    (define (expand-car bindings tree body)
      (let ((car-binding (%%car bindings)))
        (cond ((%%symbol? car-binding)
               `((let ((,car-binding (%%car ,tree)))
                   ,@(expand-cdr bindings tree body))))
              ((%%pair? car-binding)
               (let ((car-symbol (zen:generate-symbol "car")))
                 `((if (%%null? ,tree)
                       (zen:error "Unable to bind")
                     (let ((,car-symbol (%%car ,tree)))
                       ,@(expand-car car-binding car-symbol
                           (expand-cdr bindings tree body))))))))))
    
    (define (expand-cdr bindings tree body)
      (let ((cdr-binding (%%cdr bindings)))
        (cond ((%%null? cdr-binding)
               body)
              ((%%symbol? cdr-binding)
               `((let ((,cdr-binding (%%cdr ,tree)))
                   ,@body)))
              ((%%pair? cdr-binding)
               (let ((cdr-symbol (zen:generate-symbol "cdr")))
                 `((let ((,cdr-symbol (%%cdr ,tree)))
                     ,@(expand-car cdr-binding cdr-symbol body))))))))
    
    (let ((form (%%cdr (zen:source-code src))))
      (let ((bindings (%%desourcify (%%car form)))
            (tree (%%desourcify (%%cadr form)))
            (body (%%cddr form)))
        (let ((tree-symbol (zen:generate-symbol "tree")))
          `(let ((,tree-symbol ,tree))
             ,@(expand-car bindings tree-symbol body)))))))


;;;
;;;; Stack
;;;


;; work around gambit header not included
(zen:define-macro (%%thread-stack thread)
  (if zen:debug-core?
      `(^#vector-ref ,thread 34)
    `(%%vector-ref ,thread 34)))

(zen:define-macro (%%thread-frame-pointer thread)
  (if zen:debug-core?
      `(^#vector-ref ,thread 35)
    `(%%vector-ref ,thread 35)))

(zen:define-macro (%%thread-frame-pointer-set! thread fp)
  (if zen:debug-core?
      `(^#vector-set! ,thread 35 ,fp)
    `(%%vector-set! ,thread 35 ,fp)))

(zen:define-macro (%%thread-stack-limit thread)
  (if zen:debug-core?
      `(^#vector-ref ,thread 36)
    `(%%vector-ref ,thread 36)))


(zen:define-syntax %%stack-frame
  (lambda (src)
    (let ((form (%%cdr (zen:source-code src))))
      (let ((frame-quads (%%car form))
            (body (%%cdr form)))
        `(let ((___thread (%%current-thread)))
           (let ((___stack (%%thread-stack ___thread)))
             (if (%%not ___stack)
                 (zen:error "Thread doesn't have a stack")
               (let ((___fp (%%thread-frame-pointer ___thread)))
                 (let ((___new-fp (%%fx+ ___fp ,frame-quads)))
                   (if (%%fx> ___new-fp (%%thread-stack-limit ___thread))
                       (zen:error "Stack overflow")
                     (begin
                       (%%thread-frame-pointer-set! ___thread ___new-fp)
                       (let ((result (let () ,@body)))
                         (%%thread-frame-pointer-set! ___thread ___fp)
                         result))))))))))))


(zen:define-syntax %%stack
  (lambda (src)
    (let ((form (%%cdr (zen:source-code src))))
      (let ((offset (zen:source-code (%%car form)))
            (bytes (zen:source-code (%%cadr form)))
            (subtype (zen:source-code (%%car (%%cddr form)))))
        (let ((header (zen:header bytes subtype zen:tag-permanent)))
          `(let ()
             (declare (not interrupts-enabled))
             (let ((obj (%%fx+ ___stack ___fp ,offset)))
               (%%u64vector-set! obj -1 ,header)
               obj)))))))


;;;
;;;; Compose
;;;


(zen:define-macro (%%compose-kernel s1 s2)
  `(%%string->symbol (%%string-append (%%symbol->string ,s1) ":" (%%symbol->string ,s2))))


(zen:define-macro (%%compose-reference s1 s2)
  `(%%string->symbol (%%string-append (%%symbol->string ,s1) "#" (%%symbol->string ,s2))))


(zen:define-macro (%%compose-helper s1 s2)
  `(%%string->symbol (%%string-append (%%symbol->string ,s1) "!" (%%symbol->string ,s2)))))
