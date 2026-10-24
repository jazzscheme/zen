;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Exceptions
;;;


(block core.exception-runtime


;;;
;;;; Exception
;;;


(zen:define-class zen:Exception zen:Object ()
  ())


(zen:define-method (zen:print-object (zen:Exception exception) output detail)
  (let ((message (zen:exception-message exception)))
    (zen:format output "#<exception #{a}" (zen:object->serial exception))
    (if message
        (zen:format output " {s}" message))
    (zen:format output ">")))


(zen:define-virtual (zen:present-exception (zen:Exception exception)))
(zen:define-virtual (zen:exception-message (zen:Exception exception)))
(zen:define-virtual (zen:get-detail (zen:Exception exception)))


(zen:define-method (zen:present-exception (zen:Exception exception))
  (let ((output (open-output-string)))
    (zen:format output "This exception was raised: {s}" exception)
    (get-output-string output)))


(zen:define-method (zen:exception-message (zen:Exception exception))
  #f)


(zen:define-method (zen:get-detail (zen:Exception exception))
  #f)


;;;
;;;; Exception Detail
;;;


(zen:define-class zen:Exception-Detail-Class zen:Class ()
  ())


(zen:define-class zen:Exception-Detail zen:Object (metaclass: zen:Exception-Detail-Class constructor: zen:allocate-exception-detail)
  ((icon     getter: generate)
   (title    getter: generate)
   (location getter: generate)
   (children getter: generate)))


(define (zen:new-exception-detail icon title location children)
  (zen:allocate-exception-detail icon title location children))


;;;
;;;; System Exception
;;;


(zen:define-class zen:System-Exception zen:Exception ()
  ((exception getter: generate)))


(zen:define-method (zen:present-exception (zen:System-Exception exception))
  (zen:exception-reason (zen:get-system-exception-exception exception)))


;;;
;;;; Scheme
;;;


(define (zen:exception-reason exc)
  (let ((output (open-output-string)))
    (zen:debug-port-setup-width output)
    (display-exception exc output)
    (let ((str (get-output-string output)))
      (let ((len (string-length str)))
        (if (and (%%fx> len 0)
                 (%%eqv? (%%string-ref str (%%fx- len 1)) #\newline))
            (%%substring str 0 (%%fx- len 1))
          str)))))


(define (zen:exception-detail exc)
  (if (and (%%object? exc)
           (%%is? exc zen:Exception))
      (zen:get-detail exc)
    #f))


(define (zen:exception-locat exc cont)
  (%%exception->locat exc cont))


;;;
;;;; Gambit Hook
;;;


(let ((previous-hook (%%display-exception-hook-ref)))
  (%%display-exception-hook-set!
    (lambda (exc port)
      (if (and (%%object? exc)
               (%%is? exc zen:Exception))
          (begin
            (display (zen:present-exception exc) port)
            (newline port))
        (previous-hook exc port)))))


;;;
;;;; Handle
;;;


(define (zen:handle-exception-filter filter handler thunk)
  (let ((previous-handler (current-exception-handler)))
    (%%continuation-capture
      (lambda (catcher-cont)
        (with-exception-handler
          (lambda (exc)
            (with-exception-handler
              (lambda (handler-exc)
                (%%continuation-graft catcher-cont
                  (lambda ()
                    (previous-handler handler-exc))))
              (lambda ()
                (if (filter exc)
                    (let ((value (handler exc)))
                      (%%continuation-return catcher-cont value))
                  (previous-handler exc)))))
          thunk)))))


;;;
;;;; Catch
;;;


(define (zen:catch-exception-filter filter catcher thunk)
  (let ((previous-handler (current-exception-handler)))
    (%%continuation-capture
      (lambda (catcher-cont)
        (with-exception-handler
          (lambda (exc)
            (if (with-exception-handler
                  (lambda (filter-exc)
                    (%%continuation-graft catcher-cont
                      (lambda ()
                        (previous-handler filter-exc))))
                  (lambda ()
                    (filter exc)))
                (%%continuation-graft catcher-cont
                  (lambda ()
                    (catcher exc)))
              (previous-handler exc)))
          thunk)))))


;;;
;;;; Retry
;;;


(define (zen:retry-exception-handler thunk #!key (retries 500) (delay .01) (feedback #f))
  (declare (proper-tail-calls))
  (let loop ((n 0))
    (zen:handle-exception-filter
      (lambda (exc)
        (%%fx< n retries))
      (lambda (exc)
        (%%when feedback
          (feedback exc))
        (thread-sleep! delay)
        (loop (%%fx+ n 1)))
      thunk)))


;;;
;;;; Error
;;;


(zen:define-class zen:Error zen:Exception (constructor: zen:allocate-error)
  ((message getter: generate)))


(define (zen:new-error message)
  (zen:allocate-error message))


(zen:define-method (zen:exception-message (zen:Error error))
  (zen:get-error-message error))


(zen:define-method (zen:present-exception (zen:Error error))
  (zen:get-error-message error))


(define (zen:raise-zen-error fmt-string . rest)
  (declare (proper-tail-calls))
  (let ((message (apply zen:format fmt-string rest)))
    (raise (zen:new-error message))))


(set! zen:error zen:raise-zen-error))
