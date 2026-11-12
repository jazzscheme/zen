;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Mu Parallel
;;;


;;;
;;;; Master
;;;


(define (mu:make-product-parallel name)
  (let ((subproduct-table (make-table test: eq?))
        (subproduct-table-mutex (make-mutex 'subproduct-table-mutex))
        (active-processes '())
        (free-processes '())
        (outdated-processes '())
        (process-mutex (make-mutex 'process-mutex))
        (process-condition-variable (make-condition-variable 'process-condition-variable))
        (listening-port (open-tcp-server 0))
        (output-mutex (make-mutex 'output-mutex))
        (jobs (or mu:jobs (mu:build-jobs) mu:jobs-default))
        (stop-build? #f))
    (define (key-product? name)
      (memq name '(zen
                    zen-foundation
                    zen-scheme)))
    
    (define (atomic-output line)
      (mutex-lock! output-mutex)
      (display line)
      (newline)
      (force-output)
      (mutex-unlock! output-mutex))
    
    (define (end-port/echo port/echo)
      (let ((established-port (car port/echo))
            (echo-thread (cdr port/echo)))
        (write '() established-port)
        (force-output established-port)
        (close-port established-port)
        (thread-join! echo-thread)))
    
    (define (grab-build-process)
      (mutex-lock! process-mutex)
      (let ((active-count (length active-processes)))
        (cond ((pair? free-processes)
               (let ((port/echo (car free-processes)))
                 (set! free-processes (cdr free-processes))
                 (mutex-unlock! process-mutex)
                 port/echo))
              ((< active-count jobs)
               (let ((process (open-process
                                (list
                                  path: (mu:install-path "zen")
                                  arguments: `(,(if (mu:worker-repl?)
                                                    "-:darQ"
                                                  "-:daqQ-")
                                               "-worker"
                                               "-link" ,(symbol->string mu:link)
                                               ,@(if (memq 'keep-c mu:compile-options) `("-keep-c") '())
                                               ,@(if (memq 'track-scheme mu:compile-options) `("-track-scheme") '())
                                               ,@(if (memq 'expansion mu:compile-options) `("-expansion") '())
                                               ,@(if (mu:save-emit?) `("-emit") '())
                                               ,@(if (mu:build-repository) `("-build-repository" ,(mu:build-repository)) '())
                                               ,@(if (mu:repositories) `("-repositories" ,(mu:repositories)) '())
                                               ,@(if (mu:reporting?) `("-reporting") '())
                                               "-jobs" "1"
                                               "-port" ,(number->string (socket-info-port-number (tcp-server-socket-info listening-port))))
                                  stdin-redirection: #t
                                  stdout-redirection: #t
                                  stderr-redirection: #t))))
                 (let ((established-port (read listening-port))
                       (echo-thread (thread-start! (make-thread (lambda ()
                                                                  (let iter ()
                                                                       (let ((line (read-line process)))
                                                                         (if (not (eof-object? line))
                                                                             (begin
                                                                               (atomic-output line)
                                                                               (iter))))))))))
                   (let ((port/echo (cons established-port echo-thread)))
                     (set! active-processes (cons port/echo active-processes))
                     (mutex-unlock! process-mutex)
                     port/echo))))
              (else
               (mutex-unlock! process-mutex process-condition-variable)
               (grab-build-process)))))
    
    (define (build name)
      (let ((port/echo (grab-build-process)))
        (define (build-process-problem)
          (mutex-lock! process-mutex)
          (set! active-processes (mu:remove port/echo active-processes))
          (set! stop-build? #t)
          (mutex-unlock! process-mutex)
          (condition-variable-signal! process-condition-variable))
        
        (define (build-process-ended changes)
          (mutex-lock! process-mutex)
          (if (and (key-product? name) (not (null? changes)))
              (set! outdated-processes active-processes))
          (if (memq port/echo outdated-processes)
              (begin
                (set! active-processes (mu:remove port/echo active-processes))
                (set! outdated-processes (mu:remove port/echo outdated-processes))
                (end-port/echo port/echo))
            (set! free-processes (cons port/echo free-processes)))
          (mutex-unlock! process-mutex)
          (condition-variable-signal! process-condition-variable))
        
        (define (report-message fmt-string . rest)
          (let ((message (apply mu:format fmt-string rest))
                (port mu:worker-port))
            (write (cons 'report message) port)
            (force-output port)))
        
        (if stop-build?
            (build-process-ended #f)
          (let ((established-port (car port/echo)))
            (send-build-command established-port name)
            (let iter ()
                 (let ((form (read established-port)))
                   (if (eof-object? form)
                       (build-process-problem)
                     (let ((command (car form))
                           (arguments (cdr form)))
                       (case command
                         ((changes)
                          (build-process-ended arguments))
                         ((report)
                          (report-message "{a}" arguments)
                          (iter))
                         ((exception)
                          (build-process-problem)
                          (iter))
                         (else
                          (error "Unknown master command" command)))))))))))
    
    (define (local-make name)
      (for-each (lambda (subname)
                  (if (not (table-ref subproduct-table subname #f))
                      (begin
                        (make subname)
                        (table-set! subproduct-table subname #t))))
                (let ((descriptor (mu:get-product-descriptor name)))
                  (mu:cond-expanded-product-descriptor-dependencies name descriptor)))
      (mu:build-product name))
    
    (define (remote-make name)
      (for-each thread-join!
                (map (lambda (name)
                       (mutex-lock! subproduct-table-mutex)
                       (let ((thread (or (table-ref subproduct-table name #f)
                                         (let ((thread (thread-start! (make-thread (lambda ()
                                                                                       (make name))))))
                                           (table-set! subproduct-table name thread)
                                           thread))))
                         (mutex-unlock! subproduct-table-mutex)
                         thread))
                     (let ((descriptor (mu:get-product-descriptor name)))
                       (mu:cond-expanded-product-descriptor-dependencies name descriptor))))
      (build name))
    
    (define (make name)
      (if (or (not jobs) (eqv? jobs 0) (mu:debug-build?))
          (local-make name)
        (remote-make name)))
    
    (define (send-build-command established-port name)
      (write `(build ,name) established-port)
      (newline established-port)
      (force-output established-port))
    
    (parameterize ((current-user-interrupt-handler
                     (lambda ()
                       (set! stop-build? #t)
                       (for-each end-port/echo active-processes)
                       (for-each end-port/echo free-processes)
                       (exit))))
      (make name))
    (for-each end-port/echo free-processes)
    (if stop-build? 1 0)))


;;;
;;;; Worker
;;;


(define mu:worker-port
  #f)

(define mu:worker-product
  #f)


(define (mu:worker-process port)
  (declare (proper-tail-calls))
  (let ((repl? (mu:worker-repl?))
        (previous-handler (current-exception-handler))
        (established-port (open-tcp-client port)))
    (define (build product)
      (set! mu:worker-product product)
      (mu:reset-changed-units)
      (mu:build-product product)
      (write (cons 'changes (mu:get-changed-units)) established-port)
      (force-output established-port))
    
    (set! mu:worker-port established-port)
    (let iter ()
         (let ((form (read established-port)))
           (if (or (eof-object? form)
                   (null? form))
               (close-port established-port)
             (let ((command (car form))
                   (arguments (cdr form)))
               (continuation-capture
                 (lambda (cont)
                   (with-exception-handler
                     (lambda (exc)
                       (if repl?
                           (begin
                             ;; closing the port would terminate the parent
                             ;; process making the console port unavailable
                             ;; (close-port established-port)
                             (write '(exception) established-port)
                             (force-output established-port)
                             (previous-handler exc))
                         (continuation-graft cont
                           (lambda ()
                             (if mu:worker-product
                                 (begin
                                   (display "*** ERROR BUILDING ")
                                   (display mu:worker-product)
                                   (display " -- ")))
                             (display-exception exc)
                             (force-output)
                             (exit)))))
                     (lambda ()
                       (case command
                         ((build)
                          (build (car arguments)))
                         (else
                          (error "Unknown worker command" command)))
                       (iter)))))))))))
