;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Advise
;;;


(block kernel.advise


;;;
;;;; Advise
;;;


(define (zen:advise symbol advice)
  (advice symbol
          (lambda ()
            (%%global-var-ref symbol))
          (lambda (value)
            (%%global-var-set! symbol value))))


;;;
;;;; Table
;;;


(define zen:debugging-table-scan
  (make-parameter #f))


(define (zen:debug-table-assert name table)
  (if (eq? (zen:debugging-table-scan) table)
      (zen:error "Calling {a} while scanning table: {a}" name table)))


(define (zen:debug-table-find rest)
  (let loop ((rest rest))
       (cond ((null? rest)
              (zen:error "Unable to find table in arguments"))
             ((table? (car rest))
              (car rest))
             (else
              (loop (cdr rest))))))


(define (zen:debugging-table-advice symbol ref set)
  (let ((original (ref)))
    (set (lambda rest
           (let ((table (zen:debug-table-find rest)))
             (zen:debug-table-assert symbol table)
             (parameterize ((zen:debugging-table-scan table))
               (apply original rest)))))))


(define (zen:debug-table-advice symbol ref set)
  (let ((original (ref)))
    (set (lambda rest
           (let ((table (zen:debug-table-find rest)))
             (zen:debug-table-assert symbol table)
             (apply original rest))))))


(define (zen:debug-table-scan)
  (zen:advise 'table-for-each   zen:debugging-table-advice)
  (zen:advise '##table-for-each zen:debugging-table-advice)
  (zen:advise 'table-search     zen:debugging-table-advice)
  (zen:advise '##table-search   zen:debugging-table-advice)
  (zen:advise 'table-ref        zen:debug-table-advice)
  (zen:advise '##table-ref      zen:debug-table-advice)
  (zen:advise 'table-set!       zen:debug-table-advice)
  (zen:advise '##table-set!     zen:debug-table-advice)
  (zen:advise 'table-for-each   zen:debug-table-advice)
  (zen:advise '##table-for-each zen:debug-table-advice)
  (zen:advise 'table-search     zen:debug-table-advice)
  (zen:advise '##table-search   zen:debug-table-advice)
  (zen:advise 'table-copy       zen:debug-table-advice)
  (zen:advise '##table-copy     zen:debug-table-advice)
  (zen:advise 'table-merge      zen:debug-table-advice)
  (zen:advise '##table-merge    zen:debug-table-advice)
  (zen:advise 'table-merge!     zen:debug-table-advice)
  (zen:advise '##table-merge!   zen:debug-table-advice)
  (zen:advise 'table->list      zen:debug-table-advice)
  (zen:advise '##table->list    zen:debug-table-advice))


(if zen:kernel-mutable-bindings?
    (zen:debug-table-scan)))
