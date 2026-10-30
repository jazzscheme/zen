;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Declares
;;;


(declare
  (block)
  (standard-bindings)
  (extended-bindings)
  (not safe))


(zen:verbose-kernel 'kernel.declares)


(define zen:not-standard-bindings
  '(not standard-bindings
        open-input-file
        open-output-file
        call-with-input-file
        call-with-output-file
        with-input-from-file
        with-output-to-file))


(zen:define-macro (zen:kernel-declares)
  `(declare
     ,@(if zen:debug-core?
           '()
         '((block)))
     
     ,@`((standard-bindings)
         ,zen:not-standard-bindings
         (extended-bindings))
     
     ,@(if #f
           #; ;; this inline declaration or the one below breaks
              ;; compilation of zen.git/foreign for some reason
           (eq? zen:kernel-safety 'sealed)
           '()
         '((not inline)))
     
     #; ;; don't think we need those for the kernel
     ,@(if zen:kernel-optimize?
           '()
         '((not proper-tail-calls)
           (not optimize-dead-local-variables)))
     
     ,@(if zen:debug-user?
           '()
         '((not safe)))))


(define (zen:declares kind)
  `((declare
      ;; block is only really usefull for units coded in a
      ;; style where control remains mostly inside the unit
      ;; using block can give an noticable performance gain
      ;; in certain cases but breaks dynamic code reevaluation
      ,@(if (or (and (eq? kind 'unit)
                     (or (eq? zen:kernel-safety 'release)
                         (eq? zen:kernel-safety 'sealed)))
                (and (eq? kind 'module)
                     (eq? zen:kernel-safety 'sealed)))
            '((block))
          '())
      
      ,@`((standard-bindings)
          ,zen:not-standard-bindings
          (extended-bindings))
      
      ;; inlining can have a huge impact on compilation time
      ;; and really bloat the size of the generated .o1 files
      ,@(if #f
            #; ;; this inline declaration or the one below breaks
            ;; compilation of zen.git/foreign for some reason
            (eq? zen:kernel-safety 'sealed)
            '()
          '((not inline)))
      
      ,@(if (or zen:kernel-optimize?
                (eq? zen:kernel-safety 'sealed))
            '()
          '((not proper-tail-calls)
            (not optimize-dead-local-variables)))
      
      ,@(if zen:debug-user?
            '()
          '((not safe))))))
