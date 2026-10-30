;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Boot
;;;


(define zen:Kernel
  (list
    "kernel/syntax/header"
    "kernel/syntax/macro"
    "kernel/syntax/unsafe"
    "kernel/syntax/verbose"
    "kernel/syntax/declares"
    "kernel/syntax/features"
    "kernel/syntax/block"
    "kernel/syntax/library"
    "kernel/syntax/expansion"
    "kernel/syntax/primitives"
    "kernel/syntax/internal"
    "kernel/syntax/structure"
    "kernel/syntax/syntax"
    "kernel/syntax/runtime"
    "kernel/runtime/settings"
    "kernel/runtime/restricted"
    "kernel/runtime/base"
    "kernel/runtime/record"
    "kernel/syntax/repository"
    "kernel/runtime/configuration"
    "kernel/runtime/version"
    "kernel/runtime/common"
    "kernel/runtime/advise"
    "kernel/runtime/build"
    "kernel/runtime/digest"
    "kernel/runtime/readtable"
    "kernel/core/base-syntax"
    "kernel/core/base-runtime"
    "kernel/core/class-syntax"
    "kernel/core/class-classes"
    "kernel/core/class-runtime"
    "kernel/core/exception-classes"
    "kernel/core/exception-runtime"
    "kernel/core/generic-syntax"
    "kernel/core/generic-runtime"
    "kernel/core/unit-runtime"
    "kernel/dialect/dialect-syntax"
    "kernel/dialect/dialect-runtime"
    "kernel/dialect/dialect-expression"
    "kernel/dialect/dialect-development"
    "kernel/foundation/foundation-runtime"
    "kernel/scheme/scheme-backend"
    "kernel/scheme/scheme-dialect"))


(define zen:Kernel-Setup
  '("kernel/runtime/unit"
    "kernel/runtime/readtable"
    "kernel/runtime/setup"))


(define zen:load-kernel
  (let ((loaded? #f))
    (lambda (setup? #!optional (interpret? #t))
      (define verbose?
        #f)
      
      (define (load-file file)
        (if verbose?
            (begin
              (display file)
              (newline)
              (force-output)))
        (load (string-append zen:source file)))
      
      (define (load-files files)
        (for-each load-file files))
      
      (if (not loaded?)
          (begin
            (if (not interpret?)
                (load-file "kernel/runtime/product"))
            (load-files zen:Kernel)
            (if setup?
                (load-files zen:Kernel-Setup))
            (set! loaded? #t))))))
