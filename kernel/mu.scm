#!gsc -:dq-

(define mu:source
  (path-directory (path-strip-trailing-directory-separator (path-directory (path-normalize (car (command-line)))))))

(define (mu:load-source path)
  (load (string-append mu:source path)))


;;;
;;;; Load
;;;


(define mu:Mu
  (list
    "kernel/build/core"
    "kernel/build/version"
    "kernel/build/manifest"
    "kernel/mu/boot"
    "kernel/mu/configuration"
    "kernel/mu/build"))


(define mu:Kernel
  (list
    "build/stable/build/kernel/_architecture.scm" ;; tanzoo
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
    "kernel/syntax/foreign"
    "kernel/syntax/internal"
    "kernel/syntax/structure"
    "kernel/syntax/syntax"
    "kernel/syntax/runtime"
    "kernel/runtime/settings"
    "kernel/runtime/restricted"
    "kernel/runtime/base"
    "kernel/runtime/record"
    "kernel/syntax/repository"
    "kernel/runtime/common"
    "kernel/runtime/advise"
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
    "kernel/scheme/scheme-dialect"
    "kernel/zen/zen-runtime"
    "kernel/zen/zen-backend"
    "kernel/zen/zen-core"))


(define (mu:loader files)
  (let ((loaded? #f))
    (lambda ()
      (define verbose?
        #f)
      
      (define (load-file file)
        (if verbose?
            (begin
              (display file)
              (newline)
              (force-output)))
        (mu:load-source file))
      
      (define (load-files files)
        (for-each load-file files))
      
      (if (not loaded?)
          (begin
            (load-files files)
            (set! loaded? #t))))))


(define mu:load-mu
  (mu:loader mu:Mu))


(define mu:load-kernel
  (mu:loader mu:Kernel))


(mu:load-mu)


;;;
;;;; Initialize
;;;


(mu:setup-versions)
(mu:build-system-boot)
