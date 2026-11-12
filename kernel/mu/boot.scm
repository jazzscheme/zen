;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Mu Boot
;;;


;;;
;;;; Dependencies
;;;


(define mu:all-dependencies
  #f)


(define (mu:get-dependencies)
  (or mu:all-dependencies
      (let ((dependencies (let ((file ".dependencies"))
                            (if (file-exists? file)
                                (call-with-input-file file (lambda (port) (read-all port read)))
                              '()))))
        (set! mu:all-dependencies dependencies)
        dependencies)))


(define (mu:find-dependency name)
  (mu:find-if (lambda (dep)
                (eq? (mu:dependency-name dep) name))
              (mu:get-dependencies)))


(define (mu:require-dependency name)
  (or (mu:find-dependency name)
      (error "Unknown dependency" name)))


(define (mu:dependency-name dep)
  (car dep))

(define (mu:dependency-directory dep)
  (cadr dep))

(define (mu:dependency-url dep)
  (caddr dep))


(define (mu:clone-dependencies names)
  (define (clone-dependency name)
    (let ((dep (mu:require-dependency name)))
      (mu:invoke-process
         (list
           path: "git"
           arguments: `("clone" ,(mu:dependency-url dep) ,(mu:dependency-directory dep))))))
  
  (for-each clone-dependency names))


;;;
;;;; Versions
;;;


(define (mu:setup-versions)
  (set! zen:zen-versions-file (string-append mu:source "kernel/versions")))


;;;
;;;; Option
;;;


(define mu:unspecified-option-value
  (list 'unspecified-option))


(define (mu:unspecified-option)
  mu:unspecified-option-value)


(define (mu:specified-option? option)
  (not (eq? option mu:unspecified-option-value)))


(define (mu:or-option . rest)
  (let iter ((scan rest))
    (if (null? scan)
        (mu:error "Invalid option")
      (let ((option (car scan)))
        (if (mu:specified-option? option)
            option
          (iter (cdr scan)))))))


;;;
;;;; Options
;;;


(define-macro (mu:define-option name default)
  (let ((global (string->symbol (string-append (symbol->string name) "!value"))))
    `(begin
       (define ,global ,default)
       (define (,name . rest)
         (if (null? rest)
             ,global
           (set! ,global (car rest)))))))


(mu:define-option mu:default-name
  #f)

(mu:define-option mu:default-system
  'gambit)

(mu:define-option mu:default-platform
  (mu:unspecified-option))

(mu:define-option mu:default-compiler
  (mu:unspecified-option))

(mu:define-option mu:default-processor
  (mu:unspecified-option))

(mu:define-option mu:default-windowing
  (mu:unspecified-option))

(mu:define-option mu:default-safety
  'release)

(mu:define-option mu:default-optimize?
  #t)

(mu:define-option mu:default-debug-environments?
  #t)

(mu:define-option mu:default-debug-location?
  #t)

(mu:define-option mu:default-debug-source?
  #f)

(mu:define-option mu:default-debug-foreign?
  #f)

(mu:define-option mu:default-track-memory?
  #f)

(mu:define-option mu:default-mutable-bindings?
  #f)

(mu:define-option mu:default-destination
  #f)

(mu:define-option mu:default-features
  '())

(mu:define-option mu:default-properties
  '())


(mu:define-option mu:default-target
  'all)
