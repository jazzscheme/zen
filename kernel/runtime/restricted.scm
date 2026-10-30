;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Restricted
;;;


(block kernel.restricted


;;;
;;;; Authorized
;;;


(define zen:filesystem-authorized
  '())


(define (zen:filesystem-authorize root)
  (if zen:filesystem-restricted?
      (zen:error "Restricted access to authorize")
    (if (%%not (zen:filesystem-authorized? root))
        (set! zen:filesystem-authorized (append zen:filesystem-authorized (list root))))))


(define (zen:filesystem-authorized? path)
  (zen:some? (lambda (root)
                (zen:string-starts-with? path root))
              zen:filesystem-authorized))


(define (zen:filesystem-check path-or-settings)
  (let ((path (zen:path-or-settings->path path-or-settings)))
    (if (not (zen:filesystem-authorized? path))
        (zen:error "Unauthorized access to path: {a}" path))))


(define (zen:path-or-settings->path path-or-settings)
  (define (ill-formed)
    (zen:error "Ill-formed path-or-settings: {s}" path-or-settings))
  
  (if (%%string? path-or-settings)
      path-or-settings
    (if (%%pair? path-or-settings)
        (if (and (%%eq? (%%car path-or-settings) 'path:)
                 (%%not (%%null? (%%cdr path-or-settings))))
            (%%cadr path-or-settings)
          (ill-formed))
      (ill-formed))))


(define (zen:restrict-path-or-settings unrestricted)
  (lambda (path-or-settings . rest)
    (zen:filesystem-check path-or-settings)
    (apply unrestricted path-or-settings rest)))


(define (zen:restrict-transfer unrestricted)
  (lambda (from to . rest)
    (zen:filesystem-check from)
    (zen:filesystem-check to)
    (apply unrestricted from to rest)))


(define zen:filesystem-unrestricted
  (%%make-table test: eq?))

(define zen:filesystem-restricted
  (%%make-table test: eq?))


(define-macro (zen:define-restricted name #!optional (restrict 'zen:restrict-path-or-settings))
  `(begin
     (%%table-set! zen:filesystem-unrestricted ',name ,name)
     (%%table-set! zen:filesystem-restricted ',name (,restrict ,name))))


(define-macro (zen:restrict name)
  `(set! ,name (%%table-ref zen:filesystem-restricted ',name)))

(define-macro (zen:unrestrict name)
  `(set! ,name (%%table-ref zen:filesystem-unrestricted ',name)))


;;;
;;;; 13.2 Filesystem operations
;;;


(zen:define-restricted create-directory)
(zen:define-restricted create-fifo)
(zen:define-restricted create-link)
(zen:define-restricted create-symbolic-link)
(zen:define-restricted rename-file)
(zen:define-restricted copy-file zen:restrict-transfer)
(zen:define-restricted delete-file)
(zen:define-restricted delete-directory)
(zen:define-restricted directory-files)


;;;
;;;; 13.8 File information
;;;


(zen:define-restricted file-exists?)
(zen:define-restricted file-info)
(zen:define-restricted file-type)
(zen:define-restricted file-device)
(zen:define-restricted file-inode)
(zen:define-restricted file-mode)
(zen:define-restricted file-number-of-links)
(zen:define-restricted file-owner)
(zen:define-restricted file-group)
(zen:define-restricted file-size)
(zen:define-restricted file-last-access-time)
(zen:define-restricted file-last-modification-time)
(zen:define-restricted file-last-change-time)
(zen:define-restricted file-attributes)
(zen:define-restricted file-creation-time)
(zen:define-restricted file-last-access-and-modification-times-set!)


;;;
;;;; 14.7.1 Filesystem devices
;;;


(zen:define-restricted open-file)
(zen:define-restricted open-input-file)
(zen:define-restricted open-output-file)
(zen:define-restricted call-with-input-file)
(zen:define-restricted call-with-output-file)
(zen:define-restricted with-input-from-file)
(zen:define-restricted with-output-to-file)


;;;
;;;; 14.8 Directory ports
;;;


(zen:define-restricted open-directory)


;;;
;;;; Restricted
;;;


(define zen:filesystem-restrictable?
  #f)

(define (zen:filesystem-restrictable)
  (set! zen:filesystem-restrictable? #t))


(define zen:filesystem-restricted?
  #f)


(define (zen:filesystem-allowed? path)
  (or (not zen:filesystem-restricted?)
      (zen:filesystem-authorized? path)))


(define (zen:filesystem-restrict)
  (if (and zen:filesystem-restrictable? (not zen:filesystem-restricted?))
      (begin
        (zen:restrict create-directory)
        (zen:restrict create-fifo)
        (zen:restrict create-link)
        (zen:restrict create-symbolic-link)
        (zen:restrict rename-file)
        (zen:restrict copy-file)
        (zen:restrict delete-file)
        (zen:restrict delete-directory)
        (zen:restrict directory-files)
        (zen:restrict file-exists?)
        (zen:restrict file-info)
        (zen:restrict file-type)
        (zen:restrict file-device)
        (zen:restrict file-inode)
        (zen:restrict file-mode)
        (zen:restrict file-number-of-links)
        (zen:restrict file-owner)
        (zen:restrict file-group)
        (zen:restrict file-size)
        (zen:restrict file-last-access-time)
        (zen:restrict file-last-modification-time)
        (zen:restrict file-last-change-time)
        (zen:restrict file-attributes)
        (zen:restrict file-creation-time)
        (zen:restrict file-last-access-and-modification-times-set!)
        (zen:restrict open-file)
        (zen:restrict open-input-file)
        (zen:restrict open-output-file)
        (zen:restrict call-with-input-file)
        (zen:restrict call-with-output-file)
        (zen:restrict with-input-from-file)
        (zen:restrict with-output-to-file)
        (zen:restrict open-directory)
        (set! zen:filesystem-restricted? #t))))


(define (zen:filesystem-unrestrict)
  (define (under-debugger?)
    (zen:global-ref 'zen.debuggee#controller-debugger))
  
  (if (and zen:filesystem-restrictable? zen:filesystem-restricted? (not (under-debugger?)))
      (begin
        (zen:unrestrict create-directory)
        (zen:unrestrict create-fifo)
        (zen:unrestrict create-link)
        (zen:unrestrict create-symbolic-link)
        (zen:unrestrict rename-file)
        (zen:unrestrict copy-file)
        (zen:unrestrict delete-file)
        (zen:unrestrict delete-directory)
        (zen:unrestrict directory-files)
        (zen:unrestrict file-exists?)
        (zen:unrestrict file-info)
        (zen:unrestrict file-type)
        (zen:unrestrict file-device)
        (zen:unrestrict file-inode)
        (zen:unrestrict file-mode)
        (zen:unrestrict file-number-of-links)
        (zen:unrestrict file-owner)
        (zen:unrestrict file-group)
        (zen:unrestrict file-size)
        (zen:unrestrict file-last-access-time)
        (zen:unrestrict file-last-modification-time)
        (zen:unrestrict file-last-change-time)
        (zen:unrestrict file-attributes)
        (zen:unrestrict file-creation-time)
        (zen:unrestrict file-last-access-and-modification-times-set!)
        (zen:unrestrict open-file)
        (zen:unrestrict open-input-file)
        (zen:unrestrict open-output-file)
        (zen:unrestrict call-with-input-file)
        (zen:unrestrict call-with-output-file)
        (zen:unrestrict with-input-from-file)
        (zen:unrestrict with-output-to-file)
        (zen:unrestrict open-directory)
        (set! zen:filesystem-restricted? #f)))))
