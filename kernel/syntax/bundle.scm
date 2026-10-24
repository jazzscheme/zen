;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Bundle: Native Multi-File Module Ingestion
;;;


(include "~~lib/_gambit#.scm")


;;;
;;;; Extensions
;;;


(let ((exts ##scheme-file-extensions))
  (if (not (assoc ".zen" exts))
      (##scheme-file-extensions-set! (cons '(".zen" . #f) exts))))


;;;
;;;; Helpers
;;;


(define (zen:string-index str char)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (cond
        ((= i len) #f)
        ((char=? (string-ref str i) char) i)
        (else (loop (+ i 1)))))))


(define (zen:symbol->namespace sym)
  (let* ((str (symbol->string sym))
         (parts (##string-split-at-char str #\.)))
    (let loop ((p (cdr parts)) (acc (car parts)))
      (if (null? p)
          (string-append acc "#")
        (loop (cdr p) (string-append acc "/" (car p)))))))


;;;
;;;; Lazy Expander Loading
;;;


(define zen:define-library-loaded? #f)

(define (zen:ensure-define-library-loaded!)
  (if (not zen:define-library-loaded?)
      (begin
        (##eval '(##let ()
                    (##demand-module _define-library/define-library-expand)
                    #t))
        (set! zen:define-library-loaded? #t))))


;;;
;;;; Transformation
;;;


(define (zen:transform-bundle src-form)
  (let* ((form (if (##source? src-form) (##source-strip src-form) src-form))
         (raw-name (cadr form))
         (mod-sym  (if (##source? raw-name) (##source-strip raw-name) raw-name))
         (mod-str  (symbol->string mod-sym))
         (colon-pos (zen:string-index mod-str #\:)))

    (if colon-pos
        ;; Constituent Subunit: (bundle root:path ...)
        ;; Augments the parent root namespace directly!
        (let* ((root-str (substring mod-str 0 colon-pos))
               (ns-str   (zen:symbol->namespace (string->symbol root-str)))
               (body     (cddr form)))
          `(begin
             (##namespace (,ns-str))
             ,@body
             (##namespace (""))))

      ;; Master Bundle: (bundle root ...)
      ;; Expands to standard define-library that loads its constituent files
      (let* ((body        (cddr form))
             (exports     '())
             (imports     '())
             (files-list  '())
             (direct-defs '()))

        ;; Walk master bundle declarations
        (for-each
          (lambda (elem)
            (cond
              ((and (pair? elem) (eq? (car elem) 'export))
               (set! exports (append exports (cdr elem))))

              ((and (pair? elem) (eq? (car elem) 'import))
               (set! imports (cons elem imports)))

              ((and (pair? elem) (eq? (car elem) 'files))
               (set! files-list (append files-list (cdr elem))))

              (else
               (set! direct-defs (cons elem direct-defs)))))
          body)

        (let* ((locat (and (##source? src-form) (##source-locat src-form)))
               (container (and locat (##locat-container locat)))
               (file-path (and container (##container->path container)))
               (parent-dir (if file-path (##path-directory file-path) (current-directory)))
               (parts (##string-split-at-char mod-str #\.))
               (lib-name (map string->symbol parts))
               (mod-dir (let loop ((p (cdr parts)) (acc (car parts)))
                          (if (null? p)
                              acc
                            (loop (cdr p) (string-append acc "/" (car p))))))
               (base-dir (##path-reference mod-dir parent-dir))
               (load-forms
                 (map
                   (lambda (rel-file)
                     (let* ((filename (string-append rel-file ".zen"))
                            (full-path (##path-reference filename (string-append (##path-strip-trailing-directory-separator base-dir) "/"))))
                       `(load ,full-path)))
                   files-list)))
          `(define-library ,lib-name
             (export ,@exports)
             ,@(reverse imports)
             (begin
               ,@(reverse direct-defs)
               ,@load-forms)))))))


(define (zen:desourcify-all expr)
  (define (desourcify-source src)
    (desourcify-all (%%source-code src)))

  (define (desourcify-list lst)
    (cond ((%%pair? lst)
           (%%cons (desourcify-all (%%car lst))
                   (desourcify-list (%%cdr lst))))
          ((%%null? lst)
           '())
          (else
           (desourcify-all lst))))

  (define (desourcify-vector vect)
    (let* ((len (%%vector-length vect))
           (x (%%make-vector len 0)))
      (let loop ((i (%%fx- len 1)))
        (if (%%fx< i 0)
          x
          (begin
            (%%vector-set! x i (desourcify-all (%%vector-ref vect i)))
            (loop (%%fx- i 1)))))))
  
  (define (desourcify-box bx)
    (%%box (desourcify-all (%%unbox bx))))

  (define (desourcify-all expr)
    (cond ((%%source? expr)
           (desourcify-source expr))
          ((%%pair? expr)
           (desourcify-list expr))
          ((%%vector? expr)
           (desourcify-vector expr))
          ((%%box? expr)
           (desourcify-box expr))
          (else
           expr)))
  
  (desourcify-all expr))


;;;
;;;; Registration
;;;


(let ((bundle-transformer
       (lambda (src)
         (let ((synth (zen:transform-bundle src)))
           (##sourcify synth (##source-locat src))))))
  (##top-cte-add-macro-no-dups!
   ##interaction-cte
   'bundle
   (##make-macro-descr #t -1 bundle-transformer #f)))


;;;
;;;; Library Kinds Hook (Marc Feeley's Native R7RS Extension Point)
;;;


(define (zen:read-libdef-zen name reference-src import-name port
                             module-root modref-path)
  (let ((read-first (##global-var-ref '_define-library/define-library-expand#read-first))
        (parse-define-library (##global-var-ref '_define-library/define-library-expand#parse-define-library)))
    (let ((src (read-first port)))
      (if (eof-object? src)
          #f
        (let ((form (##desourcify src)))
          (if (and (pair? form) (eq? (car form) 'bundle))
              (let* ((synth (zen:transform-bundle src))
                     (synth-src (##sourcify synth (##source-locat src))))
                (parse-define-library synth-src import-name module-root modref-path))
            (parse-define-library src import-name module-root modref-path)))))))


(define zen:library-kind-installed? #f)

(define (zen:install-bundle-library-kind!)
  (if (not zen:library-kind-installed?)
      (begin
        (zen:ensure-define-library-loaded!)
        (let ((kinds (##global-var-ref '_define-library/define-library-expand#library-kinds))
              (set-kinds! (##global-var-ref '_define-library/define-library-expand#library-kinds-set!)))
          (let ((current kinds))
            (if (not (assoc ".zen" current))
                (set-kinds!
                  (cons (cons ".zen" (vector zen:read-libdef-zen))
                        current)))))
        (set! zen:library-kind-installed? #t))))


;;;
;;;; Module Search Hook (Support for _mod.zen / _mod.sld master file in module folder)
;;;


(define zen:module-search-installed? #f)

(define (zen:install-module-search-hook!)
  (if (not zen:module-search-installed?)
      (let ((orig-search ##search-module-in-search-order))
        (define (search-underscore-module modref search-order)
          (and (macro-modref? modref)
               (let* ((host  (macro-modref-host modref))
                      (tag   (macro-modref-tag modref))
                      (rpath (macro-modref-rpath modref)))
                 (and (pair? rpath)
                      (not host)
                      (not tag)
                      (let ((mod-name (car rpath)))
                        (let loop ((lst search-order))
                             (and (pair? lst)
                                  (let* ((dir (##path-expand-in-initial-current-directory (car lst)))
                                         (main-repo-path (##path-expand "@" (##path-expand mod-name dir)))
                                         (root (if (##file-exists? main-repo-path) main-repo-path dir))
                                         (nested-dirs (if (##file-exists? main-repo-path) (##butlast rpath) rpath))
                                         (mod-dir (##path-join-reversed nested-dirs root))
                                         (mod-filename-noext (string-append "_" mod-name)))
                                    (or (##search-module-with-exts
                                          mod-filename-noext
                                          mod-dir
                                          root
                                          nested-dirs
                                          ##scheme-file-extensions)
                                        (loop (cdr lst)))))))))))

        (##search-module-in-search-order-set!
          (lambda (modref search-order)
            (or (search-underscore-module modref search-order)
                (orig-search modref search-order))))
        (set! zen:module-search-installed? #t))))


(zen:install-module-search-hook!)
