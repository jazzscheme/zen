;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Library
;;;


(include "~~lib/_gambit#.scm")


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
  (string-append (%%symbol->string sym) "#"))


;;;
;;;; Transformation
;;;


(define (zen:parse-bundle src-form proc)
  (let* ((form (##source-strip src-form))
         (mod-sym (##source-strip (cadr form)))
         (mod-str (symbol->string mod-sym))
         (dialect (##source-strip (caddr form)))
         (colon-pos (zen:string-index mod-str #\:)))
    
    (if (not colon-pos)
        ;; Master Bundle: (bundle root ...)
        ;; Expands to standard define-library that loads its constituent parts
        (let* ((body        (cdddr form))
               (exports     '())
               (imports     '())
               (parts-list  '())
               (direct-defs '()))
          
          ;; Walk master bundle declarations
          (for-each
            (lambda (elem)
              (let ((form (##source-strip elem)))
                (let ((directive (and (pair? form) (##source-strip (car form)))))
                  (cond
                    ((eq? directive 'export)
                     (set! exports (append exports (map ##source-strip (cdr form)))))
                    
                    ((eq? directive 'import)
                     (set! imports (cons form imports)))
                    
                    ((eq? directive 'parts)
                     (set! parts-list (append parts-list (map ##source-strip (cdr form)))))
                    
                    (else
                     (set! direct-defs (cons elem direct-defs)))))))
            body)
          
          (let* ((parts (##string-split-at-char mod-str #\/))
                 (lib-name (map string->symbol parts)))
            (proc 'master mod-sym lib-name dialect exports imports parts-list direct-defs)))
      
      ;; Constituent Part: (part root:path ...)
      ;; Augments the parent root namespace directly!
      (let* ((root-str (substring mod-str 0 colon-pos))
             (ns-str   (zen:symbol->namespace (string->symbol root-str)))
             (body     (cdddr form)))
        (proc 'part mod-sym ns-str dialect '() '() '() body)))))


(define (zen:transform-bundle src-form)
  (zen:parse-bundle src-form
    (lambda (kind mod-sym lib-name dialect exports imports parts-list direct-defs)
      (case kind
        ((master)
         `(define-library ,lib-name
            (export ,@exports)
            ,@(reverse imports)))

        ((part)
         (zen:error "Parts cannot be loaded"))))))


(define (zen:transform-module src-form)
  (define (r7rs-export-spec? spec)
    (symbol? spec))
  
  (define (filter-r7rs-exports exports)
    (filter r7rs-export-spec? exports))
  
  (zen:parse-bundle src-form
    (lambda (kind mod-sym lib-name dialect exports imports parts-list direct-defs)
      (case kind
        ((master)
         `(define-library ,lib-name
            (export ,@(filter-r7rs-exports exports))
            (begin #f)))
        
        ((part)
         (zen:error "Parts cannot be loaded"))))))


;;;
;;;; Registration
;;;


(zen:define-syntax bundle
  (lambda (src-form)
    (zen:parse-bundle src-form
      (lambda (kind mod-sym lib-name dialect exports imports parts-list direct-defs)
        (case kind
          ((master)
           (let* ((module-str (%%symbol->string mod-sym))
                  (ns (string-append module-str "#"))
                  (demands (map (lambda (part) `(##demand-module ,(string->symbol (string-append module-str "/" part)))) parts-list))
                  (expansion
                    `(begin
                       (##supply-module ,(string->symbol module-str))
                       (##namespace (,ns))
                       (##import gambit)
                       ,@demands
                       ,@direct-defs)))
             expansion))
          
          ((part)
           `(begin
              (##namespace (,lib-name))
              ,@direct-defs
              (##namespace ("")))))))))


;;;
;;;; Extensions Hook
;;;


(let ((exts ##scheme-file-extensions))
  (if (not (assoc ".zen" exts))
      (##scheme-file-extensions-set! (cons '(".zen" . #f) exts))))


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
;;;; Library Kinds Hook
;;;


(define (zen:read-libdef-zen name reference-src import-name port
                             module-root modref-path)
  (let ((read-first (##global-var-ref '_define-library/define-library-expand#read-first))
        (parse-define-library (##global-var-ref '_define-library/define-library-expand#parse-define-library)))
    (let ((src (read-first port)))
      (if (eof-object? src)
          #f
        (let ((form (##desourcify src)))
          (cond ((and (pair? form) (eq? (car form) 'bundle))
                 (let* ((synth (zen:transform-bundle src))
                        (synth-src (##sourcify synth (##source-locat src))))
                   (parse-define-library synth-src import-name module-root modref-path)))
                ((and (pair? form) (eq? (car form) 'module))
                 (let* ((synth (zen:transform-module src))
                        (synth-src (##sourcify synth (##source-locat src))))
                   (parse-define-library synth-src import-name module-root modref-path)))
                (else
                 (parse-define-library src import-name module-root modref-path))))))))


(define zen:library-kind-installed? #f)

(define (zen:install-zen-library-kind!)
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


(zen:install-zen-library-kind!)


;;;
;;;; Module Search Hook
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
                                         (mod-underscore (string-append "_" mod-name))
                                         (mod-pure-underscore "_"))
                                    (or (##search-module-with-exts
                                          mod-underscore
                                          mod-dir
                                          root
                                          nested-dirs
                                          ##scheme-file-extensions)
                                        (##search-module-with-exts
                                          mod-pure-underscore
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
