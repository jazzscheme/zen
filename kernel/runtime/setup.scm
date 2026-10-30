;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Setup
;;;


(block kernel.setup


;;;
;;;; Stack
;;;


(c-declare #<<end-of-code
#include <stdio.h>
end-of-code
)


(define zen:debug-memory
  (c-lambda (char-string scheme-object) void
    #<<end-of-code
      printf("%s #x%016lx\n", ___arg1, ___arg2);
      ___WORD *ptr = ___UNTAG(___arg2);
      printf("  0    #x%016lx\n", ptr[0]);
      printf("  1    #x%016lx\n", ptr[1]);
      printf("  2    #x%016lx\n", ptr[2]);
      printf("  3    #x%016lx\n", ptr[3]);
end-of-code
))


(define zen:debug-stack
  (c-lambda (char-string scheme-object) void
    #<<end-of-code
      printf("stack  #x%016lx   %s\n", ___arg2, ___arg1);
end-of-code
))


;; quick solution to make classes still objects for stack allocation
(define zen:alloc-still-class
  (c-lambda (int) scheme-object
    #<<end-of-code
      ___SCMOBJ r = ___EXT(___make_vector) (___ps, ___arg1, ___VOID);
      ___EXT(___release_scmobj)(r);
      ___return(r);
end-of-code
))


;;;
;;;; Permissions
;;;


(cond-expand
  (windows)
  (else
   (c-declare #<<end-of-code
#include <sys/stat.h>
end-of-code
)
   (define zen:file-executable?
     (c-lambda (char-string) bool
     #<<end-of-code
       struct stat sb;
       stat(___arg1, &sb);
       ___return(sb.st_mode & S_IXUSR);
end-of-code
))
   (set! zen:file-permissions
         (c-lambda (char-string) int
     #<<end-of-code
       struct stat sb;
       stat(___arg1, &sb);
       ___return(sb.st_mode);
end-of-code
))
   (set! zen:file-permissions-set!
         (c-lambda (char-string int) void
     #<<end-of-code
       chmod(___arg1, ___arg2);
end-of-code
))))


;;;
;;;; Forward
;;;


(zen:define-variable zen:compile-unit)
(zen:define-variable zen:custom-compile-unit)
(zen:define-variable zen:build-unit)
(zen:define-variable zen:get-subunit-names)
(zen:define-variable zen:hub-declaration-class)
(zen:define-variable zen:new-hub-declaration)
(zen:define-variable zen:hub-declaration-hubs)
(zen:define-variable zen:hub-declaration-hubs-set!)
(zen:define-variable zen:hub-declaration-nodes)
(zen:define-variable zen:hub-declaration-nodes-set!)
(zen:define-variable zen:module-shape)
(zen:define-variable zen:manifest-ignore?)
(zen:define-variable zen:manifest-valid?)


;;;
;;;; Container
;;;


(define zen:path->container-override
  (%%make-parameter #f))


;; store unit name instead of path if available
(define (zen:path->container-hook path)
  (let ((override (zen:path->container-override)))
    (or (and override (override path))
        (let ((resource (zen:find-resource path)))
          (if resource
              (%%string->symbol (%%get-resource-path resource))
            #f)))))


(define (zen:container->path-hook container)
  (cond ;; relocate if kernel was moved from source built directory
        ((and (%%string? container)
              zen:kernel-source-built
              zen:kernel-source
              (zen:string-starts-with? container zen:kernel-source-built))
         (%%string-append zen:kernel-source
                          (%%substring container
                                       (%%string-length zen:kernel-source-built)
                                       (%%string-length container))))
        ;; find path from unit name
        ((%%symbol? container)
         (let ((src (zen:find-unit-src container error?: #f)))
           (if src
               (zen:resource-pathname src)
             #f)))
        (else
         #f)))


(define (zen:container->id-hook container)
  (if (%%symbol? container)
      (%%symbol->string container)
    #f))


(%%path->container-hook-set! zen:path->container-hook)
(%%container->path-hook-set! zen:container->path-hook)
(%%container->id-hook-set! zen:container->id-hook)


;;;
;;;; Common
;;;


(zen:define-variable zen:image-load-counter 0)
(zen:define-variable zen:object-load-counter 0)
(zen:define-variable zen:interpreted-load-counter 0)


(define (zen:increment-image-load-counter)
  (set! zen:image-load-counter (%%fx+ 1 zen:image-load-counter)))
(define (zen:increment-object-load-counter)
  (set! zen:object-load-counter (%%fx+ 1 zen:object-load-counter)))
(define (zen:increment-interpreted-load-counter)
  (set! zen:interpreted-load-counter (%%fx+ 1 zen:interpreted-load-counter)))


;;;
;;;; Exit Jobs
;;;


(define (zen:make-exit-jobs-safe)
  (let ((lst (zen:fifo->list (zen:exit-jobs))))
    (zen:clear-exit-jobs!)
    (for-each zen:add-safe-exit-job! lst)))


(define (zen:add-safe-exit-job! thunk)
  (zen:add-exit-job!
    (zen:safe-exit-job thunk)))


(define (zen:safe-exit-job thunk)
  (lambda ()
    (with-exception-catcher
      (lambda (exc)
        (let ((dir (or zen:zen-settings-directory (%%string-append (zen:home-directory) "/.zen/"))))
          (zen:create-directory dir)
          (call-with-output-file (%%string-append dir "exit.exception")
            (lambda (port)
              (write exc port)
              (newline port)
              (newline port)
              (zen:debug-port-setup-width port)
              (display-exception exc port)
              (newline port)
              (newline port)
              (continuation-capture
                (lambda (k)
                  (display-continuation-backtrace k port)))
              (force-output port))))
        (zen:clear-exit-jobs!)
        (zen:exit 1))
      thunk)))


;;;
;;;; Library
;;;


(define (zen:library-main)
  (zen:process-settings)
  (zen:make-exit-jobs-safe)
  (zen:prepare-repositories)
  (zen:setup-repositories))


;;;
;;;; Dynamic Libraries
;;;


(define (zen:load-libraries)
  (define libraries (%%make-table test: eq?))
  (define (add-library product-name library-filename)
    (if (%%table-ref libraries product-name #f)
        #f #;
        ;; quick patch for multiple binary repositories
        ;; ideally we should probably load the most recent library but
        ;; the patch is still correct as libraries validate against the digest of
        ;; the source file and multiple binary repositories are only used for development
        (zen:error "Found duplicate library: {s}" product-name)
      (%%table-set! libraries product-name library-filename)))
  
  ; find the libraries
  #; ;; bazoo
  (zen:iterate-packages #t
    (lambda (package)
      (let ((products (%%get-package-products package)))
        (for-each (lambda (product-descriptor)
                    (let ((product-name (zen:product-descriptor-name product-descriptor)))
                      (zen:with-numbered-library (string-append (zen:product-library-name-base package (%%symbol->string product-name)) "." zen:Library-Extension) #f 1
                        (lambda (filename exists?)
                          (if exists?
                              (add-library product-name filename))))))
                  products))))
  
  ; load the libraries found
  (zen:table-iterate-safe libraries
    (lambda (product-name library-filename)
      (let ((pathname (path-normalize library-filename)))
        (zen:load-binary pathname #t)))))


; this function is called from the library header when loading
(define (zen:register-image-units lib-name units)
  (define (index-for-each proc args n)
    (if (%%not (%%null? args))
        (begin
          (proc (%%car args) n)
          (index-for-each proc (%%cdr args) (%%fx+ n 1)))))
  
  (index-for-each
    (lambda (unit i)
      (let ((name (%%car unit))
            (compile-time-hash (%%cadr unit)))
        (let ((unique-module-name (%%string->symbol (%%string-append zen:lib-uniqueness-prefix (%%symbol->string name)))))
          (let ((load-proc (lambda () (zen:load-required-module unique-module-name))))
            (zen:set-image-unit
              name
              load-proc
              compile-time-hash)))))
    units
    1)
  
;  (zen:feedback (string-append "LIB: " (%%symbol->string lib-name) " (" (%%number->string (%%length units)) " units)"))
)


;;;
;;;; Executable
;;;


(declare (proper-tail-calls))


(define (zen:executable-main)
  (define (missing-argument-for-option opt)
    (set! zen:warnings
          (lambda (output-port)
            (%%write-string
              "*** WARNING -- Missing argument for option \""
              output-port)
            (%%write-string opt output-port)
            (%%write-string "\"\n" output-port)
            #t))
    (zen:repl-main))
  
  (define (number-argument arg)
    (if (%%string? arg)
        (%%string->number arg)
      arg))
  
  (define (symbol-argument arg)
    (if (%%string? arg)
        (%%string->symbol arg)
      arg))
  
  (define (expression-argument arg)
    (if (%%string? arg)
        (call-with-input-string arg read)
      arg))
  
  ;; c -> compile
  ;; e -> eval
  ;; f -> force
  ;; g -> gambit
  ;; k -> check
  ;; l -> load
  ;; p -> parse
  ;; r -> run
  ;; s -> sourcify
  ;; t -> test
  ;; v -> version
  ;; x -> expand
  ;; y -> verify
  (zen:with-quit
    (lambda ()
      (zen:split-command-line (zen:command-arguments)
                               '("v" "version" "nosource" "debug" "f" "force" "force-outlines" "sweep" "worker" "reporting" "keep-c" "track-scheme" "expansion" "gvm" "emit" "dry" "g" "gambit")
                               '("build-repository" "repositories" "dependencies" "recompile-references" "e" "eval" "i" "interpret" "l" "load" "t" "test" "r" "run" "update" "make" "build" "download" "install" "deploy" "p" "parse" "s" "sourcify" "shape" "x" "expand" "k" "check" "y" "verify" "c" "compile" "report" "target" "d" "debugger" "link" "j" "jobs" "port" "load-feedback" "load-expected" "m" "module" "dialect" "listen")
                               missing-argument-for-option
        (lambda (commands options remaining)
          (let ((version? (or (zen:find-option "v" options) (zen:find-option "version" options)))
                (nosource? (zen:find-option "nosource" options))
                (debug? (zen:find-option "debug" options))
                (force? (or (zen:find-option "f" options) (zen:find-option "force" options)))
                (sweep? (zen:find-option "sweep" options))
                (worker? (zen:find-option "worker" options))
                (keep-c? (zen:find-option "keep-c" options))
                (track-scheme? (zen:find-option "track-scheme" options))
                (expansion? (zen:find-option "expansion" options))
                (gvm? (zen:find-option "gvm" options))
                (emit? (zen:find-option "emit" options))
                (dry? (zen:find-option "dry" options))
                (force-outlines? (zen:find-option "force-outlines" options))
                (gambit? (or (zen:find-option "g" options) (zen:find-option "gambit" options)))
                (build-repository (zen:find-option "build-repository" options))
                (repositories (zen:find-option "repositories" options))
                (dependencies (zen:find-option "dependencies" options))
                (recompile-references (expression-argument (zen:find-option "recompile-references" options)))
                (ev (or (zen:find-option "e" options) (zen:find-option "eval" options)))
                (load (or (zen:find-option "l" options) (zen:find-option "load" options)))
                (interpret (or (zen:find-option "i" options) (zen:find-option "interpret" options)))
                (test (or (zen:find-option "t" options) (zen:find-option "test" options)))
                (run (or (zen:find-option "r" options) (zen:find-option "run" options)))
                (update (zen:find-option "update" options))
                (make (zen:find-option "make" options))
                (build (zen:find-option "build" options))
                (download (zen:find-option "download" options))
                (install (zen:find-option "install" options))
                (deploy (zen:find-option "deploy" options))
                (parse (or (zen:find-option "p" options) (zen:find-option "parse" options)))
                (sourcify (or (zen:find-option "s" options) (zen:find-option "sourcify" options)))
                (shape (zen:find-option "shape" options))
                (expand (or (zen:find-option "x" options) (zen:find-option "expand" options)))
                (check (or (zen:find-option "k" options) (zen:find-option "check" options)))
                (verify (or (zen:find-option "y" options) (zen:find-option "verify" options)))
                (compile (or (zen:find-option "c" options) (zen:find-option "compile" options)))
                (report (zen:find-option "report" options))
                (reporting? (zen:find-option "reporting" options))
                (target (symbol-argument (zen:find-option "target" options)))
                (debugger (or (zen:find-option "d" options) (zen:find-option "debugger" options)))
                (link (symbol-argument (zen:find-option "link" options)))
                (jobs (number-argument (or (zen:find-option "j" options) (zen:find-option "jobs" options))))
                (port (number-argument (zen:find-option "port" options)))
                (load-feedback (number-argument (zen:find-option "load-feedback" options)))
                (load-expected (number-argument (zen:find-option "load-expected" options)))
                (module (symbol-argument (or (zen:find-option "m" options) (zen:find-option "module" options))))
                (dialect (symbol-argument (zen:find-option "dialect" options))))
            (define (setup-kernel)
              (if zen:kernel-install
                  (let ((gambitdir (zen:absolutize-directory zen:kernel-install zen:gambit-dir)))
                    (if (and gambitdir (file-exists? gambitdir))
                        (zen:set-gambitdir! gambitdir))))
              (zen:allow-inner-global-define?-set! #t)
              (set! zen:debugger debugger)
              (if load-feedback
                  (zen:load-feedback-setup (open-tcp-client load-feedback) load-expected))
              (if nosource?
                  (set! zen:kernel-source-access? #f))
              (zen:process-settings)
              (zen:make-exit-jobs-safe))
            
            (define (setup-repositories make? libraries?)
              (if build-repository (zen:build-repository build-repository))
              (if repositories (zen:repositories repositories))
              (zen:prepare-repositories)
              (if libraries?
                  (zen:load-libraries))
              (zen:setup-repositories))
            
            (define (setup-runtime)
              (setup-kernel)
              (setup-repositories #f #t)
              ;; to test cross compiling REMOVE CODE WHEN DONE
              (setup-target))
            
            (define (setup-build #!optional (make? #f))
              (zen:setup-kernel-source)
              (setup-kernel)
              (setup-repositories make? #f)
              (set! zen:link (or link (zen:build-link)))
              (set! zen:link-options (zen:parse-link zen:link))
              (set! zen:jobs jobs)
              (set! zen:recompile-references recompile-references)
              (if (not zen:kernel-interpreted?)
                  (zen:disable-crash-window))
              (if (or debug? (%%eqv? jobs 0) recompile-references dry? force-outlines?)
                  (zen:debug-build? #t))
              (if keep-c?
                  (set! zen:compile-options (%%cons 'keep-c zen:compile-options)))
              (if track-scheme?
                  (set! zen:compile-options (%%cons 'track-scheme zen:compile-options)))
              (if expansion?
                  (set! zen:compile-options (%%cons 'expansion zen:compile-options)))
              (if gvm?
                  (set! zen:compile-options (%%cons 'gvm zen:compile-options)))
              (if emit?
                  (zen:save-emit? #t))
              (if dry?
                  (zen:dry-run? #t))
              (if force-outlines?
                  (zen:force-outlines? #t))
              (if report
                  (zen:setup-report report))
              (if reporting?
                  (zen:setup-reporting))
              (setup-target))
            
            (define (setup-target)
              (if target
                  (let ((configuration (zen:find-named-configuration target)))
                    (if (%%not configuration)
                        (zen:error "Unknown configuration: {s}" target)
                      (begin
                        (zen:build-target target)
                        (zen:build-configuration configuration)
                        ;; quick hack load everything needed for compilation before changing repositories
                        ;; this will clearly be missing user-defined syntax
                        (zen:load-unit 'zen)
                        (zen:load-unit 'zen/dialect/syntax)
                        (zen:load-unit 'scheme/syntax-rules)
                        (zen:load-unit 'zen/core)
                        (zen:load-unit 'zen/foreign)
                        (zen:load-unit 'zen/foreign/syntax)
                        (let ((target-platform (zen:get-configuration-platform configuration))
                              (target-processor (zen:get-configuration-processor configuration)))
                          (if (%%eq? target-platform 'ios)
                              (%%cond-expand-features (zen:remove 'cocoa (%%cond-expand-features))))
                          (%%cond-expand-features (append (%%cond-expand-features) (list target-platform)))
                          (if target-processor
                              (%%cond-expand-features (append (%%cond-expand-features) (list target-processor)))))
                        (zen:TARGET-HACK?-set! #t)
                        #; ;; bazoo
                        (let ((old zen:Build-Repository))
                          ;; quick hack around the fact that the kernel doesn't know the directory from which it was built
                          ;; this directory would be the project or solution or ... when I do a complete overhaul to all this
                          (define (determine-root)
                            (if (zen:string-ends-with? zen:kernel-source "/zen/")
                                (substring zen:kernel-source 0 (- (string-length zen:kernel-source) (string-length "zen/")))
                              zen:kernel-source))
                          
                          (set! zen:Build-Repository (zen:make-repository 'Build "lib" (%%string-append (determine-root) (zen:get-configuration-destination configuration)) binary?: #t dynamic?: #t))
                          (set! zen:Repositories (%%append (zen:remove old zen:Repositories) (%%list zen:Build-Repository)))))))))
            
            (define (setup-dowload)
              (setup-build))
            
            (define (setup-install)
              (setup-build))
            
            (define (setup-deploy)
              (setup-build))
            
            (define (run-scripts lst)
              (let iter ((scan lst))
                   (if (%%not (%%null? scan))
                       (let ((arg (%%car scan)))
                         (if (%%not (zen:option? arg))
                             (begin
                               (run-script arg)
                               (iter (%%cdr scan))))))))
            
            (define (run-script arg)
              (let ((path (add-zen arg)))
                (if (file-exists? path)
                    (zen:load-script path)
                  (zen:error "Unable to find script {s}" path))))
            
            (define (add-zen path)
              (if (zen:pathname-extension path)
                  path
                (zen:add-extension path "zen")))
            
            (define (show-version)
              (%%write-string "Gambit" zen:stdout-port)
              (%%write-string " " zen:stdout-port)
              (%%write-string (system-version-string) zen:stdout-port)
              (%%write-string " " zen:stdout-port)
              (%%write (system-stamp) zen:stdout-port)
              (%%write-string " " zen:stdout-port)
              (%%write-string (system-type-string) zen:stdout-port)
              (%%write-string " " zen:stdout-port)
              (%%write (configure-command-string) zen:stdout-port)
              (%%newline zen:stdout-port))
            
            (cond (version?
                   (show-version))
                  (ev
                   (setup-runtime)
                   (eval (call-with-input-string ev read)))
                  (load
                   (setup-runtime)
                   (zen:load-unit (%%string->symbol load)))
                  (interpret
                   (setup-runtime)
                   (zen:setup-script)
                   (cond-expand
                     (mac
                      ;; hack around open setting the current directory arbitrarily to /
                      (if (and (%%equal? (current-directory) "/")
                               (%%not (zen:string-starts-with? interpret "/")))
                          (let ((path (add-zen (%%string-append zen:kernel-bundle-install interpret))))
                            (if (file-exists? path)
                                (run-script path)
                              ;; there will be no controlling terminal so no error possible
                              (let ((port (console-port)))
                                (display (zen:format "Unable to find script {s}" path) port)
                                (newline port)
                                (force-output port)
                                (exit))))
                        (run-script interpret)))
                     (else
                      (run-script interpret))))
                  (test
                   (setup-runtime)
                   (zen:test-product (%%string->symbol test)))
                  (run
                   (setup-runtime)
                   (zen:run-product (%%string->symbol run)))
                  (zen:product
                   (setup-runtime)
                   (zen:run-product zen:product))
                  (parse
                   (setup-build)
                   ((zen:global-ref 'zen:parse-source) (%%string->symbol parse)))
                  (sourcify
                   (setup-build)
                   ((zen:global-ref 'zen:expand-source) (%%string->symbol sourcify)))
                  (shape
                   (setup-build)
                   ((zen:global-ref 'zen:shape-unit) (%%string->symbol shape)))
                  (expand
                   (setup-build)
                   ((zen:global-ref 'zen:expand-to) (%%string->symbol expand)))
                  (check
                   (setup-build)
                   ((zen:global-ref 'zen:check-unit) (%%string->symbol check)))
                  (verify
                   (setup-build)
                   ((zen:global-ref 'zen:verify-unit) (%%string->symbol verify)))
                  (compile
                   (setup-build)
                   (for-each (lambda (name)
                               (zen:custom-compile-unit (%%string->symbol name) force?: force?))
                             (zen:split-string compile #\;)))
                  (update
                   (setup-build)
                   (zen:update-product (%%string->symbol update)))
                  (make
                   (setup-build #t)
                   (exit (zen:make-product (%%string->symbol make))))
                  (worker?
                   (setup-build)
                   (zen:worker-process port))
                  (build
                   (setup-build)
                   (let ((name (%%string->symbol build)))
                     (define (assert-build-configuration)
                       (if (%%not (zen:build-configuration))
                           (zen:error "Building a kernel requires an explicit target")))
                     
                     (case name
                       ((kernel)
                        (assert-build-configuration)
                        (zen:build-kernel))
                       ((kernellib)
                        (assert-build-configuration)
                        (zen:build-kernel image: 'library))
                       (else
                        (zen:build-product name)))))
                  (download
                   (setup-dowload)
                   (zen:download-product (%%string->symbol download)))
                  (install
                   (setup-install)
                   (zen:install-product (%%string->symbol install)))
                  (deploy
                   (setup-deploy)
                   (zen:deploy-product (%%string->symbol deploy)))
                  ((or (%%not (%%null? commands))
                       (%%not (%%null? remaining)))
                   (setup-runtime)
                   (zen:setup-script)
                   (run-scripts (%%append commands remaining)))
                  (else
                   (if debug?
                       (setup-build)
                     (setup-runtime))
                   (zen:repl-main gambit? module dialect)))))))))


;;;
;;;; Quit
;;;


(define zen:quit-marker
  '(quit))


(define zen:quit-thread
  #f)


(define (zen:with-quit thunk)
  (%%continuation-capture
    (lambda (cont)
      (set! zen:quit-thread (current-thread))
      (let ((previous-handler (current-exception-handler)))
        (with-exception-handler
          (lambda (exc)
            (if (and (%%pair? exc) (%%eq? (%%car exc) zen:quit-marker))
                (let ((exit-status (%%cdr exc)))
                  (%%continuation-graft cont
                    (lambda ()
                      (exit exit-status))))
              (previous-handler exc)))
          thunk)))))


(define (zen:quit #!optional (status 0))
  (define (raise-quit)
    (raise (%%cons zen:quit-marker status)))
  
  (if (%%eq? (%%current-thread) zen:quit-thread)
      (raise-quit)
    (##thread-int! zen:quit-thread raise-quit)))


;;;
;;;; REPL
;;;


(define zen:expansion-module
  'terminal)

(define zen:expansion-dialect
  'zen)


(define (zen:repl-main #!optional (gambit? #f) (module #f) (dialect #f))
  (zen:setup-readtable)
  (zen:setup-expansion-hook)
  (if gambit?
      (set! zen:expansion-module #f))
  (if module
      (set! zen:expansion-module module))
  (if dialect
      (if (eq? dialect 'none)
          (set! zen:expansion-module #f)
        (set! zen:expansion-dialect dialect)))
  (current-input-port (repl-input-port))
  (current-output-port (repl-output-port))
  (current-error-port (repl-output-port))
  (parameterize ((zen:walk-for 'terminal)
                 (zen:requested-unit-name zen:expansion-module)
                 (zen:generate-symbol-for "&")
                 (zen:generate-symbol-context 'terminal))
    (%%repl-debug
      (lambda (first output-port)
        (if zen:warnings
            (zen:warnings output-port))
        (display "ZenScheme v" output-port)
        (display (zen:present-version zen:kernel-version) output-port)
        (newline output-port)
        (newline output-port)
        (force-output output-port)
        #f)
      #f
      #t)))


(define (zen:setup-readtable)
  (input-port-readtable-set! (repl-input-port) zen:zen-readtable))


(define (zen:setup-expansion-hook)
  (zen:expand-source-set!
    (lambda (src)
      (let ((expansion (zen:expand-src src)))
        (if (zen:debug-expansion?)
            (pp (list src '---> expansion)))
        expansion))))


(define (zen:expand-src src)
  (if (%%eq? (zen:walk-for) 'terminal)
      (let ((code (%%desourcify src)))
        (if (and (%%pair? code) (%%eq? (%%car code) 'in))
            (if (%%null? (%%cdr code))
                (%%sourcify
                  `',zen:expansion-module
                  src)
              (let ((module (%%cadr code)))
                (set! zen:expansion-module module)
                (%%sourcify
                  `',module
                  src)))
          (cond ((%%not zen:expansion-module)
                 src)
                (else
                 (if (%%neq? zen:expansion-module 'terminal)
                     (begin
                       (zen:load-unit zen:expansion-module)
                       ((zen:global-ref 'zen:outline-unit) zen:expansion-module)))
                 (%%sourcify
                   `(module ,zen:expansion-module ,zen:expansion-dialect ,src)
                   src)))))
    src))


(zen:add-safe-exit-job!
  (lambda ()
    (if zen:*report-port*
        (begin
          (close-port zen:*report-port*)
          (set! zen:*report-port* #f))))))
