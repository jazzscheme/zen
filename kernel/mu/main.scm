;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Mu Main
;;;


(declare (proper-tail-calls))


(define (mu:mu-main)
  (define (missing-argument-for-option opt)
    (set! mu:warnings
          (lambda (output-port)
            (%%write-string
              "*** WARNING -- Missing argument for option \""
              output-port)
            (%%write-string opt output-port)
            (%%write-string "\"\n" output-port)
            #t))
    (mu:repl-main))
  
  (define (number-argument arg)
    (if (string? arg)
        (string->number arg)
      arg))
  
  (define (symbol-argument arg)
    (if (string? arg)
        (string->symbol arg)
      arg))
  
  (define (expression-argument arg)
    (if (string? arg)
        (call-with-input-string arg read)
      arg))
  
  ;; c -> compile
  ;; f -> force
  ;; v -> version
  (mu:with-quit
    (lambda ()
      (mu:split-command-line (mu:command-arguments)
                               '("v" "version" "nosource" "debug" "f" "force" "force-outlines" "sweep" "worker" "reporting" "keep-c" "track-scheme" "expansion" "gvm" "emit" "dry")
                               '("build-repository" "repositories" "dependencies" "recompile-references" "update" "make" "build" "download" "install" "deploy" "c" "compile" "report" "target" "d" "debugger" "link" "j" "jobs" "port" "load-feedback" "load-expected" "m" "module" "dialect" "listen")
                               missing-argument-for-option
        (lambda (commands options remaining)
          (let ((version? (or (mu:find-option "v" options) (mu:find-option "version" options)))
                (nosource? (mu:find-option "nosource" options))
                (debug? (mu:find-option "debug" options))
                (force? (or (mu:find-option "f" options) (mu:find-option "force" options)))
                (sweep? (mu:find-option "sweep" options))
                (worker? (mu:find-option "worker" options))
                (keep-c? (mu:find-option "keep-c" options))
                (track-scheme? (mu:find-option "track-scheme" options))
                (expansion? (mu:find-option "expansion" options))
                (gvm? (mu:find-option "gvm" options))
                (emit? (mu:find-option "emit" options))
                (dry? (mu:find-option "dry" options))
                (force-outlines? (mu:find-option "force-outlines" options))
                (gambit? (or (mu:find-option "g" options) (mu:find-option "gambit" options)))
                (build-repository (mu:find-option "build-repository" options))
                (repositories (mu:find-option "repositories" options))
                (dependencies (mu:find-option "dependencies" options))
                (recompile-references (expression-argument (mu:find-option "recompile-references" options)))
                (update (mu:find-option "update" options))
                (make (mu:find-option "make" options))
                (build (mu:find-option "build" options))
                (download (mu:find-option "download" options))
                (install (mu:find-option "install" options))
                (deploy (mu:find-option "deploy" options))
                (compile (or (mu:find-option "c" options) (mu:find-option "compile" options)))
                (report (mu:find-option "report" options))
                (reporting? (mu:find-option "reporting" options))
                (target (symbol-argument (mu:find-option "target" options)))
                (debugger (or (mu:find-option "d" options) (mu:find-option "debugger" options)))
                (link (symbol-argument (mu:find-option "link" options)))
                (jobs (number-argument (or (mu:find-option "j" options) (mu:find-option "jobs" options))))
                (port (number-argument (mu:find-option "port" options)))
                (load-feedback (number-argument (mu:find-option "load-feedback" options)))
                (load-expected (number-argument (mu:find-option "load-expected" options)))
                (module (symbol-argument (or (mu:find-option "m" options) (mu:find-option "module" options))))
                (dialect (symbol-argument (mu:find-option "dialect" options))))
            (define (setup-kernel)
              (if mu:kernel-install
                  (let ((gambitdir (mu:absolutize-directory mu:kernel-install mu:gambit-dir)))
                    (if (and gambitdir (file-exists? gambitdir))
                        (mu:set-gambitdir! gambitdir))))
              (mu:allow-inner-global-define?-set! #t)
              (set! mu:debugger debugger)
              (if load-feedback
                  (mu:load-feedback-setup (open-tcp-client load-feedback) load-expected))
              (if nosource?
                  (set! mu:kernel-source-access? #f))
              (mu:process-settings)
              (mu:make-exit-jobs-safe))
            
            (define (setup-repositories libraries?)
              (if build-repository (mu:build-repository build-repository))
              (if repositories (mu:repositories repositories))
              (mu:prepare-repositories)
              (if libraries?
                  (mu:load-libraries))
              (mu:setup-repositories))
            
            (define (setup-runtime)
              (setup-kernel)
              (setup-repositories #t))
            
            (define (setup-build)
              (mu:setup-kernel-source)
              (setup-kernel)
              (setup-repositories #f)
              (set! mu:link (or link (mu:build-link)))
              (set! mu:link-options (mu:parse-link mu:link))
              (set! mu:jobs jobs)
              (set! mu:recompile-references recompile-references)
              (mu:disable-crash-window)
              (if (or debug? (eqv? jobs 0) recompile-references dry? force-outlines?)
                  (mu:debug-build? #t))
              (if keep-c?
                  (set! mu:compile-options (cons 'keep-c mu:compile-options)))
              (if track-scheme?
                  (set! mu:compile-options (cons 'track-scheme mu:compile-options)))
              (if expansion?
                  (set! mu:compile-options (cons 'expansion mu:compile-options)))
              (if gvm?
                  (set! mu:compile-options (cons 'gvm mu:compile-options)))
              (if emit?
                  (mu:save-emit? #t))
              (if dry?
                  (mu:dry-run? #t))
              (if force-outlines?
                  (mu:force-outlines? #t))
              (if report
                  (mu:setup-report report))
              (if reporting?
                  (mu:setup-reporting))
              (setup-target))
            
            (define (setup-target)
              (if target
                  (begin
                    (mu:load-configurations)
                    (let ((configuration (mu:find-configuration target)))
                      (if (not configuration)
                          (mu:error "Unknown configuration: {s}" target)
                        (begin
                          (mu:build-target target)
                          (mu:build-configuration configuration)))))))
            
            (define (show-version)
              (%%write-string "Gambit" mu:stdout-port)
              (%%write-string " " mu:stdout-port)
              (%%write-string (system-version-string) mu:stdout-port)
              (%%write-string " " mu:stdout-port)
              (%%write (system-stamp) mu:stdout-port)
              (%%write-string " " mu:stdout-port)
              (%%write-string (system-type-string) mu:stdout-port)
              (%%write-string " " mu:stdout-port)
              (%%write (configure-command-string) mu:stdout-port)
              (%%newline mu:stdout-port))
            
            (cond (version?
                   (show-version))
                  (compile
                   (setup-build)
                   (for-each (lambda (name)
                               (mu:custom-compile-unit (string->symbol name) force?: force?))
                             (mu:split-string compile #\;)))
                  (update
                   (setup-build)
                   (mu:update-product (string->symbol update)))
                  (make
                   (setup-build)
                   (exit (mu:make-product-parallel (string->symbol make))))
                  (worker?
                   (setup-build)
                   (mu:worker-process port))
                  (build
                   (setup-build)
                   (let ((name (string->symbol build)))
                     (define (assert-build-configuration)
                       (if (not (mu:build-configuration))
                           (mu:error "Building a kernel requires an explicit target")))
                     
                     (case name
                       ((kernel)
                        (assert-build-configuration)
                        (mu:build-kernel))
                       ((kernellib)
                        (assert-build-configuration)
                        (mu:build-kernel image: 'library))
                       (else
                        (mu:build-product name)))))
                  (download
                   (setup-build)
                   (mu:download-product (string->symbol download)))
                  (install
                   (setup-build)
                   (mu:install-product (string->symbol install)))
                  (deploy
                   (setup-build)
                   (mu:deploy-product (string->symbol deploy)))
                  (else
                   (mu:error "Unkown mu command: {l}" (command-args))))))))))
