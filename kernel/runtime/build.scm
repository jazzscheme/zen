;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Build
;;;


(block kernel.build


;;;
;;;; Gambit
;;;


(define (zen:gambitcomp op
                         output-dir
                         input-filenames
                         output-filename
                         cc-options
                         ld-options-prelude
                         ld-options
                         options)
  ;; bongo
  (##gambuild 'C
              op
              output-dir
              (%%memq 'verbose options)
              (##gambuild-params
                #f ;; bongo ;; base-library
                (##map (lambda (path) (##path-relative-to-dir path output-dir))
                       input-filenames)
                ;; input-filenames
                output-filename
                #f ;; bongo ;; cc
                cc-options
                ld-options-prelude
                ld-options
                #f ;; bongo ;; pkg-config
                #f ;; bongo ;; pkg-config-path
                #f ;; bongo ;; meta-info-file
                ))
  #; ;; bongo
  (if (zen:gambitzen?)
      (##gambcomp 'C
                  op
                  output-dir
                  input-filenames
                  output-filename
                  (%%memq 'verbose options)
                  (%%memq 'hide-console options)
                  (%%list (%%cons "CC_OPTIONS" cc-options)
                          (%%cons "LD_OPTIONS_PRELUDE" ld-options-prelude)
                          (%%cons "LD_OPTIONS" ld-options)))
    (##gambcomp 'C
                op
                output-dir
                input-filenames
                output-filename
                (%%memq 'verbose options)
                (%%list (%%cons "CC_OPTIONS" cc-options)
                        (%%cons "LD_OPTIONS_PRELUDE" ld-options-prelude)
                        (%%cons "LD_OPTIONS" ld-options)))))


;;;
;;;; Compiler
;;;


(define (zen:compiler-name)
  (if (eq? zen:kernel-compiler 'c++)
      "g++"
    "gcc"))


(define (zen:compiler-extension)
  (if (eq? zen:kernel-compiler 'c++)
      "cpp"
    "c"))


(define (zen:language-extension language)
  (let ((compiler zen:kernel-compiler))
    (case language
      ((objc) (case compiler ((c) "m") ((c++) "mm")))
      ((c) (case compiler ((c) "c") ((c++) "cpp")))
      (else (zen:error "Unknown language: {s}" language)))))


;;;
;;;; Version
;;;


(define (zen:for-each-zen-version proc)
  (for-each proc (zen:get-zen-versions)))


(define (zen:for-each-higher-zen-version version proc)
  (zen:for-each-higher-versions (zen:get-zen-versions) version proc))


(define (zen:for-each-higher-versions versions target-version proc)
  (let iter ((versions versions))
    (if (%%not (%%null? versions))
        (let ((version (%%car versions)))
          (if (or (%%not target-version)
                  (%%fx> (zen:get-version-number version) target-version))
              (begin
                (proc version)
                (iter (%%cdr versions))))))))


(define (zen:for-each-higher-version version-alist proc)
  (for-each (lambda (repo)
              (let ((name (%%get-repository-name repo))
                    (versions (zen:load-repository-versions repo)))
                (let ((pair (%%assq name version-alist)))
                  (let ((target-version (if (%%pair? pair) (%%cdr pair) #f)))
                    (zen:for-each-higher-versions versions target-version
                      proc)))))
            zen:manifest-repositories))


(define (zen:kernel/product-needs-rebuild? version-file)
  (receive (version gambit-version gambit-stamp) (zen:load-version-file version-file)
    (if (%%not version)
        #t
      (or (%%not (zen:gambit-uptodate? gambit-version gambit-stamp))
          (let ((rebuild? #f))
            (zen:for-each-higher-zen-version version
              (lambda (zen-version)
                (if (%%memq (zen:get-version-rebuild zen-version) '(kernel all))
                    (set! rebuild? #t))))
            rebuild?)))))


(define (zen:kernel/product-architecture-needs-rebuild? version-file)
  (receive (version gambit-version gambit-stamp) (zen:load-version-file version-file)
    (if (%%not version)
        #t
      (or (%%not (zen:gambit-uptodate? gambit-version gambit-stamp))
          (let ((rebuild-architecture? #f))
            (zen:for-each-higher-zen-version version
              (lambda (zen-version)
                (if (or (%%memq (zen:get-version-rebuild zen-version) '(kernel all))
                        (zen:get-version-recompile zen-version)
                        (zen:get-version-recompile-references zen-version))
                    (set! rebuild-architecture? #t))))
            rebuild-architecture?)))))


(define (zen:build-repository-needs-sweep-impl)
  (let ((dir (%%string-append zen:kernel-install "build/kernel/")))
    (let ((swept-file (%%string-append dir "version-swept")))
      (define (determine-version)
        (if (file-exists? swept-file)
            (receive (version gambit-version gambit-stamp) (zen:load-version-file swept-file)
              version)
          (let ((version-file (%%string-append dir "kernel/version")))
            (if (file-exists? version-file)
                (receive (version gambit-version gambit-stamp) (zen:load-version-file version-file)
                  version)
              #f))))
      
      (let ((version (determine-version)))
        (if (%%not version)
            #f
          (let ((sweep #f))
            (zen:for-each-higher-zen-version version
              (lambda (zen-version)
                (let ((version-sweep (zen:get-version-sweep zen-version)))
                  (if version-sweep
                      (set! sweep version-sweep)))))
            (if sweep
                (begin
                  (zen:create-directories dir)
                  (zen:save-version-file swept-file)))
            sweep))))))


(define (zen:manifest-needs-rebuild?-impl manifest)
  (let ((name (%%get-manifest-name manifest))
        (version (zen:manifest-zen-version manifest)))
    (let ((rebuild? #f))
      (zen:for-each-higher-zen-version version
        (lambda (zen-version)
          (let ((rebuild (zen:get-version-rebuild zen-version))
                (recompile (zen:get-version-recompile zen-version)))
            (if (or (%%eq? rebuild 'all)
                    (and recompile (%%memq name recompile)))
                (set! rebuild? #t)))))
      rebuild?)))


(define (zen:load-version-file version-file)
  (if (file-exists? version-file)
      (call-with-input-file (%%list path: version-file eol-encoding: 'cr-lf)
        (lambda (input)
          (let ((version (read input))
                (gambit-version (read input))
                (gambit-stamp (read input)))
            (values version gambit-version gambit-stamp))))
    (values #f #f #f)))


(define (zen:save-version-file version-file #!optional (version #f))
  (call-with-output-file (list path: version-file eol-encoding: (zen:platform-eol-encoding zen:kernel-platform))
    (lambda (output)
      (write (or version (zen:get-zen-version-number)) output)
      (newline output)
      (write (system-version) output)
      (newline output)
      (write (system-stamp) output)
      (newline output))))


;;;
;;;; Kernel
;;;


(define (zen:build-kernel-impl #!key (image #f))
  (let ((configuration (zen:build-configuration))
        (image (or image 'executable)))
    (let ((name (zen:get-configuration-name configuration))
          (system (zen:get-configuration-system configuration))
          (platform (zen:get-configuration-platform configuration))
          (compiler (zen:get-configuration-compiler configuration))
          (processor (zen:get-configuration-processor configuration))
          (windowing (zen:get-configuration-windowing configuration))
          (safety (zen:get-configuration-safety configuration))
          (optimize? (zen:get-configuration-optimize? configuration))
          (debug-environments? (zen:get-configuration-debug-environments? configuration))
          (debug-location? (zen:get-configuration-debug-location? configuration))
          (debug-source? (zen:get-configuration-debug-source? configuration))
          (debug-foreign? (zen:get-configuration-debug-foreign? configuration))
          (track-memory? (zen:get-configuration-track-memory? configuration))
          (mutable-bindings? (zen:get-configuration-mutable-bindings? configuration))
          (include-compiler? (not (eq? image 'library)))
          (kernel-interpret? (zen:get-configuration-kernel-interpret? configuration))
          (destination (zen:get-configuration-destination configuration))
          (destination-directory (zen:configuration-directory configuration))
          (features (zen:get-configuration-features configuration))
          (properties (zen:get-configuration-properties configuration)))
      (zen:build-image #f
                        system:                system
                        platform:              platform
                        compiler:              compiler
                        processor:             processor
                        windowing:             windowing
                        safety:                safety
                        optimize?:             optimize?
                        debug-environments?:   debug-environments?
                        debug-location?:       debug-location?
                        debug-source?:         debug-source?
                        debug-foreign?:        debug-foreign?
                        track-memory?:         track-memory?
                        mutable-bindings?:     mutable-bindings?
                        include-compiler?:     include-compiler?
                        kernel-interpret?:     kernel-interpret?
                        destination:           destination
                        destination-directory: destination-directory
                        features:              features
                        properties:            properties
                        image:                 image
                        kernel?:               #t
                        console?:              #t))))


;;;
;;;; Image
;;;


(define zen:*changed-units*
  '())


(define (zen:get-changed-units-impl)
  zen:*changed-units*)

(define (zen:push-changed-units-impl unit)
  (set! zen:*changed-units* (%%cons unit zen:*changed-units*)))

(define (zen:reset-changed-units-impl)
  (set! zen:*changed-units* '()))


(define (zen:build-image-impl product
          #!key
          (system zen:kernel-system)
          (platform zen:kernel-platform)
          (compiler zen:kernel-compiler)
          (processor zen:kernel-processor)
          (windowing zen:kernel-windowing)
          (safety zen:kernel-safety)
          (optimize? zen:kernel-optimize?)
          (debug-environments? zen:kernel-debug-environments?)
          (debug-location? zen:kernel-debug-location?)
          (debug-source? zen:kernel-debug-source?)
          (debug-foreign? zen:kernel-debug-foreign?)
          (track-memory? zen:kernel-track-memory?)
          (mutable-bindings? zen:kernel-mutable-bindings?)
          (include-compiler? #f)
          (kernel-interpret? #f)
          (source zen:kernel-source)
          (destination zen:kernel-destination)
          (destination-directory zen:kernel-install)
          (features zen:kernel-features)
          (properties zen:kernel-properties)
          (executable #f)
          (resources #f)
          (bundle #f)
          (image #f)
          (kernel? #f)
          (console? #f)
          (minimum-heap #f)
          (maximum-heap #f)
          (static-libraries '())
          (feedback zen:feedback))
  (let ((product-name (if (%%not product) "kernel" (%%symbol->string product)))
        (gambit-library (if include-compiler? "gambitgsc" "gambit"))
        (library-image? (%%eq? image 'library))
        (static-info (zen:static-info static-libraries)))
    (let ((image-name (if (%%not product) "zen" product-name))
          (gambit-dir (path-normalize "~~/"))
          (source-dir (zen:relativise-directory "./" "./" source))
          (build-dir (if product (%%get-repository-directory zen:Build-Repository) destination-directory))
          (kernel-dir (string-append destination-directory "build/kernel/"))
          (product-dir (string-append destination-directory "build/products/" product-name "/"))
          (image-dir (if (and bundle (eq? windowing 'cocoa) (not library-image?))
                         (%%string-append destination-directory bundle ".app" "/Contents/MacOS/")
                       destination-directory))
          (static-units (%%car static-info))
          (static-options (%%cadr static-info))
          (static-languages (%%cddr static-info)))
      (define mac?
        (eq? platform 'mac))
      
      (define ios?
        (and (zen:build-configuration) (eq? (zen:get-configuration-platform (zen:build-configuration)) 'ios)))
      
      (define ios-processor
        (and ios? (or (zen:get-configuration-processor (zen:build-configuration)) 'x86)))
      
      (define ios-architecture
        (and ios? (case ios-processor
                    ((x86) 'x86_64)
                    (else 'arm64))))
      
      (define ios-sysroot
        (and ios? (case ios-architecture
                ((x86_64) "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk")
                (else "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk"))))
      
      (define ios-custom-cc
        (and ios? "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang"))
      
      (define ios-custom-cc-options
        (and ios? (list "-arch" (symbol->string ios-architecture) "-fmessage-length=0" "-I/Users/magnan/Downloads/couchbase-lite-ios-enterprise_1-3/CouchbaseLite.framework/Headers" "-fdiagnostics-show-note-include-stack" "-fmacro-backtrace-limit=0" "-Wno-c++11-compat-deprecated-writable-strings" "-fmodules" "-gmodules"
                    "-Wnon-modular-include-in-framework-module" "-Werror=non-modular-include-in-framework-module" "-Wno-trigraphs" "-fpascal-strings" "-O0"
                    "-fno-common" "-fno-objc-arc" "-Wno-missing-field-initializers" "-Wno-missing-prototypes" "-Werror=return-type" "-Wunreachable-code"
                    "-Werror=deprecated-objc-isa-usage" "-Werror=objc-root-class" "-Wno-missing-braces" "-Wparentheses" "-Wswitch" "-Wempty-body"
                    "-Wconditional-uninitialized" "-Wno-unknown-pragmas" "-Wno-shadow" "-Wno-four-char-constants" "-Wno-conversion" "-Wconstant-conversion"
                    "-Wint-conversion" "-Wbool-conversion" "-Wenum-conversion" "-Wno-newline-eof"
                    "-isysroot" ios-sysroot "-fasm-blocks" "-fstrict-aliasing" "-Wdeprecated-declarations" "-mios-simulator-version-min=9.0" "-Wno-sign-conversion")))
      
      (define ios-gambit-include-dir
        (path-expand "~~ios/include"))
      
      (define ios-gambit-lib-dir
        (path-expand "~~ios/lib"))
      
      (define (source-file path)
        (%%string-append source-dir path))
      
      (define (build-file path)
        (%%string-append build-dir path))
      
      (define (dest-file path)
        (%%string-append destination-directory path))
      
      (define (kernel-file path)
        (%%string-append kernel-dir path))
      
      (define (product-file path)
        (%%string-append product-dir path))
      
      (define (print line output)
        (display line output)
        (newline output))
      
      (define (feedback-message fmt-string . rest)
        (if feedback
            (apply feedback fmt-string rest)))
      
      (define (compile-unit rebuild? name dir output)
        (let ((src (string-append dir name ".scm"))
              (dst (string-append output name "." (zen:compiler-extension)))
              (digest (string-append output name "." zen:Digest-Extension))
              (mnf (string-append output name "." zen:Manifest-Extension)))
          (let ((hash-changed? (%%not (zen:manifest-uptodate? src (zen:load-updated-manifest name digest mnf src)))))
            (if (or rebuild? hash-changed? (%%not (file-exists? dst)))
                (let ((path (%%string-append dir name))
                      (options `(,@(if debug-environments? '(debug-environments) '())
                                 ,@(if debug-location? '(debug-location) '())
                                 ,@(if debug-source? '(debug-source) '())
                                 ,@(if zen:enable-track-scheme? '(track-scheme) '()))))
                  ;; standardize path as it will be the path stored in debugging information
                  (let ((standardized-path (zen:pathname-standardize (path-normalize path))))
                    (zen:push-changed-units path)
                    (feedback-message "; compiling {a}..." path)
                    (if (not (zen:dry-run?))
                        (begin
                          (parameterize ((zen:walk-for 'compile))
                            (compile-file-to-target standardized-path options: options output: dst))
                          (if ios?
                              (let ((custom-cc-options ios-custom-cc-options))
                                (zen:call-process
                                  (list
                                    path: ios-custom-cc
                                    arguments: `(,@custom-cc-options ,(%%string-append "-I" ios-gambit-include-dir) "-D___DYNAMIC" "-c" "-o" ,(string-append output name ".o") ,dst))))
                            (let ((cc-options (case platform
                                                ((mac) (case processor
                                                         ((silicon) "-D___DYNAMIC -mmacosx-version-min=11")
                                                         (else "-D___DYNAMIC -mmacosx-version-min=10.13")))
                                                (else "-D___DYNAMIC"))))
                              ;; reduce long compilation time for purely syntactic files or dialect-runtime
                              (let ((cc-options (if (or (equal? name "primitives")
                                                        (equal? name "unsafe")
                                                        (equal? name "dialect-runtime"))
                                                    (string-append "-O0 -U___SINGLE_HOST " cc-options)
                                                  cc-options)))
                                (compile-file dst options: (%%cons 'obj options) cc-options: cc-options))))
                          (zen:update-manifest-compile-time name digest mnf src #f))))
                  #t)
              #f))))
      
      (define (with-version-file version-file proc)
        (let ((rebuild? (zen:kernel/product-needs-rebuild? version-file))
              (rebuild-architecture? (zen:kernel/product-architecture-needs-rebuild? version-file))
              (was-touched? #f))
          (define (touch)
            (if (file-exists? version-file)
                (delete-file version-file))
            (set! was-touched? #t))
          
          (define (touched?)
            was-touched?)
          
          (proc rebuild? rebuild-architecture? touch touched?)
          (if (or was-touched? (%%not (file-exists? version-file)))
              (zen:save-version-file version-file))
          was-touched?))
      
      (define (kernel-seconds)
        (let ((version-file (kernel-file "version")))
          (if (file-exists? version-file)
              (zen:file-last-modification-seconds version-file)
            #f)))
      
      ;;;
      ;;;; Kernel
      ;;;
      
      (define (build-kernel)
        (let ((version-file (kernel-file "version")))
          (let ((version-swept-file (kernel-file "version-swept")))
            (if (%%not (file-exists? version-swept-file))
                (receive (version gambit-version gambit-stamp) (zen:load-version-file version-file)
                  (zen:save-version-file version-swept-file version))))
          (with-version-file version-file
            (lambda (rebuild? rebuild-architecture? touch touched?)
              (compile-kernel rebuild? rebuild-architecture? touch touched?)))))
      
      (define (compile-kernel rebuild? rebuild-architecture? touch touched?)
        (let ((architecture? (generate-architecture rebuild? rebuild-architecture?)))
          (define (compile-kernel-file name)
            (if (compile-unit rebuild? name kernel-dir kernel-dir)
                (touch)))
          
          (define (compile-source-file path name)
            (if (compile-unit rebuild?
                              name
                              (%%string-append (source-file "kernel/") path)
                              (kernel-file path))
                (touch)))
          
          (if kernel?
              (begin
                ;; load architecture
                (load (kernel-file "_architecture"))
                
                ;; reload syntax to account for architecture
                ;; no architecture-dependent syntax in header so do not load
                ;; it as it is very time consuming because of gambit's header
                ;; (load (source-file "kernel/syntax/header"))
                (load (source-file "kernel/syntax/macro"))
                (load (source-file "kernel/syntax/unsafe"))
                (load (source-file "kernel/syntax/verbose"))
                (load (source-file "kernel/syntax/declares"))
                (load (source-file "kernel/syntax/features"))
                (load (source-file "kernel/syntax/block"))
                (load (source-file "kernel/syntax/bundle"))
                (load (source-file "kernel/syntax/foreign"))
                (load (source-file "kernel/syntax/expansion"))
                (load (source-file "kernel/syntax/primitives"))
                (load (source-file "kernel/syntax/internal"))
                (load (source-file "kernel/syntax/structure"))
                (load (source-file "kernel/syntax/syntax"))
                (load (source-file "kernel/syntax/runtime"))
                (load (source-file "kernel/runtime/base"))
                (load (source-file "kernel/runtime/record"))
                (load (source-file "kernel/syntax/repository"))))
          
          (if architecture?
              (compile-kernel-file "_architecture"))
          
          (compile-source-file "syntax/" "header")
          (compile-source-file "syntax/" "macro")
          (compile-source-file "syntax/" "unsafe")
          (compile-source-file "syntax/" "verbose")
          (compile-source-file "syntax/" "declares")
          (compile-source-file "syntax/" "features")
          (compile-source-file "syntax/" "block")
          (compile-source-file "syntax/" "bundle")
          (compile-source-file "syntax/" "foreign")
          (compile-source-file "syntax/" "expansion")
          (compile-source-file "syntax/" "primitives")
          (compile-source-file "syntax/" "internal")
          (compile-source-file "syntax/" "structure")
          (compile-source-file "syntax/" "syntax")
          (compile-source-file "syntax/" "runtime")
          (compile-source-file "runtime/" "logging")
          (compile-source-file "runtime/" "base")
          (compile-source-file "runtime/" "record")
          (compile-source-file "syntax/" "repository")
          (compile-source-file "runtime/" "crash")
          ;; to test cross compiling
          (compile-source-file "runtime/" "configuration")
          (compile-source-file "runtime/" "embedded")
          (compile-source-file "runtime/" "version")
          (compile-source-file "runtime/" "binary")
          (compile-source-file "runtime/" "common")
          (compile-source-file "runtime/" "settings")
          (compile-source-file "runtime/" "restricted")
          (compile-source-file "runtime/" "advise")
          (if include-compiler?
              (compile-source-file "runtime/" "build"))
          (compile-source-file "runtime/" "install")
          (compile-source-file "runtime/" "digest")
          (compile-source-file "runtime/" "unit")
          (compile-source-file "runtime/" "readtable")
          (compile-source-file "runtime/" "setup")
          (if include-compiler?
              (compile-source-file "runtime/" "compile"))
          (compile-source-file "core/" "base-syntax")
          (compile-source-file "core/" "base-runtime")
          (compile-source-file "core/" "class-syntax")
          (compile-source-file "core/" "class-classes")
          (compile-source-file "core/" "class-runtime")
          (compile-source-file "core/" "exception-classes")
          (compile-source-file "core/" "exception-runtime")
          (compile-source-file "core/" "generic-syntax")
          (compile-source-file "core/" "generic-runtime")
          (compile-source-file "core/" "unit-runtime")
          (compile-source-file "dialect/" "dialect-syntax")
          (compile-source-file "dialect/" "dialect-runtime")
          (compile-source-file "dialect/" "dialect-expression")
          (compile-source-file "dialect/" "dialect-development")
          (compile-source-file "foundation/" "foundation-runtime")))
      
      (define (generate-architecture rebuild? rebuild-architecture?)
        (let ((file (kernel-file "_architecture.scm")))
          (if (or rebuild? rebuild-architecture? (%%not (file-exists? file)))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file (list path: file eol-encoding: (zen:platform-eol-encoding zen:kernel-platform))
                  (lambda (output)
                    (zen:print-architecture #f system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? destination features properties output)))
                #t)
            #f)))
      
      ;;;
      ;;;; Product
      ;;;
      
      (define (build-product)
        (with-version-file (product-file "version")
          (lambda (rebuild? rebuild-architecture? touch touched?)
            (prepare-bundle rebuild?)
            (compile-product rebuild? touch touched?))))
      
      (define (prepare-bundle rebuild?)
        (if (and bundle (eq? windowing 'cocoa) (not library-image?))
            (let ((bundle-default-src (zen:package-pathname (%%get-product-package (zen:get-product product)) (%%string-append "bundles/" bundle "/")))
                  (bundle-live-src (and (%%memq 'live features) (zen:package-pathname (%%get-product-package (zen:get-product product)) (%%string-append "bundles/" bundle "_live" "/"))))
                  (bundle-prod-src (and (%%memq 'prod features) (zen:package-pathname (%%get-product-package (zen:get-product product)) (%%string-append "bundles/" bundle "_prod" "/"))))
                  (bundle-test-src (and (%%memq 'test features) (zen:package-pathname (%%get-product-package (zen:get-product product)) (%%string-append "bundles/" bundle "_test" "/"))))
                  (bundle-triage-src (and (%%memq 'triage features) (zen:package-pathname (%%get-product-package (zen:get-product product)) (%%string-append "bundles/" bundle "_triage" "/"))))
                  (bundle-dst (build-file (%%string-append bundle ".app" "/"))))
              (let ((bundle-src (cond ((and bundle-live-src
                                            (file-exists? bundle-live-src))
                                       bundle-live-src)
                                      ((and bundle-prod-src
                                            (file-exists? bundle-prod-src))
                                       bundle-prod-src)
                                      ((and bundle-test-src
                                            (file-exists? bundle-test-src))
                                       bundle-test-src)
                                      ((and bundle-triage-src
                                            (file-exists? bundle-triage-src))
                                       bundle-triage-src)
                                      (else
                                       bundle-default-src))))
                (if (or rebuild?
                        (not (file-exists? bundle-dst)))
                    (begin
                      (feedback-message "; preparing bundle...")
                      (if (file-exists? bundle-dst)
                          (zen:delete-directory bundle-dst))
                      (zen:copy-directory bundle-src bundle-dst)
                      ;; quick hack because git doesnt support empty directories
                      (let ((macos-dir (%%string-append bundle-dst "/Contents/MacOS/")))
                        (if (not (file-exists? macos-dir))
                            (create-directory macos-dir)))))))))
      
      (define (compile-product rebuild? touch touched?)
        (let ((kernel-seconds (kernel-seconds))
              (product? (generate-product rebuild?))
              (main? (generate-main rebuild?)))
          (define (compile-product-file name)
            (if (compile-unit rebuild? name product-dir product-dir)
                (touch)))
          
          (if product?
              (compile-product-file (product-filename)))
          (if main?
              (compile-product-file (main-filename)))
          
          (if (generate-resources rebuild?)
              (touch))
          
          ;;;
          ;;;; Create Link File
          ;;;
          
          (let ((link-file (link-file)))
            (if (or rebuild?
                    (%%not (file-exists? link-file))
                    (or (%%not kernel-seconds) (< (zen:file-last-modification-seconds link-file) kernel-seconds))
                    (touched?))
                (let ((base-files `(,(kernel-file "_architecture")
                                    ,(product-file (product-filename))
                                    ,(kernel-file "syntax/header")
                                    ,(kernel-file "syntax/macro")
                                    ,(kernel-file "syntax/unsafe")
                                    ,(kernel-file "syntax/verbose")
                                    ,(kernel-file "syntax/declares")
                                    ,(kernel-file "syntax/features")
                                    ,(kernel-file "syntax/block")
                                    ,(kernel-file "syntax/bundle")
                                    ,(kernel-file "syntax/foreign")
                                    ,(kernel-file "syntax/expansion")
                                    ,(kernel-file "syntax/primitives")
                                    ,(kernel-file "syntax/internal")
                                    ,(kernel-file "syntax/structure")
                                    ,(kernel-file "syntax/syntax")
                                    ,(kernel-file "syntax/runtime")
                                    ,(kernel-file "runtime/logging")
                                    ,(kernel-file "runtime/base")
                                    ,(kernel-file "runtime/record")
                                    ,(kernel-file "syntax/repository")
                                    ,(kernel-file "runtime/crash")
                                    ;; to test cross compiling
                                    ,(kernel-file "runtime/configuration")
                                    ,(kernel-file "runtime/embedded")
                                    ,(kernel-file "runtime/version")
                                    ,(kernel-file "runtime/binary")
                                    ,(kernel-file "runtime/common")
                                    ,(kernel-file "runtime/settings")
                                    ,(kernel-file "runtime/restricted")
                                    ,(kernel-file "runtime/advise")
                                    ,@(if include-compiler?
                                          `(,(kernel-file "runtime/build"))
                                        '())
                                    ,(kernel-file "runtime/install")
                                    ,(kernel-file "runtime/digest")
                                    ,(kernel-file "runtime/unit")
                                    ,(kernel-file "runtime/readtable")
                                    ,(kernel-file "runtime/setup")
                                    ,@(if include-compiler?
                                          `(,(kernel-file "runtime/compile"))
                                        '())
                                    ,(kernel-file "core/base-syntax")
                                    ,(kernel-file "core/base-runtime")
                                    ,(kernel-file "core/class-syntax")
                                    ,(kernel-file "core/class-classes")
                                    ,(kernel-file "core/class-runtime")
                                    ,(kernel-file "core/exception-classes")
                                    ,(kernel-file "core/exception-runtime")
                                    ,(kernel-file "core/generic-syntax")
                                    ,(kernel-file "core/generic-runtime")
                                    ,(kernel-file "core/unit-runtime")
                                    ,(kernel-file "dialect/dialect-syntax")
                                    ,(kernel-file "dialect/dialect-runtime")
                                    ,(kernel-file "dialect/dialect-expression")
                                    ,(kernel-file "dialect/dialect-development")
                                    ,(kernel-file "foundation/foundation-runtime"))))
                  (let ((link-info (product-file (%%string-append image-name ".link"))))
                    (call-with-output-file (list path: link-info eol-encoding: (zen:platform-eol-encoding zen:kernel-platform))
                      (lambda (output)
                        (for-each (lambda (file)
                                    (write file output)
                                    (newline output))
                                  base-files))))
                  (let ((files (append base-files (list (product-file (main-filename))))))
                    (feedback-message "; creating link file...")
                    (if library-image?
                        (link-flat files output: link-file warnings?: #f)
                      (link-incremental files output: link-file base: (string-append "~~lib/_" gambit-library)))))))
          
          ;;;
          ;;;; Link Image
          ;;;
          
          (if (or rebuild?
                  (%%not (file-exists? (image-file)))
                  (or (%%not kernel-seconds) (< (zen:file-last-modification-seconds (image-file)) kernel-seconds))
                  (touched?))
              (link-image))))
      
      (define (link-file)
        (if library-image?
            (product-file (%%string-append image-name ".o1." (zen:compiler-extension)))
          (product-file (%%string-append image-name "." (zen:compiler-extension)))))
      
      (define (generate-product rebuild?)
        (let ((file (product-file (%%string-append (product-filename) ".scm"))))
          (if (or rebuild? (%%not (file-exists? file)))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file (list path: file eol-encoding: (zen:platform-eol-encoding zen:kernel-platform))
                  (lambda (output)
                    (zen:print-variable 'zen:product product output)
                    (newline output)
                    (zen:print-variable 'zen:image (or image 'executable) output)
                    (newline output)
                    (zen:print-variable 'zen:built (zen:pathname-normalize image-dir) output)
                    (newline output)
                    (zen:print-variable 'zen:bundle-depth (and bundle (eq? windowing 'cocoa) (not library-image?) 3) output)
                    (newline output)
                    (zen:print-variable 'zen:gambit-dir (if library-image? (zen:pathname-normalize gambit-dir) (zen:relativise-directory image-dir "./" gambit-dir)) output)
                    (newline output)
                    (zen:print-variable 'zen:source-built (zen:pathname-standardize (path-normalize source)) output)
                    (newline output)
                    (zen:print-variable 'zen:source (if library-image? (zen:pathname-normalize source) (zen:determine-source-repository image-dir source)) output)
                    (newline output)
                    (zen:print-variable 'zen:binary-repositories (zen:determine-binary-repositories image-dir library-image?) output)
                    (newline output)
                    (zen:print-variable 'zen:source-repositories (zen:determine-source-repositories image-dir library-image?) output)
                    (newline output)
                    (zen:print-variable 'zen:source-access? (zen:build-source-access?) output)
                    (newline output)
                    (zen:print-variable 'zen:zenini-access? (zen:build-zenini-access?) output)
                    (newline output)
                    (zen:print-variable 'zen:windows-homedir (zen:build-windows-homedir) output)
                    (newline output)
                    (zen:print-variable 'zen:single-objects? (zen:build-single-objects?) output)
                    (newline output)
                    (zen:print-variable 'zen:zen-versions (map zen:record->vector (zen:get-zen-versions)) output)
                    (newline output)
                    (zen:print-variable 'zen:zen-updates (map zen:record->vector (zen:get-zen-updates)) output)))
                #t)
            #f)))
      
      (define (generate-main rebuild?)
        (let ((file (product-file (%%string-append (main-filename) ".scm"))))
          (if (or rebuild? (%%not (file-exists? file)))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file (list path: file eol-encoding: 'lf)
                  (lambda (output)
                    (cond (library-image?
                           (display "(zen:verbose-kernel 'kernel.main)" output)
                           (newline output)
                           (newline output)
                           (display "(zen:library-main)" output)
                           (newline output))
                          (else
                           (display "#!gsi -:darR,t8,f8,-8" output)
                           (if minimum-heap
                               (begin
                                 (display ",m" output)
                                 (display minimum-heap output)))
                           (if maximum-heap
                               (begin
                                 (display ",h" output)
                                 (display maximum-heap output)))
                           (newline output)
                           (newline output)
                           (display "(zen:verbose-kernel 'kernel.main)" output)
                           (newline output)
                           (newline output)
                           (display "(define (zen:main)" output)
                           (newline output)
                           (display "  (zen:executable-main)" output)
                           (newline output)
                           (display "  (zen:exit-cleanup))" output)
                           (newline output)
                           (newline output)
                           (display "(zen:main-set! zen:main)" output)
                           (newline output)))))
                #t)
            #f)))
      
      (define (product-filename)
        (if library-image?
            "_libproduct"
          "_product"))
      
      (define (main-filename)
        (if library-image?
            "_libmain"
          "_main"))
      
      (define (generate-resources rebuild?)
        (case platform
          ((windows)
           (with-resources
             (lambda (resources rc res)
               (let ((rcfile (%%string-append resources "/" rc)))
                 (if (or rebuild?
                         (zen:file-needs-update? rcfile res))
                     (begin
                       (zen:push-changed-units rcfile)
                       (feedback-message "; compiling {a}..." rcfile)
                       (zen:call-process (list path: "windres" arguments: (list rc "-o" res) directory: resources))
                       #t)
                   #f)))))
          (else
           #f)))
      
      (define (resource-files)
        (case platform
          ((windows)
           (with-resources
             (lambda (resources rc res)
               (%%list (zen:quote-pathname res platform)))))
          (else
           '())))
      
      (define (with-resources proc)
        (case platform
          ((windows)
           (let ()
             (define (split resources rcname)
               (let ((resources-live (and (%%memq 'live features) (%%string-append resources "_live")))
                     (resources-prod (and (%%memq 'prod features) (%%string-append resources "_prod")))
                     (resources-test (and (%%memq 'test features) (%%string-append resources "_test")))
                     (resources-triage (and (%%memq 'triage features) (%%string-append resources "_triage"))))
                 (let ((resources (cond ((and resources-live
                                              (file-exists? resources-live))
                                         resources-live)
                                        ((and resources-prod
                                              (file-exists? resources-prod))
                                         resources-prod)
                                        ((and resources-test
                                              (file-exists? resources-test))
                                         resources-test)
                                        ((and resources-triage
                                              (file-exists? resources-triage))
                                         resources-triage)
                                        (else
                                         resources))))
                   (let ((rc (%%string-append rcname ".rc"))
                         (res (%%string-append (zen:pathname-normalize (product-file "")) product-name "res.o")))
                     (proc resources rc res)))))
             
             (if resources
                 (split (zen:package-pathname (%%get-product-package (zen:get-product product)) resources) product-name)
               (split (source-file "kernel/resources/windows") "zen"))))
          (else
           #f)))
      
      (define (gambit-link-libraries)
        (if (%%not library-image?)
            `(,(%%string-append "-l" gambit-library))
          '()))
      
      (define (link-libraries)
        (case platform
          ((windows)
           '("-lws2_32"))
          ((linux)
           (case (zen:linux-family)
             ((bsd) '("-lm" "-lutil"))
             (else '("-lm" "-ldl" "-lutil"))))
          (else
           '())))
      
      (define (link-options)
        (if library-image?
            (case platform
              ((windows) '("-shared" "-D___DYNAMIC"))
              (else '("-bundle" "-D___DYNAMIC")))
          (case platform
            ((windows)
             (if console?
                 '("-mconsole")
               '("-mwindows")))
            ((mac)
             (case processor
               ((silicon) '("-headerpad_max_install_names" "-mmacosx-version-min=11"))
               (else '("-headerpad_max_install_names" "-mmacosx-version-min=10.13"))))
            (else
             '()))))
      
      (define (link-image)
        (let ((kernel-dir (zen:pathname-normalize (zen:pathname-dir (image-file))))
              (kernel-name (zen:pathname-name (image-file)))
              (base-files `(,(kernel-file "_architecture.o")
                            ,(product-file (string-append (product-filename) ".o"))
                            ,(kernel-file "syntax/header.o")
                            ,(kernel-file "syntax/macro.o")
                            ,(kernel-file "syntax/unsafe.o")
                            ,(kernel-file "syntax/verbose.o")
                            ,(kernel-file "syntax/declares.o")
                            ,(kernel-file "syntax/features.o")
                            ,(kernel-file "syntax/block.o")
                            ,(kernel-file "syntax/bundle.o")
                            ,(kernel-file "syntax/foreign.o")
                            ,(kernel-file "syntax/expansion.o")
                            ,(kernel-file "syntax/primitives.o")
                            ,(kernel-file "syntax/internal.o")
                            ,(kernel-file "syntax/structure.o")
                            ,(kernel-file "syntax/syntax.o")
                            ,(kernel-file "syntax/runtime.o")
                            ,(kernel-file "runtime/logging.o")
                            ,(kernel-file "runtime/base.o")
                            ,(kernel-file "runtime/record.o")
                            ,(kernel-file "syntax/repository.o")
                            ,(kernel-file "runtime/crash.o")
                            ;; to test cross compiling
                            ,(kernel-file "runtime/configuration.o")
                            ,(kernel-file "runtime/embedded.o")
                            ,(kernel-file "runtime/version.o")
                            ,(kernel-file "runtime/binary.o")
                            ,(kernel-file "runtime/common.o")
                            ,(kernel-file "runtime/settings.o")
                            ,(kernel-file "runtime/restricted.o")
                            ,(kernel-file "runtime/advise.o")
                            ,@(if include-compiler?
                                  `(,(kernel-file "runtime/build.o"))
                                '())
                            ,(kernel-file "runtime/install.o")
                            ,(kernel-file "runtime/digest.o")
                            ,(kernel-file "runtime/unit.o")
                            ,(kernel-file "runtime/readtable.o")
                            ,(kernel-file "runtime/setup.o")
                            ,@(if include-compiler?
                                  `(,(kernel-file "runtime/compile.o"))
                                '())
                            ,(kernel-file "core/base-syntax.o")
                            ,(kernel-file "core/base-runtime.o")
                            ,(kernel-file "core/class-syntax.o")
                            ,(kernel-file "core/class-classes.o")
                            ,(kernel-file "core/class-runtime.o")
                            ,(kernel-file "core/exception-classes.o")
                            ,(kernel-file "core/exception-runtime.o")
                            ,(kernel-file "core/generic-syntax.o")
                            ,(kernel-file "core/generic-runtime.o")
                            ,(kernel-file "core/unit-runtime.o")
                            ,(kernel-file "dialect/dialect-syntax.o")
                            ,(kernel-file "dialect/dialect-runtime.o")
                            ,(kernel-file "dialect/dialect-expression.o")
                            ,(kernel-file "dialect/dialect-development.o")
                            ,(kernel-file "foundation/foundation-runtime.o")))
              (main-file (product-file (string-append (main-filename) ".o")))
              (link-file (link-file)))
          (let ((c-files (append base-files (%%list main-file link-file))))
            (feedback-message "; linking {a}..." (if library-image? "library" "executable"))
            (zen:create-directories kernel-dir)
            ;; static kernel
            (zen:call-process
              (list
                path: "ar"
                arguments: `("-rcs"
                             ,(product-file (string-append image-name ".a"))
                             ,@base-files)))
            (if ios?
                (let ((custom-cc-options (%%cons "-bundle" ios-custom-cc-options))
                      (link-options (case compiler ((c++) '("-lc++")) (else '()))))
                  (zen:invoke-process
                    (list
                      path: ios-custom-cc
                      arguments: `(,@custom-cc-options ,@link-options ,(%%string-append "-I" ios-gambit-include-dir) ,(%%string-append "-L" ios-gambit-lib-dir) ,@c-files "-o" ,(string-append kernel-dir "/" kernel-name)))))
              (zen:gambitcomp
                'exe
                (zen:pathname-normalize build-dir)
                c-files
                (string-append kernel-dir "/" kernel-name)
                (string-append "-I" (zen:quote-pathname (path-strip-trailing-directory-separator (path-normalize "~~include")) platform))
                ""
                (zen:join-strings `(,(string-append "-L" (zen:quote-pathname (path-strip-trailing-directory-separator (path-normalize "~~lib")) platform))
                                     ,@(gambit-link-libraries)
                                     ,@(link-libraries)
                                     ,@(resource-files)
                                     ,@(link-options))
                                   " ")
                '()))
            (case windowing
              ((cocoa)
               (zen:call-process
                 (list
                   path: "install_name_tool"
                   arguments: `("-add_rpath" "@executable_path/Libraries" ,(image-file))))
               ;; add gstreamer quick hack around install_name_tool to many open files
               (zen:call-process
                 (list
                   path: "install_name_tool"
                   arguments: `("-add_rpath" "@executable_path/Libraries/gstreamer" ,(image-file))))
               (if (and bundle (not library-image?))
                   (begin
                     (zen:call-process
                       (list
                         path: "install_name_tool"
                         arguments: `("-add_rpath" "@executable_path/../Libraries" ,(image-file))))
                     ;; add gstreamer quick hack around install_name_tool to many open files
                     (zen:call-process
                       (list
                         path: "install_name_tool"
                         arguments: `("-add_rpath" "@executable_path/../Libraries/gstreamer" ,(image-file))))
                     (zen:call-process
                       (list
                         path: "install_name_tool"
                         arguments: `("-add_rpath" "@executable_path/../../../Libraries" ,(image-file))))
                     ;; add gstreamer quick hack around install_name_tool to many open files
                     (zen:call-process
                       (list
                         path: "install_name_tool"
                         arguments: `("-add_rpath" "@executable_path/../../../Libraries/gstreamer" ,(image-file))))))))
            (if (or mac? ios?)
                (let ((apple-id (getenv "ZEN_APPLE_DEVELOPER_ID" #f))
                      (entitlements (getenv "ZEN_APPLE_ENTITLEMENTS" #f)))
                  (if apple-id
                      (let ((executable (string-append kernel-dir "/" kernel-name)))
                        (feedback-message "; signing {a}..." (if library-image? "library" "executable"))
                        (zen:codesign mac? ios? apple-id executable entitlements: entitlements))))))))
      
      (define (image-file)
        (cond ((and bundle (eq? windowing 'cocoa) (not library-image?))
               (build-file (%%string-append bundle ".app" "/Contents/MacOS/" (or executable image-name))))
              (library-image?
               (build-file (zen:add-extension (or executable image-name) "o1")))
              (else
               (build-file (zen:add-extension (or executable image-name) (zen:executable-extension platform))))))
      
      ;;;
      ;;;; Configuration
      ;;;
      
      (define (generate-configuration)
        (let ((file (dest-file ".configuration")))
          (if (%%not (file-exists? file))
              (begin
                (zen:feedback "; generating {a}..." file)
                (zen:save-configuration #f system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? kernel-interpret? destination features properties file zen:kernel-platform)))))
      
      ;;;
      ;;;; Kernel Interpret
      ;;;
      
      (define (print-absolute/relative-path-variable variable directory output)
        (receive (rel-dir relative?) (zen:maybe-relativise-directory destination-directory "./" directory)
          (if relative?
              (zen:print-expression-variable variable `(string-append install-dir ,rel-dir) output)
            (zen:print-variable variable (zen:pathname-standardize (path-normalize rel-dir)) output))))
      
      (define (generate-kernel-interpret)
        (let ((file (dest-file "zen-interpret")))
          (if (%%not (file-exists? file))
              (begin
                (zen:feedback "; generating {a}..." file)
                (call-with-output-file (list path: file eol-encoding: (zen:platform-eol-encoding zen:kernel-platform))
                  (lambda (output)
                    (print "#!/bin/sh" output)
                    (newline output)
                    (print "REL=$(dirname \"$0\")" output)
                    (newline output)
                    (receive (gambit-dir relative?) (zen:maybe-relativise-directory destination-directory "./" gambit-dir)
                      (let ((relative-prefix (if relative? "$REL/" "")))
                        (print (string-append "GAM=" relative-prefix gambit-dir) output)
                        (print (string-append "GSC=" relative-prefix gambit-dir "bin/gsc") output)))
                    (print "SCM=$REL/zen-interpret.scm" output)
                    (newline output)
                    (print "exec \"$GSC\" -:dar,=\"$GAM\" -i \"$SCM\" \"$@\"" output))))))
        (let ((file (dest-file "zen-interpret.scm")))
          (if (%%not (file-exists? file))
              (begin
                (zen:feedback "; generating {a}..." file)
                (call-with-output-file (list path: file eol-encoding: (zen:platform-eol-encoding zen:kernel-platform))
                  (lambda (output)
                    (print "#!gsc -:dar" output)
                    (newline output)
                    (newline output)
                    (print "(define install-dir" output)
                    (print "  (path-directory (path-normalize (car (command-line)))))" output)
                    (newline output)
                    (newline output)
                    (zen:print-architecture #t system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? destination features properties output)
                    (newline output)
                    (zen:print-variable 'zen:kernel-interpreted? #t output)
                    (newline output)
                    (zen:print-variable 'zen:product #f output)
                    (newline output)
                    (zen:print-variable 'zen:image (or image 'executable) output)
                    (newline output)
                    (zen:print-expression-variable 'zen:built 'install-dir output)
                    (newline output)
                    (zen:print-variable 'zen:bundle-depth (and bundle (eq? windowing 'cocoa) (not library-image?) 3) output)
                    (newline output)
                    (print-absolute/relative-path-variable 'zen:gambit-dir gambit-dir output)
                    (newline output)
                    (zen:print-variable 'zen:source-built (zen:pathname-standardize (path-normalize source)) output)
                    (newline output)
                    (print-absolute/relative-path-variable 'zen:source source output)
                    (newline output)
                    (zen:print-variable 'zen:binary-repositories (zen:determine-binary-repositories destination-directory) output)
                    (newline output)
                    (zen:print-variable 'zen:source-repositories (zen:determine-source-repositories destination-directory) output)
                    (newline output)
                    (zen:print-variable 'zen:source-access? (zen:build-source-access?) output)
                    (newline output)
                    (zen:print-variable 'zen:zenini-access? (zen:build-zenini-access?) output)
                    (newline output)
                    (zen:print-variable 'zen:windows-homedir (zen:build-windows-homedir) output)
                    (newline output)
                    (zen:print-variable 'zen:single-objects? (zen:build-single-objects?) output)
                    (newline output)
                    (zen:print-variable 'zen:zen-updates (map zen:record->vector (zen:get-zen-updates)) output)
                    (newline output)
                    (newline output)
                    (display "(load (string-append zen:source \"kernel/boot\"))" output)
                    (newline output)
                    (display "(zen:load-kernel #t)" output)
                    (newline output)
                    (display "(zen:executable-main)" output)
                    (newline output)))))))
      
      (zen:invoke-build-setup platform safety optimize? source destination properties)
      
      (zen:create-directories product-dir feedback: feedback)
      (zen:create-directories (kernel-file "syntax/") feedback: feedback)
      (zen:create-directories (kernel-file "runtime/") feedback: feedback)
      
      (if kernel?
          (generate-configuration))
      
      (build-kernel)
      (build-product)
      
      (if kernel-interpret?
          (generate-kernel-interpret)))))


;;;
;;;; Utilities
;;;


(define (zen:invoke-build-setup platform safety optimize? source destination properties)
  (let ((proc (zen:build-setup)))
    (if proc
        (proc platform safety optimize? source destination properties)
      (let ((binaries? (zen:getf properties binaries?: #f)))
        (zen:build-source-access? (zen:getf properties source-access?: (not binaries?)))
        (zen:build-single-objects? (zen:getf properties single-objects?: binaries?))))))


(define (zen:determine-binary-repositories destination-directory #!optional (absolutize? #f))
  (zen:determine-repositories destination-directory
    (zen:build-binary-repositories)
    absolutize?))


(define (zen:determine-source-repositories destination-directory #!optional (absolutize? #f))
  (zen:determine-repositories destination-directory
    (zen:repositories)
    absolutize?))


(define (zen:determine-repositories destination-directory repositories #!optional (absolutize? #f))
  (if repositories
      (zen:collect (lambda (path)
                      (zen:determine-source-repository destination-directory path absolutize?))
                    (zen:split-string repositories #\;))
    '()))


(define (zen:determine-source-repository destination-directory path #!optional (absolutize? #f))
  (if (or absolutize? (zen:absolutize-sources?))
      (zen:absolutize-directory "./" path)
    (zen:relativise-directory destination-directory "./" path)))


;;;
;;;; Pkg Config
;;;


(define (zen:pkg-config what libname #!key (return-status? #f))
  (let ((string-port (open-output-string))
        (process-port (open-process (%%list path: "pkg-config" arguments: (%%list what libname)))))
    (let ((status (process-status process-port)))
      (if return-status?
          status
        (if (%%fx= status 0)
            (begin
              (zen:pipe-no-return process-port string-port)
              (get-output-string string-port))
          (zen:error "failed"))))))

(define (zen:pkg-config-exists? libname)
  (= (zen:pkg-config "--exists" libname return-status?: #t) 0))

(define (zen:pkg-config-version libname)
  (zen:pkg-config "--modversion" libname))

(define (zen:pkg-config-cflags libname)
  (zen:pkg-config "--cflags" libname))

(define (zen:pkg-config-libs libname)
  (zen:pkg-config "--libs" libname))

(define (zen:pipe-no-return input output)
  (declare (proper-tail-calls))
  (let iterate ()
    (let ((c (read-char input)))
      (if (%%not (or (eof-object? c) (%%eq? #\newline c)))
          (begin
            (write-char c output)
            (iterate))))))


;;;
;;;; Static
;;;


(define (zen:static-info library-names)
  (let ((units '())
        (ld-options '())
        (unit-language '()))
    
    (define (remove-duplicates list)
      (map car (table->list (list->table (map (lambda (x) (%%cons x #f)) list)))))
    
    (define (process-units descriptor library-name)
      (let ((update (zen:cond-expanded-product-descriptor-update library-name descriptor)))
        (let ((sub-units (remove-duplicates (%%apply append (map zen:get-subunit-names update)))))
          (set! units (append units sub-units)))))
    
    (define (process-options descriptor library-options)
      (if library-options
          (let ()
            (define (add-language unit-name language)
              (set! unit-language (%%cons (%%cons unit-name language) unit-language)))
            
            (set! ld-options (append ld-options (let ((ld-options (library-options descriptor add-language)))
                                                  (if (string? ld-options)
                                                      (zen:split-string ld-options #\space)
                                                    ld-options)))))))
    
    (for-each (lambda (library-name)
                (let ((product (zen:get-product library-name)))
                  (let ((descriptor (%%get-product-descriptor product))
                        (library-options (%%get-product-library-options product)))
                    (process-units descriptor library-name)
                    (process-options descriptor library-options))))
              library-names)
    (%%cons units (%%cons ld-options unit-language))))


(define (zen:make-static-loader file library units)
  (call-with-output-file (list path: file eol-encoding: (zen:platform-eol-encoding zen:kernel-platform))
    (lambda (port)
      (display (string-append "(zen:register-image-units '" (%%symbol->string library) " '(") port)
      (newline port)
      (for-each (lambda (unit-name)
                  (zen:with-unit-resources unit-name #f
                    (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                      (display (string-append "  (" (%%symbol->string unit-name) " ") port)
                      (write (%%get-manifest-compile-time-hash manifest) port)
                      (display ")" port)
                      (newline port))))
                units)
      (display "))" port)
      (newline port))))


;;;
;;;; Library
;;;


(define (zen:build-library-impl product-name descriptor
          #!key
          (options '())
          (platform zen:kernel-platform)
          (destination-directory zen:kernel-install)
          (feedback zen:feedback))
  (define mac?
    (eq? platform 'mac))
  
  (define (feedback-message fmt-string . rest)
    (if feedback
        (apply feedback fmt-string rest)))
  
  (define (link-options)
    (define (platform-options link-options platform)
      (let ((platform-options-pair (%%assq platform link-options)))
        (if (%%pair? platform-options-pair)
            (%%cdr platform-options-pair)
          #f)))
    
    (define (expand-link-option opt)
      (define (expand-libdir dir)
        (define (prefix p s) ; if p is a prefix of s, return what follows p in s
          (let ((p-length (%%string-length p))
                (s-length (%%string-length s)))
            (if (and (%%fx>= s-length p-length)
                     (%%string=? p (substring s 0 p-length)))
                (%%substring s p-length s-length)
              #f)))
        (if (%%string? dir)
            (let* ((zen-relative-dir (prefix "~~zen" dir))
                   (quoted-dir
                     (if zen-relative-dir
                         (zen:quote-zen-pathname zen-relative-dir)
                       (zen:quote-pathname dir platform))))
              (%%list (%%string-append "-L" quoted-dir)))
          (zen:error "Ill-formed libdir parameter in product link options")))
      
      (cond ((%%string? opt)
             (%%list opt))
            (else
             (case (%%car opt)
               ((pkg-config)
                (zen:split-string (%%apply zen:pkg-config (%%cdr opt)) #\space))
               ((libdir)
                (%%apply expand-libdir (%%cdr opt)))
               (else '())))))
    
    (let ((link-options-pair (%%assq 'link-options options)))
      (let ((raw-options
              (or (and (%%pair? link-options-pair)
                       (or (platform-options (%%cdr link-options-pair) platform)
                           (platform-options (%%cdr link-options-pair) 'else)))
                  '())))
        (%%apply append (map expand-link-option raw-options)))))
 
  ; is the lib up-to-date according to the lib manifest?
  (define (library-manifest-uptodate? loader sub-units)
    (define digest-table (%%make-table test: eq?))
    
    (define (load-image-units-manifest)
      (if (file-exists? loader)
          (begin
            (set! zen:register-image-units
                  (lambda (lib-name units)
                    (for-each (lambda (unit)
                                (%%table-set! digest-table (%%car unit) (%%cadr unit)))
                              units)))
            (load loader))))
    
    (define (unit-uptodate? unit-name)
      (let ((image-unit-compile-time-hash (%%table-ref digest-table unit-name #f)))
        (and image-unit-compile-time-hash
             (zen:with-unit-resources unit-name #f
               (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                 (let ((src-pathname (zen:resource-pathname src)))
                   (let ((digest (and manifest (zen:manifest-uptodate? src-pathname manifest))))
                     (and (%%not (zen:manifest-needs-rebuild? manifest))
                          (%%string=? image-unit-compile-time-hash (%%get-digest-hash digest))))))))))
    
    (define (subunits-uptodate? units)
      (or (%%null? units)
          (and (unit-uptodate? (%%car units))
               (subunits-uptodate? (%%cdr units)))))
            
    (load-image-units-manifest)
    (subunits-uptodate? sub-units))
  
  (let* ((product (zen:get-product product-name))
         (package (%%get-product-package product))
         (library-base (zen:relocate-product-library-name-base zen:Build-Repository package (%%symbol->string product-name)))
         (library-dir (zen:pathname-dir library-base))
         (static-info (zen:static-info (list product-name))))
    (zen:with-numbered-library (string-append library-base "." zen:Library-Extension) #t 1
      (lambda (library-o1 o1-exists?)
        (let* ((static-dir (string-append library-dir "static/"))
               (static-lib (string-append static-dir (%%symbol->string product-name) ".a"))
               (static-o1 (string-append static-dir (zen:pathname-name library-o1)))
               (linkfile (string-append static-o1 "." (zen:compiler-extension)))
               (loader (string-append static-dir (%%symbol->string product-name)))
               (loader-name (string-append zen:product-uniqueness-prefix (%%symbol->string product-name)))
               (loader-s (string-append loader "." zen:Library-Manifest-Extension))
               (loader-c (string-append loader "." (zen:compiler-extension)))
               (loader-o (string-append loader ".o"))
               (sub-units (%%car static-info))
               (ld-options (%%cadr static-info))
               (unit-language (%%cddr static-info))
               (linked-files (map (lambda (subunit-name)
                                    (zen:with-unit-resources subunit-name #f
                                      (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                                        (let ((extension (let ((pair (%%assq subunit-name unit-language)))
                                                           (if (%%not pair)
                                                               (zen:compiler-extension)
                                                             (zen:language-extension (%%cdr pair))))))
                                          (string-append (zen:resource-pathname obj) "." extension)))))
                                  sub-units)))
          (define (build-library)
            (zen:load/create-build-package package)
            (zen:make-static-loader loader-s product-name sub-units)
            (compile-file-to-target loader-s output: loader-c module-name: loader-name)
            (compile-file loader-c options: '(obj) cc-options: "-D___DYNAMIC ")
            
            (zen:call-process
              (list
                path: "ar"
                arguments: `("-rcs"
                             ,static-lib
                             ,loader-o
                             ,@(map (lambda (subunit-name)
                                      (zen:with-unit-resources subunit-name #f
                                        (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                                          (%%string-append (zen:resource-pathname obj) ".o"))))
                                    sub-units))))
            
            (let ((link-info (string-append static-dir (%%symbol->string product-name) ".link")))
              (call-with-output-file (list path: link-info eol-encoding: (zen:platform-eol-encoding zen:kernel-platform))
                (lambda (output)
                  (for-each (lambda (file)
                              (write file output)
                              (newline output))
                            (%%cons loader-c linked-files)))))
            
            (if (zen:link-libraries?)
                (begin
                  ;(feedback-message "; creating link file...")
                  (link-flat (%%cons loader-c
                                     (map (lambda (module)
                                            (%%cons module '((preload . #f))))
                                          linked-files))
                             output: linkfile
                             warnings?: #f)
                  (feedback-message "; linking library... ({a} units)" (%%number->string (%%length sub-units)))
                  (zen:call-process
                    (list
                      path: (zen:compiler-name)
                      arguments: `(,@(case platform
                                       ((windows) '("-shared" "-D___DYNAMIC"))
                                       (else '("-bundle" "-D___DYNAMIC")))
                                   ,loader-o
                                   ,@(cond-expand
                                       (windows
                                         (map (lambda (subunit-name)
                                                  (zen:with-unit-resources subunit-name #f
                                                    (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                                                      (%%string-append (zen:resource-pathname obj) ".o"))))
                                                sub-units))
                                       (else
                                        (%%list static-lib)))
                                   ,linkfile
                                   ;; patch because of a warning when C++ is used
                                   "-w"
                                   "-o" ,static-o1
                                   ,(string-append "-I" (zen:pathname-standardize (path-strip-trailing-directory-separator (path-normalize "~~include"))))
                                   ,(string-append "-L" (zen:pathname-standardize (path-strip-trailing-directory-separator (path-normalize "~~lib"))))
                                   ,@ld-options
                                   ,@(link-options))))
                  (if mac?
                      (let ((apple-id (getenv "ZEN_APPLE_DEVELOPER_ID" #f)))
                        (if apple-id
                            (begin
                              (feedback-message "; signing library...")
                              (zen:codesign mac? #f apple-id static-o1)))))
                  ;; cleanup
                  (for-each delete-file (%%list loader-c loader-o linkfile))
                  (if (file-exists? library-o1)
                      (delete-file library-o1))
                  (rename-file static-o1 library-o1)))
            #t)
          
          (zen:create-directories static-dir)
          (or (and o1-exists? (library-manifest-uptodate? loader-s sub-units))
              (build-library)))))))


(define (zen:print-architecture for-kernel-interpret? system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? destination features properties output)
  (zen:print-variable 'zen:kernel-interpreted? #f output)
  (newline output)
  (zen:print-variable 'zen:kernel-system system output)
  (newline output)
  (zen:print-variable 'zen:kernel-platform platform output)
  (newline output)
  (zen:print-variable 'zen:kernel-compiler compiler output)
  (newline output)
  (zen:print-variable 'zen:kernel-processor processor output)
  (newline output)
  (zen:print-variable 'zen:kernel-windowing windowing output)
  (newline output)
  (zen:print-variable 'zen:kernel-safety safety output)
  (newline output)
  (zen:print-variable 'zen:kernel-optimize? optimize? output)
  (newline output)
  (zen:print-variable 'zen:kernel-debug-environments? debug-environments? output)
  (newline output)
  (zen:print-variable 'zen:kernel-debug-location? debug-location? output)
  (newline output)
  (zen:print-variable 'zen:kernel-debug-source? debug-source? output)
  (newline output)
  (zen:print-variable 'zen:kernel-debug-foreign? debug-foreign? output)
  (newline output)
  (zen:print-variable 'zen:kernel-track-memory? track-memory? output)
  (newline output)
  (zen:print-variable 'zen:kernel-mutable-bindings? mutable-bindings? output)
  (newline output)
  (zen:print-variable 'zen:kernel-destination destination output)
  (newline output)
  (zen:print-variable 'zen:kernel-features features output)
  (newline output)
  (zen:print-variable 'zen:kernel-properties properties output)
  (newline output)
  (zen:print-variable 'zen:kernel-version (zen:get-zen-version-number) output))


(define (zen:print-variable variable value output)
  (display "(define " output)
  (display variable output)
  (newline output)
  (display "  " output)
  (if (or (%%symbol? value)
          (list? value))
      (display "'" output))
  (write value output)
  (display ")" output)
  (newline output))


(define (zen:print-expression-variable variable expression output)
  (display "(define " output)
  (display variable output)
  (newline output)
  (display "  " output)
  (write expression output)
  (display ")" output)
  (newline output))


;;;
;;;; Override
;;;


(zen:define-variable-override zen:build-repository-needs-sweep zen:build-repository-needs-sweep-impl)
(zen:define-variable-override zen:manifest-needs-rebuild? zen:manifest-needs-rebuild?-impl)
(zen:define-variable-override zen:get-changed-units zen:get-changed-units-impl)
(zen:define-variable-override zen:push-changed-units zen:push-changed-units-impl)
(zen:define-variable-override zen:reset-changed-units zen:reset-changed-units-impl)
(zen:define-variable-override zen:build-kernel zen:build-kernel-impl)
(zen:define-variable-override zen:build-image zen:build-image-impl)
(zen:define-variable-override zen:build-library zen:build-library-impl))
