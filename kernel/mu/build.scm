;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Mu Build
;;;


(include "mu#.scm")


;;;
;;;; Forward
;;;


(mu:define-variable mu:compile-unit)
(mu:define-variable mu:custom-compile-unit)
(mu:define-variable mu:build-unit)
(mu:define-variable mu:get-subunit-names)
(mu:define-variable mu:hub-declaration-class)
(mu:define-variable mu:new-hub-declaration)
(mu:define-variable mu:hub-declaration-hubs)
(mu:define-variable mu:hub-declaration-hubs-set!)
(mu:define-variable mu:hub-declaration-nodes)
(mu:define-variable mu:hub-declaration-nodes-set!)
(mu:define-variable mu:module-shape)
(mu:define-variable mu:manifest-ignore?)
(mu:define-variable mu:manifest-valid?)


;;;
;;;; Gambit
;;;


(define (mu:gambitbuild op
                         output-dir
                         input-filenames
                         output-filename
                         cc-options
                         ld-options-prelude
                         ld-options
                         options)
  (##gambuild 'C
              op
              output-dir
              (memq 'verbose options)
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
                )))


;;;
;;;; Compiler
;;;


(define (mu:compiler-name)
  (if (eq? mu:kernel-compiler 'c++)
      "g++"
    "gcc"))


(define (mu:compiler-extension)
  (if (eq? mu:kernel-compiler 'c++)
      "cpp"
    "c"))


(define (mu:language-extension language)
  (let ((compiler mu:kernel-compiler))
    (case language
      ((objc) (case compiler ((c) "m") ((c++) "mm")))
      ((c) (case compiler ((c) "c") ((c++) "cpp")))
      (else (mu:error "Unknown language: {s}" language)))))


;;;
;;;; Version
;;;


(define (mu:for-each-zen-version proc)
  (for-each proc (mu:get-zen-versions)))


(define (mu:for-each-higher-zen-version version proc)
  (mu:for-each-higher-versions (mu:get-zen-versions) version proc))


(define (mu:for-each-higher-versions versions target-version proc)
  (let iter ((versions versions))
    (if (not (null? versions))
        (let ((version (car versions)))
          (if (or (not target-version)
                  (> (mu:get-version-number version) target-version))
              (begin
                (proc version)
                (iter (cdr versions))))))))


(define (mu:for-each-higher-version version-alist proc)
  (for-each (lambda (repo)
              (let ((name (%%get-repository-name repo))
                    (versions (mu:load-repository-versions repo)))
                (let ((pair (assq name version-alist)))
                  (let ((target-version (if (pair? pair) (cdr pair) #f)))
                    (mu:for-each-higher-versions versions target-version
                      proc)))))
            mu:manifest-repositories))


(define (mu:kernel/product-needs-rebuild? version-file)
  (receive (version gambit-version gambit-stamp) (mu:load-version-file version-file)
    (if (not version)
        #t
      (or (not (mu:gambit-uptodate? gambit-version gambit-stamp))
          (let ((rebuild? #f))
            (mu:for-each-higher-zen-version version
              (lambda (zen-version)
                (if (memq (mu:get-version-rebuild zen-version) '(kernel all))
                    (set! rebuild? #t))))
            rebuild?)))))


(define (mu:kernel/product-architecture-needs-rebuild? version-file)
  (receive (version gambit-version gambit-stamp) (mu:load-version-file version-file)
    (if (not version)
        #t
      (or (not (mu:gambit-uptodate? gambit-version gambit-stamp))
          (let ((rebuild-architecture? #f))
            (mu:for-each-higher-zen-version version
              (lambda (zen-version)
                (if (or (memq (mu:get-version-rebuild zen-version) '(kernel all))
                        (mu:get-version-recompile zen-version)
                        (mu:get-version-recompile-references zen-version))
                    (set! rebuild-architecture? #t))))
            rebuild-architecture?)))))


(define (mu:manifest-needs-rebuild?-impl manifest)
  (let ((name (%%get-manifest-name manifest))
        (version (mu:manifest-zen-version manifest)))
    (let ((rebuild? #f))
      (mu:for-each-higher-zen-version version
        (lambda (zen-version)
          (let ((rebuild (mu:get-version-rebuild zen-version))
                (recompile (mu:get-version-recompile zen-version)))
            (if (or (eq? rebuild 'all)
                    (and recompile (memq name recompile)))
                (set! rebuild? #t)))))
      rebuild?)))


(define (mu:load-version-file version-file)
  (if (file-exists? version-file)
      (call-with-input-file (list path: version-file eol-encoding: 'cr-lf)
        (lambda (input)
          (let ((version (read input))
                (gambit-version (read input))
                (gambit-stamp (read input)))
            (values version gambit-version gambit-stamp))))
    (values #f #f #f)))


(define (mu:save-version-file version-file #!optional (version #f))
  (call-with-output-file (list path: version-file eol-encoding: (mu:platform-eol-encoding mu:kernel-platform))
    (lambda (output)
      (write (or version (mu:get-zen-version-number)) output)
      (newline output)
      (write (system-version) output)
      (newline output)
      (write (system-stamp) output)
      (newline output))))


;;;
;;;; Kernel
;;;


(define (mu:build-kernel #!key (image #f))
  (let ((configuration (mu:build-configuration))
        (image (or image 'executable)))
    (let ((name (mu:get-configuration-name configuration))
          (system (mu:get-configuration-system configuration))
          (platform (mu:get-configuration-platform configuration))
          (compiler (mu:get-configuration-compiler configuration))
          (processor (mu:get-configuration-processor configuration))
          (windowing (mu:get-configuration-windowing configuration))
          (safety (mu:get-configuration-safety configuration))
          (optimize? (mu:get-configuration-optimize? configuration))
          (debug-environments? (mu:get-configuration-debug-environments? configuration))
          (debug-location? (mu:get-configuration-debug-location? configuration))
          (debug-source? (mu:get-configuration-debug-source? configuration))
          (debug-foreign? (mu:get-configuration-debug-foreign? configuration))
          (track-memory? (mu:get-configuration-track-memory? configuration))
          (mutable-bindings? (mu:get-configuration-mutable-bindings? configuration))
          (include-compiler? #f)
          (destination (mu:get-configuration-destination configuration))
          (destination-directory (mu:configuration-directory configuration))
          (features (mu:get-configuration-features configuration))
          (properties (mu:get-configuration-properties configuration)))
      (mu:build-image #f
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


(define mu:*changed-units*
  '())


(define (mu:get-changed-units)
  mu:*changed-units*)

(define (mu:push-changed-units unit)
  (set! mu:*changed-units* (cons unit mu:*changed-units*)))

(define (mu:reset-changed-units)
  (set! mu:*changed-units* '()))


(define (mu:build-image product
          #!key
          (system mu:kernel-system)
          (platform mu:kernel-platform)
          (compiler mu:kernel-compiler)
          (processor mu:kernel-processor)
          (windowing mu:kernel-windowing)
          (safety mu:kernel-safety)
          (optimize? mu:kernel-optimize?)
          (debug-environments? mu:kernel-debug-environments?)
          (debug-location? mu:kernel-debug-location?)
          (debug-source? mu:kernel-debug-source?)
          (debug-foreign? mu:kernel-debug-foreign?)
          (track-memory? mu:kernel-track-memory?)
          (mutable-bindings? mu:kernel-mutable-bindings?)
          (include-compiler? #f)
          (source mu:kernel-source)
          (destination mu:kernel-destination)
          (destination-directory mu:kernel-install)
          (features mu:kernel-features)
          (properties mu:kernel-properties)
          (executable #f)
          (resources #f)
          (bundle #f)
          (image #f)
          (mu? #f)
          (kernel? #f)
          (console? #f)
          (minimum-heap #f)
          (maximum-heap #f)
          (static-libraries '())
          (feedback mu:feedback))
  (let ((product-name (if (not product) (if mu? "mu" "kernel") (symbol->string product)))
        (gambit-library (if include-compiler? "gambitgsc" "gambit"))
        (library-image? (eq? image 'library))
        (static-info (mu:static-info static-libraries)))
    (let ((image-name (if (not product) (if mu? "mu" "zen") product-name))
          (gambit-dir (path-normalize "~~/"))
          (source-dir (mu:relativise-directory "./" "./" source))
          (build-dir (if product (%%get-repository-directory mu:Build-Repository) destination-directory))
          (kernel-dir (string-append destination-directory "build/kernel/"))
          (product-dir (string-append destination-directory "build/products/" product-name "/"))
          (image-dir (if (and bundle (eq? windowing 'cocoa) (not library-image?))
                         (string-append destination-directory bundle ".app" "/Contents/MacOS/")
                       destination-directory))
          (static-units (car static-info))
          (static-options (cadr static-info))
          (static-languages (cddr static-info)))
      (define mac?
        (eq? platform 'mac))
      
      (define ios?
        (and (mu:build-configuration) (eq? (mu:get-configuration-platform (mu:build-configuration)) 'ios)))
      
      (define ios-processor
        (and ios? (or (mu:get-configuration-processor (mu:build-configuration)) 'x86)))
      
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
        (string-append source-dir path))
      
      (define (build-file path)
        (string-append build-dir path))
      
      (define (dest-file path)
        (string-append destination-directory path))
      
      (define (kernel-file path)
        (string-append kernel-dir path))
      
      (define (product-file path)
        (string-append product-dir path))
      
      (define (print line output)
        (display line output)
        (newline output))
      
      (define (feedback-message fmt-string . rest)
        (if feedback
            (apply feedback fmt-string rest)))
      
      (define (compile-unit rebuild? name dir output)
        (let ((src (string-append dir name ".scm"))
              (dst (string-append output name "." (mu:compiler-extension)))
              (digest (string-append output name "." mu:Digest-Extension))
              (mnf (string-append output name "." mu:Manifest-Extension)))
          (let ((hash-changed? (not (mu:manifest-uptodate? src (mu:load-updated-manifest name digest mnf src)))))
            (if (or rebuild? hash-changed? (not (file-exists? dst)))
                (let ((path (string-append dir name))
                      (options `(,@(if debug-environments? '(debug-environments) '())
                                 ,@(if debug-location? '(debug-location) '())
                                 ,@(if debug-source? '(debug-source) '())
                                 ,@(if mu:enable-track-scheme? '(track-scheme) '()))))
                  ;; standardize path as it will be the path stored in debugging information
                  (let ((standardized-path (mu:pathname-standardize (path-normalize path))))
                    (mu:push-changed-units path)
                    (feedback-message "; compiling {a}..." path)
                    (if (not (mu:dry-run?))
                        (begin
                          (parameterize ((mu:walk-for 'compile))
                            (compile-file-to-target standardized-path options: options output: dst))
                          (if ios?
                              (let ((custom-cc-options ios-custom-cc-options))
                                (mu:call-process
                                  (list
                                    path: ios-custom-cc
                                    arguments: `(,@custom-cc-options ,(string-append "-I" ios-gambit-include-dir) "-D___DYNAMIC" "-c" "-o" ,(string-append output name ".o") ,dst))))
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
                                (compile-file dst options: (cons 'obj options) cc-options: cc-options))))
                          (mu:update-manifest-compile-time name digest mnf src #f))))
                  #t)
              #f))))
      
      (define (with-version-file version-file proc)
        (let ((rebuild? (mu:kernel/product-needs-rebuild? version-file))
              (rebuild-architecture? (mu:kernel/product-architecture-needs-rebuild? version-file))
              (was-touched? #f))
          (define (touch)
            (if (file-exists? version-file)
                (delete-file version-file))
            (set! was-touched? #t))
          
          (define (touched?)
            was-touched?)
          
          (proc rebuild? rebuild-architecture? touch touched?)
          (if (or was-touched? (not (file-exists? version-file)))
              (mu:save-version-file version-file))
          was-touched?))
      
      (define (kernel-seconds)
        (let ((version-file (kernel-file "version")))
          (if (file-exists? version-file)
              (mu:file-last-modification-seconds version-file)
            #f)))
      
      ;;;
      ;;;; Kernel
      ;;;
      
      (define (build-kernel)
        (let ((version-file (kernel-file "version")))
          (let ((version-swept-file (kernel-file "version-swept")))
            (if (not (file-exists? version-swept-file))
                (receive (version gambit-version gambit-stamp) (mu:load-version-file version-file)
                  (mu:save-version-file version-swept-file version))))
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
                              (string-append (source-file "kernel/") path)
                              (kernel-file path))
                (touch)))
          
          (if architecture?
              (compile-kernel-file "_architecture"))
          
          (compile-source-file "syntax/" "header")
          (compile-source-file "syntax/" "macro")
          (compile-source-file "syntax/" "unsafe")
          (compile-source-file "syntax/" "verbose")
          (compile-source-file "syntax/" "declares")
          (compile-source-file "syntax/" "features")
          (compile-source-file "syntax/" "block")
          (compile-source-file "syntax/" "library")
          (compile-source-file "syntax/" "expansion")
          (compile-source-file "syntax/" "primitives")
          (compile-source-file "syntax/" "foreign")
          (compile-source-file "syntax/" "internal")
          (compile-source-file "syntax/" "structure")
          (compile-source-file "syntax/" "syntax")
          (compile-source-file "syntax/" "runtime")
          (compile-source-file "runtime/" "logging")
          (compile-source-file "runtime/" "base")
          (compile-source-file "runtime/" "record")
          (compile-source-file "syntax/" "repository")
          (compile-source-file "runtime/" "crash")
          (compile-source-file "runtime/" "embedded")
          (compile-source-file "runtime/" "version")
          (compile-source-file "runtime/" "binary")
          (compile-source-file "runtime/" "common")
          (compile-source-file "runtime/" "settings")
          (compile-source-file "runtime/" "restricted")
          (compile-source-file "runtime/" "advise")
          (compile-source-file "runtime/" "install")
          (compile-source-file "runtime/" "digest")
          (compile-source-file "runtime/" "unit")
          (compile-source-file "runtime/" "readtable")
          (compile-source-file "runtime/" "setup")
          (if include-compiler?
              (begin
                (compile-source-file "mu/" "boot")
                (compile-source-file "mu/" "configuration")
                (compile-source-file "mu/" "build")
                (compile-source-file "mu/" "compile")
                (compile-source-file "mu/" "parallel")
                (compile-source-file "mu/" "main")))
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
          (compile-source-file "foundation/" "foundation-runtime")
          (compile-source-file "scheme/" "scheme-backend")
          (compile-source-file "scheme/" "scheme-dialect")
          (compile-source-file "zen/" "zen-runtime")
          (compile-source-file "zen/" "zen-backend")
          (compile-source-file "zen/" "zen-core")
          #;
          (compile-source-file "zen/" "zen-foundation")))
      
      (define (generate-architecture rebuild? rebuild-architecture?)
        (let ((file (kernel-file "_architecture.scm")))
          (if (or rebuild? rebuild-architecture? (not (file-exists? file)))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file (list path: file eol-encoding: (mu:platform-eol-encoding mu:kernel-platform))
                  (lambda (output)
                    (mu:print-architecture system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? destination features properties output)))
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
            (let ((bundle-default-src (mu:package-pathname (%%get-product-package (mu:get-product product)) (string-append "bundles/" bundle "/")))
                  (bundle-live-src (and (memq 'live features) (mu:package-pathname (%%get-product-package (mu:get-product product)) (string-append "bundles/" bundle "_live" "/"))))
                  (bundle-prod-src (and (memq 'prod features) (mu:package-pathname (%%get-product-package (mu:get-product product)) (string-append "bundles/" bundle "_prod" "/"))))
                  (bundle-test-src (and (memq 'test features) (mu:package-pathname (%%get-product-package (mu:get-product product)) (string-append "bundles/" bundle "_test" "/"))))
                  (bundle-triage-src (and (memq 'triage features) (mu:package-pathname (%%get-product-package (mu:get-product product)) (string-append "bundles/" bundle "_triage" "/"))))
                  (bundle-dst (build-file (string-append bundle ".app" "/"))))
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
                          (mu:delete-directory bundle-dst))
                      (mu:copy-directory bundle-src bundle-dst)
                      ;; quick hack because git doesnt support empty directories
                      (let ((macos-dir (string-append bundle-dst "/Contents/MacOS/")))
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
                    (not (file-exists? link-file))
                    (or (not kernel-seconds) (< (mu:file-last-modification-seconds link-file) kernel-seconds))
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
                                    ,(kernel-file "syntax/library")
                                    ,(kernel-file "syntax/expansion")
                                    ,(kernel-file "syntax/primitives")
                                    ,(kernel-file "syntax/foreign")
                                    ,(kernel-file "syntax/internal")
                                    ,(kernel-file "syntax/structure")
                                    ,(kernel-file "syntax/syntax")
                                    ,(kernel-file "syntax/runtime")
                                    ,(kernel-file "runtime/logging")
                                    ,(kernel-file "runtime/base")
                                    ,(kernel-file "runtime/record")
                                    ,(kernel-file "syntax/repository")
                                    ,(kernel-file "runtime/crash")
                                    ,(kernel-file "runtime/embedded")
                                    ,(kernel-file "runtime/version")
                                    ,(kernel-file "runtime/binary")
                                    ,(kernel-file "runtime/common")
                                    ,(kernel-file "runtime/settings")
                                    ,(kernel-file "runtime/restricted")
                                    ,(kernel-file "runtime/advise")
                                    ,(kernel-file "runtime/install")
                                    ,(kernel-file "runtime/digest")
                                    ,(kernel-file "runtime/unit")
                                    ,(kernel-file "runtime/readtable")
                                    ,(kernel-file "runtime/setup")
                                    ,@(if include-compiler?
                                          `(,(kernel-file "mu/boot")
                                            ,(kernel-file "mu/configuration")
                                            ,(kernel-file "mu/build")
                                            ,(kernel-file "mu/compile")
                                            ,(kernel-file "mu/parallel")
                                            ,(kernel-file "mu/main"))
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
                                    ,(kernel-file "foundation/foundation-runtime")
                                    ,(kernel-file "scheme/scheme-backend")
                                    ,(kernel-file "scheme/scheme-dialect")
                                    ,(kernel-file "zen/zen-runtime")
                                    ,(kernel-file "zen/zen-backend")
                                    ,(kernel-file "zen/zen-core")
                                    #;
                                    ,(kernel-file "zen/zen-foundation"))))
                  (let ((link-info (product-file (string-append image-name ".link"))))
                    (call-with-output-file (list path: link-info eol-encoding: (mu:platform-eol-encoding mu:kernel-platform))
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
                  (not (file-exists? (image-file)))
                  (or (not kernel-seconds) (< (mu:file-last-modification-seconds (image-file)) kernel-seconds))
                  (touched?))
              (link-image))))
      
      (define (link-file)
        (if library-image?
            (product-file (string-append image-name ".o1." (mu:compiler-extension)))
          (product-file (string-append image-name "." (mu:compiler-extension)))))
      
      (define (generate-product rebuild?)
        (let ((file (product-file (string-append (product-filename) ".scm"))))
          (if (or rebuild? (not (file-exists? file)))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file (list path: file eol-encoding: (mu:platform-eol-encoding mu:kernel-platform))
                  (lambda (output)
                    (mu:print-variable 'mu:product product output)
                    (newline output)
                    (mu:print-variable 'mu:image (or image 'executable) output)
                    (newline output)
                    (mu:print-variable 'mu:built (mu:pathname-normalize image-dir) output)
                    (newline output)
                    (mu:print-variable 'mu:bundle-depth (and bundle (eq? windowing 'cocoa) (not library-image?) 3) output)
                    (newline output)
                    (mu:print-variable 'mu:gambit-dir (if library-image? (mu:pathname-normalize gambit-dir) (mu:relativise-directory image-dir "./" gambit-dir)) output)
                    (newline output)
                    (mu:print-variable 'mu:source-built (mu:pathname-standardize (path-normalize source)) output)
                    (newline output)
                    (mu:print-variable 'mu:source (if library-image? (mu:pathname-normalize source) (mu:determine-source-repository image-dir source)) output)
                    (newline output)
                    (mu:print-variable 'mu:binary-repositories (mu:determine-binary-repositories image-dir library-image?) output)
                    (newline output)
                    (mu:print-variable 'mu:source-repositories (mu:determine-source-repositories image-dir library-image?) output)
                    (newline output)
                    (mu:print-variable 'mu:source-access? (mu:build-source-access?) output)
                    (newline output)
                    (mu:print-variable 'mu:zenini-access? (mu:build-zenini-access?) output)
                    (newline output)
                    (mu:print-variable 'mu:windows-homedir (mu:build-windows-homedir) output)
                    (newline output)
                    (mu:print-variable 'mu:single-objects? (mu:build-single-objects?) output)
                    (newline output)
                    (mu:print-variable 'mu:zen-versions (map mu:record->vector (mu:get-zen-versions)) output)
                    (newline output)
                    (mu:print-variable 'mu:zen-updates (map mu:record->vector (mu:get-zen-updates)) output)))
                #t)
            #f)))
      
      (define (generate-main rebuild?)
        (let ((file (product-file (string-append (main-filename) ".scm"))))
          (if (or rebuild? (not (file-exists? file)))
              (begin
                (feedback-message "; generating {a}..." file)
                (call-with-output-file (list path: file eol-encoding: 'lf)
                  (lambda (output)
                    (cond (library-image?
                           (display "(mu:verbose-kernel 'kernel.main)" output)
                           (newline output)
                           (newline output)
                           (display "(mu:library-main)" output)
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
                           (display "(mu:verbose-kernel 'kernel.main)" output)
                           (newline output)
                           (newline output)
                           (display "(define (mu:main)" output)
                           (newline output)
                           (if mu?
                               (display "  (mu:mu-main)" output)
                             (display "  (mu:zen-main)" output))
                           (newline output)
                           (display "  (mu:exit-cleanup))" output)
                           (newline output)
                           (newline output)
                           (display "(mu:main-set! mu:main)" output)
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
               (let ((rcfile (string-append resources "/" rc)))
                 (if (or rebuild?
                         (mu:file-needs-update? rcfile res))
                     (begin
                       (mu:push-changed-units rcfile)
                       (feedback-message "; compiling {a}..." rcfile)
                       (mu:call-process (list path: "windres" arguments: (list rc "-o" res) directory: resources))
                       #t)
                   #f)))))
          (else
           #f)))
      
      (define (resource-files)
        (case platform
          ((windows)
           (with-resources
             (lambda (resources rc res)
               (list (mu:quote-pathname res platform)))))
          (else
           '())))
      
      (define (with-resources proc)
        (case platform
          ((windows)
           (let ()
             (define (split resources rcname)
               (let ((resources-live (and (memq 'live features) (string-append resources "_live")))
                     (resources-prod (and (memq 'prod features) (string-append resources "_prod")))
                     (resources-test (and (memq 'test features) (string-append resources "_test")))
                     (resources-triage (and (memq 'triage features) (string-append resources "_triage"))))
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
                   (let ((rc (string-append rcname ".rc"))
                         (res (string-append (mu:pathname-normalize (product-file "")) product-name "res.o")))
                     (proc resources rc res)))))
             
             (if resources
                 (split (mu:package-pathname (%%get-product-package (mu:get-product product)) resources) product-name)
               (split (source-file "kernel/resources/windows") "zen"))))
          (else
           #f)))
      
      (define (gambit-link-libraries)
        (if (not library-image?)
            `(,(string-append "-l" gambit-library))
          '()))
      
      (define (link-libraries)
        (case platform
          ((windows)
           '("-lws2_32"))
          ((linux)
           (case (mu:linux-family)
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
        (let ((kernel-dir (mu:pathname-normalize (mu:pathname-dir (image-file))))
              (kernel-name (mu:pathname-name (image-file)))
              (base-files `(,(kernel-file "_architecture.o")
                            ,(product-file (string-append (product-filename) ".o"))
                            ,(kernel-file "syntax/header.o")
                            ,(kernel-file "syntax/macro.o")
                            ,(kernel-file "syntax/unsafe.o")
                            ,(kernel-file "syntax/verbose.o")
                            ,(kernel-file "syntax/declares.o")
                            ,(kernel-file "syntax/features.o")
                            ,(kernel-file "syntax/block.o")
                            ,(kernel-file "syntax/library.o")
                            ,(kernel-file "syntax/expansion.o")
                            ,(kernel-file "syntax/primitives.o")
                            ,(kernel-file "syntax/foreign.o")
                            ,(kernel-file "syntax/internal.o")
                            ,(kernel-file "syntax/structure.o")
                            ,(kernel-file "syntax/syntax.o")
                            ,(kernel-file "syntax/runtime.o")
                            ,(kernel-file "runtime/logging.o")
                            ,(kernel-file "runtime/base.o")
                            ,(kernel-file "runtime/record.o")
                            ,(kernel-file "syntax/repository.o")
                            ,(kernel-file "runtime/crash.o")
                            ,(kernel-file "runtime/embedded.o")
                            ,(kernel-file "runtime/version.o")
                            ,(kernel-file "runtime/binary.o")
                            ,(kernel-file "runtime/common.o")
                            ,(kernel-file "runtime/settings.o")
                            ,(kernel-file "runtime/restricted.o")
                            ,(kernel-file "runtime/advise.o")
                            ,(kernel-file "runtime/install.o")
                            ,(kernel-file "runtime/digest.o")
                            ,(kernel-file "runtime/unit.o")
                            ,(kernel-file "runtime/readtable.o")
                            ,(kernel-file "runtime/setup.o")
                            ,@(if include-compiler?
                                  `(,(kernel-file "mu/boot.o")
                                    ,(kernel-file "mu/configuration.o")
                                    ,(kernel-file "mu/build.o")
                                    ,(kernel-file "mu/compile.o")
                                    ,(kernel-file "mu/parallel.o")
                                    ,(kernel-file "mu/main.o"))
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
                            ,(kernel-file "foundation/foundation-runtime.o")
                            ,(kernel-file "scheme/scheme-backend.o")
                            ,(kernel-file "scheme/scheme-dialect.o")
                            ,(kernel-file "zen/zen-runtime.o")
                            ,(kernel-file "zen/zen-backend.o")
                            ,(kernel-file "zen/zen-core.o")
                            #;
                            ,(kernel-file "zen/zen-foundation.o")))
              (main-file (product-file (string-append (main-filename) ".o")))
              (link-file (link-file)))
          (let ((c-files (append base-files (list main-file link-file))))
            (feedback-message "; linking {a}..." (if library-image? "library" "executable"))
            (mu:create-directories kernel-dir)
            ;; static kernel
            (mu:call-process
              (list
                path: "ar"
                arguments: `("-rcs"
                             ,(product-file (string-append image-name ".a"))
                             ,@base-files)))
            (if ios?
                (let ((custom-cc-options (cons "-bundle" ios-custom-cc-options))
                      (link-options (case compiler ((c++) '("-lc++")) (else '()))))
                  (mu:invoke-process
                    (list
                      path: ios-custom-cc
                      arguments: `(,@custom-cc-options ,@link-options ,(string-append "-I" ios-gambit-include-dir) ,(string-append "-L" ios-gambit-lib-dir) ,@c-files "-o" ,(string-append kernel-dir "/" kernel-name)))))
              (mu:gambitbuild
                'exe
                (mu:pathname-normalize build-dir)
                c-files
                (string-append kernel-dir "/" kernel-name)
                (string-append "-I" (mu:quote-pathname (path-strip-trailing-directory-separator (path-normalize "~~include")) platform))
                ""
                (mu:join-strings `(,(string-append "-L" (mu:quote-pathname (path-strip-trailing-directory-separator (path-normalize "~~lib")) platform))
                                     ,@(gambit-link-libraries)
                                     ,@(link-libraries)
                                     ,@(resource-files)
                                     ,@(link-options))
                                   " ")
                '()))
            (case windowing
              ((cocoa)
               (mu:call-process
                 (list
                   path: "install_name_tool"
                   arguments: `("-add_rpath" "@executable_path/Libraries" ,(image-file))))
               ;; add gstreamer quick hack around install_name_tool to many open files
               (mu:call-process
                 (list
                   path: "install_name_tool"
                   arguments: `("-add_rpath" "@executable_path/Libraries/gstreamer" ,(image-file))))
               (if (and bundle (not library-image?))
                   (begin
                     (mu:call-process
                       (list
                         path: "install_name_tool"
                         arguments: `("-add_rpath" "@executable_path/../Libraries" ,(image-file))))
                     ;; add gstreamer quick hack around install_name_tool to many open files
                     (mu:call-process
                       (list
                         path: "install_name_tool"
                         arguments: `("-add_rpath" "@executable_path/../Libraries/gstreamer" ,(image-file))))
                     (mu:call-process
                       (list
                         path: "install_name_tool"
                         arguments: `("-add_rpath" "@executable_path/../../../Libraries" ,(image-file))))
                     ;; add gstreamer quick hack around install_name_tool to many open files
                     (mu:call-process
                       (list
                         path: "install_name_tool"
                         arguments: `("-add_rpath" "@executable_path/../../../Libraries/gstreamer" ,(image-file))))))))
            (if (or mac? ios?)
                (let ((apple-id (getenv "ZEN_APPLE_DEVELOPER_ID" #f))
                      (entitlements (getenv "ZEN_APPLE_ENTITLEMENTS" #f)))
                  (if apple-id
                      (let ((executable (string-append kernel-dir "/" kernel-name)))
                        (feedback-message "; signing {a}..." (if library-image? "library" "executable"))
                        (mu:codesign mac? ios? apple-id executable entitlements: entitlements))))))))
      
      (define (image-file)
        (cond ((and bundle (eq? windowing 'cocoa) (not library-image?))
               (build-file (string-append bundle ".app" "/Contents/MacOS/" (or executable image-name))))
              (library-image?
               (build-file (mu:add-extension (or executable image-name) "o1")))
              (else
               (build-file (mu:add-extension (or executable image-name) (mu:executable-extension platform))))))
      
      ;;;
      ;;;; Configuration
      ;;;
      
      (define (generate-configuration)
        (let ((file (dest-file ".configuration")))
          (if (not (file-exists? file))
              (begin
                (mu:feedback "; generating {a}..." file)
                (mu:save-configuration #f system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? destination features properties file mu:kernel-platform)))))
      
      ;;;
      ;;;; Kernel Interpret
      ;;;
      
      (define (print-absolute/relative-path-variable variable directory output)
        (receive (rel-dir relative?) (mu:maybe-relativise-directory destination-directory "./" directory)
          (if relative?
              (mu:print-expression-variable variable `(string-append install-dir ,rel-dir) output)
            (mu:print-variable variable (mu:pathname-standardize (path-normalize rel-dir)) output))))
      
      (mu:invoke-build-setup platform safety optimize? source destination properties)
      
      (mu:create-directories product-dir feedback: feedback)
      (mu:create-directories (kernel-file "syntax/") feedback: feedback)
      (mu:create-directories (kernel-file "runtime/") feedback: feedback)
      
      (if (or mu? kernel?)
          (generate-configuration))
      
      (build-kernel)
      (build-product))))


;;;
;;;; Utilities
;;;


(define (mu:invoke-build-setup platform safety optimize? source destination properties)
  (let ((proc (mu:build-setup)))
    (if proc
        (proc platform safety optimize? source destination properties)
      (let ((binaries? (mu:getf properties binaries?: #f)))
        (mu:build-source-access? (mu:getf properties source-access?: (not binaries?)))
        (mu:build-single-objects? (mu:getf properties single-objects?: binaries?))))))


(define (mu:determine-binary-repositories destination-directory #!optional (absolutize? #f))
  (mu:determine-repositories destination-directory
    (mu:build-binary-repositories)
    absolutize?))


(define (mu:determine-source-repositories destination-directory #!optional (absolutize? #f))
  (mu:determine-repositories destination-directory
    (mu:repositories)
    absolutize?))


(define (mu:determine-repositories destination-directory repositories #!optional (absolutize? #f))
  (if repositories
      (map (lambda (path)
               (let ((name (string->symbol (mu:pathname-name path))))
                 (cons name (mu:determine-source-repository destination-directory path absolutize?))))
           (mu:split-string repositories #\;))
    '()))


(define (mu:determine-source-repository destination-directory path #!optional (absolutize? #f))
  (if (or absolutize? (mu:absolutize-sources?))
      (mu:absolutize-directory "./" path)
    (mu:relativise-directory destination-directory "./" path)))


;;;
;;;; Pkg Config
;;;


(define (mu:pkg-config what libname #!key (return-status? #f))
  (let ((string-port (open-output-string))
        (process-port (open-process (list path: "pkg-config" arguments: (list what libname)))))
    (let ((status (process-status process-port)))
      (if return-status?
          status
        (if (= status 0)
            (begin
              (mu:pipe-no-return process-port string-port)
              (get-output-string string-port))
          (mu:error "failed"))))))

(define (mu:pkg-config-exists? libname)
  (= (mu:pkg-config "--exists" libname return-status?: #t) 0))

(define (mu:pkg-config-version libname)
  (mu:pkg-config "--modversion" libname))

(define (mu:pkg-config-cflags libname)
  (mu:pkg-config "--cflags" libname))

(define (mu:pkg-config-libs libname)
  (mu:pkg-config "--libs" libname))

(define (mu:pipe-no-return input output)
  (declare (proper-tail-calls))
  (let iterate ()
    (let ((c (read-char input)))
      (if (not (or (eof-object? c) (eq? #\newline c)))
          (begin
            (write-char c output)
            (iterate))))))


;;;
;;;; Static
;;;


(define (mu:static-info library-names)
  (let ((units '())
        (ld-options '())
        (unit-language '()))
    
    (define (remove-duplicates list)
      (map car (table->list (list->table (map (lambda (x) (cons x #f)) list)))))
    
    (define (process-units descriptor library-name)
      (let ((update (mu:cond-expanded-product-descriptor-update library-name descriptor)))
        (let ((sub-units (remove-duplicates (apply append (map mu:get-subunit-names update)))))
          (set! units (append units sub-units)))))
    
    (define (process-options descriptor library-options)
      (if library-options
          (let ()
            (define (add-language unit-name language)
              (set! unit-language (cons (cons unit-name language) unit-language)))
            
            (set! ld-options (append ld-options (let ((ld-options (library-options descriptor add-language)))
                                                  (if (string? ld-options)
                                                      (mu:split-string ld-options #\space)
                                                    ld-options)))))))
    
    (for-each (lambda (library-name)
                (let ((product (mu:get-product library-name)))
                  (let ((descriptor (%%get-product-descriptor product))
                        (library-options (%%get-product-library-options product)))
                    (process-units descriptor library-name)
                    (process-options descriptor library-options))))
              library-names)
    (cons units (cons ld-options unit-language))))


(define (mu:make-static-loader file library units)
  (call-with-output-file (list path: file eol-encoding: (mu:platform-eol-encoding mu:kernel-platform))
    (lambda (port)
      (display (string-append "(mu:register-image-units '" (symbol->string library) " '(") port)
      (newline port)
      (for-each (lambda (unit-name)
                  (mu:with-unit-resources unit-name #f
                    (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                      (display (string-append "  (" (symbol->string unit-name) " ") port)
                      (write (%%get-manifest-compile-time-hash manifest) port)
                      (display ")" port)
                      (newline port))))
                units)
      (display "))" port)
      (newline port))))


;;;
;;;; Library
;;;


(define (mu:build-library product-name descriptor
          #!key
          (options '())
          (platform mu:kernel-platform)
          (destination-directory mu:kernel-install)
          (feedback mu:feedback))
  (define mac?
    (eq? platform 'mac))
  
  (define (feedback-message fmt-string . rest)
    (if feedback
        (apply feedback fmt-string rest)))
  
  (define (link-options)
    (define (platform-options link-options platform)
      (let ((platform-options-pair (assq platform link-options)))
        (if (pair? platform-options-pair)
            (cdr platform-options-pair)
          #f)))
    
    (define (expand-link-option opt)
      (define (expand-libdir dir)
        (define (prefix p s) ; if p is a prefix of s, return what follows p in s
          (let ((p-length (string-length p))
                (s-length (string-length s)))
            (if (and (>= s-length p-length)
                     (string=? p (substring s 0 p-length)))
                (substring s p-length s-length)
              #f)))
        (if (string? dir)
            (let* ((zen-relative-dir (prefix "~~zen" dir))
                   (quoted-dir
                     (if zen-relative-dir
                         (mu:quote-zen-pathname zen-relative-dir)
                       (mu:quote-pathname dir platform))))
              (list (string-append "-L" quoted-dir)))
          (mu:error "Ill-formed libdir parameter in product link options")))
      
      (cond ((string? opt)
             (list opt))
            (else
             (case (car opt)
               ((pkg-config)
                (mu:split-string (apply mu:pkg-config (cdr opt)) #\space))
               ((libdir)
                (apply expand-libdir (cdr opt)))
               (else '())))))
    
    (let ((link-options-pair (assq 'link-options options)))
      (let ((raw-options
              (or (and (pair? link-options-pair)
                       (or (platform-options (cdr link-options-pair) platform)
                           (platform-options (cdr link-options-pair) 'else)))
                  '())))
        (apply append (map expand-link-option raw-options)))))
 
  ; is the lib up-to-date according to the lib manifest?
  (define (library-manifest-uptodate? loader sub-units)
    (define digest-table (make-table test: eq?))
    
    (define (load-image-units-manifest)
      (if (file-exists? loader)
          (begin
            (set! mu:register-image-units
                  (lambda (lib-name units)
                    (for-each (lambda (unit)
                                (table-set! digest-table (car unit) (cadr unit)))
                              units)))
            (load loader))))
    
    (define (unit-uptodate? unit-name)
      (let ((image-unit-compile-time-hash (table-ref digest-table unit-name #f)))
        (and image-unit-compile-time-hash
             (mu:with-unit-resources unit-name #f
               (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                 (let ((src-pathname (mu:resource-pathname src)))
                   (let ((digest (and manifest (mu:manifest-uptodate? src-pathname manifest))))
                     (and (not (mu:manifest-needs-rebuild? manifest))
                          (string=? image-unit-compile-time-hash (%%get-digest-hash digest))))))))))
    
    (define (subunits-uptodate? units)
      (or (null? units)
          (and (unit-uptodate? (car units))
               (subunits-uptodate? (cdr units)))))
            
    (load-image-units-manifest)
    (subunits-uptodate? sub-units))
  
  (let* ((product (mu:get-product product-name))
         (package (%%get-product-package product))
         (library-base (mu:relocate-product-library-name-base mu:Build-Repository (%%get-package-name package) (symbol->string product-name)))
         (library-dir (mu:pathname-dir library-base))
         (static-info (mu:static-info (list product-name))))
    (mu:with-numbered-library (string-append library-base "." mu:Library-Extension) #t 1
      (lambda (library-o1 o1-exists?)
        (let* ((static-dir (string-append library-dir "static/"))
               (static-lib (string-append static-dir (symbol->string product-name) ".a"))
               (static-o1 (string-append static-dir (mu:pathname-name library-o1)))
               (linkfile (string-append static-o1 "." (mu:compiler-extension)))
               (loader (string-append static-dir (symbol->string product-name)))
               (loader-name (string-append mu:product-uniqueness-prefix (symbol->string product-name)))
               (loader-s (string-append loader "." mu:Library-Manifest-Extension))
               (loader-c (string-append loader "." (mu:compiler-extension)))
               (loader-o (string-append loader ".o"))
               (sub-units (car static-info))
               (ld-options (cadr static-info))
               (unit-language (cddr static-info))
               (linked-files (map (lambda (subunit-name)
                                    (mu:with-unit-resources subunit-name #f
                                      (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                                        (let ((extension (let ((pair (assq subunit-name unit-language)))
                                                           (if (not pair)
                                                               (mu:compiler-extension)
                                                             (mu:language-extension (cdr pair))))))
                                          (string-append (mu:resource-pathname obj) "." extension)))))
                                  sub-units)))
          (define (build-library)
            (mu:make-static-loader loader-s product-name sub-units)
            (compile-file-to-target loader-s output: loader-c module-name: loader-name)
            (compile-file loader-c options: '(obj) cc-options: "-D___DYNAMIC ")
            
            (mu:call-process
              (list
                path: "ar"
                arguments: `("-rcs"
                             ,static-lib
                             ,loader-o
                             ,@(map (lambda (subunit-name)
                                      (mu:with-unit-resources subunit-name #f
                                        (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                                          (string-append (mu:resource-pathname obj) ".o"))))
                                    sub-units))))
            
            (let ((link-info (string-append static-dir (symbol->string product-name) ".link")))
              (call-with-output-file (list path: link-info eol-encoding: (mu:platform-eol-encoding mu:kernel-platform))
                (lambda (output)
                  (for-each (lambda (file)
                              (write file output)
                              (newline output))
                            (cons loader-c linked-files)))))
            
            (if (mu:link-libraries?)
                (begin
                  ;(feedback-message "; creating link file...")
                  (link-flat (cons loader-c
                                   (map (lambda (module)
                                          (cons module '((preload . #f))))
                                        linked-files))
                             output: linkfile
                             warnings?: #f)
                  (feedback-message "; linking library... ({a} units)" (number->string (length sub-units)))
                  (mu:call-process
                    (list
                      path: (mu:compiler-name)
                      arguments: `(,@(case platform
                                       ((windows) '("-shared" "-D___DYNAMIC"))
                                       (else '("-bundle" "-D___DYNAMIC")))
                                   ,loader-o
                                   ,@(cond-expand
                                       (windows
                                         (map (lambda (subunit-name)
                                                  (mu:with-unit-resources subunit-name #f
                                                    (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
                                                      (string-append (mu:resource-pathname obj) ".o"))))
                                                sub-units))
                                       (else
                                        (list static-lib)))
                                   ,linkfile
                                   ;; patch because of a warning when C++ is used
                                   "-w"
                                   "-o" ,static-o1
                                   ,(string-append "-I" (mu:pathname-standardize (path-strip-trailing-directory-separator (path-normalize "~~include"))))
                                   ,(string-append "-L" (mu:pathname-standardize (path-strip-trailing-directory-separator (path-normalize "~~lib"))))
                                   ,@ld-options
                                   ,@(link-options))))
                  (if mac?
                      (let ((apple-id (getenv "ZEN_APPLE_DEVELOPER_ID" #f)))
                        (if apple-id
                            (begin
                              (feedback-message "; signing library...")
                              (mu:codesign mac? #f apple-id static-o1)))))
                  ;; cleanup
                  (for-each delete-file (list loader-c loader-o linkfile))
                  (if (file-exists? library-o1)
                      (delete-file library-o1))
                  (rename-file static-o1 library-o1)))
            #t)
          
          (mu:create-directories static-dir)
          (or (and o1-exists? (library-manifest-uptodate? loader-s sub-units))
              (build-library)))))))


(define (mu:print-architecture system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? destination features properties output)
  (mu:print-variable 'mu:kernel-system system output)
  (newline output)
  (mu:print-variable 'mu:kernel-platform platform output)
  (newline output)
  (mu:print-variable 'mu:kernel-compiler compiler output)
  (newline output)
  (mu:print-variable 'mu:kernel-processor processor output)
  (newline output)
  (mu:print-variable 'mu:kernel-windowing windowing output)
  (newline output)
  (mu:print-variable 'mu:kernel-safety safety output)
  (newline output)
  (mu:print-variable 'mu:kernel-optimize? optimize? output)
  (newline output)
  (mu:print-variable 'mu:kernel-debug-environments? debug-environments? output)
  (newline output)
  (mu:print-variable 'mu:kernel-debug-location? debug-location? output)
  (newline output)
  (mu:print-variable 'mu:kernel-debug-source? debug-source? output)
  (newline output)
  (mu:print-variable 'mu:kernel-debug-foreign? debug-foreign? output)
  (newline output)
  (mu:print-variable 'mu:kernel-track-memory? track-memory? output)
  (newline output)
  (mu:print-variable 'mu:kernel-mutable-bindings? mutable-bindings? output)
  (newline output)
  (mu:print-variable 'mu:kernel-destination destination output)
  (newline output)
  (mu:print-variable 'mu:kernel-features features output)
  (newline output)
  (mu:print-variable 'mu:kernel-properties properties output)
  (newline output)
  (mu:print-variable 'mu:kernel-version (mu:get-zen-version-number) output))


(define (mu:print-variable variable value output)
  (display "(define " output)
  (display variable output)
  (newline output)
  (display "  " output)
  (if (or (symbol? value)
          (list? value))
      (display "'" output))
  (write value output)
  (display ")" output)
  (newline output))


(define (mu:print-expression-variable variable expression output)
  (display "(define " output)
  (display variable output)
  (newline output)
  (display "  " output)
  (write expression output)
  (display ")" output)
  (newline output))


;;;
;;;; Product
;;;


(define (mu:update-product name)
  (let ((product (mu:setup-product name)))
    (let ((update (%%get-product-update product))
          (descriptor (%%get-product-descriptor product)))
      (if update
          (update descriptor)
        (mu:update-product-descriptor descriptor)))))


(define (mu:update-product-descriptor descriptor)
  (let ((name (mu:product-descriptor-name descriptor)))
    (let ((update (mu:cond-expanded-product-descriptor-update name descriptor)))
      (for-each mu:build-unit update))))


(define (mu:build-product name)
  (let ((product (mu:setup-product name)))
    (let ((build (%%get-product-build product))
          (descriptor (%%get-product-descriptor product)))
      (mu:feedback "make {a}" name)
      (if build
          (build descriptor)
        (mu:build-product-descriptor descriptor))
      (if (or (mu:link-libraries?) (mu:link-static?))
          (mu:build-library-descriptor descriptor)))))


(define (mu:build-product-descriptor descriptor #!key (unit #f) (skip-references? #f) (force? #f))
  (define (build-product)
    (mu:update-product-descriptor descriptor)
    (let ((build (mu:product-descriptor-build descriptor)))
      (if build
          (for-each (lambda (obj)
                      (if (symbol? obj)
                          (mu:build-image obj)
                        (apply mu:build-image obj)))
                    build))))
  
  (if unit
      (mu:compile-unit unit skip-references?: skip-references? force?: force?)
    (build-product)))


(define (mu:build-library-descriptor descriptor)
  (let ((library (mu:product-descriptor-library descriptor)))
    (let ((options (or library '())))
      (mu:build-library (mu:product-descriptor-name descriptor) descriptor options: options))))


;;;
;;;; Override
;;;


#; ( ;; tanzoo
(mu:define-variable-override mu:manifest-needs-rebuild? mu:manifest-needs-rebuild?-impl)
(mu:define-variable-override mu:get-changed-units mu:get-changed-units-impl)
(mu:define-variable-override mu:push-changed-units mu:push-changed-units-impl)
(mu:define-variable-override mu:reset-changed-units mu:reset-changed-units-impl)
(mu:define-variable-override mu:build-kernel mu:build-kernel-impl)
(mu:define-variable-override mu:build-image mu:build-image-impl)
(mu:define-variable-override mu:build-library mu:build-library-impl)
)
