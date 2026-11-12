;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Mu Compile
;;;


;;;
;;;; Manifest
;;;


(define (mu:manifest-references-valid? bin)
  (define (get-manifest)
    (let ((digest-filepath (mu:digest-pathname (%%get-resource-repository bin) bin))
          (manifest-filepath (mu:manifest-pathname (%%get-resource-repository bin) bin)))
      (mu:load-manifest digest-filepath manifest-filepath)))
  
  (define (module-references-valid? version-alist lst)
    (define (recompile-reference? module-locator module-references)
      (define (recompile-references-match? recompile-references)
        (mu:some? (lambda (recompile-reference)
                      (if (symbol? recompile-reference)
                          (eq? recompile-reference module-locator)
                        (and (eq? (car recompile-reference) module-locator)
                             (mu:some? (lambda (recompile-symbol)
                                           (memq recompile-symbol module-references))
                                         (cdr recompile-reference)))))
                    recompile-references))
      
      (if (and mu:recompile-references
               (recompile-references-match? mu:recompile-references))
          #t
        (continuation-capture
          (lambda (return)
            (mu:for-each-higher-version version-alist
              (lambda (version)
                (let ((recompile-references (mu:get-version-recompile-references version)))
                  (if (and recompile-references
                           (recompile-references-match? recompile-references))
                      (continuation-return return #t)))))
            #f))))
    
    (let ((module-locator (car lst))
          (module-references (cdr lst)))
      (and (not (recompile-reference? module-locator module-references))
           (let ((module-declaration (mu:outline-module module-locator error?: #f)))
             (and module-declaration
                  (mu:every? (lambda (symbol)
                                 (let ((found (if (pair? symbol)
                                                  (let iter ((symbols symbol)
                                                             (declaration module-declaration))
                                                       (cond ((not declaration)
                                                              #f)
                                                             ((pair? symbols)
                                                              (iter (cdr symbols) (mu:find-declaration declaration (car symbols))))
                                                             (else
                                                              declaration)))
                                                (mu:find-public-declaration module-declaration symbol))))
                                   (and found
                                        (or (eq? (mu:get-lexical-binding-name (mu:get-declaration-toplevel found)) module-locator)
                                            ;; GAZOUM mega quick dirty hack around the problem that because hubs do not
                                            ;; trigger import conflicts then this code is totally brittle and it is possible
                                            ;; to find a hub in one module and next time in another thus triggering incessant
                                            ;; recompilations
                                            (and mu:manifest-ignore?
                                                 (mu:manifest-ignore? found))))))
                               module-references))))))
  
  (let ((manifest (get-manifest)))
    (if manifest
        (let ((version-alist (%%get-manifest-version manifest))
              (references (%%get-manifest-references manifest)))
          (if references
              (mu:every? (lambda (lst)
                             (module-references-valid? version-alist lst))
                           references)
            #f))
      #f)))


(mu:define-variable-override mu:manifest-valid?
  mu:manifest-references-valid?)


;;;
;;;; Compile
;;;


(mu:define-variable-override mu:compile-unit
  (lambda (unit-name #!key (output-language #f) (options #f) (custom-cc #f) (cc-options #f) (ld-options #f) (rpaths #f) (skip-references? #f) (force? #f))
    (mu:with-unit-resources unit-name #f
      (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
        (if src
            (parameterize ((mu:requested-unit-name unit-name)
                           (mu:requested-unit-resource src)
                           (mu:requested-pathname #f))
              (mu:compile-source src obj bin obj-uptodate? bin-uptodate? unit-name output-language: output-language options: options custom-cc: custom-cc cc-options: cc-options ld-options: ld-options rpaths: rpaths skip-references?: skip-references? force?: force?))
          (%%unless (and bin (file-exists? (mu:resource-pathname (mu:bin->otl bin))))
            (mu:error "Unable to find source for: {s}" unit-name)))))))


;; this function should be unified with mu:compile-unit
;; (being careful about mu:find-unit-product overhead)
(mu:define-variable-override mu:custom-compile-unit
  (lambda (unit-name #!key (options #f) (skip-references? #f) (force? #f))
    (let ((product (mu:find-unit-product unit-name)))
      (let ((build (and product
                        (%%get-product-build product))))
        (if build
            (build (%%get-product-descriptor product) unit: unit-name skip-references?: skip-references? force?: force?)
          (mu:compile-unit unit-name options: options skip-references?: skip-references? force?: force?))))))


(define (mu:custom-compile/build unit-specs #!key (unit #f) (pre-build #f) (force? #f))
  (if unit
      (let ((compile-args (assq unit unit-specs)))
        (if compile-args
            (apply mu:compile-unit `(,@compile-args force?: ,force?))
          (mu:compile-unit unit force?: force?)))
    (begin
      (if pre-build
          (pre-build))
      (for-each (lambda (compile-args)
                  (apply mu:compile-unit `(,@compile-args force?: ,force?)))
                unit-specs))))


(define (mu:find-unit-product unit-name)
  #f #; ;; bazoo
  (let ((src (mu:find-unit-src unit-name)))
    (let ((package (%%get-resource-package src)))
      (let ((products (%%get-package-products package)))
        (continuation-capture
          (lambda (return)
            ;; first go through all units in all updates
            (for-each (lambda (product-descriptor)
                        (let* ((product-name (mu:product-descriptor-name product-descriptor))
                               (product (mu:get-product product-name)))
                          (let ((update (mu:cond-expanded-product-descriptor-update product-name product-descriptor)))
                            (for-each (lambda (unit)
                                        (if (eq? unit unit-name)
                                            (continuation-return return product)))
                                      update))))
                      products)
            ;; then do the much more costly subunits scan
            (for-each (lambda (product-descriptor)
                        (let* ((product-name (mu:product-descriptor-name product-descriptor))
                               (product (mu:get-product product-name)))
                          (let ((update (mu:cond-expanded-product-descriptor-update product-name product-descriptor)))
                            (for-each (lambda (unit)
                                        (mu:for-each-subunit unit
                                          (lambda (sub-unit declaration phase)
                                            (if (eq? sub-unit unit-name)
                                                (continuation-return return product)))))
                                      update))))
                      products)
            #f))))))


(define (mu:find-unit-options unit-name)
  (let ((product (mu:find-unit-product unit-name)))
    (and product
         (let ((options (%%get-product-options product)))
           (and options
                (let ((pair (assq unit-name options)))
                  (and pair
                       (cdr pair))))))))


;; at the moment building large files like functional in develop is much too
;; slow because of the safe declare, hence this least of two evils solution
(define (mu:wrap-single-host-cc-options str)
  (if (or (eq? mu:kernel-safety 'core)
          (eq? mu:kernel-safety 'debug)
          (eq? mu:kernel-safety 'develop)
          (eq? mu:kernel-safety 'release))
      (string-append "-U___SINGLE_HOST " str)
    str))


;; compilation time is just too long in core
(define (mu:wrap-no-optimize-cc-options str)
  (if (eq? mu:kernel-safety 'core)
      (string-append "-O0 " str)
    str))


;; hack around some antivirus like avast and avg
;; returning sometimes an evo-gen false positive
(define (mu:wrap-antivirus-hack-around manifest-name str)
  (cond-expand
    (windows (case manifest-name
               ((zen.ui/view/Tab-View)
                (string-append "-Os " str))
               ((zen.project/project/Project)
                (string-append "-Os " str))
               (else
                str)))
    (else str)))


(define mu:custom-cc-path
  (let ((cache (make-table test: eq?)))
    (lambda (custom-cc)
      (or (table-ref cache custom-cc #f)
          (let ((path
                  (case custom-cc
                    ((llvm)
                     (string-append "/usr/bin/" (mu:compiler-name))))))
            (table-set! cache custom-cc path)
            path)))))

(define mu:custom-cc-arguments
  (let ((cache (make-table test: eq?)))
    (lambda (custom-cc)
      (or (table-ref cache custom-cc #f)
          ;; bongo
          (let ((path (string-append (path-expand "~~bin") "gambuild-C" mu:os-bat-extension-string-saved)))
            (define (read-info argument)
              (let ((port (open-process (list path: path arguments: (list argument)))))
                (let ((info (mu:remove "" (mu:split-string (read-line port) #\space) test: equal?)))
                  (close-port port)
                  info)))
            
            ;; too many differences between LLVM and GCC to use FLAGS_DYN
            ;; this list was copied from a Gambit LLVM build
            (define mu:llvm-arguments
              '("-O1"
                "-Wno-unused"
                "-Wno-write-strings"
                "-Wdisabled-optimization"
                "-fno-trapping-math"
                "-fno-math-errno"
                "-fwrapv"
                "-fno-strict-aliasing"
                "-fomit-frame-pointer"
                "-fPIC"
                "-fno-common"
                "-mieee-fp"
                ;; "-fschedule-insns2"
                ;; "-mpc64"
                ))

            (let ((flags mu:llvm-arguments)
                  (defs (read-info "DEFS_DYN")))
              (let ((arguments (append flags defs)))
                (table-set! cache custom-cc arguments)
                arguments)))))))


(define (mu:compile-source src obj bin obj-uptodate? bin-uptodate? manifest-name #!key (output-language #f) (options #f) (custom-cc #f) (cc-options #f) (ld-options #f) (rpaths #f) (skip-references? #f) (force? #f))
  (let ((references-valid? (or skip-references? (and (or obj-uptodate? bin-uptodate?) (mu:manifest-references-valid? (or obj bin))))))
    (let ((options (or options mu:compile-options))
          (cc-options (mu:wrap-single-host-cc-options (mu:wrap-no-optimize-cc-options (mu:wrap-antivirus-hack-around manifest-name (or cc-options "")))))
          (ld-options (or ld-options ""))
          (update-obj? (or force? (not obj-uptodate?) (not references-valid?)))
          (update-bin? (or force? (not bin-uptodate?) (not references-valid?))))
      (let ((compile? (or update-obj? (and update-bin? (mu:link-objects?)))))
        (if compile?
            (let ((pathname (mu:resource-pathname src))
                  (bindir (mu:resource-build-dir src)))
              (let ((build-repository mu:Build-Repository))
                (mu:create-directories bindir)
                (mu:with-extension-reader (%%get-resource-extension src)
                  (lambda ()
                    (parameterize ((mu:walk-for 'compile))
                      (mu:compile-source-file src bin update-obj? update-bin? build-repository output-language: output-language options: options custom-cc: custom-cc cc-options: cc-options ld-options: ld-options rpaths: rpaths unit-name: manifest-name)))))))
        (if (or compile? (mu:force-outlines?))
            (let ((path (mu:binary-with-extension src ".otl")))
              (mu:create-directories (mu:pathname-dir path))
              (call-with-output-file (list path: path eol-encoding: (mu:platform-eol-encoding mu:kernel-platform))
                (lambda (output)
                  (let ((declaration (mu:outline-unit manifest-name)))
                    (mu:outline-generate declaration output))))))))))


(define (mu:compile-source-file src bin update-obj? update-bin? build-repository #!key (output-language #f) (options #f) (custom-cc #f) (cc-options #f) (ld-options #f) (rpaths #f) (unit-name #f) (platform mu:kernel-platform))
  (define bin-pathname-base
    (mu:binary-with-extension src ""))
  
  (define bin-extension
    (string-append "." (mu:language-extension (or output-language 'c))))
  
  (define mac?
    (eq? mu:kernel-platform 'mac))
  
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
      "-fno-common" "-fno-objc-arc" "-Wno-shift-negative-value" "-Wno-missing-field-initializers" "-Wno-missing-prototypes" "-Werror=return-type" "-Wunreachable-code"
      "-Werror=deprecated-objc-isa-usage" "-Werror=objc-root-class" "-Wno-missing-braces" "-Wparentheses" "-Wswitch" "-Wempty-body"
      "-Wconditional-uninitialized" "-Wno-unknown-pragmas" "-Wno-shadow" "-Wno-four-char-constants" "-Wno-conversion" "-Wconstant-conversion"
      "-Wint-conversion" "-Wbool-conversion" "-Wenum-conversion" "-Wno-newline-eof"
      "-isysroot" ios-sysroot "-fasm-blocks" "-fstrict-aliasing" "-Wdeprecated-declarations" "-mios-simulator-version-min=9.0" "-Wno-sign-conversion")))
  
  (define ios-gambit-include-dir
    (path-expand "~~ios/include"))
  
  (define options
    `((module-ref ,unit-name) ,@(if (eq? options #!void) '() options)))
  
  (define (compile)
    (let ((src-pathname (mu:resource-pathname src))
          (bin-output (string-append bin-pathname-base bin-extension))
          (link-file-output (string-append bin-pathname-base "_" bin-extension)))
      (parameterize ((mu:generate-symbol-for "^")
                     (mu:generate-symbol-context unit-name)
                     (mu:compiled-source src))
        ;; temporary until a cleaner solution
        (mu:set-gensym-counter! -1)
        (if (not (and (compile-file-to-target src-pathname output: bin-output options: options)
                      (if ios?
                          (let ((custom-cc-options ios-custom-cc-options))
                            (mu:call-process
                              (list
                                path: ios-custom-cc
                                arguments: `(,@custom-cc-options ,(string-append "-I" ios-gambit-include-dir) "-D___DYNAMIC" ,@(mu:split-string cc-options #\space) "-c" "-o" ,(string-append bin-pathname-base ".o") ,bin-output)
                                show-console: #f)))
                        (if custom-cc
                            (let ((gambit-include-dir (path-expand "~~include")))
                              (case custom-cc
                                ((llvm)
                                 (mu:call-process
                                   (list
                                     path: (mu:custom-cc-path custom-cc)
                                     arguments: `("-D___SUPPORT_MULTIPLE_C_COMPILERS" "-D___DONT_USE_builtin_setjmp" ,@(mu:custom-cc-arguments custom-cc) ,(string-append "-I" gambit-include-dir) ,@(mu:split-string cc-options #\space) "-c" "-o" ,(string-append bin-pathname-base ".o") ,bin-output)
                                     show-console: #f)))))
                          (compile-file bin-output options: (cons 'obj options) cc-options: (string-append "-D___DYNAMIC " cc-options))))))
            (mu:error "Compilation failed")))))
  
  (define (update-manifest)
    (let ((digest-filepath (mu:digest-pathname build-repository src))
          (manifest-filepath (mu:manifest-pathname build-repository src))
          (src-filepath (mu:resource-pathname src))
          (references (let ((module-declaration (mu:get-catalog-entry unit-name)))
                        (cond ((%%is? module-declaration mu:Module-Declaration)
                               (mu:generate-reference-list module-declaration))
                              ((%%is? module-declaration mu:Unit-Declaration)
                               '())
                              (else ; pure scheme
                               '())))))
      (mu:update-manifest-compile-time unit-name digest-filepath manifest-filepath src-filepath references)))
  
  (define (delete-o1-file file)
    (with-exception-catcher
      (lambda (exc)
        (mu:error "Compilation failed while deleting binary file {a}. A process may be using it." file))
      (lambda ()
        (delete-file file))))
  
  (define (delete-o1-files)
    (mu:for-each-numbered-pathname (string-append bin-pathname-base ".o") 1 delete-o1-file))
  
  (define (determine-o1)
    (if mu:single-objects?
        (begin
          (delete-o1-files)
          (string-append bin-pathname-base ".o1"))
      (mu:probe-numbered-pathname (string-append bin-pathname-base ".o") 1)))
  
  (define (link-o1 apple-id)
    (let ((bin-o1 (determine-o1)))
      (let ((bin-output (string-append bin-pathname-base bin-extension))
            (linkfile (string-append bin-o1 bin-extension)))
        (link-flat (list bin-output) output: linkfile warnings?: #f)
        (let ((exit-status
                (if ios?
                    (let ((custom-cc-options (cons "-bundle" ios-custom-cc-options))
                          (link-options (case mu:kernel-compiler ((c++) '("-lc++" "-framework" "Foundation" "-framework" "UIKit" "-framework" "CoreText")) (else '()))))
                      (mu:invoke-process
                        (list
                          path: ios-custom-cc
                          arguments: `(,@custom-cc-options ,@link-options ,(string-append "-I" ios-gambit-include-dir) ,@(mu:split-string cc-options #\space) ,@(mu:split-string ld-options #\space) ,linkfile ,(string-append bin-pathname-base ".o") "-o" ,bin-o1)
                          show-console: #f)))
                  (mu:gambitbuild
                    'dyn
                    (mu:resource-build-dir src)
                    (list linkfile (string-append bin-pathname-base ".o"))
                    bin-o1
                    cc-options
                    ""
                    (case platform
                      ((mac)
                       (string-append ld-options " -headerpad_max_install_names"))
                      (else
                       ld-options))
                    options))))
          (if (not (= exit-status 0))
              (mu:error "C compilation failed while linking module"))
          (case platform
            ((mac)
             (if rpaths
                 (for-each (lambda (rpath)
                             (mu:call-process
                               (list
                                 path: "install_name_tool"
                                 arguments: `("-add_rpath" ,rpath ,bin-o1))))
                           rpaths))))
          (if apple-id
              (mu:codesign mac? ios? apple-id bin-o1))
          (delete-file linkfile)))))
  
  (let ((will-link? (and update-bin? (or (mu:link-objects?) (and bin (not mu:single-objects?))))))
    (let ((will-compile? (and update-obj? (or will-link? (mu:link-libraries?))))
          (dry? (mu:dry-run?))
          (apple-id (getenv "ZEN_APPLE_DEVELOPER_ID" #f)))
      (if (or will-compile? will-link?)
          (let ((path (%%get-resource-path src)))
            (mu:push-changed-units path)
            (display (if (and apple-id (mu:link-objects?)) "; compiling & signing " "; compiling "))
            (display path)
            (display "...")
            (newline)
            (force-output)))
      (if (not dry?)
          (begin
            (if will-compile? (compile))
            (if update-bin?
                (if will-link?
                    (link-o1 apple-id)
                  (delete-o1-files)))
            (if will-compile? (update-manifest)))))))


;;;
;;;; Build
;;;


(mu:define-variable-override mu:build-unit
  (lambda (unit-name)
    (mu:for-each-subunit unit-name
      (lambda (unit-name declaration phase)
        (mu:compile-unit unit-name)))))


;;;
;;;; Unit
;;;


(mu:define-variable-override mu:get-subunit-names
  (lambda (parent-name #!key (walk-continue? #f))
    (let ((sub-units '()))
      (let ((proc (lambda (unit-name declaration phase)
                    (set! sub-units (cons unit-name sub-units)))))
        (parameterize ((mu:for-each-subunit-continue
                         (and walk-continue? (lambda (unit-name exc)
                                               (if (%%is? exc mu:Walk-Problems)
                                                   (set! sub-units (cons unit-name sub-units)))))))
          (mu:for-each-subunit parent-name proc))
        sub-units))))
