;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Mu Configuration
;;;


;;;
;;;; Source
;;;


(define mu:zen-source
  mu:source)


;;;
;;;; Muini
;;;


(define (mu:process-muini)
  (mu:load-global/local ".muini"))


;;;
;;;; Configuration
;;;


(define-type mu:configuration constructor: mu:make-configuration predicate: mu:configuration?
  (name mu:get-configuration-name mu:set-configuration-name)
  (system mu:get-configuration-system mu:set-configuration-system)
  (platform mu:get-configuration-platform mu:set-configuration-platform)
  (compiler mu:get-configuration-compiler mu:set-configuration-compiler)
  (processor mu:get-configuration-processor mu:set-configuration-processor)
  (windowing mu:get-configuration-windowing mu:set-configuration-windowing)
  (safety mu:get-configuration-safety mu:set-configuration-safety)
  (optimize? mu:get-configuration-optimize? mu:set-configuration-optimize?)
  (debug-environments? mu:get-configuration-debug-environments? mu:set-configuration-debug-environments?)
  (debug-location? mu:get-configuration-debug-location? mu:set-configuration-debug-location?)
  (debug-source? mu:get-configuration-debug-source? mu:set-configuration-debug-source?)
  (debug-foreign? mu:get-configuration-debug-foreign? mu:set-configuration-debug-foreign?)
  (track-memory? mu:get-configuration-track-memory? mu:set-configuration-track-memory?)
  (mutable-bindings? mu:get-configuration-mutable-bindings? mu:set-configuration-mutable-bindings?)
  (destination mu:get-configuration-destination mu:set-configuration-destination)
  (features mu:get-configuration-features mu:set-configuration-features)
  (properties mu:get-configuration-properties mu:set-configuration-properties)
  (local? mu:get-configuration-local? mu:set-configuration-local?))


;;;
;;;; Configuration
;;;


(define (mu:new-configuration
          #!key
          (name (mu:unspecified-option))
          (system (mu:unspecified-option))
          (platform (mu:unspecified-option))
          (compiler (mu:unspecified-option))
          (processor (mu:unspecified-option))
          (windowing (mu:unspecified-option))
          (safety (mu:unspecified-option))
          (optimize? (mu:unspecified-option))
          (debug-environments? (mu:unspecified-option))
          (debug-location? (mu:unspecified-option))
          (debug-source? (mu:unspecified-option))
          (debug-foreign? (mu:unspecified-option))
          (track-memory? (mu:unspecified-option))
          (mutable-bindings? (mu:unspecified-option))
          (destination (mu:unspecified-option))
          (features (mu:unspecified-option))
          (properties (mu:unspecified-option))
          (local? #f))
  (mu:make-configuration
    name
    system
    platform
    compiler
    processor
    windowing
    safety
    optimize?
    debug-environments?
    debug-location?
    debug-source?
    debug-foreign?
    track-memory?
    mutable-bindings?
    destination
    features
    properties
    local?))


(define (mu:validate-configuration
          template
          #!key
          (name (mu:unspecified-option))
          (system (mu:unspecified-option))
          (platform (mu:unspecified-option))
          (compiler (mu:unspecified-option))
          (processor (mu:unspecified-option))
          (windowing (mu:unspecified-option))
          (safety (mu:unspecified-option))
          (optimize? (mu:unspecified-option))
          (debug-environments? (mu:unspecified-option))
          (debug-location? (mu:unspecified-option))
          (debug-source? (mu:unspecified-option))
          (debug-foreign? (mu:unspecified-option))
          (track-memory? (mu:unspecified-option))
          (mutable-bindings? (mu:unspecified-option))
          (destination (mu:unspecified-option))
          (features (mu:unspecified-option))
          (properties (mu:unspecified-option))
          (local? #f))
  (let* ((name (mu:validate-name (mu:require-name name template)))
         (system (mu:validate-system (mu:require-system system template)))
         (platform (mu:validate-platform (mu:require-platform platform template)))
         (compiler (mu:validate-compiler (mu:require-compiler platform compiler template)))
         (processor (mu:validate-processor (mu:require-processor platform processor template)))
         (windowing (mu:validate-windowing (mu:require-windowing platform windowing template)))
         (safety (mu:validate-safety (mu:require-safety safety template)))
         (optimize? (mu:validate-optimize? (mu:require-optimize? optimize? template)))
         (debug-environments? (mu:validate-debug-environments? (mu:require-debug-environments? debug-environments? template)))
         (debug-location? (mu:validate-debug-location? (mu:require-debug-location? debug-location? template)))
         (debug-source? (mu:validate-debug-source? (mu:require-debug-source? debug-source? template)))
         (debug-foreign? (mu:validate-debug-foreign? (mu:require-debug-foreign? debug-foreign? template)))
         (track-memory? (mu:validate-track-memory? (mu:require-track-memory? track-memory? template)))
         (mutable-bindings? (mu:validate-mutable-bindings? (mu:require-mutable-bindings? mutable-bindings? template)))
         (destination (mu:validate-destination (mu:require-destination destination template)))
         (features (mu:validate-features (mu:require-features features template)))
         (properties (mu:validate-properties (mu:require-properties properties template))))
    (mu:make-configuration
      name
      system
      platform
      compiler
      processor
      windowing
      safety
      optimize?
      debug-environments?
      debug-location?
      debug-source?
      debug-foreign?
      track-memory?
      mutable-bindings?
      destination
      features
      properties
      local?)))


(define (mu:configuration-directory configuration)
  (mu:destination-directory
    (mu:get-configuration-name configuration)
    (mu:get-configuration-destination configuration)
    "./"))


(define (mu:setup-configurations)
  #f)


;;;
;;;; Configurations
;;;


(define mu:anonymous-configuration-file
  "./.configuration")

(define mu:configurations
  '())


(define (mu:list-configurations)
  (for-each mu:describe-configuration (mu:sort-configurations mu:configurations)))


(define (mu:require-configuration name)
  (or (mu:find-configuration name)
      (if (not name)
          (mu:error "Unable to find default configuration")
        (mu:error "Unable to find configuration: {s}" name))))

(define (mu:require-default-configuration)
  (or (let ((muconf (getenv "MUCONF" #f)))
        (if muconf
            (mu:find-configuration (string->symbol muconf))
          (mu:find-configuration #f)))
      (begin
        (mu:feedback "configure")
        (mu:configure mu:unspecified-configuration)
        (mu:require-configuration #f))))


(define (mu:find-configuration name)
  (let ((configuration
          (let ((pair (mu:find-configuration-pair name)))
            (if (not pair)
                #f
              (car pair)))))
    ;; special case to support make of binaries
    (if (and (not name) (not configuration))
        (let ((configuration-dir (mu:destination-directory #f "bin:" "./")))
          (let ((configuration-file (string-append configuration-dir ".configuration")))
            (if (file-exists? configuration-file)
                (mu:load-configuration configuration-file)
              #f)))
      configuration)))

(define (mu:find-configuration-pair name)
  (let iter ((configurations mu:configurations))
    (if (null? configurations)
        #f
      (let ((configuration (car configurations)))
        (if (eq? (mu:get-configuration-name configuration) name)
            configurations
          (iter (cdr configurations)))))))


(define (mu:sort-configurations configurations)
  (list-sort (lambda (c1 c2)
               (let ((n1 (mu:get-configuration-name c1))
                     (n2 (mu:get-configuration-name c2)))
                 (cond ((not n1)
                        #t)
                       ((not n2)
                        #f)
                       (else
                        (string-ci<? (symbol->string n1) (symbol->string n2))))))
             configurations))


(define (mu:register-configuration configuration)
  (let ((name (mu:get-configuration-name configuration)))
    (let ((pair (mu:find-configuration-pair name)))
      (if pair
          (set-car! pair configuration)
        (set! mu:configurations (append mu:configurations (list configuration)))))))


(define (mu:delete-configuration name)
  (set! mu:configurations
        (mu:delete name mu:configurations
          (lambda (c1 c2)
            (eq? (mu:get-configuration-name c1)
                 (mu:get-configuration-name c2))))))


(define (mu:load-configurations)
  (if (file-exists? mu:configurations-file)
      (call-with-input-file (list path: mu:configurations-file eol-encoding: 'cr-lf)
        (lambda (input)
          (define (read-configuration input)
            (let ((list (read input)))
              (if (eof-object? list)
                  list
                (apply mu:validate-configuration mu:unspecified-configuration local?: #t list))))
          
          (for-each mu:register-configuration (read-all input read-configuration)))))
  (if (file-exists? mu:anonymous-configuration-file)
      (mu:register-configuration (mu:load-configuration mu:anonymous-configuration-file))))


(define (mu:load-configuration file)
  (define (parse-properties data)
    (if (and (pair? data)
             (eq? (car data) 'configuration))
        (caddr data)
      data))
  
  (call-with-input-file (list path: file eol-encoding: 'cr-lf)
    (lambda (input)
      (let ((data (read input)))
        ;; quicky until a full-fledged mu:versioned-file similar to mu:versioned-directory
        ;; removing mu:convert-configuration-205001 buggy for absolute path ("c:/" -> "c//")
        (apply mu:validate-configuration mu:unspecified-configuration (parse-properties data))))))


(define (mu:describe-configuration configuration)
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
        (destination (mu:get-configuration-destination configuration))
        (features (mu:get-configuration-features configuration))
        (properties (mu:get-configuration-properties configuration)))
    (mu:feedback "{a}" (or name "<default>"))
    (mu:feedback "  system: {s}" system)
    (mu:feedback "  platform: {s}" platform)
    (mu:feedback "  compiler: {s}" compiler)
    (mu:feedback "  processor: {s}" processor)
    (mu:feedback "  windowing: {s}" windowing)
    (mu:feedback "  safety: {s}" safety)
    (mu:feedback "  optimize?: {s}" optimize?)
    (mu:feedback "  debug-environments?: {s}" debug-environments?)
    (mu:feedback "  debug-location?: {s}" debug-location?)
    (mu:feedback "  debug-source?: {s}" debug-source?)
    (mu:feedback "  debug-foreign?: {s}" debug-foreign?)
    (mu:feedback "  track-memory?: {s}" track-memory?)
    (mu:feedback "  mutable-bindings?: {s}" mutable-bindings?)
    (mu:feedback "  destination: {s}" destination)
    (mu:feedback "  features: {s}" features)
    (mu:feedback "  properties: {s}" properties)))


;;;
;;;; Configure
;;;


(define (mu:configure
          template
          #!key
          (name (mu:unspecified-option))
          (system (mu:unspecified-option))
          (platform (mu:unspecified-option))
          (compiler (mu:unspecified-option))
          (processor (mu:unspecified-option))
          (windowing (mu:unspecified-option))
          (safety (mu:unspecified-option))
          (optimize? (mu:unspecified-option))
          (debug-environments? (mu:unspecified-option))
          (debug-location? (mu:unspecified-option))
          (debug-source? (mu:unspecified-option))
          (debug-foreign? (mu:unspecified-option))
          (track-memory? (mu:unspecified-option))
          (mutable-bindings? (mu:unspecified-option))
          (destination (mu:unspecified-option))
          (features (mu:unspecified-option))
          (properties (mu:unspecified-option)))
  (let ((configuration
          (mu:validate-configuration
            template
            name: name
            system: system
            platform: platform
            compiler: compiler
            processor: processor
            windowing: windowing
            safety: safety
            optimize?: optimize?
            debug-environments?: debug-environments?
            debug-location?: debug-location?
            debug-source?: debug-source?
            debug-foreign?: debug-foreign?
            track-memory?: track-memory?
            mutable-bindings?: mutable-bindings?
            destination: destination
            features: features
            properties: properties)))
    (mu:register-configuration configuration)
    (mu:describe-configuration configuration)))


;;;
;;;; Name
;;;


(define (mu:require-name name template)
  (mu:or-option name (mu:get-configuration-name template) (mu:default-name)))


(define (mu:validate-name name)
  (if (or (not name) (and (symbol? name) (mu:valid-filename? (symbol->string name))))
      name
    (mu:error "Invalid name: {s}" name)))


;;;
;;;; System
;;;


(define mu:valid-systems
  '(gambit))


(define (mu:require-system system template)
  (mu:or-option system (mu:get-configuration-system template) (mu:default-system)))


(define (mu:validate-system system)
  (if (memq system mu:valid-systems)
      system
    (mu:error "Invalid system: {s}" system)))


;;;
;;;; Platform
;;;


(define mu:valid-platforms
  '(mac
    ios
    windows
    linux))


(define (mu:guess-platform)
  (let ((system (cadr (system-type)))
        (os (caddr (system-type))))
    (cond ((eq? system 'apple)
           'mac)
          ((mu:linux-family)
           'linux)
          (else
           'windows))))


(define (mu:require-platform platform template)
  (mu:or-option platform (mu:get-configuration-platform template) (mu:default-platform) (mu:guess-platform)))


(define (mu:validate-platform platform)
  (if (memq platform mu:valid-platforms)
      platform
    (mu:error "Invalid platform: {s}" platform)))


;;;
;;;; Compiler
;;;


(define mu:valid-compilers
  '(c
    c++))


(define (mu:guess-compiler platform)
  ;; at the moment only the C++ compiler with its exceptions
  ;; plays well with visual C's structured exception handling
  (if (eq? platform 'windows)
      'c++
    'c))


(define (mu:require-compiler platform compiler template)
  (mu:or-option compiler (mu:get-configuration-compiler template) (mu:default-compiler) (mu:guess-compiler platform)))


(define (mu:validate-compiler compiler)
  (if (memq compiler mu:valid-compilers)
      compiler
    (mu:error "Invalid compiler: {s}" compiler)))


;;;
;;;; Processor
;;;


(define mu:valid-processors
  '(#f
    x86
    arm
    silicon))


(define (mu:guess-processor platform)
  (if (eq? platform 'mac)
      (let ((processor (car (system-type))))
        (if (eq? processor 'aarch64)
            'silicon
          #f))
    #f))


(define (mu:require-processor platform processor template)
  (mu:or-option processor (mu:get-configuration-processor template) (mu:default-processor) (mu:guess-processor platform)))


(define (mu:validate-processor processor)
  (if (memq processor mu:valid-processors)
      processor
    (mu:error "Invalid processor: {s}" processor)))


;;;
;;;; Windowing
;;;


(define mu:valid-windowings
  '(cocoa
    #f
    x11))


(define (mu:require-windowing platform windowing template)
  (define (guess-windowing platform)
    (case platform
      ((mac) 'cocoa)
      ((windows) #f)
      ((linux) 'x11)))
  
  (mu:or-option windowing (mu:get-configuration-windowing template) (mu:default-windowing) (guess-windowing platform)))


(define (mu:validate-windowing windowing)
  (if (memq windowing mu:valid-windowings)
      windowing
    (mu:error "Invalid windowing: {s}" windowing)))


;;;
;;;; Safety
;;;


(define mu:valid-safeties
  '(core
    debug
    develop
    release
    sealed))


(define (mu:require-safety safety template)
  (mu:or-option safety (mu:get-configuration-safety template) (mu:default-safety)))


(define (mu:validate-safety safety)
  (if (memq safety mu:valid-safeties)
      safety
    (mu:error "Invalid safety: {s}" safety)))


;;;
;;;; Optimize
;;;


(define mu:valid-optimize
  '(#f
    #t))


(define (mu:require-optimize? optimize template)
  (mu:or-option optimize (mu:get-configuration-optimize? template) (mu:default-optimize?)))


(define (mu:validate-optimize? optimize)
  (if (memq optimize mu:valid-optimize)
      optimize
    (mu:error "Invalid optimize?: {s}" optimize)))


;;;
;;;; Debug-Environments
;;;


(define mu:valid-debug-environments
  '(#f
    #t))


(define (mu:require-debug-environments? debug-environments template)
  (mu:or-option debug-environments (mu:get-configuration-debug-environments? template) (mu:default-debug-environments?)))


(define (mu:validate-debug-environments? debug-environments)
  (if (memq debug-environments mu:valid-debug-environments)
      debug-environments
    (mu:error "Invalid debug-environments?: {s}" debug-environments)))


;;;
;;;; Debug-Location
;;;


(define mu:valid-debug-location
  '(#f
    #t))


(define (mu:require-debug-location? debug-location template)
  (mu:or-option debug-location (mu:get-configuration-debug-location? template) (mu:default-debug-location?)))


(define (mu:validate-debug-location? debug-location)
  (if (memq debug-location mu:valid-debug-location)
      debug-location
    (mu:error "Invalid debug-location?: {s}" debug-location)))


;;;
;;;; Debug-Source
;;;


(define mu:valid-debug-source
  '(#f
    #t))


(define (mu:require-debug-source? debug-source template)
  (mu:or-option debug-source (mu:get-configuration-debug-source? template) (mu:default-debug-source?)))


(define (mu:validate-debug-source? debug-source)
  (if (memq debug-source mu:valid-debug-source)
      debug-source
    (mu:error "Invalid debug-source?: {s}" debug-source)))


;;;
;;;; Debug-Foreign
;;;


(define mu:valid-debug-foreign
  '(#f
    #t))


(define (mu:require-debug-foreign? debug-foreign template)
  (mu:or-option debug-foreign (mu:get-configuration-debug-foreign? template) (mu:default-debug-foreign?)))


(define (mu:validate-debug-foreign? debug-foreign)
  (if (memq debug-foreign mu:valid-debug-foreign)
      debug-foreign
    (mu:error "Invalid debug-foreign?: {s}" debug-foreign)))


;;;
;;;; Track-Memory
;;;


(define mu:valid-track-memory
  '(#f
    #t))


(define (mu:require-track-memory? track-memory template)
  (mu:or-option track-memory (mu:get-configuration-track-memory? template) (mu:default-track-memory?)))


(define (mu:validate-track-memory? track-memory)
  (if (memq track-memory mu:valid-track-memory)
      track-memory
    (mu:error "Invalid track-memory?: {s}" track-memory)))


;;;
;;;; Mutable-Bindings
;;;


(define mu:valid-mutable-bindings
  '(#f
    #t))


(define (mu:require-mutable-bindings? mutable-bindings template)
  (mu:or-option mutable-bindings (mu:get-configuration-mutable-bindings? template) (mu:default-mutable-bindings?)))


(define (mu:validate-mutable-bindings? mutable-bindings)
  (if (memq mutable-bindings mu:valid-mutable-bindings)
      mutable-bindings
    (mu:error "Invalid mutable-bindings?: {s}" mutable-bindings)))


;;;
;;;; Destination
;;;


(define (mu:require-destination destination template)
  (mu:or-option destination (mu:get-configuration-destination template) (mu:default-destination)))


(define (mu:validate-destination destination)
  (if (or (not destination) (string? destination))
      destination
    (mu:error "Invalid destination: {s}" destination)))


(define (mu:configuration-file configuration)
  (let ((dir (mu:configuration-directory configuration)))
    (string-append dir ".configuration")))


;;;
;;;; Features
;;;


(define (mu:require-features features template)
  (mu:or-option features (mu:get-configuration-features template) (mu:default-features)))


(define (mu:validate-features features)
  (if (list? features)
      features
    (mu:error "Invalid features: {s}" features)))


;;;
;;;; Properties
;;;


(define (mu:require-properties properties template)
  (mu:or-option properties (mu:get-configuration-properties template) (mu:default-properties)))


(define (mu:validate-properties properties)
  (if (list? properties)
      properties
    (mu:error "Invalid properties: {s}" properties)))


;;;
;;;; Symbol
;;;


(define (mu:parse-symbols lst proc)
  (let iter ((scan lst)
             (symbols '()))
       (if (null? scan)
           (proc (reverse symbols) '())
         (let ((obj (car scan)))
           (if (or (keyword? obj) (and (symbol? obj) (eqv? #\- (string-ref (symbol->string obj) 0))))
               (proc (reverse symbols) (map mu:stringify scan))
             (iter (cdr scan) (cons obj symbols)))))))


(define (mu:parse-symbol symbol proc)
  (define (parse-target/configuration/image str proc)
    (let ((colon (mu:string-find str #\:)))
      (if (not colon)
          (parse-target/configuration str
            (lambda (target configuration)
              (proc target configuration #f)))
        (let ((image
                (if (= (+ colon 1) (string-length str))
                    #f
                  (standardize-image (string->symbol (substring str (+ colon 1) (string-length str)))))))
          (parse-target/configuration (substring str 0 colon)
            (lambda (target configuration)
              (proc target configuration image)))))))
  
  (define (parse-target/configuration str proc)
    (let ((at (mu:string-find str #\@)))
      (if (not at)
          (proc (if (string=? str "") (mu:default-target) (string->symbol str)) (mu:require-default-configuration))
        (let ((target
                (if (= at 0)
                    (mu:default-target)
                  (string->symbol (substring str 0 at))))
              (configuration
                (if (= (+ at 1) (string-length str))
                    (mu:require-default-configuration)
                  (mu:require-configuration (string->symbol (substring str (+ at 1) (string-length str)))))))
          (proc target configuration)))))
  
  (define (standardize-image image)
    (cond ((memv image '(lib library)) 'library)
          ((memv image '(exe executable)) 'executable)
          (else (mu:error "Unknown image type: {s}" image))))
  
  (let ((name (symbol->string symbol)))
    (parse-target/configuration/image name proc)))


;;;
;;;; Make
;;;


(define (mu:make-symbols symbols local?)
  (define (make-symbol symbol arguments)
    (mu:parse-symbol symbol
      (lambda (target configuration image)
        (make-target target configuration image arguments local?))))
  
  (define (make-target target configuration image arguments local?)
    (case target
      ((clean) (mu:make-clean configuration))
      ((cleankernel) (mu:make-cleankernel configuration))
      ((cleanproducts) (mu:make-cleanproducts configuration))
      ((cleanobject) (mu:make-cleanobject configuration))
      ((cleanlibrary) (mu:make-cleanlibrary configuration))
      ((mu) (mu:make-mu configuration image local?))
      ((kernel) (mu:make-kernel configuration image local?))
      ((install) (mu:make-install configuration))
      (else (mu:make-product target configuration arguments))))
  
  (mu:parse-symbols symbols
    (lambda (symbols arguments)
      (let iter ((scan (if (null? symbols)
                           (list (mu:default-target))
                         symbols)))
           (if (not (null? scan))
               (let ((symbol (car scan)))
                 (and (make-symbol symbol arguments)
                      (let ((tail (cdr scan)))
                        (if (not (null? tail))
                            (newline (console-port)))
                        (iter tail)))))))))


(define (mu:make symbol)
  (mu:make-symbols (list symbol) #t))


;;;
;;;; Download
;;;


(define (mu:download-symbols symbols local?)
  (define (download-symbol symbol)
    (mu:parse-symbol symbol
      (lambda (target configuration image)
        (download-target target configuration image '() local?))))
  
  (define (download-target target configuration image arguments local?)
    (mu:download-product target configuration arguments))
  
  (mu:parse-symbols symbols
    (lambda (symbols arguments)
      (let iter ((scan (if (null? symbols)
                           (list (mu:default-target))
                         symbols)))
           (if (not (null? scan))
               (let ((symbol (car scan)))
                 (download-symbol symbol)
                 (let ((tail (cdr scan)))
                   (if (not (null? tail))
                       (newline (console-port)))
                   (iter tail))))))))


;;;
;;;; Install
;;;


(define (mu:install-symbols symbols local?)
  (define (install-symbol symbol)
    (mu:parse-symbol symbol
      (lambda (target configuration image)
        (install-target target configuration image '() local?))))
  
  (define (install-target target configuration image arguments local?)
    (mu:install-product target configuration arguments))
  
  (mu:parse-symbols symbols
    (lambda (symbols arguments)
      (let iter ((scan (if (null? symbols)
                           (list (mu:default-target))
                         symbols)))
           (if (not (null? scan))
               (let ((symbol (car scan)))
                 (install-symbol symbol)
                 (let ((tail (cdr scan)))
                   (if (not (null? tail))
                       (newline (console-port)))
                   (iter tail))))))))


;;;
;;;; Deploy
;;;


(define (mu:deploy-symbol symbol arguments)
  (mu:parse-symbol symbol
    (lambda (target configuration image)
      (mu:deploy-product target configuration arguments))))


;;;
;;;; Run
;;;


(define (mu:run-symbol symbol arguments)
  (mu:parse-symbol symbol
    (lambda (target configuration image)
      (mu:run-product target configuration arguments))))


;;;
;;;; Test
;;;


(define (mu:test-symbol symbol arguments)
  (mu:parse-symbol symbol
    (lambda (target configuration image)
      (mu:test-product target configuration arguments))))


;;;
;;;; Clean
;;;


(define (mu:clean-toplevel-file? file level)
  (define ignored-toplevel-files
    '(".gitignore"))
  
  (not (member (mu:pathname-name file) ignored-toplevel-files)))


(define (mu:clean-toplevel-directory? dir level)
  (define ignored-toplevel-dirnames
    '(".git"))
  
  (not (member (mu:pathname-name dir) ignored-toplevel-dirnames)))


(define (mu:ignored-libraries-directory configuration)
  (if (eq? (mu:get-configuration-platform configuration) 'mac)
      "Libraries"
    "lib"))


(define (mu:make-clean configuration)
  (define delete-feedback
    (mu:delete-feedback 1))
  
  (mu:feedback "make clean")
  (let ((dir (mu:configuration-directory configuration)))
    (if (file-exists? dir)
        (mu:delete-directory dir
                               0
                               mu:clean-toplevel-file?
                               (lambda (dir level)
                                 (if (mu:clean-toplevel-directory? dir level)
                                     (mu:empty-directory dir
                                                           level
                                                           #f
                                                           #f
                                                           delete-feedback)
                                   #f))
                               delete-feedback))))


(define (mu:make-cleankernel configuration)
  (define delete-feedback
    (mu:delete-feedback 1))
  
  (mu:feedback "make cleankernel")
  (let ((dir (mu:configuration-directory configuration))
        (ignored-lib (mu:ignored-libraries-directory configuration)))
    (if (file-exists? dir)
        (mu:delete-directory dir
                               0
                               mu:clean-toplevel-file?
                               (lambda (dir level)
                                 (if (and (mu:clean-toplevel-directory? dir level)
                                          (not (string=? (mu:pathname-name dir) ignored-lib)))
                                     (mu:empty-directory dir
                                                           level
                                                           #f
                                                           #f
                                                           delete-feedback)
                                   #f))
                               delete-feedback))))


(define (mu:make-cleanproducts configuration)
  (define delete-feedback
    (mu:delete-feedback 0))
  
  (mu:feedback "make cleanproducts")
  (let ((dir (mu:configuration-directory configuration)))
    (if (file-exists? dir)
        (let ((products-dir (string-append dir "build/products/")))
          (if (file-exists? products-dir)
              (mu:delete-directory products-dir
                                     0
                                     #f
                                     #f
                                     delete-feedback))))))


(define (mu:make-cleanobject configuration)
  (define delete-feedback
    (mu:delete-feedback 2))
  
  (define (object-file? file level)
    (let ((ext (mu:pathname-extension file)))
      (or (mu:extension? ext "c")
          (mu:extension? ext "cpp")
          (mu:extension? ext "dgs")
          (mu:extension? ext "mnf")
          (mu:extension? ext "otl")
          (mu:extension? ext "o")
          (mu:numeric-extension? ext "o"))))
  
  (define (empty-objects dir level)
    (mu:empty-directory dir
                          level
                          object-file?
                          empty-objects
                          delete-feedback)
    (mu:cleanup-package dir level delete-feedback))
  
  (mu:feedback "make cleanobject")
  (let ((dir (mu:configuration-directory configuration))
        (ignored-lib (mu:ignored-libraries-directory configuration)))
    (if (file-exists? dir)
        (mu:delete-directory dir
                               0
                               (lambda (file level)
                                 #f)
                               (lambda (dir level)
                                 (if (string=? (mu:pathname-name dir) ignored-lib)
                                     (mu:empty-directory dir
                                                           level
                                                           (lambda (file level)
                                                             #f)
                                                           empty-objects
                                                           delete-feedback)
                                   #f))
                               delete-feedback))))


(define (mu:make-cleanlibrary configuration)
  (define delete-feedback
    (mu:delete-feedback 2))
  
  (define (library-file? file level)
    (let ((base (mu:pathname-base file))
          (ext (mu:pathname-extension file)))
      (or (and (mu:string-ends-with? base "lmf")
               (mu:extension? ext "scm"))
          (mu:numeric-extension? ext "l"))))
  
  (define (empty-libraries dir level)
    (let ((static-dir (string-append dir "static/")))
      (if (file-exists? static-dir)
          (begin
            (mu:feedback "; deleting {a}..." static-dir)
            (mu:delete-directory static-dir))))
    (mu:empty-directory dir
                          level
                          library-file?
                          empty-libraries
                          delete-feedback)
    (mu:cleanup-package dir level delete-feedback))
  
  (mu:feedback "make cleanlibrary")
  (let ((dir (mu:configuration-directory configuration))
        (ignored-lib (mu:ignored-libraries-directory configuration)))
    (if (file-exists? dir)
        (mu:delete-directory dir
                               0
                               (lambda (file level)
                                 #f)
                               (lambda (dir level)
                                 (if (string=? (mu:pathname-name dir) ignored-lib)
                                     (mu:empty-directory dir
                                                           level
                                                           (lambda (file level)
                                                             #f)
                                                           empty-libraries
                                                           delete-feedback)
                                   #f))
                               delete-feedback))))


;; doing this as an after scan of the content seems to break on Windows (of course!)
;; when an explorer is displaying folders that will be deleted
;; we could try to do it in one scan and also verify behavior on other platforms
(define (mu:cleanup-package dir level feedback)
  (let ((content (mu:directory-content dir)))
    (case (length content)
      ((0) #t)
      ((1) (let ((name (car content)))
             (if (string=? name ".package")
                 (let ((path (string-append dir name)))
                   (feedback path level)
                   (delete-file path)
                   #t)
               #f)))
      (else #f))))


(define (mu:delete-feedback depth)
  (lambda (path level)
    (if (<= level depth)
        (mu:feedback "; deleting {a}..." path))))


;;;
;;;; Install
;;;


(define (mu:make-install configuration)
  (mu:error "Make install is not supported. See INSTALL for details"))


;;;
;;;; Mu
;;;


(define (mu:make-mu configuration image local?)
  (define (build-mu configuration image)
    (define (build configuration)
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
            (include-compiler? #t)
            (source mu:source)
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
                         source:                source
                         destination:           destination
                         destination-directory: destination-directory
                         features:              features
                         properties:            properties
                         image:                 image
                         mu?:                   #t
                         kernel?:               #f
                         console?:              #t)))
    
    (define (compare-configuration configuration-zen configuration-kernel)
      (define (compare-parameter name proc)
        (if (not (equal? (proc configuration-zen) (proc configuration-kernel)))
            (list name (proc configuration-zen) (proc configuration-kernel))
          #f))
      
      (or (compare-parameter system:              mu:get-configuration-system)
          (compare-parameter platform:            mu:get-configuration-platform)
          (compare-parameter compiler:            mu:get-configuration-compiler)
          (compare-parameter processor:           mu:get-configuration-processor)
          (compare-parameter windowing:           mu:get-configuration-windowing)
          (compare-parameter safety:              mu:get-configuration-safety)
          (compare-parameter optimize?:           mu:get-configuration-optimize?)
          (compare-parameter debug-environments?: mu:get-configuration-debug-environments?)
          (compare-parameter debug-location?:     mu:get-configuration-debug-location?)
          (compare-parameter debug-source?:       mu:get-configuration-debug-source?)
          (compare-parameter debug-foreign:       mu:get-configuration-debug-foreign?)
          (compare-parameter track-memory:        mu:get-configuration-track-memory?)
          (compare-parameter mutable-bindings:    mu:get-configuration-mutable-bindings?)
          (compare-parameter destination:         mu:get-configuration-destination)
          (compare-parameter features:            mu:get-configuration-features)
          (compare-parameter properties:          mu:get-configuration-properties)))
    
    (mu:feedback "make mu")
    (let ((configuration (or configuration (mu:require-default-configuration))))
      (let ((configuration-file (mu:configuration-file configuration)))
        (if (file-exists? configuration-file)
            (let ((file-configuration (mu:load-configuration configuration-file)))
              (let ((difference (compare-configuration configuration file-configuration)))
                (if difference
                    (mu:error "Configuration mismatch in {a}" difference)
                  (build file-configuration))))
          (build configuration)))))
  
  (build-mu configuration image))


;;;
;;;; Kernel
;;;


(define (mu:make-kernel configuration image local?)
  (define (build-kernel configuration image)
    (define (build configuration)
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
            (source mu:source)
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
                         source:                source
                         destination:           destination
                         destination-directory: destination-directory
                         features:              features
                         properties:            properties
                         image:                 image
                         kernel?:               #t
                         console?:              #t)))
    
    (define (compare-configuration configuration-zen configuration-kernel)
      (define (compare-parameter name proc)
        (if (not (equal? (proc configuration-zen) (proc configuration-kernel)))
            (list name (proc configuration-zen) (proc configuration-kernel))
          #f))
      
      (or (compare-parameter system:              mu:get-configuration-system)
          (compare-parameter platform:            mu:get-configuration-platform)
          (compare-parameter compiler:            mu:get-configuration-compiler)
          (compare-parameter processor:           mu:get-configuration-processor)
          (compare-parameter windowing:           mu:get-configuration-windowing)
          (compare-parameter safety:              mu:get-configuration-safety)
          (compare-parameter optimize?:           mu:get-configuration-optimize?)
          (compare-parameter debug-environments?: mu:get-configuration-debug-environments?)
          (compare-parameter debug-location?:     mu:get-configuration-debug-location?)
          (compare-parameter debug-source?:       mu:get-configuration-debug-source?)
          (compare-parameter debug-foreign:       mu:get-configuration-debug-foreign?)
          (compare-parameter track-memory:        mu:get-configuration-track-memory?)
          (compare-parameter mutable-bindings:    mu:get-configuration-mutable-bindings?)
          (compare-parameter destination:         mu:get-configuration-destination)
          (compare-parameter features:            mu:get-configuration-features)
          (compare-parameter properties:          mu:get-configuration-properties)))
    
    (mu:feedback "make kernel")
    (let ((configuration (or configuration (mu:require-default-configuration))))
      (let ((configuration-file (mu:configuration-file configuration)))
        (if (file-exists? configuration-file)
            (let ((file-configuration (mu:load-configuration configuration-file)))
              (let ((difference (compare-configuration configuration file-configuration)))
                (if difference
                    (mu:error "Configuration mismatch in {a}" difference)
                  (build file-configuration))))
          (build configuration)))))
  
  (define (build-recursive target configuration image)
    (let ((configuration-name (mu:get-configuration-name configuration)))
      (let ((mu (string-append mu:source "mu"))
            (argument (string-append (if configuration-name
                                         (mu:format "{a}@{a}" target configuration-name)
                                       (symbol->string target))
                                     (if image
                                         (string-append ":" (symbol->string image))
                                       ""))))
        (mu:call-process (list path: "sh" arguments: `(,mu "make" ,argument))))))
  
  (if local?
      (build-kernel configuration image)
    (build-recursive 'kernel configuration image)))


;;;
;;;; Product
;;;


(define (mu:make-product product configuration arguments)
  (mu:make-kernel configuration #f #f)
  (= 0 (mu:invoke-process
         (list
           path: (string-append (mu:configuration-directory configuration) "zen")
           arguments: `("-:daD"
                        "-make"
                        ,(symbol->string product)
                        ,@arguments)))))


(define (mu:download-product product configuration arguments)
  (mu:call-process
     (list
       path: (string-append (mu:configuration-directory configuration) "zen")
       arguments: `("-download"
                    ,(symbol->string product)
                    ,@arguments))))


(define (mu:install-product product configuration arguments)
  (mu:call-process
     (list
       path: (string-append (mu:configuration-directory configuration) "zen")
       arguments: `("-install"
                    ,(symbol->string product)
                    ,@arguments))))


(define (mu:deploy-product product configuration arguments)
  (mu:call-process
     (list
       path: (string-append (mu:configuration-directory configuration) "zen")
       arguments: `("-deploy"
                    ,(symbol->string product)
                    ,@arguments))))


(define (mu:run-product product configuration arguments)
  (mu:call-process
     (list
       path: (string-append (mu:configuration-directory configuration) "zen")
       arguments: `("-run"
                    ,(symbol->string product)
                    ,@arguments))))


(define (mu:test-product product configuration arguments)
  (mu:call-process
     (list
       path: (string-append (mu:configuration-directory configuration) "zen")
       arguments: `("-test"
                    ,(symbol->string product)
                    ,@arguments))))


;;;
;;;; Boot
;;;


(define (mu:build-system-boot)
  (define (fatal message)
    (display message)
    (newline)
    (force-output)
    (exit 1))
  
  (define (missing-argument-for-option opt)
    (fatal (mu:format "Missing argument for option: {a}" opt)))
  
  (define (read-argument arg)
    (call-with-input-string (list init: arg)
      read))
  
  (define (string-option name options)
    (let ((opt (mu:find-option name options)))
      (if (not opt)
          (mu:unspecified-option)
        opt)))
  
  (define (symbol-option name options)
    (let ((opt (mu:find-option name options)))
      (if (not opt)
          (mu:unspecified-option)
        (string->symbol opt))))
  
  (define (boolean-option name options)
    (let ((opt (mu:find-option name options)))
      (cond ((not opt)
             (mu:unspecified-option))
            ((string-ci=? opt "false")
             #f)
            ((string-ci=? opt "true")
             #t)
            (else
             (fatal (mu:format "Invalid boolean argument for option: {a}" name))))))
  
  (define (list-option name options)
    (let ((opt (mu:find-option name options)))
      (if (not opt)
          (mu:unspecified-option)
        opt)))
  
  (define (unknown-option opt)
    (fatal (mu:format "Unknown option: {a}" opt)))
  
  (let ((command-arguments (command-args)));; tanzoo
    (if (null? command-arguments)
        (set! command-arguments '("help")))
    (let ((action (car command-arguments))
          (arguments (cdr command-arguments)))
      ;; tanzoo (mu:setup-settings)
      (mu:load-configurations)
      ;; tanzoo (mu:process-muini)
      (cond ((equal? action "clone")
             (mu:clone-dependencies (map read-argument arguments))
             (exit))
            ((equal? action "list")
             (mu:list-configurations)
             (exit))
            ((equal? action "configure")
             (let ()
               (define (configure template arguments)
                 (mu:split-command-line arguments '() '("name" "system" "platform" "compiler" "processor" "windowing" "safety" "optimize" "debug-environments" "debug-location" "debug-source" "debug-foreign" "track-memory" "mutable-bindings" "destination" "features" "properties") missing-argument-for-option
                   (lambda (commands options remaining)
                     (if (null? remaining)
                         (let ((name (symbol-option "name" options))
                               (system (symbol-option "system" options))
                               (platform (symbol-option "platform" options))
                               (compiler (symbol-option "compiler" options))
                               (processor (symbol-option "processor" options))
                               (windowing (symbol-option "windowing" options))
                               (safety (symbol-option "safety" options))
                               (optimize (boolean-option "optimize" options))
                               (debug-environments (boolean-option "debug-environments" options))
                               (debug-location (boolean-option "debug-location" options))
                               (debug-source (boolean-option "debug-source" options))
                               (debug-foreign (boolean-option "debug-foreign" options))
                               (track-memory (boolean-option "track-memory" options))
                               (mutable-bindings (boolean-option "mutable-bindings" options))
                               (destination (string-option "destination" options))
                               (features (list-option "features" options))
                               (properties (list-option "properties" options)))
                           (mu:configure template name: name system: system platform: platform compiler: compiler processor: processor windowing: windowing safety: safety optimize?: optimize debug-environments?: debug-environments debug-location?: debug-location debug-source?: debug-source debug-foreign?: debug-foreign track-memory?: track-memory mutable-bindings?: mutable-bindings destination: destination features: features properties: properties)
                           (exit))
                       (unknown-option (car remaining))))))
               
               (if (and (pair? arguments)
                        (not (mu:option? (car arguments))))
                   (let ((template (mu:configuration-template (string->symbol (car arguments)))))
                     (configure template (cdr arguments)))
                 (configure mu:unspecified-configuration arguments))))
            ((equal? action "make")
             (if (mu:make-symbols (map read-argument arguments) #t)
                 (exit 0)
               (exit 1)))
            ((equal? action "download")
             (mu:download-symbols (map read-argument arguments) #t)
             (exit))
            ((equal? action "install")
             (mu:install-symbols (map read-argument arguments) #t)
             (exit))
            ((equal? action "deploy")
             (if (null? arguments)
                 (mu:deploy-symbol (mu:default-target) '())
               (mu:deploy-symbol (read-argument (car arguments)) (cdr arguments))))
            ((equal? action "run")
             (mu:run-symbol (read-argument (car arguments)) (cdr arguments))
             (exit))
            ((equal? action "test")
             (mu:test-symbol (read-argument (car arguments)) (cdr arguments))
             (exit))
            ((or (equal? action "help") (equal? action "?"))
             (let ((console (console-port)))
               (mu:print "Usage:" console)
               (mu:print "  mu configure [-name] [-system] [-platform] [-compiler] [-processor] [-windowing] [-safety] [-optimize] [-debug-environments] [-debug-location] [-debug-source] [-destination] [-features] [-properties]" console)
               (mu:print "  mu make [target | clean | cleankernel | cleanproducts | cleanobject | cleanlibrary]@[configuration]:[image]" console)
               (mu:print "  mu download" console)
               (mu:print "  mu install" console)
               (mu:print "  mu deploy" console)
               (mu:print "  mu run" console)
               (mu:print "  mu test" console)
               (mu:print "  mu list" console)
               (mu:print "  mu help or ?" console)
               (mu:print "  mu debug" console))
             (exit))
            ((equal? action "debug")
             (##repl-debug-main))
            (else
             (fatal (mu:format "Unknown build system action: {a}" action)))))))


;;;
;;;; Templates
;;;


(define mu:unspecified-configuration
  (mu:new-configuration
    name: (mu:unspecified-option)
    system: (mu:unspecified-option)
    platform: (mu:unspecified-option)
    compiler: (mu:unspecified-option)
    processor: (mu:unspecified-option)
    windowing: (mu:unspecified-option)
    safety: (mu:unspecified-option)
    optimize?: (mu:unspecified-option)
    debug-environments?: (mu:unspecified-option)
    debug-location?: (mu:unspecified-option)
    debug-source?: (mu:unspecified-option)
    debug-foreign?: (mu:unspecified-option)
    track-memory?: (mu:unspecified-option)
    mutable-bindings?: (mu:unspecified-option)
    destination: (mu:unspecified-option)
    features: (mu:unspecified-option)
    properties: (mu:unspecified-option)))

(define mu:core-configuration
  (mu:new-configuration
    name: 'c
    system: (mu:unspecified-option)
    platform: (mu:unspecified-option)
    compiler: (mu:unspecified-option)
    processor: (mu:unspecified-option)
    windowing: (mu:unspecified-option)
    safety: 'core
    optimize?: #f
    debug-environments?: #t
    debug-location?: #t
    debug-source?: #f
    debug-foreign?: #f
    track-memory?: (mu:unspecified-option)
    mutable-bindings?: (mu:unspecified-option)
    destination: "build/core"
    features: (mu:unspecified-option)
    properties: (mu:unspecified-option)))

(define mu:debug-configuration
  (mu:new-configuration
    name: 'd
    system: (mu:unspecified-option)
    platform: (mu:unspecified-option)
    compiler: (mu:unspecified-option)
    processor: (mu:unspecified-option)
    windowing: (mu:unspecified-option)
    safety: 'debug
    optimize?: #f
    debug-environments?: #t
    debug-location?: #t
    debug-source?: #f
    debug-foreign?: #f
    track-memory?: (mu:unspecified-option)
    mutable-bindings?: (mu:unspecified-option)
    destination: "build/debug"
    features: (mu:unspecified-option)
    properties: (mu:unspecified-option)))

(define mu:develop-configuration
  (mu:new-configuration
    name: 'l
    system: (mu:unspecified-option)
    platform: (mu:unspecified-option)
    compiler: (mu:unspecified-option)
    processor: (mu:unspecified-option)
    windowing: (mu:unspecified-option)
    safety: 'develop
    optimize?: #f
    debug-environments?: #t
    debug-location?: #t
    debug-source?: #f
    debug-foreign?: #f
    track-memory?: (mu:unspecified-option)
    mutable-bindings?: (mu:unspecified-option)
    destination: "build/develop"
    features: (mu:unspecified-option)
    properties: (mu:unspecified-option)))

(define mu:release-configuration
  (mu:new-configuration
    name: 'r
    system: (mu:unspecified-option)
    platform: (mu:unspecified-option)
    compiler: (mu:unspecified-option)
    processor: (mu:unspecified-option)
    windowing: (mu:unspecified-option)
    safety: 'release
    optimize?: #f
    debug-environments?: #t
    debug-location?: #t
    debug-source?: #f
    debug-foreign?: #f
    track-memory?: (mu:unspecified-option)
    mutable-bindings?: (mu:unspecified-option)
    destination: "build/release"
    features: (mu:unspecified-option)
    properties: (mu:unspecified-option)))

(define mu:sealed-configuration
  (mu:new-configuration
    name: 's
    system: (mu:unspecified-option)
    platform: (mu:unspecified-option)
    compiler: (mu:unspecified-option)
    processor: (mu:unspecified-option)
    windowing: (mu:unspecified-option)
    safety: 'sealed
    optimize?: #t
    debug-environments?: #f
    debug-location?: #f
    debug-source?: #f
    debug-foreign?: #f
    track-memory?: (mu:unspecified-option)
    mutable-bindings?: (mu:unspecified-option)
    destination: "build/sealed"
    features: (mu:unspecified-option)
    properties: (mu:unspecified-option)))


(define mu:configuration-templates
  `((c . ,mu:core-configuration)
    (d . ,mu:debug-configuration)
    (l . ,mu:develop-configuration)
    (r . ,mu:release-configuration)
    (s . ,mu:sealed-configuration)))


(define (mu:configuration-template name)
  (let ((pair (assq name mu:configuration-templates)))
    (if pair
        (cdr pair)
      (mu:error "Unknown configuration template: {s}" name))))
