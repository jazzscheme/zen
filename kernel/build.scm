;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Build
;;;


;;;
;;;; Dependencies
;;;


(define zen:all-dependencies
  #f)


(define (zen:get-dependencies)
  (or zen:all-dependencies
      (let ((dependencies (let ((file ".dependencies"))
                            (if (file-exists? file)
                                (call-with-input-file file (lambda (port) (read-all port read)))
                              '()))))
        (set! zen:all-dependencies dependencies)
        dependencies)))


(define (zen:find-dependency name)
  (zen:find-if (lambda (dep)
                  (eq? (zen:dependency-name dep) name))
                (zen:get-dependencies)))


(define (zen:require-dependency name)
  (or (zen:find-dependency name)
      (error "Unknown dependency" name)))


(define (zen:dependency-name dep)
  (car dep))

(define (zen:dependency-directory dep)
  (cadr dep))

(define (zen:dependency-url dep)
  (caddr dep))


(define (zen:clone-dependencies names)
  (define (clone-dependency name)
    (let ((dep (zen:require-dependency name)))
      (zen:invoke-process
         (list
           path: "git"
           arguments: `("clone" ,(zen:dependency-url dep) ,(zen:dependency-directory dep))))))
  
  (for-each clone-dependency names))


;;;
;;;; Versions
;;;


(define (zen:setup-versions)
  (set! zen:zen-versions-file (string-append zen:source "kernel/versions")))


;;;
;;;; Option
;;;


(define zen:unspecified-option-value
  (list 'unspecified-option))


(define (zen:unspecified-option)
  zen:unspecified-option-value)


(define (zen:specified-option? option)
  (not (eq? option zen:unspecified-option-value)))


(define (zen:or-option . rest)
  (let iter ((scan rest))
    (if (null? scan)
        default
      (let ((option (car scan)))
        (if (zen:specified-option? option)
            option
          (iter (cdr scan)))))))


;;;
;;;; Options
;;;


(define-macro (zen:define-option name default)
  (let ((global (gensym name)))
    `(begin
       (define ,global ,default)
       (define (,name . rest)
         (if (null? rest)
             ,global
           (set! ,global (car rest)))))))


(zen:define-option zen:default-name
  #f)

(zen:define-option zen:default-system
  'gambit)

(zen:define-option zen:default-platform
  (zen:unspecified-option))

(zen:define-option zen:default-compiler
  (zen:unspecified-option))

(zen:define-option zen:default-processor
  (zen:unspecified-option))

(zen:define-option zen:default-windowing
  (zen:unspecified-option))

(zen:define-option zen:default-safety
  'release)

(zen:define-option zen:default-optimize?
  #t)

(zen:define-option zen:default-debug-environments?
  #t)

(zen:define-option zen:default-debug-location?
  #t)

(zen:define-option zen:default-debug-source?
  #f)

(zen:define-option zen:default-debug-foreign?
  #f)

(zen:define-option zen:default-track-memory?
  #f)

(zen:define-option zen:default-mutable-bindings?
  #f)

(zen:define-option zen:default-kernel-interpret?
  #f)

(zen:define-option zen:default-destination
  #f)

(zen:define-option zen:default-features
  '())

(zen:define-option zen:default-properties
  '())


(zen:define-option zen:default-target
  'all)


;;;
;;;; Configuration
;;;


(define (zen:new-configuration
          #!key
          (name (zen:unspecified-option))
          (system (zen:unspecified-option))
          (platform (zen:unspecified-option))
          (compiler (zen:unspecified-option))
          (processor (zen:unspecified-option))
          (windowing (zen:unspecified-option))
          (safety (zen:unspecified-option))
          (optimize? (zen:unspecified-option))
          (debug-environments? (zen:unspecified-option))
          (debug-location? (zen:unspecified-option))
          (debug-source? (zen:unspecified-option))
          (debug-foreign? (zen:unspecified-option))
          (track-memory? (zen:unspecified-option))
          (mutable-bindings? (zen:unspecified-option))
          (kernel-interpret? (zen:unspecified-option))
          (destination (zen:unspecified-option))
          (features (zen:unspecified-option))
          (properties (zen:unspecified-option))
          (local? #f))
  (zen:make-configuration
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
    kernel-interpret?
    destination
    features
    properties
    local?))


(define (zen:validate-configuration
          template
          #!key
          (name (zen:unspecified-option))
          (system (zen:unspecified-option))
          (platform (zen:unspecified-option))
          (compiler (zen:unspecified-option))
          (processor (zen:unspecified-option))
          (windowing (zen:unspecified-option))
          (safety (zen:unspecified-option))
          (optimize? (zen:unspecified-option))
          (debug-environments? (zen:unspecified-option))
          (debug-location? (zen:unspecified-option))
          (debug-source? (zen:unspecified-option))
          (debug-foreign? (zen:unspecified-option))
          (track-memory? (zen:unspecified-option))
          (mutable-bindings? (zen:unspecified-option))
          (kernel-interpret? (zen:unspecified-option))
          (destination (zen:unspecified-option))
          (features (zen:unspecified-option))
          (properties (zen:unspecified-option))
          (local? #f))
  (let* ((name (zen:validate-name (zen:require-name name template)))
         (system (zen:validate-system (zen:require-system system template)))
         (platform (zen:validate-platform (zen:require-platform platform template)))
         (compiler (zen:validate-compiler (zen:require-compiler platform compiler template)))
         (processor (zen:validate-processor (zen:require-processor platform processor template)))
         (windowing (zen:validate-windowing (zen:require-windowing platform windowing template)))
         (safety (zen:validate-safety (zen:require-safety safety template)))
         (optimize? (zen:validate-optimize? (zen:require-optimize? optimize? template)))
         (debug-environments? (zen:validate-debug-environments? (zen:require-debug-environments? debug-environments? template)))
         (debug-location? (zen:validate-debug-location? (zen:require-debug-location? debug-location? template)))
         (debug-source? (zen:validate-debug-source? (zen:require-debug-source? debug-source? template)))
         (debug-foreign? (zen:validate-debug-foreign? (zen:require-debug-foreign? debug-foreign? template)))
         (track-memory? (zen:validate-track-memory? (zen:require-track-memory? track-memory? template)))
         (mutable-bindings? (zen:validate-mutable-bindings? (zen:require-mutable-bindings? mutable-bindings? template)))
         (kernel-interpret? (zen:validate-kernel-interpret? (zen:require-kernel-interpret? kernel-interpret? template)))
         (destination (zen:validate-destination (zen:require-destination destination template)))
         (features (zen:validate-features (zen:require-features features template)))
         (properties (zen:validate-properties (zen:require-properties properties template))))
    (zen:make-configuration
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
      kernel-interpret?
      destination
      features
      properties
      local?)))


;;;
;;;; Configurations
;;;


(define zen:anonymous-configuration-file
  "./.configuration")

(define zen:configurations
  '())


(define (zen:list-configurations)
  (for-each zen:describe-configuration (zen:sort-configurations zen:configurations)))


(define (zen:require-configuration name)
  (or (zen:find-configuration name)
      (if (not name)
          (zen:error "Unable to find default configuration")
        (zen:error "Unable to find configuration: {s}" name))))

(define (zen:require-default-configuration)
  (or (let ((muconf (getenv "MUCONF" #f)))
        (if muconf
            (zen:find-configuration (string->symbol muconf))
          (zen:find-configuration #f)))
      (begin
        (zen:feedback "configure")
        (zen:configure zen:unspecified-configuration)
        (zen:require-configuration #f))))


(define (zen:find-configuration name)
  (let ((configuration
          (let ((pair (zen:find-configuration-pair name)))
            (if (not pair)
                #f
              (car pair)))))
    ;; special case to support make of binaries
    (if (and (not name) (not configuration))
        (let ((configuration-dir (zen:destination-directory #f "bin:" "./")))
          (let ((configuration-file (string-append configuration-dir ".configuration")))
            (if (file-exists? configuration-file)
                (zen:load-configuration configuration-file)
              #f)))
      configuration)))

(define (zen:find-configuration-pair name)
  (let iter ((configurations zen:configurations))
    (if (null? configurations)
        #f
      (let ((configuration (car configurations)))
        (if (eq? (zen:get-configuration-name configuration) name)
            configurations
          (iter (cdr configurations)))))))


(define (zen:sort-configurations configurations)
  (zen:sort-list (lambda (c1 c2)
                    (let ((n1 (zen:get-configuration-name c1))
                          (n2 (zen:get-configuration-name c2)))
                      (cond ((not n1)
                             #t)
                            ((not n2)
                             #f)
                            (else
                             (string-ci<? (symbol->string n1) (symbol->string n2))))))
                  configurations))


(define (zen:register-configuration configuration)
  (let ((name (zen:get-configuration-name configuration)))
    (let ((pair (zen:find-configuration-pair name)))
      (if pair
          (set-car! pair configuration)
        (set! zen:configurations (append zen:configurations (list configuration))))))
  (zen:save-configurations))


(define (zen:delete-configuration name)
  (set! zen:configurations
        (zen:delete name zen:configurations
          (lambda (c1 c2)
            (eq? (zen:get-configuration-name c1)
                 (zen:get-configuration-name c2)))))
  (zen:save-configurations))


(define (zen:load-configurations)
  (if (file-exists? zen:named-configurations-file)
      (call-with-input-file (list path: zen:named-configurations-file eol-encoding: 'cr-lf)
        (lambda (input)
          (define (read-configuration input)
            (let ((list (read input)))
              (if (eof-object? list)
                  list
                (apply zen:validate-configuration zen:unspecified-configuration list))))
          
          (set! zen:configurations (read-all input read-configuration)))))
  (if (file-exists? zen:local-configurations-file)
      (call-with-input-file (list path: zen:local-configurations-file eol-encoding: 'cr-lf)
        (lambda (input)
          (define (read-configuration input)
            (let ((list (read input)))
              (if (eof-object? list)
                  list
                (apply zen:validate-configuration zen:unspecified-configuration local?: #t list))))
          
          (for-each zen:register-configuration (read-all input read-configuration)))))
  (if (file-exists? zen:anonymous-configuration-file)
      (zen:register-configuration (zen:load-configuration zen:anonymous-configuration-file))))


(define (zen:load-configuration file)
  (define (parse-properties data)
    (if (and (pair? data)
             (eq? (car data) 'configuration))
        (caddr data)
      data))
  
  (call-with-input-file (list path: file eol-encoding: 'cr-lf)
    (lambda (input)
      (let ((data (read input)))
        ;; quicky until a full-fledged zen:versioned-file similar to zen:versioned-directory
        ;; removing zen:convert-configuration-205001 buggy for absolute path ("c:/" -> "c//")
        (apply zen:validate-configuration zen:unspecified-configuration (parse-properties data))))))


(define (zen:save-configurations)
  (define (split-configurations configurations)
    (let split ((configurations configurations) (anonymous #f) (local '()) (named '()))
         (if (null? configurations)
             (values anonymous local named)
           (let ((configuration (car configurations)))
             (cond ((not (zen:get-configuration-name configuration))
                    (split (cdr configurations) configuration local named))
                   ((zen:get-configuration-local? configuration)
                    (split (cdr configurations) anonymous (cons configuration local) named))
                   (else
                    (split (cdr configurations) anonymous local (cons configuration named))))))))
  
  (define (save-configuration configuration file system-platform)
    (zen:save-configuration
      (zen:get-configuration-name configuration)
      (zen:get-configuration-system configuration)
      (zen:get-configuration-platform configuration)
      (zen:get-configuration-compiler configuration)
      (zen:get-configuration-processor configuration)
      (zen:get-configuration-windowing configuration)
      (zen:get-configuration-safety configuration)
      (zen:get-configuration-optimize? configuration)
      (zen:get-configuration-debug-environments? configuration)
      (zen:get-configuration-debug-location? configuration)
      (zen:get-configuration-debug-source? configuration)
      (zen:get-configuration-debug-foreign? configuration)
      (zen:get-configuration-track-memory? configuration)
      (zen:get-configuration-mutable-bindings? configuration)
      (zen:get-configuration-kernel-interpret? configuration)
      (zen:get-configuration-destination configuration)
      (zen:get-configuration-features configuration)
      (zen:get-configuration-properties configuration)
      file
      system-platform))
  
  (define (print-configuration configuration output)
    (zen:print-configuration
      (zen:get-configuration-name configuration)
      (zen:get-configuration-system configuration)
      (zen:get-configuration-platform configuration)
      (zen:get-configuration-compiler configuration)
      (zen:get-configuration-processor configuration)
      (zen:get-configuration-windowing configuration)
      (zen:get-configuration-safety configuration)
      (zen:get-configuration-optimize? configuration)
      (zen:get-configuration-debug-environments? configuration)
      (zen:get-configuration-debug-location? configuration)
      (zen:get-configuration-debug-source? configuration)
      (zen:get-configuration-debug-foreign? configuration)
      (zen:get-configuration-track-memory? configuration)
      (zen:get-configuration-mutable-bindings? configuration)
      (zen:get-configuration-kernel-interpret? configuration)
      (zen:get-configuration-destination configuration)
      (zen:get-configuration-features configuration)
      (zen:get-configuration-properties configuration)
      output))
  
  (receive (anonymous local named) (split-configurations zen:configurations)
    (if anonymous
        (save-configuration anonymous zen:anonymous-configuration-file (zen:guess-platform))
      (if (file-exists? zen:anonymous-configuration-file)
          (delete-file zen:anonymous-configuration-file)))
    (let ((configurations (zen:sort-configurations named)))
      (if (not (null? configurations))
          (let ((path zen:named-configurations-file))
            (zen:create-directories (zen:pathname-dir path) feedback: zen:feedback)
            (call-with-output-file (list path: path eol-encoding: (zen:platform-eol-encoding (zen:guess-platform)))
              (lambda (output)
                (for-each (lambda (configuration)
                            (print-configuration configuration output)
                            (newline output))
                          configurations))))))))


(define (zen:describe-configuration configuration)
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
        (kernel-interpret? (zen:get-configuration-kernel-interpret? configuration))
        (destination (zen:get-configuration-destination configuration))
        (features (zen:get-configuration-features configuration))
        (properties (zen:get-configuration-properties configuration)))
    (zen:feedback "{a}" (or name "<default>"))
    (zen:feedback "  system: {s}" system)
    (zen:feedback "  platform: {s}" platform)
    (zen:feedback "  compiler: {s}" compiler)
    (zen:feedback "  processor: {s}" processor)
    (zen:feedback "  windowing: {s}" windowing)
    (zen:feedback "  safety: {s}" safety)
    (zen:feedback "  optimize?: {s}" optimize?)
    (zen:feedback "  debug-environments?: {s}" debug-environments?)
    (zen:feedback "  debug-location?: {s}" debug-location?)
    (zen:feedback "  debug-source?: {s}" debug-source?)
    (zen:feedback "  debug-foreign?: {s}" debug-foreign?)
    (zen:feedback "  track-memory?: {s}" track-memory?)
    (zen:feedback "  mutable-bindings?: {s}" mutable-bindings?)
    (zen:feedback "  kernel-interpret?: {s}" kernel-interpret?)
    (zen:feedback "  destination: {s}" destination)
    (zen:feedback "  features: {s}" features)
    (zen:feedback "  properties: {s}" properties)))


;;;
;;;; Configure
;;;


(define (zen:configure
          template
          #!key
          (name (zen:unspecified-option))
          (system (zen:unspecified-option))
          (platform (zen:unspecified-option))
          (compiler (zen:unspecified-option))
          (processor (zen:unspecified-option))
          (windowing (zen:unspecified-option))
          (safety (zen:unspecified-option))
          (optimize? (zen:unspecified-option))
          (debug-environments? (zen:unspecified-option))
          (debug-location? (zen:unspecified-option))
          (debug-source? (zen:unspecified-option))
          (debug-foreign? (zen:unspecified-option))
          (track-memory? (zen:unspecified-option))
          (mutable-bindings? (zen:unspecified-option))
          (kernel-interpret? (zen:unspecified-option))
          (destination (zen:unspecified-option))
          (features (zen:unspecified-option))
          (properties (zen:unspecified-option)))
  (let ((configuration
          (zen:validate-configuration
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
            kernel-interpret?: kernel-interpret?
            destination: destination
            features: features
            properties: properties)))
    (zen:register-configuration configuration)
    (zen:describe-configuration configuration)))


;;;
;;;; Name
;;;


(define (zen:require-name name template)
  (zen:or-option name (zen:get-configuration-name template) (zen:default-name)))


(define (zen:validate-name name)
  (if (or (not name) (and (symbol? name) (zen:valid-filename? (symbol->string name))))
      name
    (zen:error "Invalid name: {s}" name)))


;;;
;;;; System
;;;


(define zen:valid-systems
  '(gambit))


(define (zen:require-system system template)
  (zen:or-option system (zen:get-configuration-system template) (zen:default-system)))


(define (zen:validate-system system)
  (if (memq system zen:valid-systems)
      system
    (zen:error "Invalid system: {s}" system)))


;;;
;;;; Platform
;;;


(define zen:valid-platforms
  '(mac
    ios
    windows
    linux))


(define (zen:guess-platform)
  (let ((system (cadr (system-type)))
        (os (caddr (system-type))))
    (cond ((eq? system 'apple)
           'mac)
          ((zen:linux-family)
           'linux)
          (else
           'windows))))


(define (zen:require-platform platform template)
  (zen:or-option platform (zen:get-configuration-platform template) (zen:default-platform) (zen:guess-platform)))


(define (zen:validate-platform platform)
  (if (memq platform zen:valid-platforms)
      platform
    (zen:error "Invalid platform: {s}" platform)))


;;;
;;;; Compiler
;;;


(define zen:valid-compilers
  '(c
    c++))


(define (zen:guess-compiler platform)
  ;; at the moment only the C++ compiler with its exceptions
  ;; plays well with visual C's structured exception handling
  (if (eq? platform 'windows)
      'c++
    'c))


(define (zen:require-compiler platform compiler template)
  (zen:or-option compiler (zen:get-configuration-compiler template) (zen:default-compiler) (zen:guess-compiler platform)))


(define (zen:validate-compiler compiler)
  (if (memq compiler zen:valid-compilers)
      compiler
    (zen:error "Invalid compiler: {s}" compiler)))


;;;
;;;; Processor
;;;


(define zen:valid-processors
  '(#f
    x86
    arm
    silicon))


(define (zen:guess-processor platform)
  (if (eq? platform 'mac)
      (let ((processor (car (system-type))))
        (if (eq? processor 'aarch64)
            'silicon
          #f))
    #f))


(define (zen:require-processor platform processor template)
  (zen:or-option processor (zen:get-configuration-processor template) (zen:default-processor) (zen:guess-processor platform)))


(define (zen:validate-processor processor)
  (if (memq processor zen:valid-processors)
      processor
    (zen:error "Invalid processor: {s}" processor)))


;;;
;;;; Windowing
;;;


(define zen:valid-windowings
  '(cocoa
    #f
    x11))


(define (zen:require-windowing platform windowing template)
  (define (guess-windowing platform)
    (case platform
      ((mac) 'cocoa)
      ((windows) #f)
      ((linux) 'x11)))
  
  (zen:or-option windowing (zen:get-configuration-windowing template) (zen:default-windowing) (guess-windowing platform)))


(define (zen:validate-windowing windowing)
  (if (memq windowing zen:valid-windowings)
      windowing
    (zen:error "Invalid windowing: {s}" windowing)))


;;;
;;;; Safety
;;;


(define zen:valid-safeties
  '(core
    debug
    develop
    release
    sealed))


(define (zen:require-safety safety template)
  (zen:or-option safety (zen:get-configuration-safety template) (zen:default-safety)))


(define (zen:validate-safety safety)
  (if (memq safety zen:valid-safeties)
      safety
    (zen:error "Invalid safety: {s}" safety)))


;;;
;;;; Optimize
;;;


(define zen:valid-optimize
  '(#f
    #t))


(define (zen:require-optimize? optimize template)
  (zen:or-option optimize (zen:get-configuration-optimize? template) (zen:default-optimize?)))


(define (zen:validate-optimize? optimize)
  (if (memq optimize zen:valid-optimize)
      optimize
    (zen:error "Invalid optimize?: {s}" optimize)))


;;;
;;;; Debug-Environments
;;;


(define zen:valid-debug-environments
  '(#f
    #t))


(define (zen:require-debug-environments? debug-environments template)
  (zen:or-option debug-environments (zen:get-configuration-debug-environments? template) (zen:default-debug-environments?)))


(define (zen:validate-debug-environments? debug-environments)
  (if (memq debug-environments zen:valid-debug-environments)
      debug-environments
    (zen:error "Invalid debug-environments?: {s}" debug-environments)))


;;;
;;;; Debug-Location
;;;


(define zen:valid-debug-location
  '(#f
    #t))


(define (zen:require-debug-location? debug-location template)
  (zen:or-option debug-location (zen:get-configuration-debug-location? template) (zen:default-debug-location?)))


(define (zen:validate-debug-location? debug-location)
  (if (memq debug-location zen:valid-debug-location)
      debug-location
    (zen:error "Invalid debug-location?: {s}" debug-location)))


;;;
;;;; Debug-Source
;;;


(define zen:valid-debug-source
  '(#f
    #t))


(define (zen:require-debug-source? debug-source template)
  (zen:or-option debug-source (zen:get-configuration-debug-source? template) (zen:default-debug-source?)))


(define (zen:validate-debug-source? debug-source)
  (if (memq debug-source zen:valid-debug-source)
      debug-source
    (zen:error "Invalid debug-source?: {s}" debug-source)))


;;;
;;;; Debug-Foreign
;;;


(define zen:valid-debug-foreign
  '(#f
    #t))


(define (zen:require-debug-foreign? debug-foreign template)
  (zen:or-option debug-foreign (zen:get-configuration-debug-foreign? template) (zen:default-debug-foreign?)))


(define (zen:validate-debug-foreign? debug-foreign)
  (if (memq debug-foreign zen:valid-debug-foreign)
      debug-foreign
    (zen:error "Invalid debug-foreign?: {s}" debug-foreign)))


;;;
;;;; Track-Memory
;;;


(define zen:valid-track-memory
  '(#f
    #t))


(define (zen:require-track-memory? track-memory template)
  (zen:or-option track-memory (zen:get-configuration-track-memory? template) (zen:default-track-memory?)))


(define (zen:validate-track-memory? track-memory)
  (if (memq track-memory zen:valid-track-memory)
      track-memory
    (zen:error "Invalid track-memory?: {s}" track-memory)))


;;;
;;;; Mutable-Bindings
;;;


(define zen:valid-mutable-bindings
  '(#f
    #t))


(define (zen:require-mutable-bindings? mutable-bindings template)
  (zen:or-option mutable-bindings (zen:get-configuration-mutable-bindings? template) (zen:default-mutable-bindings?)))


(define (zen:validate-mutable-bindings? mutable-bindings)
  (if (memq mutable-bindings zen:valid-mutable-bindings)
      mutable-bindings
    (zen:error "Invalid mutable-bindings?: {s}" mutable-bindings)))


;;;
;;;; Kernel-Interpret
;;;


(define zen:valid-kernel-interpret
  '(#f
    #t))


(define (zen:require-kernel-interpret? kernel-interpret template)
  (zen:or-option kernel-interpret (zen:get-configuration-kernel-interpret? template) (zen:default-kernel-interpret?)))


(define (zen:validate-kernel-interpret? kernel-interpret)
  (if (memq kernel-interpret zen:valid-kernel-interpret)
      kernel-interpret
    (zen:error "Invalid kernel-interpret?: {s}" kernel-interpret)))


;;;
;;;; Destination
;;;


(define (zen:require-destination destination template)
  (zen:or-option destination (zen:get-configuration-destination template) (zen:default-destination)))


(define (zen:validate-destination destination)
  (if (or (not destination) (string? destination))
      destination
    (zen:error "Invalid destination: {s}" destination)))


(define (zen:configuration-file configuration)
  (let ((dir (zen:configuration-directory configuration)))
    (string-append dir ".configuration")))


;;;
;;;; Features
;;;


(define (zen:require-features features template)
  (zen:or-option features (zen:get-configuration-features template) (zen:default-features)))


(define (zen:validate-features features)
  (if (list? features)
      features
    (zen:error "Invalid features: {s}" features)))


;;;
;;;; Properties
;;;


(define (zen:require-properties properties template)
  (zen:or-option properties (zen:get-configuration-properties template) (zen:default-properties)))


(define (zen:validate-properties properties)
  (if (list? properties)
      properties
    (zen:error "Invalid properties: {s}" properties)))


;;;
;;;; Symbol
;;;


(define (zen:parse-symbols lst proc)
  (let iter ((scan lst)
             (symbols '()))
       (if (null? scan)
           (proc (reverse symbols) '())
         (let ((obj (car scan)))
           (if (or (keyword? obj) (and (symbol? obj) (eqv? #\- (string-ref (symbol->string obj) 0))))
               (proc (reverse symbols) (map zen:stringify scan))
             (iter (cdr scan) (cons obj symbols)))))))


(define (zen:parse-symbol symbol proc)
  (define (parse-target/configuration/image str proc)
    (let ((colon (zen:string-find str #\:)))
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
    (let ((at (zen:string-find str #\@)))
      (if (not at)
          (proc (if (string=? str "") (zen:default-target) (string->symbol str)) (zen:require-default-configuration))
        (let ((target
                (if (= at 0)
                    (zen:default-target)
                  (string->symbol (substring str 0 at))))
              (configuration
                (if (= (+ at 1) (string-length str))
                    (zen:require-default-configuration)
                  (zen:require-configuration (string->symbol (substring str (+ at 1) (string-length str)))))))
          (proc target configuration)))))
  
  (define (standardize-image image)
    (cond ((memv image '(lib library)) 'library)
          ((memv image '(exe executable)) 'executable)
          (else (zen:error "Unknown image type: {s}" image))))
  
  (let ((name (symbol->string symbol)))
    (parse-target/configuration/image name proc)))


;;;
;;;; Make
;;;


(define (zen:make-symbols symbols local?)
  (define (make-symbol symbol arguments)
    (zen:parse-symbol symbol
      (lambda (target configuration image)
        (make-target target configuration image arguments local?))))
  
  (define (make-target target configuration image arguments local?)
    (case target
      ((clean) (zen:make-clean configuration))
      ((cleankernel) (zen:make-cleankernel configuration))
      ((cleanproducts) (zen:make-cleanproducts configuration))
      ((cleanobject) (zen:make-cleanobject configuration))
      ((cleanlibrary) (zen:make-cleanlibrary configuration))
      ((kernel) (zen:make-kernel configuration image local?))
      ((install) (zen:make-install configuration))
      (else (zen:make-product target configuration arguments))))
  
  (zen:parse-symbols symbols
    (lambda (symbols arguments)
      (let iter ((scan (if (null? symbols)
                           (list (zen:default-target))
                         symbols)))
           (if (not (null? scan))
               (let ((symbol (car scan)))
                 (and (make-symbol symbol arguments)
                      (let ((tail (cdr scan)))
                        (if (not (null? tail))
                            (newline (console-port)))
                        (iter tail)))))))))


(define (zen:make symbol)
  (zen:make-symbols (list symbol) #t))


;;;
;;;; Download
;;;


(define (zen:download-symbols symbols local?)
  (define (download-symbol symbol)
    (zen:parse-symbol symbol
      (lambda (target configuration image)
        (download-target target configuration image '() local?))))
  
  (define (download-target target configuration image arguments local?)
    (zen:download-product target configuration arguments))
  
  (zen:parse-symbols symbols
    (lambda (symbols arguments)
      (let iter ((scan (if (null? symbols)
                           (list (zen:default-target))
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


(define (zen:install-symbols symbols local?)
  (define (install-symbol symbol)
    (zen:parse-symbol symbol
      (lambda (target configuration image)
        (install-target target configuration image '() local?))))
  
  (define (install-target target configuration image arguments local?)
    (zen:install-product target configuration arguments))
  
  (zen:parse-symbols symbols
    (lambda (symbols arguments)
      (let iter ((scan (if (null? symbols)
                           (list (zen:default-target))
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


(define (zen:deploy-symbol symbol arguments)
  (zen:parse-symbol symbol
    (lambda (target configuration image)
      (zen:deploy-product target configuration arguments))))


;;;
;;;; Run
;;;


(define (zen:run-symbol symbol arguments)
  (zen:parse-symbol symbol
    (lambda (target configuration image)
      (zen:run-product target configuration arguments))))


;;;
;;;; Test
;;;


(define (zen:test-symbol symbol arguments)
  (zen:parse-symbol symbol
    (lambda (target configuration image)
      (zen:test-product target configuration arguments))))


;;;
;;;; Clean
;;;


(define (zen:clean-toplevel-file? file level)
  (define ignored-toplevel-files
    '(".gitignore"))
  
  (not (member (zen:pathname-name file) ignored-toplevel-files)))


(define (zen:clean-toplevel-directory? dir level)
  (define ignored-toplevel-dirnames
    '(".git"))
  
  (not (member (zen:pathname-name dir) ignored-toplevel-dirnames)))


(define (zen:ignored-libraries-directory configuration)
  (if (eq? (zen:get-configuration-platform configuration) 'mac)
      "Libraries"
    "lib"))


(define (zen:make-clean configuration)
  (define delete-feedback
    (zen:delete-feedback 1))
  
  (zen:feedback "make clean")
  (let ((dir (zen:configuration-directory configuration)))
    (if (file-exists? dir)
        (zen:delete-directory dir
                               0
                               zen:clean-toplevel-file?
                               (lambda (dir level)
                                 (if (zen:clean-toplevel-directory? dir level)
                                     (zen:empty-directory dir
                                                           level
                                                           #f
                                                           #f
                                                           delete-feedback)
                                   #f))
                               delete-feedback))))


(define (zen:make-cleankernel configuration)
  (define delete-feedback
    (zen:delete-feedback 1))
  
  (zen:feedback "make cleankernel")
  (let ((dir (zen:configuration-directory configuration))
        (ignored-lib (zen:ignored-libraries-directory configuration)))
    (if (file-exists? dir)
        (zen:delete-directory dir
                               0
                               zen:clean-toplevel-file?
                               (lambda (dir level)
                                 (if (and (zen:clean-toplevel-directory? dir level)
                                          (not (string=? (zen:pathname-name dir) ignored-lib)))
                                     (zen:empty-directory dir
                                                           level
                                                           #f
                                                           #f
                                                           delete-feedback)
                                   #f))
                               delete-feedback))))


(define (zen:make-cleanproducts configuration)
  (define delete-feedback
    (zen:delete-feedback 0))
  
  (zen:feedback "make cleanproducts")
  (let ((dir (zen:configuration-directory configuration)))
    (if (file-exists? dir)
        (let ((products-dir (string-append dir "build/products/")))
          (if (file-exists? products-dir)
              (zen:delete-directory products-dir
                                     0
                                     #f
                                     #f
                                     delete-feedback))))))


(define (zen:make-cleanobject configuration)
  (define delete-feedback
    (zen:delete-feedback 2))
  
  (define (object-file? file level)
    (let ((ext (zen:pathname-extension file)))
      (or (zen:extension? ext "c")
          (zen:extension? ext "cpp")
          (zen:extension? ext "dgs")
          (zen:extension? ext "mnf")
          (zen:extension? ext "otl")
          (zen:extension? ext "o")
          (zen:numeric-extension? ext "o"))))
  
  (define (empty-objects dir level)
    (zen:empty-directory dir
                          level
                          object-file?
                          empty-objects
                          delete-feedback)
    (zen:cleanup-package dir level delete-feedback))
  
  (zen:feedback "make cleanobject")
  (let ((dir (zen:configuration-directory configuration))
        (ignored-lib (zen:ignored-libraries-directory configuration)))
    (if (file-exists? dir)
        (zen:delete-directory dir
                               0
                               (lambda (file level)
                                 #f)
                               (lambda (dir level)
                                 (if (string=? (zen:pathname-name dir) ignored-lib)
                                     (zen:empty-directory dir
                                                           level
                                                           (lambda (file level)
                                                             #f)
                                                           empty-objects
                                                           delete-feedback)
                                   #f))
                               delete-feedback))))


(define (zen:make-cleanlibrary configuration)
  (define delete-feedback
    (zen:delete-feedback 2))
  
  (define (library-file? file level)
    (let ((base (zen:pathname-base file))
          (ext (zen:pathname-extension file)))
      (or (and (zen:string-ends-with? base "lmf")
               (zen:extension? ext "scm"))
          (zen:numeric-extension? ext "l"))))
  
  (define (empty-libraries dir level)
    (let ((static-dir (string-append dir "static/")))
      (if (file-exists? static-dir)
          (begin
            (zen:feedback "; deleting {a}..." static-dir)
            (zen:delete-directory static-dir))))
    (zen:empty-directory dir
                          level
                          library-file?
                          empty-libraries
                          delete-feedback)
    (zen:cleanup-package dir level delete-feedback))
  
  (zen:feedback "make cleanlibrary")
  (let ((dir (zen:configuration-directory configuration))
        (ignored-lib (zen:ignored-libraries-directory configuration)))
    (if (file-exists? dir)
        (zen:delete-directory dir
                               0
                               (lambda (file level)
                                 #f)
                               (lambda (dir level)
                                 (if (string=? (zen:pathname-name dir) ignored-lib)
                                     (zen:empty-directory dir
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
(define (zen:cleanup-package dir level feedback)
  (let ((content (zen:directory-content dir)))
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


(define (zen:delete-feedback depth)
  (lambda (path level)
    (if (<= level depth)
        (zen:feedback "; deleting {a}..." path))))


;;;
;;;; Install
;;;


(define (zen:make-install configuration)
  (zen:error "Make install is not supported. See INSTALL for details"))


;;;
;;;; Kernel
;;;


(define (zen:make-kernel configuration image local?)
  (define (build-kernel configuration image)
    (define (build configuration)
      (let ((configuration (zen:invoke-build-configure configuration)))
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
              (source zen:source)
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
                            source:                source
                            destination:           destination
                            destination-directory: destination-directory
                            features:              features
                            properties:            properties
                            image:                 image
                            kernel?:               #t
                            console?:              #t))))
    
    (define (compare-configuration configuration-zen configuration-kernel)
      (define (compare-parameter name proc)
        (if (not (equal? (proc configuration-zen) (proc configuration-kernel)))
            (list name (proc configuration-zen) (proc configuration-kernel))
          #f))
      
      (or (compare-parameter system:              zen:get-configuration-system)
          (compare-parameter platform:            zen:get-configuration-platform)
          (compare-parameter compiler:            zen:get-configuration-compiler)
          (compare-parameter processor:           zen:get-configuration-processor)
          (compare-parameter windowing:           zen:get-configuration-windowing)
          (compare-parameter safety:              zen:get-configuration-safety)
          (compare-parameter optimize?:           zen:get-configuration-optimize?)
          (compare-parameter debug-environments?: zen:get-configuration-debug-environments?)
          (compare-parameter debug-location?:     zen:get-configuration-debug-location?)
          (compare-parameter debug-source?:       zen:get-configuration-debug-source?)
          (compare-parameter debug-foreign:       zen:get-configuration-debug-foreign?)
          (compare-parameter track-memory:        zen:get-configuration-track-memory?)
          (compare-parameter mutable-bindings:    zen:get-configuration-mutable-bindings?)
          (compare-parameter kernel-interpret?:   zen:get-configuration-kernel-interpret?)
          (compare-parameter destination:         zen:get-configuration-destination)
          (compare-parameter features:            zen:get-configuration-features)
          (compare-parameter properties:          zen:get-configuration-properties)))
    
    (zen:feedback "make kernel")
    (let ((configuration (or configuration (zen:require-default-configuration))))
      (let ((configuration-file (zen:configuration-file configuration)))
        (if (file-exists? configuration-file)
            (let ((file-configuration (zen:load-configuration configuration-file)))
              (let ((difference (compare-configuration configuration file-configuration)))
                (if difference
                    (zen:error "Configuration mismatch in {a}" difference)
                  (build file-configuration))))
          (build configuration)))))
  
  (define (build-recursive target configuration image)
    (let ((configuration-name (zen:get-configuration-name configuration)))
      (let ((mu (string-append zen:source "mu"))
            (argument (string-append (if configuration-name
                                         (zen:format "{a}@{a}" target configuration-name)
                                       (symbol->string target))
                                     (if image
                                         (string-append ":" (symbol->string image))
                                       ""))))
        (zen:call-process (list path: "sh" arguments: `(,mu "make" ,argument))))))
  
  (if local?
      (build-kernel configuration image)
    (build-recursive 'kernel configuration image)))


(define (zen:invoke-build-configure configuration)
  (let ((proc (zen:build-configure)))
    (if proc
        (proc configuration)
      configuration)))


;;;
;;;; Product
;;;


(define (zen:make-product product configuration arguments)
  (zen:make-kernel configuration #f #f)
  (= 0 (zen:invoke-process
         (list
           path: (string-append (zen:configuration-directory configuration) "zen")
           arguments: `("-:daD"
                        "-make"
                        ,(symbol->string product)
                        #; ;; dynamic-dependencies
                        ,@(let ((dependencies (string-append (current-directory) ".dependencies")))
                            (if (file-exists? dependencies)
                                `("-dependencies" ,dependencies)
                              '()))
                        ,@arguments)))))


(define (zen:download-product product configuration arguments)
  (zen:call-process
     (list
       path: (string-append (zen:configuration-directory configuration) "zen")
       arguments: `("-download"
                    ,(symbol->string product)
                    ,@arguments))))


(define (zen:install-product product configuration arguments)
  (zen:call-process
     (list
       path: (string-append (zen:configuration-directory configuration) "zen")
       arguments: `("-install"
                    ,(symbol->string product)
                    ,@arguments))))


(define (zen:deploy-product product configuration arguments)
  (zen:call-process
     (list
       path: (string-append (zen:configuration-directory configuration) "zen")
       arguments: `("-deploy"
                    ,(symbol->string product)
                    ,@arguments))))


(define (zen:run-product product configuration arguments)
  (zen:call-process
     (list
       path: (string-append (zen:configuration-directory configuration) "zen")
       arguments: `("-run"
                    ,(symbol->string product)
                    ,@arguments))))


(define (zen:test-product product configuration arguments)
  (zen:call-process
     (list
       path: (string-append (zen:configuration-directory configuration) "zen")
       arguments: `("-test"
                    ,(symbol->string product)
                    ,@arguments))))


;;;
;;;; Output
;;;


(define (zen:print line output)
  (display line output)
  (newline output))


(define (zen:debug . rest)
  (zen:print rest (console-port)))


;;;
;;;; Format
;;;


(define (zen:format fmt-string . arguments)
  (define (format-to output fmt-string arguments)
    (let ((control (open-input-string fmt-string))
          (done? #f))
      (define (format-directive)
        (let ((directive (read control)))
          (read-char control)
          (case directive
            ((a)
             (display (car arguments) output)
             (set! arguments (cdr arguments)))
            ((s)
             (write (car arguments) output)
             (set! arguments (cdr arguments)))
            ((%)
             (newline output)))))
      
      (let iter ()
           (let ((c (read-char control)))
             (if (not (eof-object? c))
                 (begin
                   (cond ((eqv? c #\~)
                          (write-char (read-char control) output))
                         ((eqv? c #\{)
                          (format-directive))
                         (else
                          (write-char c output)))
                   (iter)))))))

  (let ((output (open-output-string)))
    (format-to output fmt-string arguments)
    (get-output-string output)))


;;;
;;;; List
;;;


(define (zen:collect-if predicate lst)
  (let iter ((scan lst))
    (if (not (null? scan))
        (let ((value (car scan)))
          (if (predicate value)
              (cons value (iter (cdr scan)))
            (iter (cdr scan))))
      '())))


(define (zen:filter pred lis)
  (let recur ((lis lis))
    (if (null? lis) lis
      (let ((head (car lis))
            (tail (cdr lis)))
        (if (pred head)
            (let ((new-tail (recur tail)))
              (if (eq? tail new-tail) lis
                (cons head new-tail)))
          (recur tail))))))


(define (zen:delete x lis test)
  (zen:filter (lambda (y) (not (test x y))) lis))


;;;
;;;; String
;;;


(define (zen:string-find str c)
  (let ((len (string-length str)))
    (let iter ((n 0))
      (cond ((>= n len)
             #f)
            ((char=? (string-ref str n) c)
             n)
            (else
             (iter (+ n 1)))))))


(define (zen:string-ends-with? str target)
  (let ((sl (string-length str))
        (tl (string-length target)))
    (and (>= sl tl)
         (string=? (substring str (- sl tl) sl) target))))


(define (zen:split-string str separator)
  (let ((lst '())
        (end (string-length str)))
    (let iter ((pos (- end 1)))
      (if (>= pos 0)
          (begin
            (if (eqv? (string-ref str pos) separator)
                (begin
                  (set! lst (cons (substring str (+ pos 1) end) lst))
                  (set! end pos)))
            (iter (- pos 1))))
        (cons (substring str 0 end) lst))))


(define (zen:join-strings strings separator)
  (let ((output (open-output-string)))
    (if (pair? strings)
        (begin
          (display (car strings) output)
          (for-each (lambda (string)
                      (display separator output)
                      (display string output))
                    (cdr strings))))
    (get-output-string output)))


(define (zen:stringify expr)
  (if (string? expr)
      expr
    (let ((output (open-output-string)))
      (display expr output)
      (get-output-string output))))


;;;
;;;; Pathname
;;;


(define (zen:valid-filename? str)
  (let iter ((n (- (string-length str) 1)))
    (if (< n 0)
        #t
      (let ((c (string-ref str n)))
        (if (or (char-alphabetic? c)
                (char-numeric? c)
                (memv c '(#\- #\_)))
            (iter (- n 1))
          #f)))))


;;;
;;;; Error
;;;


(define (zen:error fmt-string . rest)
  (let ((error-string (apply zen:format fmt-string rest)))
    (error error-string)))


;;;
;;;; Repl
;;;


(define zen:prompt
  "% ")

(define zen:display-exception?
  #t)

(define zen:display-backtrace?
  #f)


(define (zen:build-system-repl)
  (define (process-command command output)
    (if (eof-object? command)
        (quit-command '() output)
      (call-with-input-string command
        (lambda (input)
          (let ((command (read input)))
            (if (eof-object? command)
                #f
              (begin
                (let ((arguments (read-all input read)))
                  (case command
                    ((list) (list-command arguments output))
                    ((delete) (delete-command arguments output))
                    ((configure) (configure-command arguments output))
                    ((make) (make-command arguments output))
                    ((download) (download-command arguments output))
                    ((install) (install-command arguments output))
                    ((deploy) (deploy-command arguments output))
                    ((run) (run-command arguments output))
                    ((test) (test-command arguments output))
                    ((help ?) (help-command arguments output))
                    ((quit) (quit-command arguments output))
                    (else (zen:error "Unknown command: {s}" command))))
                #t)))))))
  
  (define (list-command arguments output)
    (zen:list-configurations))
  
  (define (delete-command arguments output)
    (let ((name (if (null? arguments) #f (car arguments))))
      (zen:delete-configuration (zen:require-configuration name))
      (zen:list-configurations)))
  
  (define (configure-command arguments output)
    (if (and (pair? arguments)
             (symbol? (car arguments)))
        (let ((template (zen:configuration-template (car arguments))))
          (apply zen:configure template (cdr arguments)))
      (apply zen:configure zen:unspecified-configuration arguments)))
  
  (define (make-command arguments output)
    (zen:setup-kernel-build)
    (zen:make-symbols arguments #f))
  
  (define (download-command arguments output)
    (zen:download-symbols arguments #f))
  
  (define (install-command arguments output)
    (zen:setup-kernel-install)
    (zen:install-symbols arguments #f))
  
  (define (deploy-command arguments output)
    (zen:setup-kernel-install)
    (zen:deploy-symbol (car arguments) (map zen:stringify (cdr arguments))))
  
  (define (run-command arguments output)
    (zen:setup-kernel-install)
    (zen:run-symbol (car arguments) (map zen:stringify (cdr arguments))))
  
  (define (test-command arguments output)
    (zen:setup-kernel-install)
    (zen:test-symbol (car arguments) (map zen:stringify (cdr arguments))))
  
  (define (help-command arguments output)
    (zen:print "Commands:" output)
    (zen:print "  configure [name:] [system:] [platform:] [compiler:] [processor:] [windowing:] [safety:] [optimize?:] [debug-environments?:] [debug-location?:] [debug-source?:] [debug-foreign?:] [track-memory?:] [mutable-bindings?:] [kernel-interpret?:] [destination:] [features:] [properties:]" output)
    (zen:print "  make [target | clean | cleankernel | cleanproducts | cleanobject | cleanlibrary]@[configuration]:[image]" output)
    (zen:print "  download [target]" output)
    (zen:print "  install [target]" output)
    (zen:print "  deploy [target]" output)
    (zen:print "  run [target]" output)
    (zen:print "  test [target]" output)
    (zen:print "  list" output)
    (zen:print "  delete [configuration]" output)
    (zen:print "  help or ?" output)
    (zen:print "  quit" output))
  
  (define (quit-command arguments output)
    (exit))
  
  (define (debug-exception exc console)
    (if zen:display-exception?
        (display-exception exc console))
    (if zen:display-backtrace?
        (continuation-capture
          (lambda (cont)
            ;; bongo
            (display-continuation-backtrace cont console)
            #;
            (display-continuation-backtrace cont console #t #t 100 100)))))
  
  (let ((console (console-port)))
    (zen:print (zen:format "ZenScheme Build System v{a}" (zen:present-version (zen:get-zen-version-number))) console)
    (force-output console)
    (zen:setup-settings)
    (zen:load-configurations)
    (zen:process-muini)
    (let loop ((newline? #t))
      (if newline?
          (newline console))
      (display zen:prompt console)
      (force-output console)
      (let ((command (read-line console))
            (processed? #f))
        (continuation-capture
          (lambda (stop)
            (with-exception-handler
              (lambda (exc)
                (debug-exception exc console)
                (continuation-return stop #f))
              (lambda ()
                (set! processed? (process-command command console))))))
        (loop processed?)))))


;;;
;;;; Boot
;;;


(define (zen:build-system-boot)
  (define (fatal message)
    (display message)
    (newline)
    (force-output)
    (exit 1))
  
  (define (missing-argument-for-option opt)
    (fatal (zen:format "Missing argument for option: {a}" opt)))
  
  (define (read-argument arg)
    (call-with-input-string (list init: arg)
      read))
  
  (define (string-option name options)
    (let ((opt (zen:find-option name options)))
      (if (not opt)
          (zen:unspecified-option)
        opt)))
  
  (define (symbol-option name options)
    (let ((opt (zen:find-option name options)))
      (if (not opt)
          (zen:unspecified-option)
        (string->symbol opt))))
  
  (define (boolean-option name options)
    (let ((opt (zen:find-option name options)))
      (cond ((not opt)
             (zen:unspecified-option))
            ((string-ci=? opt "false")
             #f)
            ((string-ci=? opt "true")
             #t)
            (else
             (fatal (zen:format "Invalid boolean argument for option: {a}" name))))))
  
  (define (list-option name options)
    (let ((opt (zen:find-option name options)))
      (if (not opt)
          (zen:unspecified-option)
        opt)))
  
  (define (unknown-option opt)
    (fatal (zen:format "Unknown option: {a}" opt)))
  
  (let ((command-arguments (zen:command-arguments)))
    (if (null? command-arguments)
        (zen:build-system-repl)
      (let ((action (car command-arguments))
            (arguments (cdr command-arguments)))
        (zen:setup-settings)
        (zen:load-configurations)
        (zen:process-muini)
        (cond ((equal? action "clone")
               (zen:clone-dependencies (map read-argument arguments))
               (exit))
              ((equal? action "list")
               (zen:list-configurations)
               (exit))
              ((equal? action "delete")
               (let ((name (if (null? arguments) #f (read-argument (car arguments)))))
                 (zen:delete-configuration (zen:require-configuration name))
                 (zen:list-configurations))
               (exit))
              ((equal? action "configure")
               (let ()
                 (define (configure template arguments)
                   (zen:split-command-line arguments '() '("name" "system" "platform" "compiler" "processor" "windowing" "safety" "optimize" "debug-environments" "debug-location" "debug-source" "debug-foreign" "track-memory" "mutable-bindings" "kernel-interpret" "destination" "features" "properties") missing-argument-for-option
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
                                 (kernel-interpret (boolean-option "kernel-interpret" options))
                                 (destination (string-option "destination" options))
                                 (features (list-option "features" options))
                                 (properties (list-option "properties" options)))
                             (zen:configure template name: name system: system platform: platform compiler: compiler processor: processor windowing: windowing safety: safety optimize?: optimize debug-environments?: debug-environments debug-location?: debug-location debug-source?: debug-source debug-foreign?: debug-foreign track-memory?: track-memory mutable-bindings?: mutable-bindings kernel-interpret?: kernel-interpret destination: destination features: features properties: properties)
                             (exit))
                         (unknown-option (car remaining))))))
                 
                 (if (and (pair? arguments)
                          (not (zen:option? (car arguments))))
                     (let ((template (zen:configuration-template (string->symbol (car arguments)))))
                       (configure template (cdr arguments)))
                   (configure zen:unspecified-configuration arguments))))
              ((equal? action "make")
               (zen:setup-kernel-build)
               (if (zen:make-symbols (map read-argument arguments) #t)
                   (exit 0)
                 (exit 1)))
              ((equal? action "download")
               (zen:setup-kernel-install)
               (zen:download-symbols (map read-argument arguments) #t)
               (exit))
              ((equal? action "install")
               (zen:setup-kernel-install)
               (zen:install-symbols (map read-argument arguments) #t)
               (exit))
              ((equal? action "deploy")
               (zen:setup-kernel-install)
               (if (null? arguments)
                   (zen:deploy-symbol (zen:default-target) '())
                 (zen:deploy-symbol (read-argument (car arguments)) (cdr arguments))))
              ((equal? action "run")
               (zen:setup-kernel-install)
               (zen:run-symbol (read-argument (car arguments)) (cdr arguments))
               (exit))
              ((equal? action "test")
               (zen:setup-kernel-install)
               (zen:test-symbol (read-argument (car arguments)) (cdr arguments))
               (exit))
              ((or (equal? action "help") (equal? action "?"))
               (let ((console (console-port)))
                 (zen:print "Usage:" console)
                 (zen:print "  mu configure [-name] [-system] [-platform] [-compiler] [-processor] [-windowing] [-safety] [-optimize] [-debug-environments] [-debug-location] [-debug-source] [-kernel-interpret] [-destination] [-features] [-properties]" console)
                 (zen:print "  mu make [target | clean | cleankernel | cleanproducts | cleanobject | cleanlibrary]@[configuration]:[image]" console)
                 (zen:print "  mu download" console)
                 (zen:print "  mu install" console)
                 (zen:print "  mu deploy" console)
                 (zen:print "  mu run" console)
                 (zen:print "  mu test" console)
                 (zen:print "  mu list" console)
                 (zen:print "  mu delete [configuration]" console)
                 (zen:print "  mu help or ?" console)
                 (zen:print "  mu debug" console))
               (exit))
              ((equal? action "debug")
               (zen:setup-kernel-build)
               (zen:repl-debug-main))
              (else
               (fatal (zen:format "Unknown build system action: {a}" action))))))))


;;;
;;;; Kernel
;;;


(define zen:kernel-system
  'gambit)

(define zen:kernel-platform
  #f)

(define zen:kernel-compiler
  'c)

(define zen:kernel-processor
  #f)

(define zen:kernel-windowing
  #f)

(define zen:kernel-safety
  'develop)

(define zen:kernel-optimize?
  #t)

(define zen:kernel-debug-environments?
  #f)

(define zen:kernel-debug-location?
  #f)

(define zen:kernel-debug-source?
  #f)

(define zen:kernel-debug-foreign?
  #f)

(define zen:kernel-track-memory?
  #f)

(define zen:kernel-mutable-bindings?
  #f)

(define zen:kernel-destination
  #f)

(define zen:kernel-features
  '())

(define zen:kernel-properties
  '())


(define zen:setup-kernel-build
  (let ((kernel-build-setup? #f))
    (lambda ()
      (if (not kernel-build-setup?)
          (begin
            (zen:process-buildini #f)
            (set! kernel-build-setup? #t))))))


(define (zen:setup-kernel-install)
  (zen:setup-kernel-build))


(define (zen:process-muini)
  (zen:load-global/local ".muini"))


;;;
;;;; Load
;;;


(load (string-append zen:source "kernel/boot"))
(zen:load-kernel #f)


;;;
;;;; Templates
;;;


(define zen:unspecified-configuration
  (zen:new-configuration
    name: (zen:unspecified-option)
    system: (zen:unspecified-option)
    platform: (zen:unspecified-option)
    compiler: (zen:unspecified-option)
    processor: (zen:unspecified-option)
    windowing: (zen:unspecified-option)
    safety: (zen:unspecified-option)
    optimize?: (zen:unspecified-option)
    debug-environments?: (zen:unspecified-option)
    debug-location?: (zen:unspecified-option)
    debug-source?: (zen:unspecified-option)
    debug-foreign?: (zen:unspecified-option)
    track-memory?: (zen:unspecified-option)
    mutable-bindings?: (zen:unspecified-option)
    kernel-interpret?: (zen:unspecified-option)
    destination: (zen:unspecified-option)
    features: (zen:unspecified-option)
    properties: (zen:unspecified-option)))

(define zen:core-configuration
  (zen:new-configuration
    name: 'c
    system: (zen:unspecified-option)
    platform: (zen:unspecified-option)
    compiler: (zen:unspecified-option)
    processor: (zen:unspecified-option)
    windowing: (zen:unspecified-option)
    safety: 'core
    optimize?: #f
    debug-environments?: #t
    debug-location?: #t
    debug-source?: #f
    debug-foreign?: #f
    track-memory?: (zen:unspecified-option)
    mutable-bindings?: (zen:unspecified-option)
    kernel-interpret?: #t
    destination: "build/core"
    features: (zen:unspecified-option)
    properties: (zen:unspecified-option)))

(define zen:debug-configuration
  (zen:new-configuration
    name: 'd
    system: (zen:unspecified-option)
    platform: (zen:unspecified-option)
    compiler: (zen:unspecified-option)
    processor: (zen:unspecified-option)
    windowing: (zen:unspecified-option)
    safety: 'debug
    optimize?: #f
    debug-environments?: #t
    debug-location?: #t
    debug-source?: #f
    debug-foreign?: #f
    track-memory?: (zen:unspecified-option)
    mutable-bindings?: (zen:unspecified-option)
    kernel-interpret?: #t
    destination: "build/debug"
    features: (zen:unspecified-option)
    properties: (zen:unspecified-option)))

(define zen:develop-configuration
  (zen:new-configuration
    name: 'l
    system: (zen:unspecified-option)
    platform: (zen:unspecified-option)
    compiler: (zen:unspecified-option)
    processor: (zen:unspecified-option)
    windowing: (zen:unspecified-option)
    safety: 'develop
    optimize?: #f
    debug-environments?: #t
    debug-location?: #t
    debug-source?: #f
    debug-foreign?: #f
    track-memory?: (zen:unspecified-option)
    mutable-bindings?: (zen:unspecified-option)
    kernel-interpret?: #t
    destination: "build/develop"
    features: (zen:unspecified-option)
    properties: (zen:unspecified-option)))

(define zen:release-configuration
  (zen:new-configuration
    name: 'r
    system: (zen:unspecified-option)
    platform: (zen:unspecified-option)
    compiler: (zen:unspecified-option)
    processor: (zen:unspecified-option)
    windowing: (zen:unspecified-option)
    safety: 'release
    optimize?: #f
    debug-environments?: #t
    debug-location?: #t
    debug-source?: #f
    debug-foreign?: #f
    track-memory?: (zen:unspecified-option)
    mutable-bindings?: (zen:unspecified-option)
    kernel-interpret?: #t
    destination: "build/release"
    features: (zen:unspecified-option)
    properties: (zen:unspecified-option)))

(define zen:sealed-configuration
  (zen:new-configuration
    name: 's
    system: (zen:unspecified-option)
    platform: (zen:unspecified-option)
    compiler: (zen:unspecified-option)
    processor: (zen:unspecified-option)
    windowing: (zen:unspecified-option)
    safety: 'sealed
    optimize?: #t
    debug-environments?: #f
    debug-location?: #f
    debug-source?: #f
    debug-foreign?: #f
    track-memory?: (zen:unspecified-option)
    mutable-bindings?: (zen:unspecified-option)
    kernel-interpret?: #f
    destination: "build/sealed"
    features: (zen:unspecified-option)
    properties: (zen:unspecified-option)))


(define zen:configuration-templates
  `((c . ,zen:core-configuration)
    (d . ,zen:debug-configuration)
    (l . ,zen:develop-configuration)
    (r . ,zen:release-configuration)
    (s . ,zen:sealed-configuration)))


(define (zen:configuration-template name)
  (let ((pair (assq name zen:configuration-templates)))
    (if pair
        (cdr pair)
      (zen:error "Unknown configuration template: {s}" name))))


;;;
;;;; Initialize
;;;


(zen:setup-versions)
(zen:build-system-boot)
