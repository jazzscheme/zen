;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Version
;;;


(block kernel.version


;;;
;;;; Version
;;;


(zen:define-structure Version () (constructor: zen:make-version predicate: zen:version?)
  ((number               getter: generate)
   (gambit-version       getter: generate)
   (gambit-stamp         getter: generate)
   (rebuild              getter: generate)
   (recompile            getter: generate)
   (recompile-references getter: generate)
   (sweep                getter: generate)
   (update               getter: generate)
   (description          getter: generate)))


(define (zen:new-version
          #!key
          (version #f)
          (gambit-version #f)
          (gambit-stamp #f)
          (rebuild #f)
          (recompile #f)
          (recompile-references #f)
          (sweep #f)
          (update #f)
          (description #f))
  (zen:make-version
    version
    gambit-version
    gambit-stamp
    rebuild
    recompile
    recompile-references
    sweep
    update
    description))


(define (zen:split-version number)
  (let ((str (number->string number)))
    (let ((len (string-length str)))
      (let ((major (string->number (substring str 0 (- len 5))))
            (minor (string->number (substring str (- len 5) (- len 3))))
            (revision (string->number (substring str (- len 3) len))))
        (values major minor revision)))))


(define (zen:present-version number)
  (receive (major minor revision) (zen:split-version number)
    (string-append (number->string major)
                   "."
                   (number->string minor)
                   "."
                   (number->string revision))))


;;;
;;;; Versions
;;;


(define zen:zen-versions-file
  #f)

(define zen:zen-versions-cache
  #f)

(define zen:zen-version-number
  #f)

(define zen:zen-gambit-version
  #f)

(define zen:zen-gambit-stamp
  #f)


(define (zen:load-versions-file file)
  (if (and file (file-exists? file))
      (call-with-input-file (list path: file eol-encoding: 'cr-lf)
        (lambda (input)
          (map (lambda (arguments)
                 (apply zen:new-version arguments))
               (read-all input read))))
    #f))


(define zen:load-zen-versions
  (let ((loaded? #f))
    (lambda ()
      (define (determine-zen-versions-file)
        (or zen:zen-versions-file
            (and zen:zen-source (string-append zen:zen-source "kernel/versions"))))
      
      (define (setup-zen-gambit-version/stamp)
        (if zen:zen-versions-cache
            (let iter ((zen-versions zen:zen-versions-cache))
              (if (not (null? zen-versions))
                  (let ((zen-version (car zen-versions)))
                    (let ((gambit-version (zen:get-version-gambit-version zen-version))
                          (gambit-stamp (zen:get-version-gambit-stamp zen-version)))
                      (if gambit-version
                          (begin
                            (set! zen:zen-gambit-version gambit-version)
                            (set! zen:zen-gambit-stamp gambit-stamp))
                        (iter (cdr zen-versions)))))))))
      
      (if (not loaded?)
          (begin
            (set! zen:zen-versions-cache (or (zen:load-versions-file (determine-zen-versions-file)) zen:zen-versions))
            (set! zen:zen-version-number (zen:get-version-number (car zen:zen-versions-cache)))
            (setup-zen-gambit-version/stamp)
            (set! loaded? #t))))))


(define (zen:get-zen-versions)
  (zen:load-zen-versions)
  zen:zen-versions-cache)


(define (zen:get-zen-version-number)
  (zen:load-zen-versions)
  zen:zen-version-number)


(define (zen:get-zen-gambit-version)
  (zen:load-zen-versions)
  zen:zen-gambit-version)


(define (zen:get-zen-gambit-stamp)
  (zen:load-zen-versions)
  zen:zen-gambit-stamp)


(define (zen:gambit-zen? gambit-vendor)
  (and gambit-vendor
       (string=? gambit-vendor "Zen")))

(define (zen:gambit-uptodate? gambit-version gambit-stamp)
  (let ((zen-gambit-version (zen:get-zen-gambit-version))
        (zen-gambit-stamp (zen:get-zen-gambit-stamp)))
    (or (not zen-gambit-version)
        (> gambit-version zen-gambit-version)
        (if zen-gambit-stamp
            (>= gambit-stamp zen-gambit-stamp)
          (>= gambit-version zen-gambit-version)))))


;;;
;;;; Update
;;;


(zen:define-structure Update () (constructor: zen:make-update)
  ((version     getter: generate)
   (targets     getter: generate)
   (description getter: generate)))


(define (zen:new-update
          #!key
          (version #f)
          (targets #f)
          (description #f))
  (zen:make-update
    version
    targets
    description))


;;;
;;;; Updates
;;;


(define zen:zen-updates-file
  #f)

(define zen:zen-updates-cache
  #f)


(define (zen:get-zen-updates)
  (define (determine-zen-updates-file)
    (or zen:zen-updates-file
        (and zen:zen-source (string-append zen:zen-source "kernel/updates"))))
  
  (define (load-updates)
    (let ((file (determine-zen-updates-file)))
      (if (and file (file-exists? file))
          (call-with-input-file (list path: file eol-encoding: 'cr-lf)
            (lambda (input)
              (zen:list->updates (read-all input read))))
        #f)))
  
  (if (not zen:zen-updates-cache)
      (set! zen:zen-updates-cache (or (load-updates) zen:zen-updates '())))
  zen:zen-updates-cache)


(define (zen:list->updates lst)
  (map (lambda (arguments)
         (apply zen:new-update arguments))
       lst))


(define (zen:for-each-update target updates proc)
  (let iter ((updates updates))
    (if (not (null? updates))
        (let ((update (car updates)))
          (let ((targets (zen:get-update-targets update)))
            (if (and targets (if (symbol? targets)
                                 (eq? target targets)
                               (memq target targets)))
                (proc update))
            (iter (cdr updates)))))))


(define (zen:versioned-directory root target updates converter #!key (feedback? #t))
  (define (determine-version)
    (let ((uptodate? #t))
      (continuation-capture
        (lambda (return)
          (zen:for-each-update target updates
            (lambda (update)
              (let ((version-number (zen:get-update-version update)))
                (let ((version-dir (version-directory version-number)))
                  (if (file-exists? version-dir)
                      (continuation-return return (values uptodate? version-number))
                    (set! uptodate? #f))))))
          (values uptodate? #f)))))
  
  (define (version-directory version-number)
    (if (not version-number)
        root
      (string-append root (zen:present-version version-number) "/")))
  
  (zen:create-directories root)
  (receive (uptodate? current-version-number) (determine-version)
    (if uptodate?
        (version-directory current-version-number)
      (let ((current-dir (version-directory current-version-number))
            (conversion-dir (string-append root "conversion/")))
        (if (file-exists? conversion-dir)
            (begin
              (if feedback?
                  (zen:feedback "; deleting {a}..." conversion-dir))
              (zen:delete-directory conversion-dir)
              ;; workaround to yet another windows bug
              (thread-sleep! .1)))
        (if feedback?
            (zen:feedback "; converting {a}..." target))
        (zen:copy-directory current-dir conversion-dir feedback: (and feedback? (lambda (src level) (if (<= level 1) (zen:feedback "; copying {a}..." src)))))
        (let iter ((working-version-number current-version-number))
             (let ((converted-version-number (converter conversion-dir working-version-number)))
               (if converted-version-number
                   (iter converted-version-number)
                 (let ((dir (version-directory working-version-number)))
                   (if feedback?
                       (zen:feedback "; {a} converted to version {a}" target (zen:present-version working-version-number)))
                   (rename-file conversion-dir dir)
                   dir))))))))


(define (zen:versioned-version target updates)
  (continuation-capture
    (lambda (return)
      (zen:for-each-update target updates
        (lambda (update)
          (continuation-return return (zen:get-update-version update))))
      #f)))


;;;
;;;; Settings
;;;


(define zen:zen-settings-directory
  #f)

(define zen:zen-settings-version
  #f)

(define zen:named-configurations-file
  #f)

(define zen:local-configurations-file
  "./.configurations")


(define (zen:setup-settings)
  (let ((settings-root (%%string-append (zen:home-directory) "/.zen/")))
    (cond ((not (file-exists? settings-root))
           (let ((version (zen:versioned-version 'settings (zen:get-zen-updates))))
             (if version
                 (begin
                   (set! zen:zen-settings-directory (string-append settings-root (zen:present-version version) "/"))
                   (set! zen:zen-settings-version version)
                   (set! zen:named-configurations-file (string-append zen:zen-settings-directory ".configurations"))))))
          (else
           (set! zen:zen-settings-directory (zen:versioned-directory settings-root 'settings (zen:get-zen-updates) zen:convert-settings))
           (set! zen:zen-settings-version (zen:versioned-version 'settings (zen:get-zen-updates)))
           (set! zen:named-configurations-file (string-append zen:zen-settings-directory ".configurations"))))))


(define (zen:convert-settings dir old)
  (define (convert-initial)
    (zen:convert-configurations dir
      (lambda (configurations)
        (map zen:convert-configuration-205000 configurations)))
    205000)
  
  (define (convert-205000)
    (zen:convert-configurations dir
      (lambda (configurations)
        (map zen:convert-configuration-205001 configurations)))
    205001)
  
  (define (convert-205001)
    205002)
  
  (define (convert-205002)
    205003)
  
  (case old
    ((#f) (convert-initial))
    ((205000) (convert-205000))
    ((205001) (convert-205001))
    ((205002) (convert-205002))
    (else #f)))


(define (zen:convert-configurations dir converter)
  (let ((configurations-file (string-append dir ".configurations")))
    (define (read-configurations)
      (call-with-input-file (list path: configurations-file eol-encoding: 'cr-lf)
        (lambda (input)
          (read-all input read))))
    
    (define (write-configurations configurations)
      (call-with-output-file (list path: configurations-file #; eol-encoding: #; (zen:platform-eol-encoding (zen:guess-platform)))
        (lambda (output)
          (for-each (lambda (configuration)
                      (write configuration output)
                      (newline output))
                    configurations))))
    
    (if (file-exists? configurations-file)
        (write-configurations (converter (read-configurations))))))


(define (zen:convert-configuration-205000 configuration)
  (zen:convert-properties configuration
    (lambda (property value)
      (case property
        ((interpret-kernel?:) (list kernel-interpret?: value))
        ((source-access?:) (list))
        (else (list property value))))))


(define (zen:convert-configuration-205001 configuration)
  (zen:convert-properties configuration
    (lambda (property value)
      (case property
        ((destination:) (list destination: (if (string? value) (zen:string-replace value #\: #\/) value)))
        (else (list property value))))))


(define (zen:convert-properties plist converter)
  (let iter ((scan plist) (result '()))
    (if (null? scan)
        result
      (iter (cddr scan) (append result (converter (car scan) (cadr scan)))))))


(define (zen:load-global/local filename)
  (let ((global (string-append zen:zen-settings-directory filename))
        (local filename))
    (zen:load-if-exists global)
    (zen:load-if-exists local)))


;;;
;;;; Configurations
;;;

;; to test cross compiling


(define zen:named-configurations
  #f)


(define (zen:get-named-configurations)
  (or zen:named-configurations
      (let ((configurations
              (if (file-exists? zen:named-configurations-file)
                  (call-with-input-file (list path: zen:named-configurations-file eol-encoding: 'cr-lf)
                    (lambda (input)
                      (define (read-configuration input)
                        (let ((list (read input)))
                          (if (eof-object? list)
                              list
                            (apply zen:new-named-configuration list))))
                
                      (read-all input read-configuration)))
                '())))
        (set! zen:named-configurations configurations)
        configurations)))


(define (zen:find-named-configuration name)
  (let iter ((configurations (zen:get-named-configurations)))
    (if (null? configurations)
        #f
      (let ((configuration (car configurations)))
        (if (eq? (zen:get-configuration-name configuration) name)
            configuration
          (iter (cdr configurations)))))))


(define (zen:new-named-configuration
          #!key
          (name (%%unspecified))
          (system (%%unspecified))
          (platform (%%unspecified))
          (compiler (%%unspecified))
          (processor (%%unspecified))
          (windowing (%%unspecified))
          (safety (%%unspecified))
          (optimize? (%%unspecified))
          (debug-environments? (%%unspecified))
          (debug-location? (%%unspecified))
          (debug-source? (%%unspecified))
          (debug-foreign? (%%unspecified))
          (track-memory? (%%unspecified))
          (mutable-bindings? (%%unspecified))
          (kernel-interpret? (%%unspecified))
          (destination (%%unspecified))
          (features (%%unspecified))
          (properties (%%unspecified))
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


;;;
;;;; Configuration
;;;


(define (zen:save-configuration name system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? kernel-interpret? destination features properties file system-platform)
  (call-with-output-file (list path: file eol-encoding: (zen:platform-eol-encoding system-platform))
    (lambda (output)
      (display "(configuration " output)
      (display (zen:get-zen-version-number) output)
      (newline output)
      (newline output)
      (display "  " output)
      (zen:print-configuration name system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? kernel-interpret? destination features properties output)
      (display ")" output)
      (newline output))))


(define (zen:print-configuration name system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? kernel-interpret? destination features properties output)
  (define first?
    #t)
  
  (define (print-property property value)
    (if first?
        (set! first? #f)
      (display " " output))
    (write property output)
    (display " " output)
    (write value output))
  
  (display "(" output)
  (print-property name: name)
  (print-property system: system)
  (print-property platform: platform)
  (print-property compiler: compiler)
  (print-property processor: processor)
  (print-property windowing: windowing)
  (print-property safety: safety)
  (print-property optimize?: optimize?)
  (print-property debug-environments?: debug-environments?)
  (print-property debug-location?: debug-location?)
  (print-property debug-source?: debug-source?)
  (print-property debug-foreign?: debug-foreign?)
  (print-property track-memory?: track-memory?)
  (print-property mutable-bindings?: mutable-bindings?)
  (print-property kernel-interpret?: kernel-interpret?)
  (print-property destination: destination)
  (print-property features: features)
  (print-property properties: properties)
  (display ")" output))


;;;
;;;; Link
;;;


(define (zen:parse-link link)
  (define link-options
    '(("obj" objects)
      ("objects" objects)
      ("lib" libraries)
      ("libraries" libraries)
      ("obj/lib" objects libraries)
      ("objects/libraries" objects libraries)
      ("static" objects static)
      ("all" objects libraries static)))
  
  (define (invalid option)
    (zen:error "Invalid link option: {a}" option))
  
  (if (symbol? link)
      (let ((lst (zen:split-string (symbol->string link) #\/))
            (result '()))
        (for-each (lambda (option)
                    (let ((pair (assoc option link-options)))
                      (if pair
                          (let ((options (cdr pair)))
                            (for-each (lambda (option)
                                        (if (not (member option result))
                                            (set! result (cons option result))))
                                      options))
                        (invalid option))))
                  lst)
        result)
    (invalid link)))


;;;
;;;; Destination
;;;


(define (zen:destination-directory name destination dir)
  (zen:dirname-normalize
    (or destination "bin"))))
