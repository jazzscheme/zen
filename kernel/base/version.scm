;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Version
;;;


;;;
;;;; Version
;;;


(define-type zen:version constructor: zen:make-version predicate: zen:version?
  (number               zen:get-version-number zen:set-version-number)
  (gambit-version       zen:get-version-gambit-version zen:set-version-gambit-version)
  (gambit-stamp         zen:get-version-gambit-stamp zen:set-version-gambit-stamp)
  (rebuild              zen:get-version-rebuild zen:set-version-rebuild)
  (recompile            zen:get-version-recompile zen:set-version-recompile)
  (recompile-references zen:get-version-recompile-references zen:set-version-recompile-references)
  (sweep                zen:get-version-sweep zen:set-version-sweep)
  (update               zen:get-version-update zen:set-version-update)
  (description          zen:get-version-description zen:set-version-description))


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


(define-type zen:update constructor: zen:make-update predicate: zen:update?
  (version     zen:get-update-version zen:set-update-version)
  (targets     zen:get-update-targets zen:set-update-targets)
  (description zen:get-update-description zen:set-update-description))


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


(define zen:settings-directory
  #f)

(define zen:configurations-file
  "./.configurations")


(define (zen:setup-settings)
  (let ((settings-root (string-append (zen:home-directory) "/.zen/")))
    (zen:create-directory settings-root)
    (set! zen:settings-directory settings-root)))


(define (zen:load-global/local filename)
  (let ((global (string-append zen:settings-directory filename))
        (local filename))
    (zen:load-if-exists global)
    (zen:load-if-exists local)))


;;;
;;;; Configuration
;;;


(define (zen:save-configuration name system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? destination features properties file system-platform)
  (call-with-output-file (list path: file eol-encoding: (zen:platform-eol-encoding system-platform))
    (lambda (output)
      (display "(configuration " output)
      (display (zen:get-zen-version-number) output)
      (newline output)
      (newline output)
      (display "  " output)
      (zen:print-configuration name system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? destination features properties output)
      (display ")" output)
      (newline output))))


(define (zen:print-configuration name system platform compiler processor windowing safety optimize? debug-environments? debug-location? debug-source? debug-foreign? track-memory? mutable-bindings? destination features properties output)
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
    (or destination "bin")))
