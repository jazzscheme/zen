;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Common
;;;


(block kernel.common


;;;
;;;; Load
;;;


(define (zen:load-file pathname . rest)
  (let ((quiet? (if (%%null? rest) #f (%%car rest))))
    (%%load pathname (lambda rest #f) #f #t #f quiet?))
  (void))


(define (zen:load-binary pathname . rest)
  ;; because we statically link the same .o into the .o1 and .l1
  ;; we need to hack the module-descrs to remove the name conflict
  ;; it might be possible to only register the most recent one..
  (define (hack-module-descrs module-descrs)
    (let loop ((i (%%fx- (%%vector-length module-descrs) 1)))
         (if (%%fx>= i 0)
             (let ((module-descr (%%vector-ref module-descrs i)))
               (let ((name (%%symbol->string (%%vector-ref module-descr 0))))
                 (if (zen:string-starts-with? name zen:bin-uniqueness-prefix)
                     (%%vector-set! module-descr 0 (%%string->symbol (%%string-append zen:lib-uniqueness-prefix (%%substring name (%%string-length zen:bin-uniqueness-prefix) (%%string-length name)))))))
               (loop (%%fx- i 1))))))
  
  (let ((quiet? (if (%%null? rest) #f (%%car rest))))
    (let ((result (zen:load-object-file pathname quiet?)))
      (if (%%not (%%vector? result))
          (zen:error "Unable to load {a}" pathname)
        (let ((module-descrs (zen:object-file-module-descrs result 0)))
          (if (%%not (%%vector? module-descrs))
              (zen:error "Unable to load {a}: {a}" pathname module-descrs)
            (begin
              (hack-module-descrs module-descrs)
              (zen:register-module-descrs-and-load! module-descrs)))))))
  (void))


;;;
;;;; Configure
;;;


(define zen:enable-track-scheme?
  (zen:string-contains? (configure-command-string) "--enable-track-scheme"))

(define zen:enable-debug-garbage-collect?
  (zen:string-contains? (configure-command-string) "--enable-debug-garbage-collect"))


;;;
;;;; Configuration
;;;


(zen:define-variable zen:debugger
  #f)

(zen:define-variable zen:compile-options
  `(,@(if zen:kernel-debug-environments? '(debug-environments) '())
    ,@(if zen:kernel-debug-location? '(debug-location) '())
    ,@(if zen:kernel-debug-source? '(debug-source) '())
    ,@(if zen:enable-track-scheme? '(track-scheme) '())))

(zen:define-variable zen:link
  #f)

(zen:define-variable zen:link-options
  '(objects))

(zen:define-variable zen:jobs
  #f)

(zen:define-variable zen:warnings
  #f)

(define (zen:set-link-options options)
  (set! zen:link-options options))

(define zen:recompile-references
  #f)

(define (zen:link-objects?)
  (%%memq 'objects zen:link-options))

(define (zen:link-libraries?)
  (%%memq 'libraries zen:link-options))

(define (zen:link-static?)
  (%%memq 'static zen:link-options))

(define zen:zenini
  ".zenini")


(define (zen:load-if-exists file)
  (if (file-exists? file)
      (zen:load-file file)))


(define (zen:load-configuration-files filename install?)
  (let ((global (and zen:zen-settings-directory (%%string-append zen:zen-settings-directory filename)))
        (local filename))
    (if install?
        (zen:load-if-exists (zen:install-path filename)))
    (if global
        (zen:load-if-exists global))
    (zen:load-if-exists local)))


(define (zen:process-zenini install?)
  (zen:load-configuration-files zen:zenini install?))


(define (zen:process-settings)
  (if zen:kernel-source-access?
      (begin
        (zen:setup-settings)
        (zen:process-zenini #t))
    (if zen:kernel-zenini-access?
        (zen:load-if-exists (zen:install-path zen:zenini)))))


;;;
;;;; Build
;;;


(zen:define-variable zen:manifest-needs-rebuild?
  (lambda rest
    #f))

(zen:define-variable zen:get-changed-units #f)
(zen:define-variable zen:push-changed-units #f)
(zen:define-variable zen:reset-changed-units #f)
(zen:define-variable zen:build-kernel #f)
(zen:define-variable zen:build-image #f)
(zen:define-variable zen:build-library #f)


;;;
;;;; Custom build
;;;


(define (zen:custom-compile/build unit-specs #!key (unit #f) (pre-build #f) (force? #f))
  (if unit
      (let ((compile-args (assq unit unit-specs)))
        (if compile-args
            (apply zen:compile-unit `(,@compile-args force?: ,force?))
          (zen:compile-unit unit force?: force?)))
    (begin
      (if pre-build
          (pre-build))
      (for-each (lambda (compile-args)
                  (apply zen:compile-unit `(,@compile-args force?: ,force?)))
                unit-specs))))


;;;
;;;; Feedback
;;;


(define zen:build-feedback
  zen:feedback)


;;;
;;;; List
;;;


(define (zen:listify obj)
  (if (or (%%null? obj) (%%pair? obj))
      obj
    (%%list obj)))


(define (zen:getprop plist target)
  (let iter ((scan plist))
    (cond ((%%null? scan)
           #f)
          ((%%eqv? (%%car scan) target)
           scan)
          (else
           (iter (%%cddr scan))))))


(define (zen:getf plist target #!optional (not-found #f))
  (let ((pair (zen:getprop plist target)))
    (if pair
        (%%cadr pair)
      not-found)))


(define (zen:collect-if predicate lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (%%not (%%null? scan))
        (let ((value (%%car scan)))
          (if (predicate value)
              (%%cons value (iter (%%cdr scan)))
            (iter (%%cdr scan))))
      '())))


(define (zen:collect test lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (%%not (%%null? scan))
        (let ((value (test (%%car scan))))
          (if value
              (%%cons value (iter (%%cdr scan)))
            (iter (%%cdr scan))))
      '())))


(define (zen:member? obj lst #!key (test equal?))
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (cond ((%%null? scan)
           #f)
          ((test obj (%%car scan))
           #t)
          (else
           (iter (%%cdr scan))))))


(define (zen:union actual new #!key (test #f))
  (for-each (lambda (obj)
              (if (%%not (if (%%not test)
                             (%%memq obj actual)
                           (zen:member? obj actual test: test)))
                  (set! actual (%%cons obj actual))))
            new)
  actual)


(define (zen:remove item lst #!key (test #f))
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (%%not (%%null? scan))
        (let ((value (%%car scan)))
          (if (if test
                  (test value item)
                (%%eq? value item))
              (iter (%%cdr scan))
            (%%cons value (iter (%%cdr scan)))))
      '())))


(define zen:kernel-collect-if zen:collect-if)


;;;
;;;; String
;;;


(define (zen:string-find-reversed str c)
  (declare (proper-tail-calls))
  (let iter ((n (%%fx- (%%string-length str) 1)))
    (cond ((%%fx< n 0)
           #f)
          ((%%char=? (%%string-ref str n) c)
           n)
          (else
           (iter (%%fx- n 1))))))


(define (zen:string-starts-with-ci? str target)
  (let ((sl (%%string-length str))
        (tl (%%string-length target)))
    (and (%%fx>= sl tl)
         (%%string-ci=? (%%substring str 0 tl) target))))


(define (zen:string-ends-with? str target)
  (let ((sl (%%string-length str))
        (tl (%%string-length target)))
    (and (%%fx>= sl tl)
         (%%string=? (%%substring str (%%fx- sl tl) sl) target))))


(define (zen:string-numeric? str)
  (let iter ((n (%%fx- (%%string-length str) 1)))
       (if (%%fx>= n 0)
           (let ((c (%%string-ref str n)))
             (if (%%memv c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                 (iter (%%fx- n 1))
               #f))
         #t)))


(define (zen:split-string str separator)
  (declare (proper-tail-calls))
  (let ((lst '())
        (end (%%string-length str)))
    (let iter ((pos (%%fx- end 1)))
      (if (%%fx>= pos 0)
          (begin
            (if (%%eqv? (%%string-ref str pos) separator)
                (begin
                  (set! lst (%%cons (%%substring str (%%fx+ pos 1) end) lst))
                  (set! end pos)))
            (iter (%%fx- pos 1))))
        (%%cons (%%substring str 0 end) lst))))


(define (zen:join-strings strings separator)
  (let ((output (open-output-string)))
    (if (%%pair? strings)
        (begin
          (display (%%car strings) output)
          (for-each (lambda (string)
                      (display separator output)
                      (display string output))
                    (%%cdr strings))))
    (get-output-string output)))


;;;
;;;; Symbol
;;;


(define (zen:compose-reference . rest)
  (%%string->symbol (zen:join-strings (map symbol->string rest) "#")))


;;;
;;;; Table
;;;


;; unsafe


(define (zen:table-iterate table proc)
  (%%table-for-each proc table))


;; safe version that can access the table


(define (zen:table-iterate-safe table proc)
  (for-each (lambda (pair)
              (proc (%%car pair) (%%cdr pair)))
            (%%table->list table)))


;;;
;;;; Output
;;;


(zen:define-variable zen:display display)
(zen:define-variable zen:write write)


;;;
;;;; Pathname
;;;


(zen:define-variable zen:executable-path
  #f)

(zen:define-variable zen:executable-directory
  #f)


(define (zen:pathname-name pathname)
  (let ((pos (zen:string-find-reversed pathname #\/))
        (len (%%string-length pathname)))
    (cond ((%%not pos)
           pathname)
          ((%%fx= pos (%%fx- len 1))
           (zen:pathname-name (%%substring pathname 0 pos)))
          (else
           (%%substring pathname (%%fx+ pos 1) len)))))


(define (zen:pathname-base pathname)
  (let ((name (zen:pathname-name pathname)))
    (let ((pos (zen:string-find-reversed name #\.)))
      (if pos
          (%%substring name 0 pos)
        name))))


(define (zen:pathname-spine pathname)
  (let ((pos (zen:string-find-reversed pathname #\.)))
    (if pos
        (%%substring pathname 0 pos)
      pathname)))


(define (zen:pathname-extension pathname)
  (let ((name (zen:pathname-name pathname)))
    (let ((pos (zen:string-find-reversed name #\.)))
      (if pos
          (%%substring name (%%fx+ pos 1) (%%string-length name))
        #f))))


(define (zen:pathname-brother pathname name)
  (let ((pos (zen:string-find-reversed pathname #\/))
        (len (%%string-length pathname)))
    (cond ((%%not pos)
           name)
          (else
           (%%string-append (%%substring pathname 0 (%%fx+ pos 1)) name)))))


(define (zen:extension? extension target)
  (or (and (%%not extension) (%%not target))
      (and extension
           target
           (%%string=? extension target))))


(define (zen:numeric-extension? extension prefix)
  (and extension
       (zen:string-starts-with? extension prefix)
       (zen:string-numeric? (%%substring extension (%%string-length prefix) (%%string-length extension)))))


(define (zen:executable-extension platform)
  (case platform
    ((windows)
     "exe")
    (else
     #f)))


(define (zen:pathname-dir pathname)
  (let ((pos (zen:string-find-reversed pathname #\/)))
    (if (%%not pos)
        #f
      (%%substring pathname 0 (%%fx+ pos 1)))))


(define (zen:file-last-access-seconds pathname)
  (time->seconds (file-last-access-time pathname)))

(define (zen:file-last-modification-seconds pathname)
  (time->seconds (file-last-modification-time pathname)))


(define (zen:add-extension filename extension)
  (if (%%not extension)
      filename
    (%%string-append filename "." extension)))


(cond-expand
  (windows)
  (else
   ;; redefined in compiled code
   (define zen:file-permissions
     #f)
   
   (define zen:file-permissions-set!
     #f)
   
   (set! zen:file-permissions
     (lambda (filename)
       (error "Called too early")))
   
   (set! zen:file-permissions-set!
     (lambda (filename permissions)
       (error "Called too early")))))


(define (zen:copy-file src dst)
  (cond-expand
    (windows
     (copy-file src dst))
    (else
     (let ((permissions (zen:file-permissions src)))
       (copy-file src dst)
       (zen:file-permissions-set! dst permissions)))))


(define (zen:copy-file-if-needed src dst #!key (feedback #f))
  (if (zen:file-needs-update? src dst)
      (begin
        (if feedback
            (feedback "; copying {a}..." src))
        (if (file-exists? dst)
            (delete-file dst))
        (zen:copy-file src dst))))


(define (zen:copy-files-if-needed src dst #!key (feedback #f))
  (if (not (file-exists? dst))
      (begin
        (if feedback
            (feedback "; copying {a}..." src))
        (create-directory dst)))
  (for-each (lambda (file)
              (let ((src-pathname (string-append src "/" file))
                    (dst-pathname (string-append dst "/" file)))
                (case (zen:pathname-type src-pathname)
                  ((file)
                   (zen:copy-file-if-needed src-pathname dst-pathname feedback: #f)))))
            (directory-files src)))


(define (zen:copy&sign-if-needed src dst #!key (feedback #f))
  (if (zen:file-needs-update? src dst)
      (begin
        (cond-expand
          (mac
           (let ((apple-id (getenv "ZEN_APPLE_DEVELOPER_ID" #f))
                 (entitlements (getenv "ZEN_APPLE_ENTITLEMENTS" #f)))
             (if apple-id
                 (begin
                   (if feedback
                       (feedback "; copying & signing {a}..." src))
                   (if (file-exists? dst)
                       (delete-file dst))
                   (zen:copy-file src dst)
                   (zen:codesign #t #f apple-id dst))
               (zen:copy-file-if-needed src dst feedback: feedback))))
          (else
           (zen:copy-file-if-needed src dst feedback: feedback))))))


(define (zen:codesign-required?)
  (cond-expand
    (mac (getenv "ZEN_APPLE_DEVELOPER_ID" #f))
    (else #f)))


(define (zen:codesign-if bin)
  (cond-expand
    (mac
     (let ((apple-id (getenv "ZEN_APPLE_DEVELOPER_ID" #f))
           (entitlements (getenv "ZEN_APPLE_ENTITLEMENTS" #f)))
       (if apple-id
           (zen:codesign #t #f apple-id bin))))
    (else
     #f)))


(define (zen:codesign mac? ios? apple-id bin #!key (entitlements #f))
  (cond (mac?
         (let ((entitlements-arguments (if (not entitlements)
                                           '()
                                         (list "--entitlements" entitlements))))
           (zen:call-process
             (list
               path: "/usr/bin/codesign"
               arguments: `("--options" "runtime" ,@entitlements-arguments "--timestamp" "--sign" ,apple-id ,bin)))))
        (ios?
         (zen:call-process
           (list
             path: "/usr/bin/codesign"
             arguments: `("--force" "--sign" ,apple-id "--preserve-metadata=identifier,entitlements" "--timestamp=none" ,bin))))))


(define (zen:file-needs-update? src dst)
  (or (%%not (file-exists? dst))
      (> (zen:file-last-modification-seconds src)
         (zen:file-last-modification-seconds dst))))


(define (zen:maybe-relativise-directory basedir rootdir targdir)
  (let ((targdir (zen:pathname-normalize targdir))
        (basedir (zen:pathname-normalize basedir))
        (rootdir (zen:pathname-normalize rootdir)))
    (let ((targlen (%%string-length targdir))
          (baselen (%%string-length basedir))
          (rootlen (%%string-length rootdir)))
      (if (and (%%fx>= baselen rootlen)
               (%%string=? (%%substring basedir 0 rootlen) rootdir)
               (%%fx>= targlen rootlen)
               (%%string=? (%%substring targdir 0 rootlen) rootdir))
          (let ((suffix (%%substring basedir rootlen baselen))
                (relative-dir #f))
            (let iter ((n (%%fx- (%%string-length suffix) 1)))
              (if (%%fx>= n 0)
                  (begin
                    (if (%%eqv? (%%string-ref suffix n) #\/)
                        (set! relative-dir (%%string-append (or relative-dir "") "../")))
                    (iter (%%fx- n 1)))))
            (values (%%string-append (or relative-dir "./") (%%substring targdir rootlen targlen)) #t))
        (values targdir #f)))))


(define (zen:relativise-directory basedir rootdir targdir)
  (receive (directory relative?) (zen:maybe-relativise-directory basedir rootdir targdir)
    directory))


(define (zen:absolutize-directory basedir reldir)
  (if (or (zen:string-starts-with? reldir "./")
          (zen:string-starts-with? reldir "../"))
      (zen:pathname-normalize (%%string-append basedir reldir) #f)
    (zen:pathname-normalize reldir #f)))


(define (zen:parent-directory dir)
  (path-directory (path-strip-trailing-directory-separator dir)))


(define (zen:nth-parent-directory dir n)
  (let iter ((n n)
             (dir dir))
       (if (%%fx= n 0)
           dir
         (iter (%%fx- n 1) (zen:parent-directory dir)))))


(define (zen:quote-pathname pathname #!optional (platform zen:kernel-platform))
  (case platform
    ((windows)
     (string-append "\"" pathname "\""))
    (else
     ;; quoting is only necessary on windows as arguments are passed explicitly in linux
     pathname)))


(define (zen:quote-zen-pathname suffix)
  (zen:quote-pathname (zen:zen-pathname suffix)))


(define (zen:zen-pathname suffix)
  (path-expand (string-append zen:kernel-source suffix)))


;;;
;;;; Restricted
;;;


(define-macro (zen:check-restricted name)
  (let ((unrestricted (%%string->symbol (%%string-append (%%symbol->string name) "-unrestricted"))))
    `(let ((,unrestricted ,name))
       (zen:filesystem-restrict)
       (let ((eq-restricted (if (eq? ,unrestricted ,name) '*** '---)))
         (zen:filesystem-unrestrict)
         (let ((eq-unrestricted (if (eq? ,unrestricted ,name) '--- '$$$)))
           (pp (list eq-restricted eq-unrestricted ',name)))))))


(define (zen:validate-restricted)
  (zen:check-restricted create-directory)
  (zen:check-restricted create-fifo)
  (zen:check-restricted create-link)
  (zen:check-restricted create-symbolic-link)
  (zen:check-restricted rename-file)
  (zen:check-restricted copy-file)
  (zen:check-restricted delete-file)
  (zen:check-restricted delete-directory)
  (zen:check-restricted directory-files)
  (zen:check-restricted file-exists?)
  (zen:check-restricted file-info)
  (zen:check-restricted file-type)
  (zen:check-restricted file-device)
  (zen:check-restricted file-inode)
  (zen:check-restricted file-mode)
  (zen:check-restricted file-number-of-links)
  (zen:check-restricted file-owner)
  (zen:check-restricted file-group)
  (zen:check-restricted file-size)
  (zen:check-restricted file-last-access-time)
  (zen:check-restricted file-last-modification-time)
  (zen:check-restricted file-last-change-time)
  (zen:check-restricted file-attributes)
  (zen:check-restricted file-creation-time)
  (zen:check-restricted file-last-access-and-modification-times-set!)
  (zen:check-restricted open-file)
  (zen:check-restricted open-input-file)
  (zen:check-restricted open-output-file)
  (zen:check-restricted call-with-input-file)
  (zen:check-restricted call-with-output-file)
  (zen:check-restricted with-input-from-file)
  (zen:check-restricted with-output-to-file)
  (zen:check-restricted open-directory))


;;;
;;;; Digest
;;;


(define (zen:updated-digest-source? digest src-filepath)
  (let ((seconds (zen:file-last-modification-seconds src-filepath)))
    (if (= seconds (%%get-digest-seconds digest))
        #f
      (begin
        (%%set-digest-seconds digest seconds)
        (%%set-digest-hash digest (digest-file src-filepath 'SHA-1))
        #t))))


;;;
;;;; Manifest
;;;


(define zen:Manifest-Extension
  "mnf")

(define zen:Digest-Extension
  "dgs")


(define (zen:load-source-digests digest-filepath)
  (if (file-exists? digest-filepath)
      (call-with-input-file (%%list path: digest-filepath eol-encoding: 'cr-lf)
        (lambda (input)
          (let ((digest-forms (read input)))
            (map (lambda (form)
                   (let ((pathname (%%car (%%cdr form)))
                         (source-hash (%%cadr (%%cdr form)))
                         (source-time (%%car (%%cddr (%%cdr form)))))
                     (%%make-digest pathname source-hash source-time)))
                 (%%cdr digest-forms)))))
    '()))


(define (zen:manifest-zen-version manifest)
  (let ((version-alist (%%get-manifest-version manifest)))
    (let ((pair (%%assq 'Zen version-alist)))
      (if pair
          (%%cdr pair)
        #f))))


(define (zen:load-manifest digest-filepath manifest-filepath)
  (if (file-exists? manifest-filepath)
      (call-with-input-file (%%list path: manifest-filepath eol-encoding: 'cr-lf)
        (lambda (input)
          (let ((form (read input)))
            (let ((name (%%cadr form))
                  (version-form (%%assq 'version (%%cddr form)))
                  (digest-form (%%assq 'digest (%%cddr form)))
                  (references-form (%%assq 'references (%%cddr form))))
              (let ((version (let ((lst (%%cdr version-form)))
                               ;; backward compatibility with number version and could be removed in the future
                               (if (and (%%pair? lst)
                                        (%%fixnum? (%%car lst)))
                                   (%%list (%%cons 'Zen (%%car lst)))
                                 lst)))
                    (compile-time-hash (%%cadr digest-form))
                    (source-digests (zen:load-source-digests digest-filepath))
                    (references (if references-form (%%cdr references-form) #f)))
                (%%make-manifest name version compile-time-hash source-digests references))))))
    #f))


(define (zen:save-manifest filepath manifest)
  (let ((name (%%get-manifest-name manifest))
        (version-alist (%%get-manifest-version manifest))
        (references (%%get-manifest-references manifest)))
    (zen:create-directories (zen:pathname-dir filepath))
    (call-with-output-file (list path: filepath eol-encoding: (zen:platform-eol-encoding zen:kernel-platform))
      (lambda (output)
        (display "(manifest " output)
        (display name output)
        (newline output)
        (newline output)
        (display "  (version" output)
        (for-each (lambda (version)
                    (display " " output)
                    (write version output))
                  version-alist)
        (display ")" output)
        (newline output)
        (display "  (digest " output)
        (write (%%get-manifest-compile-time-hash manifest) output)
        (if references
            (begin
              (display ")" output)
              (newline output)
              (display "  (references" output)
              (if (%%pair? references)
                  (begin
                    (display " " output)
                    (write (%%car references) output)
                    (for-each (lambda (module-references)
                                (newline output)
                                (display "              " output)
                                (write module-references output))
                              (%%cdr references))))))
        (display "))" output)
        (newline output)))))


(define (zen:filter proc lst)
  (if (%%null? lst)
      '()
    (let ((head (%%car lst)))
      (if (proc head)
          (%%cons head (zen:filter proc (%%cdr lst)))
        (zen:filter proc (%%cdr lst))))))


(define (zen:save-digest filepath manifest)
  (define (save-digest digests)
    (zen:create-directories (zen:pathname-dir filepath))
    (call-with-output-file (list path: filepath eol-encoding: (zen:platform-eol-encoding zen:kernel-platform))
      (lambda (output)
        (display "(digest" output)
        (newline output)
        (for-each (lambda (digest)
                    (newline output)
                    (display "  (file " output)
                    (write (zen:pathname-normalize (%%get-digest-pathname digest)) output)
                    (display " " output)
                    (write (%%get-digest-hash digest) output)
                    (display " " output)
                    (write (%%get-digest-seconds digest) output)
                    (display ")" output))
                  digests)
        (display ")" output)
        (newline output))))
  
  (let ((existing-files-digests (zen:filter (lambda (digest)
                                               (file-exists? (%%get-digest-pathname digest)))
                                             (%%get-manifest-source-digests manifest))))
    ;; not deleting the digest when there is no existing files makes
    ;; it more robust when multiple processes access the same digest
    (save-digest existing-files-digests)))


(define (zen:find-source-digest src-pathname manifest)
  (define (find-digest)
    (let ((pathname (zen:pathname-normalize src-pathname)))
      (let iter ((digests (%%get-manifest-source-digests manifest)))
           (if (%%pair? digests)
               (let ((digest (%%car digests)))
                 (if (zen:path=? pathname (%%get-digest-pathname digest))
                     digest
                   (iter (%%cdr digests))))
             #f))))
  
  (define (new-digest)
    (let ((digest (%%make-digest (zen:pathname-normalize src-pathname) "" 0)))
      (%%set-manifest-source-digests manifest (%%cons digest (%%get-manifest-source-digests manifest)))
      digest))
  
  (or (find-digest) (new-digest)))


(define (zen:manifest-uptodate? src-pathname manifest)
  (let ((digest (zen:find-source-digest (zen:pathname-normalize src-pathname) manifest)))
    (and (%%string=? (%%get-digest-hash digest) (%%get-manifest-compile-time-hash manifest))
         digest)))


(define zen:manifest-repositories
  #f)

(define zen:manifest-version
  #f)


(define (zen:setup-manifest-repositories)
  (set! zen:manifest-repositories (zen:collect-if (lambda (repository)
                                                      (and (%%not (%%get-repository-binary? repository))
                                                           (let ((versions (zen:load-repository-versions repository)))
                                                             (%%not (%%null? versions)))))
                                                    zen:Repositories))
  (set! zen:manifest-version (map (lambda (repository)
                                     (let ((versions (zen:load-repository-versions repository)))
                                       (%%cons (%%get-repository-name repository)
                                               (zen:get-version-number (%%car versions)))))
                                   zen:manifest-repositories)))


(define (zen:load-repository-versions repository)
  (define (load-versions)
    (if (%%eq? (%%get-repository-name repository) 'Zen)
        (zen:get-zen-versions)
      (let ((file (%%string-append (%%get-repository-directory repository) ".versions")))
        (zen:load-versions-file file))))
  
  (or (%%get-repository-versions repository)
      (let ((versions (or (load-versions) '())))
        (%%set-repository-versions repository versions)
        versions)))


(define (zen:kernel-manifest-version)
  (or zen:manifest-version
      (%%list (%%cons 'Zen zen:kernel-version))))


(define (zen:load/create-manifest name digest-filepath manifest-filepath)
  (or (zen:load-manifest digest-filepath manifest-filepath)
      (%%make-manifest name (zen:kernel-manifest-version) "" '() #f)))


(define (zen:load-updated-manifest name digest-filepath manifest-filepath src-filepath)
  (let ((manifest (zen:load/create-manifest name digest-filepath manifest-filepath)))
    (let ((digest (and src-filepath (zen:find-source-digest src-filepath manifest))))
      (if (and digest
               (zen:updated-digest-source? digest src-filepath))
          (zen:save-digest digest-filepath manifest))
      manifest)))


(define (zen:update-manifest-compile-time name digest-filepath manifest-filepath src-filepath updated-references)
  (let ((manifest (zen:load/create-manifest name digest-filepath manifest-filepath)))
    (%%set-manifest-version manifest (zen:kernel-manifest-version))
    (let ((digest (zen:find-source-digest src-filepath manifest)))
      (zen:updated-digest-source? digest src-filepath)
      (if updated-references
          (%%set-manifest-references manifest updated-references))
      (%%set-manifest-compile-time-hash manifest (%%get-digest-hash digest))
      (zen:save-manifest manifest-filepath manifest))))


;;;
;;;; Library
;;;


(define zen:bin-uniqueness-prefix "bin:")
(define zen:lib-uniqueness-prefix "lib:")
(define zen:product-uniqueness-prefix "product:")


(define zen:Library-Extension "l")
(define zen:Library-Manifest-Extension "lmf"))
