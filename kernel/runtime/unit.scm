;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Unit
;;;


(block kernel.unit


;;;
;;;; Format
;;;


(define (zen:format . rest)
  (define (parse-format rest proc)
    (if (%%string? (%%car rest))
        (proc ':string (%%car rest) (%%cdr rest))
      (proc (%%car rest) (%%cadr rest) (%%cddr rest))))
  
  (define (format-to output fmt-string arguments)
    (let ((control (open-input-string fmt-string))
          (done? #f))
      (define (format-directive)
        (let ((directive (read control)))
          (read-char control)
          (case directive
            ((a)
             (zen:display (%%car arguments) output)
             (set! arguments (%%cdr arguments)))
            ((s)
             (zen:write (%%car arguments) output)
             (set! arguments (%%cdr arguments)))
            ((t)
             (zen:write (%%car arguments) output)
             (set! arguments (%%cdr arguments)))
            ((l)
             (let ((first? #t))
               (for-each (lambda (element)
                           (if first?
                               (set! first? #f)
                             (display " " output))
                           (zen:display element output))
                         (%%car arguments)))
             (set! arguments (%%cdr arguments)))
            ((%)
             (newline output))
            (else
             (zen:kernel-error "Unknown format directive:" directive)))))
      
      (let iter ()
           (let ((c (read-char control)))
             (if (%%not (%%eof-object? c))
                 (begin
                   (cond ((%%eqv? c #\~)
                          (write-char (read-char control) output))
                         ((%%eqv? c #\{)
                          (format-directive))
                         (else
                          (write-char c output)))
                   (iter)))))))
  
  (parse-format rest
    (lambda (port fmt-string arguments)
      (case port
        ((:string)
         (let ((output (open-output-string)))
           (format-to output fmt-string arguments)
           (get-output-string output)))
        (else
         (format-to port fmt-string arguments))))))


(define zen:system-format
  zen:format)


;;;
;;;; Exception
;;;


(define zen:pristine-thread-continuation
  (thread-join!
    (thread-start!
      (%%make-thread
        (lambda ()
          (continuation-capture
            (lambda (cont)
              cont)))))))


;;;
;;;; Error
;;;


(define (zen:kernel-error . rest)
  (%%apply error rest))


(define (zen:raise-system-error fmt-string . rest)
  (let ((error-string (apply zen:format fmt-string rest)))
    (error error-string)))


(zen:define-variable zen:error
  zen:raise-system-error)


(define (zen:primitive-type-error num type proc args)
  (error (zen:format "(Argument {a}) {a} expected :" num type) (%%cons proc args)))


;;;
;;;; List
;;;


(define (zen:some? predicate lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (%%null? scan)
        #f
      (or (predicate (%%car scan))
          (iter (%%cdr scan))))))


(define (zen:every? predicate lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (or (%%null? scan)
        (and (predicate (%%car scan))
             (iter (%%cdr scan))))))


(define zen:kernel-some? zen:some?)
(define zen:kernel-every? zen:every?)


;;;
;;;; Marker
;;;


(define (zen:marker? obj)
  (or (%%eq? obj #!optional)
      (%%eq? obj #!key)
      (%%eq? obj #!rest)))


;;;
;;;; Product
;;;


(define zen:kernel-built
  zen:built)


(define zen:kernel-path
  (and (%%eq? zen:image 'executable) zen:executable-path (zen:executable-path)))


(define zen:kernel-install
  (or (and (cond-expand (ios #t) (else (%%eq? zen:image 'executable))) zen:executable-directory (zen:executable-directory))
      (and (zen:global-bound? '*kernel-install*)
           (zen:pathname-normalize (zen:global-ref '*kernel-install*)))
      (if (file-exists? zen:built)
          (zen:pathname-normalize zen:built)
        (error "Unable to determine kernel install"))))


(define zen:kernel-bundle-contents
  (if zen:bundle-depth
      (zen:nth-parent-directory zen:kernel-install 1)
    #f))


(define zen:kernel-bundle-root
  (if zen:bundle-depth
      (zen:nth-parent-directory zen:kernel-install (%%fx- zen:bundle-depth 1))
    #f))


(define zen:kernel-bundle-install
  (if zen:bundle-depth
      (zen:nth-parent-directory zen:kernel-install zen:bundle-depth)
    #f))


(define zen:kernel-root
  (or zen:kernel-bundle-root zen:kernel-install))


(define (zen:install-path filename)
  (if (%%not filename)
      zen:kernel-install
    (%%string-append zen:kernel-install filename)))


(define zen:kernel-source-built
  zen:source-built)


(define zen:kernel-source-accessible?
  zen:source-access?)


(define zen:kernel-source-access?
  zen:source-access?)


(define zen:kernel-zenini-access?
  zen:zenini-access?)


(define zen:kernel-source
  (if (and zen:kernel-source-access? zen:source)
      (zen:absolutize-directory zen:kernel-install zen:source)
    #f))


(set! zen:zen-source zen:kernel-source)


;; quick solution that assumes the zen repo is at the toplevel
(define zen:kernel-source-root
  (if zen:kernel-source
      (zen:parent-directory zen:kernel-source)
    #f))


;; quick solution to build needing source
(define (zen:setup-kernel-source)
  (set! zen:kernel-source-access? #t)
  (set! zen:kernel-source (zen:absolutize-directory zen:kernel-install zen:source))
  (set! zen:zen-source zen:kernel-source))


(define zen:kernel-binary-repositories
  zen:binary-repositories)


(define zen:kernel-source-repositories
  zen:source-repositories)


(define zen:kernel-windows-homedir
  ;; the build system needs access to home
  (if (%%not zen:product)
      #f
    zen:windows-homedir))


(define (zen:zen-product)
  zen:product)


(define (zen:zen-profile)
  (zen:profile))


(define zen:profile
  (%%make-parameter #f))


;; to enable timing loading of an application
(define zen:run-loop?
  (%%make-parameter #t))


;;;
;;;; Repository
;;;


(define zen:Repository-Filename
  ".repository")


(zen:define-variable zen:Repositories
  '())

(zen:define-variable zen:Build-Repository
  #f)


(define (zen:repositories-get)
  zen:Repositories)

(define (zen:build-repository-get)
  zen:Build-Repository)


(define (zen:prepare-repositories)
  (define (all-repositories build)
    (let ((source-access? zen:kernel-source-access?))
      (let ((zen (and source-access? (zen:make-repository 'Zen "lib" (or (zen:zen-repository) zen:kernel-source))))
            (binary-repositories (or zen:kernel-binary-repositories '()))
            (source-repositories (or (and source-access? zen:kernel-source-repositories) '()))
            (binary-dynamic-repositories '()) ;; dynamic-dependencies (or (zen:dependencies) '())
            (source-dynamic-repositories '()) ;; dynamic-dependencies (or (and source-access? (zen:dependencies)) '())
            (repositories (zen:repositories)))
        (let ((binary-dynamic-root (zen:parent-directory (zen:parent-directory zen:kernel-install)))
              (source-dynamic-root (zen:parent-directory (zen:parent-directory zen:source))))
          (if (and (or binary-dynamic-root (null? binary-dynamic-repositories))
                   (or source-dynamic-root (null? source-dynamic-repositories)))
              (let ((all '()))
                (define (add repo)
                  ;; quicky around some repositories being in the list more than once
                  (let ((name (%%get-repository-name repo))
                        (binary? (%%get-repository-binary? repo)))
                    (if (%%not (zen:find-if (lambda (repo)
                                               (and (%%eq? (%%get-repository-name repo) name)
                                                    (%%eqv? (%%get-repository-binary? repo) binary?)))
                                             all))
                        (set! all (%%cons repo all)))))
                
                (if build
                    (add build))
                (if zen
                    (add zen))
                (if repositories
                    (for-each (lambda (dir)
                                (add (zen:load-repository dir)))
                              (zen:split-string repositories #\;)))
                (for-each (lambda (path)
                            (let ((dir (zen:absolutize-directory zen:kernel-install path)))
                              (add (zen:load-repository dir name: 'Binaries error?: #f))))
                          binary-repositories)
                (for-each (lambda (path)
                            (let ((dir (zen:absolutize-directory zen:kernel-install path)))
                              (if dir
                                  (add (zen:load-repository dir error?: #f)))))
                          source-repositories)
                #; ;; dynamic-dependencies
                (for-each (lambda (entry)
                            (let ((name (%%car entry))
                                  (branch (%%cadr entry)))
                              ;; add source dynamic repositories in $SOURCE/$REPO/$BRANCH
                              (let ((path (%%string-append source-dynamic-root (zen:join-strings (list name branch) #\/))))
                                (add (zen:load-repository path name: name error?: #f)))))
                          source-dynamic-repositories)
                #; ;; dynamic-dependencies
                (for-each (lambda (entry)
                            (let ((name (%%car entry))
                                  (branch (%%cadr entry)))
                              ;; add binary dynamic repositories in $BINARY/$REPO/$BRANCH
                              (let ((path (%%string-append binary-dynamic-root (zen:join-strings (list name branch) #\/))))
                                (add (zen:load-repository path name: name error?: #f)))))
                          binary-dynamic-repositories)
                (reverse all))
            (zen:error "Invalid .dependencies"))))))
  
  (define (build-directory/lib)
    (if (%%eq? zen:kernel-platform 'mac)
        (cond ((and zen:kernel-bundle-install (file-exists? (%%string-append zen:kernel-bundle-install "Libraries/lib")))
               (values zen:kernel-bundle-install "Libraries/lib"))
              ((and zen:kernel-bundle-contents (file-exists? (%%string-append zen:kernel-bundle-contents "Libraries/lib")))
               (values zen:kernel-bundle-contents "Libraries/lib"))
              (else
               (values (or (zen:build-repository) zen:kernel-install) "Libraries/lib")))
      (values (or (zen:build-repository) zen:kernel-install) "lib")))
  
  (receive (directory lib) (build-directory/lib)
    (let ((build (zen:make-repository 'Build lib directory binary?: #t dynamic?: #t)))
      (set! zen:Build-Repository build)
      (set! zen:Repositories (%%append zen:Repositories (all-repositories build))))))


(define (zen:make-repository name library directory #!key (binary? #f) (dynamic? #f))
  (define (create-repository repository-file)
    (call-with-output-file (list path: repository-file eol-encoding: (zen:platform-eol-encoding zen:kernel-platform))
      (lambda (output)
        (display "(repository " output)
        (display name output)
        (if (or binary? library)
            (begin
              (newline output)
              (newline output)))
        (if binary?
            (begin
              (display "  (binary? #t)" output)
              (newline output)))
        (if library
            (begin
              (display "  (library " output)
              (write library output)
              (display ")" output)))
        (display ")" output)
        (newline output))))
  
  (define (repository-form)
    `(repository ,name
       ,@(if binary?
             `((binary? ,binary?))
           '())
       ,@(if library
             `((library ,library))
           '())))
  
  (define (repository-inexistant)
    (zen:error "{a} repository is inexistant: {a}" name directory))
  
  (if (or (%%not directory) (%%equal? directory "none"))
      #f
    (let ((directory (zen:dirname-normalize directory)))
      (let ((repository-file (%%string-append directory zen:Repository-Filename)))
        (cond ((file-exists? repository-file)
               (zen:load-repository directory))
              (dynamic?
               (if (%%not zen:product)
                   (begin
                     (zen:create-directories directory)
                     (create-repository repository-file)
                     (if (file-exists? repository-file)
                         (zen:load-repository directory)
                       (repository-inexistant)))
                 (zen:load-repository-form directory (repository-form))))
              (else
               (repository-inexistant)))))))


(define (zen:load-repository directory #!key (name #f) (error? #t))
  (define (load-repository directory)
    (if (file-exists? directory)
        (let ((repository-file (%%string-append directory zen:Repository-Filename)))
          (if (file-exists? repository-file)
              (call-with-input-file (%%list path: repository-file eol-encoding: 'cr-lf)
                (lambda (input)
                  (let ((form (read input)))
                    (zen:load-repository-form directory form name: name))))
            #f))
      #f))
  
  (define (repository-inexistant)
    (zen:error "Repository is inexistant: {a}" directory))
  
  (let ((directory (zen:dirname-normalize directory)))
    (or (load-repository directory)
        (if error?
            (repository-inexistant)
          #f))))


(define (zen:load-repository-form directory form #!key (name #f))
  (let ((name (or name (%%cadr form)))
        (alist (%%cddr form)))
    (let ((directory (zen:pathname-normalize directory))
          (binary-pair (%%assq 'binary? alist))
          (library-pair (%%assq 'library alist))
          (dependencies-pair (%%assq 'dependencies alist)))
      (let ((binary? (if binary-pair (%%cadr binary-pair) #f))
            (library-root (if library-pair (%%cadr library-pair) #f))
            (dependencies (if dependencies-pair (%%cdr dependencies-pair) '())))
        (let ((library-directory (if (%%not library-root) directory (%%string-append directory library-root "/"))))
          (%%make-repository name directory library-root library-directory binary? #f dependencies #f))))))


(define (zen:install-repository directory/repository #!key (name #f))
  (let ((repository (if (zen:repository? directory/repository) directory/repository (zen:load-repository directory/repository name: name))))
    (set! zen:Repositories (%%append zen:Repositories (%%list repository)))
    (if zen:setup-repositories-called?
        (zen:setup-repository repository))
    repository))


(define (zen:install-repository-if-exists directory #!key (name #f))
  (if (file-exists? directory)
      (zen:install-repository directory name: name)))


(define (zen:uninstall-repository repository)
  (set! zen:Repositories (%%remove repository zen:Repositories)))


(define (zen:require-repository name)
  (or (zen:find-repository name)
      (zen:error "Unknown repository: {s}" name)))


(define (zen:find-repository name)
  (let iter ((repositories zen:Repositories))
    (if (%%null? repositories)
        #f
      (let ((repository (%%car repositories)))
        (if (%%eq? (%%get-repository-name repository) name)
            repository
          (iter (%%cdr repositories)))))))


(define (zen:find-package package-name)
  (let iter ((repositories zen:Repositories))
    (if (%%null? repositories)
        #f
      (let ((repository (%%car repositories)))
        (if (%%get-repository-binary? repository)
            (iter (%%cdr repositories))
          (let ((package (zen:repository-find-package repository package-name)))
            (if package
                package
              (iter (%%cdr repositories)))))))))


(define (zen:repository-pathname repository path)
  (%%string-append (%%get-repository-library-directory repository)
                   path))


(define (zen:repository-packages-table repository)
  (or (%%get-repository-packages-table repository)
      (let ((table (%%make-table test: eq?)))
        (%%set-repository-packages-table repository table)
        (zen:repository-install-packages repository)
        table)))


(define (zen:sweep-build-repository remove-packages)
  (if zen:Build-Repository
      (begin
        (zen:feedback "; sweeping binaries...")
        (let ((table (zen:repository-packages-table zen:Build-Repository)))
          (zen:table-iterate-safe table
            (lambda (name package)
              ;; when each repository has its own build repository we can then safely remove the dangling binary package
              (cond ((zen:find-package name)
                     (zen:sweep-build-package package))
                    ((and (pair? remove-packages) (memq name remove-packages))
                     (zen:remove-build-package name)))))))))


(define (zen:sweep-build-package package)
  (let ((units (%%apply append (map (lambda (descriptor)
                                      (let ((name (zen:product-descriptor-name descriptor)))
                                        (let ((update (zen:cond-expanded-product-descriptor-update name descriptor)))
                                          (%%apply append (map (lambda (unit-name)
                                                                 (zen:get-subunit-names unit-name walk-continue?: #t))
                                                               update)))))
                                    (%%get-package-products package)))))
    (let ((resources (map (lambda (unit-name) (zen:find-unit-src unit-name)) units)))
      (let ((referenced (map (lambda (resource) (%%get-resource-path resource)) resources))
            (root (zen:package-root-pathname package "")))
        (define (sweep-file pathname path)
          (let ((spine (zen:pathname-spine path)))
            (if (%%not (zen:some? (lambda (ref)
                                     (%%equal? ref spine))
                                   referenced))
                (begin
                  (zen:feedback "; removing {a}..." path)
                  (if (%%not (zen:dry-run?))
                      (delete-file pathname))))))
        
        (define (sweep-directory pathname path)
          (if (%%not (or (%%equal? path "")
                         (zen:some? (lambda (ref)
                                       (zen:string-starts-with? ref path))
                                     referenced)))
              (begin
                (zen:feedback "; removing {a}..." path)
                (if (%%not (zen:dry-run?))
                    (zen:delete-directory pathname)))
            (for-each (lambda (name)
                        (let ((pathname (%%string-append pathname name))
                              (path (%%string-append path name)))
                          (case (zen:pathname-type pathname)
                            ((file)
                             (sweep-file pathname path))
                            ((directory)
                             (sweep-directory (%%string-append pathname "/") (%%string-append path "/"))))))
                      (zen:directory-content pathname))))
        
        (if (zen:pathname-exists? root)
            (sweep-directory root ""))))))


(define (zen:remove-build-package name)
  (if zen:Build-Repository
      (let ((library-directory (%%get-repository-library-directory zen:Build-Repository)))
        (let ((package-directory (%%string-append library-directory (%%symbol->string name) "/")))
          (if (file-exists? package-directory)
              (begin
                (zen:feedback "; removing {a}..." name)
                (if (%%not (zen:dry-run?))
                    (begin
                      (zen:delete-directory package-directory)
                      (%%table-clear (zen:repository-packages-table zen:Build-Repository) name)))))))))


(zen:define-variable zen:setup-repositories-called?
  #f)


(define (zen:setup-repositories)
  (for-each zen:setup-repository zen:Repositories)
  (zen:setup-manifest-repositories)
  (set! zen:setup-repositories-called? #t))


(define (zen:setup-repository repository)
  (let ((table (zen:repository-packages-table repository)))
    (zen:table-iterate-safe table
      (lambda (name package)
        (zen:setup-package package)))
    #; ;; this removes the randomness of scanning an eq? table
       ;; and can sometimes be usefull for debugging purposes
    (for-each (lambda (pair)
                (zen:setup-package (%%cdr pair)))
              (zen:sort-list (lambda (x y)
                                (%%string<? (%%symbol->string (%%car x))
                                            (%%symbol->string (%%car y))))
                              (%%table->list table)))))


(define (zen:repository-packages repository)
  (let ((table (zen:repository-packages-table repository))
        (packages '()))
    (zen:table-iterate table
      (lambda (name package)
        (set! packages (%%cons package packages))))
    packages))


(define (zen:repository-find-package repository package-name)
  (%%table-ref (zen:repository-packages-table repository) package-name #f))


(define (zen:repository-install-packages repository)
  (define (repository-discover-packages repository)
    (let ((table (%%get-repository-packages-table repository)))
      (define (discover-packages parent library-directory packages)
        (if (file-exists? library-directory)
            (let iter ((dirnames (zen:directory-directories library-directory))
                       (packages packages))
                 (if (%%null? dirnames)
                     packages
                   (let ((dirname (%%car dirnames)))
                     (let ((directory (%%string-append library-directory dirname "/")))
                       (let ((package-pathname (%%string-append directory zen:Package-Filename)))
                         (if (file-exists? package-pathname)
                             (let ((package-name (%%string->symbol dirname)))
                               (if (%%table-ref table package-name #f)
                                   (iter (%%cdr dirnames) packages)
                                 (let ((package (if (%%eq? repository zen:Build-Repository)
                                                    (let ((source-package (zen:find-package package-name)))
                                                      (if source-package
                                                          (zen:load/create-build-package source-package)
                                                        ;; when each repository has its own build repository we can then safely remove the dangling binary package
                                                        (zen:load-package repository parent package-name package-pathname)))
                                                  (zen:load-package repository parent package-name package-pathname))))
                                   (iter (%%cdr dirnames) (%%cons package (let ((library-path (%%get-package-library-path package)))
                                                                            (if library-path
                                                                                (let ((library-directory (zen:repository-pathname (%%get-package-repository package) (%%string-append library-path "/"))))
                                                                                  (discover-packages package library-directory packages))
                                                                              packages)))))))
                           (iter (%%cdr dirnames) packages)))))))
          packages))
      
      (discover-packages #f (%%get-repository-library-directory repository) '())))
  
  (let ((table (%%get-repository-packages-table repository))
        (packages (repository-discover-packages repository)))
    (for-each (lambda (package)
                (%%table-set! table (%%get-package-name package) package))
              packages)
    packages))


(define (zen:repository-add-package repository package)
  (let ((table (zen:repository-packages-table repository)))
    (%%table-set! table (%%get-package-name package) package)))


(define (zen:repository-remove-package repository package)
  (let ((table (zen:repository-packages-table repository)))
    (%%table-clear table (%%get-package-name package))))


(define (zen:load-package repository parent package-name package-pathname)
  (call-with-input-file (%%list path: package-pathname eol-encoding: 'cr-lf)
    (lambda (input)
      (let ((form (read input)))
        (let ((name (%%cadr form))
              (alist (%%cddr form)))
          (if (%%eq? name package-name)
              (let ((library (%%assq 'library alist))
                    (root (%%assq 'root alist))
                    (install (%%assq 'install alist))
                    (char-encoding (%%assq 'char-encoding alist))
                    (products (%%assq 'products alist))
                    (profiles (%%assq 'profiles alist))
                    (project (%%assq 'project alist))
                    (title (%%assq 'title alist))
                    (description (%%assq 'description alist))
                    (authors (%%assq 'authors alist))
                    (stage (%%assq 'stage alist)))
                (let ((package
                        (zen:make-package repository name parent
                          (if library (%%cadr library) #f)
                          (if root (%%cadr root) #f)
                          (if install (%%cadr install) #f)
                          (if char-encoding (%%cadr char-encoding) #f)
                          (if products (%%cdr products) '())
                          (if profiles (%%cdr profiles) '())
                          (if project (%%cadr project) #f)
                          (if title (%%cadr title) #f)
                          (if description (%%cadr description) #f)
                          (if authors (%%cdr authors) #f)
                          (if stage (%%cadr stage) #f))))
                  (zen:cache-package-roots package)
                  package))
            (zen:error "Package at {s} is defining: {s}" package-pathname name)))))))


(define (zen:load/create-build-package package)
  (let* ((name (%%get-package-name package))
         (parent (%%get-package-parent package))
         (bin-parent (if parent (zen:load/create-build-package parent) #f))
         (dir (%%string-append (if parent (%%string-append (%%get-package-library-path parent) "/") "") (%%symbol->string name) "/"))
         (path (%%string-append dir zen:Package-Filename))
         (src (zen:repository-pathname (%%get-package-repository package) path))
         (dst (zen:repository-pathname zen:Build-Repository path)))
    (define (updated-uptodate?)
      (if (file-exists? dst)
          (if (= (zen:file-last-modification-seconds src) (zen:file-last-modification-seconds dst))
              #t
            (if (%%string=? (digest-file src 'SHA-1) (digest-file dst 'SHA-1))
                (begin
                  (file-last-access-and-modification-times-set! dst (file-last-access-time src) (file-last-modification-time src))
                  #t)
              #f))
        #f))
    
    (define (load-package)
      (let ((package (zen:load-package zen:Build-Repository bin-parent name dst)))
        (%%table-set! (zen:repository-packages-table zen:Build-Repository)
                      name
                      package)
        package))
    
    (if (updated-uptodate?)
        (or (zen:repository-find-package zen:Build-Repository name)
            (load-package))
      (begin
        (zen:create-directories (zen:repository-pathname zen:Build-Repository dir))
        (if (file-exists? dst)
            (delete-file dst))
        (zen:copy-file src dst)
        (load-package)))))


(define (zen:setup-package package)
  (let ((install (%%get-package-install package)))
    (if install
        (zen:load-unit install))))


(define (zen:inspect-install)
  (define (inspect-path path)
    `(:path ,path ,(path-expand path)))
  
  (define (inspect-repository repository)
    `(:repository
      ,(%%get-repository-name repository)
      ,(%%get-repository-library-directory repository)
      ,@(map inspect-package (zen:repository-packages repository))))
  
  (define (inspect-package package)
    `(:package
      ,(%%get-package-name package)
      ,(%%get-package-units-root package)
      ,(%%get-package-units-path package)))
  
  `(,(inspect-path "./")
    ,(inspect-path "~/")
    ,@(map inspect-repository zen:Repositories)))


;;;
;;;; Package
;;;


(define zen:Package-Filename
  ".package")


(define (zen:make-package repository name parent library-root units-root install char-encoding products profiles project title description authors stage)
  (let ((library-path (if (%%not library-root)
                          #f
                        (%%string-append (%%symbol->string name) "/" library-root)))
        (units-path (if (%%not units-root)
                        (%%symbol->string name)
                      (%%string-append (%%symbol->string name) "/" units-root))))
    (%%make-package repository name parent library-root library-path units-root units-path install char-encoding products profiles project title description authors stage)))


(define (zen:package-root package)
  (let ((parent (%%get-package-parent package)))
    (%%string-append (let ((library-root (%%get-repository-library-root (%%get-package-repository package))))
                       (if library-root
                           (%%string-append library-root "/")
                         ""))
                     (if parent (%%string-append (%%get-package-library-path parent) "/") "")
                     (%%symbol->string (%%get-package-name package)))))


(define (zen:package-units-root package)
  (let ((root (zen:package-root package))
        (units-root (%%get-package-units-root package)))
    (if (%%not units-root)
        root
      (%%string-append root "/" units-root))))


(define (zen:package-pathname package path)
  (zen:repository-pathname (%%get-package-repository package)
                            (%%string-append (%%symbol->string (%%get-package-name package)) "/" path)))


(define (zen:package-root-pathname package path)
  (zen:relocate-package-pathname (%%get-package-repository package) package path))


(define (zen:relocate-package-pathname repository package path)
  (let ((parent (%%get-package-parent package)))
    (zen:repository-pathname repository
      (%%string-append (if parent (%%string-append (%%get-package-library-path parent) "/") "")
                       (%%get-package-units-path package)
                       "/"
                       (or path "")))))


(define (zen:iterate-packages binary? proc)
  (let iter-repo ((repositories zen:Repositories))
    (if (%%not (%%null? repositories))
        (let ((repository (%%car repositories)))
          (if (%%neq? binary? (%%get-repository-binary? repository))
              (iter-repo (%%cdr repositories))
            (let iter ((packages (zen:repository-packages repository)))
              (if (%%null? packages)
                  (iter-repo (%%cdr repositories))
                (let ((package (%%car packages)))
                  (proc package)
                  (iter (%%cdr packages))))))))))


(define (zen:find-resource pathname)
  (let iter-repo ((repositories zen:Repositories))
    (if (%%null? repositories)
        #f
      (let iter ((packages (zen:repository-packages (%%car repositories))))
        (if (%%null? packages)
            (iter-repo (%%cdr repositories))
          (let ((package (%%car packages)))
            (let ((package-pathname (zen:package-root-pathname package "")))
              (let ((pathname-length (%%string-length pathname))
                    (package-length (%%string-length package-pathname)))
                (if (and (%%fx<= package-length pathname-length)
                         (%%string=? (%%substring pathname 0 package-length) package-pathname))
                    (let* ((path (%%substring pathname package-length pathname-length))
                           (underscore? #f)
                           (extension (and path (zen:pathname-extension path))))
                      ;; remove extension
                      (if extension
                          (set! path (%%substring path 0 (%%fx- (%%string-length path) (%%fx+ 1 (%%string-length extension))))))
                      ;; remove redundant underscore prefixed name
                      (let ((len (%%string-length path))
                            (pos (zen:string-find-reversed path #\/)))
                        (if pos
                            (let ((name-pos (%%fx+ pos 2)))
                              (if (and (%%fx< name-pos len) (%%eqv? (%%string-ref path (%%fx+ pos 1)) #\_))
                                  (let* ((name (%%substring path name-pos len))
                                         (name-length (%%string-length name))
                                         (previous-pos (%%fx- pos name-length)))
                                    (if (and (%%fx>= previous-pos 0) (%%string=? (%%substring path previous-pos pos) name))
                                        (begin
                                          (set! path (%%substring path 0 pos))
                                          (set! underscore? #t))))))))
                      (%%make-resource package path underscore? extension))
                  (iter (%%cdr packages)))))))))))


(define (zen:find-pathname-unit pathname)
  (let ((resource (zen:find-resource pathname)))
    (if resource
        (zen:path->name (%%get-resource-path resource))
      #f)))


;;;
;;;; Profiles
;;;


(define (zen:gather-profiles)
  (let iter-repo ((repositories zen:Repositories) (profiles '()))
    (if (%%null? repositories)
        profiles
      (let ((repository (%%car repositories)))
        (if (%%get-repository-binary? repository)
            (iter-repo (%%cdr repositories) profiles)
          (let iter ((packages (zen:repository-packages repository)) (profiles profiles))
            (if (%%null? packages)
                (iter-repo (%%cdr repositories) profiles)
              (let ((package (%%car packages)))
                (let ((package-profiles (%%get-package-profiles package)))
                  (iter (%%cdr packages) (%%append (map (lambda (package-profile)
                                                          (%%cons package package-profile))
                                                        package-profiles)
                                                   profiles)))))))))))


(define (zen:make-profile name unit-name)
  `(,name (unit ,unit-name)))


(define (zen:profile-name profile)
  (%%car profile))

(define (zen:profile-title profile)
  (%%symbol->string (zen:profile-name profile)))

(define (zen:profile-appl profile)
  (let ((pair (%%assq 'appl (%%cdr profile))))
    (if (%%not pair)
        #f
      (%%cadr pair))))

(define (zen:profile-unit profile)
  (let ((pair (%%assq 'unit (%%cdr profile))))
    (if (%%not pair)
        (zen:error "Unable to find unit in profile: {s}" profile)
      (%%cadr pair))))


;;;
;;;; Unit
;;;


(define (zen:lower-case-unit-name? unit-name)
  (define (first-char-of-last-name str)
    (let loop ((offset (%%fx- (%%string-length str) 1)))
         (let ((ch (%%string-ref str offset)))
           (cond ((%%char=? #\. (%%string-ref str offset))
                  (if (%%fx< (%%fx+ offset 1) (%%string-length str))
                      (%%string-ref str (%%fx+ 1 offset))
                    (zen:error "Unit name {a} ends with a ." str)))
                 ((%%fx= offset 0)
                  ch)
                 (else
                  (loop (%%fx- offset 1)))))))
  
  (let ((first-char (first-char-of-last-name (%%symbol->string unit-name))))
    (and (%%char<=? #\a first-char) (%%char<=? first-char #\z))))


(define (zen:find-unit-src unit-name #!key (extensions #f) (error? #t))
  (define (find-src package path)
    (define (try path underscore?)
      (define (try-extension extension)
        (if (file-exists? (zen:package-root-pathname package (%%string-append path "." extension)))
            (%%make-resource package path underscore? extension)
          #f))
      
      (let iter ((extensions (or extensions '("zen" "scm"))))
           (if (%%null? extensions)
               #f
             (or (try-extension (%%car extensions))
                 (iter (%%cdr extensions))))))
    
    (if (and (file-exists? (zen:package-root-pathname package path))
             ;; why was this arbitrary test necessary?
             #;
             (zen:lower-case-unit-name? unit-name))
        (try (%%string-append path "/_" (zen:pathname-name path)) #t)
      (try path #f)))
  
  (continuation-capture
    (lambda (return)
      (let ((path (zen:name->path unit-name)))
        (for-each (lambda (package)
                    (let ((src (find-src package path)))
                      (if src
                          (continuation-return return src))))
                  (zen:cached-packages zen:*source-packages-cache* unit-name))
        (zen:iterate-packages #f
          (lambda (package)
            (let ((src (find-src package path)))
              (if src
                  (begin
                    (zen:cache-package zen:*source-packages-cache* unit-name package)
                    #; ;; test
                    (zen:validate-repository-unicity (%%get-package-repository package)
                                                      unit-name
                                                      (lambda (package)
                                                        (find-src package path)))
                    (continuation-return return src)))))))
      (if error?
          (zen:error "Unable to find unit source: {s}" unit-name)
        #f))))


(define (zen:require-unit-src unit-name)
  (zen:find-unit-src unit-name error?: #t))


(define (zen:with-unit-resources unit-name extensions proc)
  (define force-interpreted?
    (let ((interpreted? (zen:force-interpreted?)))
      (cond ((%%boolean? interpreted?)
             interpreted?)
            ((%%symbol? interpreted?)
             (%%eq? unit-name interpreted?))
            (else
             (%%memv unit-name interpreted?)))))
  
  (define (find-unit-binaries src)
    (define (find package path extension)
      (define (try path underscore?)
        ;; we only test .o1 and let gambit find the right file by returning no extension when found
        (if (file-exists? (zen:package-root-pathname package (%%string-append path extension)))
            (%%make-resource package path underscore? #f)
          #f))
      
      (if (and (file-exists? (zen:package-root-pathname package path))
               (zen:lower-case-unit-name? unit-name))
          (try (%%string-append path "/_" (zen:pathname-name path)) #t)
        (try path #f)))
    
    (define (find-uptodate package path)
      (let ((obj (find package path ".o"))
            (bin (find package path ".o1")))
        (let ((manifest (let ((obj/bin (or obj bin)))
                          (and obj/bin
                               (zen:load-updated-manifest
                                 unit-name
                                 (zen:digest-pathname (%%get-resource-package obj/bin) obj/bin)
                                 (zen:manifest-pathname (%%get-resource-package obj/bin) obj/bin)
                                 (and src (zen:resource-pathname src)))))))
          (let ((uptodate? (or (%%not src)
                               (and manifest
                                    (zen:manifest-uptodate? (zen:resource-pathname src) manifest)
                                    (%%not (zen:manifest-needs-rebuild? manifest))))))
            (if uptodate?
                (values obj
                        (if force-interpreted? #f bin)
                        manifest)
              (values #f #f #f))))))
    
    (continuation-capture
      (lambda (return)
        (let ((path (zen:name->path unit-name)))
          (for-each (lambda (package)
                      (receive (obj bin manifest) (find-uptodate package path)
                        (if manifest
                            (continuation-return return (values obj bin manifest)))))
                    (zen:cached-packages zen:*binary-packages-cache* unit-name))
          (zen:iterate-packages #t
            (lambda (package)
              (receive (obj bin manifest) (find-uptodate package path)
                (if manifest
                    (begin
                      (zen:cache-package zen:*binary-packages-cache* unit-name package)
                      #; ;; test
                      (zen:validate-repository-unicity (%%get-package-repository package)
                                                        unit-name
                                                        (lambda (package)
                                                          (find-bin package path)))
                      (continuation-return return (values obj bin manifest))))))))
        (values #f #f #f))))
  
  (let ((src (zen:find-unit-src unit-name extensions: extensions error?: #f))
        (image-unit (zen:get-image-unit unit-name)))
    (receive (obj bin manifest) (find-unit-binaries src)
      (let ((obj-uptodate? (if obj #t #f))
            (bin-uptodate? (if bin #t #f))
            (lib-uptodate?
              (and image-unit
                   (or (%%not (or src bin))
                       ; validate lib against manifest
                       (and manifest
                            (zen:image-unit-uptodate? image-unit src manifest)
                            (%%not (zen:manifest-needs-rebuild? manifest))
                            (%%not force-interpreted?))
                       ; validate lib against source
                       (and src
                            (zen:image-unit-uptodate-src? image-unit src)
                            (%%not force-interpreted?))))))
        (proc src
              obj
              bin
              (if image-unit (%%get-image-unit-load-proc image-unit) #f)
              obj-uptodate?
              bin-uptodate?
              lib-uptodate?
              manifest)))))


(define (zen:unit-status unit-name #!key (all? #f))
  (zen:with-unit-resources unit-name #f
    (lambda (src obj bin load-proc obj-uptodate? bin-uptodate? lib-uptodate? manifest)
      (let ((loaded? (zen:unit-loaded? unit-name)))
        (display "unit ") (display unit-name) (newline)
        ;(display "src package pathname: ") (and src (display (zen:resource-package-pathname src))) (newline)
        ;(display "bin package pathname: ") (and bin (display (zen:resource-package-pathname bin))) (newline)
        (if src
            (begin
              (display "source hash: ")
              (display (digest-file (zen:resource-pathname src) 'SHA-1))
              (newline))
          (begin (display "source not found") (newline)))
        (if all?
            (begin
              (if obj (begin (display "obj ") (display (zen:resource-pathname obj)) (newline)))
              (if bin (begin (display "bin ") (display (zen:resource-pathname bin)) (newline)))))
        (display "obj-uptodate? ") (display obj-uptodate?) (newline)
        (display "bin-uptodate? ") (display bin-uptodate?) (newline)
        (display "lib-uptodate? ") (display lib-uptodate?) (newline)
        (if (and bin zen:manifest-valid?)
            (begin
              (display "manifest-valid? ") (display (zen:manifest-valid? bin)) (newline)))
        (display "loaded? ") (display loaded?) (newline)))))


(define (zen:unit-uptodate-binary? unit-name)
  (zen:with-unit-resources unit-name #f
    (lambda (src obj bin lib obj-uptodate? bin-uptodate? lib-uptodate? manifest)
      (or bin-uptodate? lib-uptodate?))))


(define (zen:image-unit-uptodate? image-unit src manifest)
  (or (%%not src)
      (let ((digest (zen:find-source-digest (zen:resource-pathname src) manifest)))
        (and digest (%%string=? (%%get-image-unit-compile-time-hash image-unit) (%%get-digest-hash digest))))))


(define (zen:image-unit-uptodate-src? image-unit src)
  (let ((source-hash (digest-file (zen:resource-pathname src) 'SHA-1)))
    (%%string=? (%%get-image-unit-compile-time-hash image-unit) source-hash)))


(define (zen:validate-repository-unicity repository unit-name proc)
  (define (repository-unique? repository proc)
    (let iter ((packages (zen:repository-packages repository))
               (found? #f))
         (if (%%null? packages)
             #t
           (let ((package (%%car packages)))
             (if (proc package)
                 (if found?
                     #f
                   (iter (%%cdr packages) #t))
               (iter (%%cdr packages) found?))))))
  
  (if (%%not (repository-unique? repository proc))
      (zen:error "Found duplicate resource in {a} repository: {s}"
                  (or (%%get-repository-name repository) "anonymous")
                  unit-name)))


;;;
;;;; Cache
;;;


;; This is a cache of
;;   - every subdirectory of level 2 and
;;   - every subdirectory of level 1 that contains files
;; It associates the directory name as a symbol to a list of packages containing it
;; Stopping at level 2 is a heuristic decision to minimize cache size and the need to refresh
;; it while going deep enough so directory names are unique in a high percentage of cases


(define zen:*binary-packages-cache*
  (%%make-table test: eq?))

(define zen:*source-packages-cache*
  (%%make-table test: eq?))


(define (zen:cache-package cache unit-name package)
  (zen:with-cached-prefix unit-name
    (lambda (prefix singleton-prefix)
      (let ((packages (%%table-ref cache prefix '())))
        (if (%%not (%%memq package packages))
            (%%table-set! cache prefix (%%cons package packages)))))))


(define (zen:cached-packages cache unit-name)
  (zen:with-cached-prefix unit-name
    (lambda (prefix singleton-prefix)
      (or (%%table-ref cache prefix #f)
          (if singleton-prefix
              (%%table-ref cache singleton-prefix '())
            '())))))


;; return as symbols the first 1 or 2 parts and the first part if there exactly 2 parts
(define (zen:with-cached-prefix unit-name proc)
  (let ((name (%%symbol->string unit-name)))
    (let ((first-period (zen:string-find name #\.)))
      (if first-period
          (let ((second-period (zen:string-find name #\. (%%fx+ first-period 1))))
            (if second-period
                (proc (%%string->symbol (%%substring name 0 second-period)) #f)
              (proc unit-name (%%string->symbol (%%substring name 0 first-period)))))
        (proc unit-name #f)))))


;; cache every subdirectory of level 2 and every subdirectory of level 1 that contains files
(define (zen:cache-package-roots package)
  (let ((cache (if (%%get-repository-binary? (%%get-package-repository package))
                   zen:*binary-packages-cache*
                 zen:*source-packages-cache*))
        (toplevel-dir (zen:package-root-pathname package "")))
    (if (file-exists? toplevel-dir)
        (for-each (lambda (first-part)
                    (let ((first-dir (string-append toplevel-dir first-part "/")))
                      (if (file-exists? first-dir)
                          (let ((has-files? #f))
                            (for-each (lambda (second-part)
                                        (let ((second-path (%%string-append first-dir second-part)))
                                          (case (zen:pathname-type second-path)
                                            ((file) (set! has-files? #t))
                                            ((directory) (let ((unit-name (%%string->symbol (string-append first-part "." second-part))))
                                                           (zen:cache-package cache unit-name package))))))
                                      (zen:directory-content first-dir))
                            (if has-files?
                                (let ((unit-name (%%string->symbol first-part)))
                                  (zen:cache-package cache unit-name package)))))))
                  (zen:directory-directories toplevel-dir)))))


;;;
;;;; Debug
;;;


;; it is critical that these units be explicitly loaded up front
;; else we can get very hard to debug locking on the load mutex


(define (zen:load-debuggee)
  (zen:load-debuggee-units)
  (zen:load-unit 'zen.debuggee.setup)
  (zen:load-unit 'zen.debuggee.update))


(define (zen:load-debuggee-units)
  (zen:load-unit 'zen)
  (zen:load-unit 'zen.debuggee)
  (zen:load-unit 'zen.debuggee.base)
  (zen:load-unit 'zen.debuggee.stub)
  (zen:load-unit 'zen.debuggee.zen)
  (zen:load-unit 'zen.debuggee.zen.autoload)
  (zen:load-unit 'zen.debuggee.zen.autoload-external)
  (zen:load-unit 'zen.debuggee.zen.Zen-Debuggee-Frame)
  (zen:load-unit 'zen.debuggee.zen.Zen-Debuggee-Process)
  (zen:load-unit 'zen.debuggee.zen.Zen-Debuggee-Restart)
  (zen:load-unit 'zen.debuggee.zen.Zen-Debuggee-Stop)
  (zen:load-unit 'zen.debuggee.zen.Zen-Debuggee-Thread)
  (zen:load-unit 'zen.debugger.stub))


;;;
;;;; Product
;;;


(define (zen:find-product-descriptor name)
  (define (find-descriptor products)
    (let iter ((scan products))
         (if (%%null? scan)
             #f
           (let ((product (%%car scan)))
             (if (zen:product-descriptor-named? product name)
                 product
               (iter (%%cdr scan)))))))
  
  (let ((binary-package #f)
        (binary-descriptor #f))
    (let iter-repo ((repositories zen:Repositories))
      (if (%%null? repositories)
          (values binary-package binary-descriptor)
        (let ((repository (%%car repositories)))
          (let iter ((packages (zen:repository-packages (%%car repositories))))
            (if (%%null? packages)
                (iter-repo (%%cdr repositories))
              (let ((package (%%car packages)))
                (let ((pair (find-descriptor (%%get-package-products package))))
                  (if pair
                      (cond ((%%get-repository-binary? repository)
                             (set! binary-package package)
                             (set! binary-descriptor pair)
                             (iter (%%cdr packages)))
                            (else
                             (values package pair)))
                    (iter (%%cdr packages))))))))))))


(define (zen:product-descriptor-named? descriptor name)
  (or (%%eq? (zen:product-descriptor-name descriptor) name)
      (%%eq? (zen:product-descriptor-alias descriptor) name)))


(define (zen:product-descriptor-name descriptor)
  (%%car descriptor))

(define (zen:product-descriptor-alias descriptor)
  (let ((pair (%%assq 'alias (%%cdr descriptor))))
    (if pair
        (%%cadr pair)
      #f)))

(define (zen:product-descriptor-remote descriptor)
  (let ((pair (%%assq 'remote (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      #f)))

(define (zen:product-descriptor-title descriptor)
  (let ((pair (%%assq 'title (%%cdr descriptor))))
    (if pair
        (%%cadr pair)
      #f)))

(define (zen:product-descriptor-prefix descriptor)
  (let ((pair (%%assq 'prefix (%%cdr descriptor))))
    (if pair
        (%%cadr pair)
      #f)))

(define (zen:product-descriptor-description descriptor)
  (let ((pair (%%assq 'description (%%cdr descriptor))))
    (if pair
        (%%cadr pair)
      #f)))

(define (zen:product-descriptor-icon descriptor)
  (let ((pair (%%assq 'icon (%%cdr descriptor))))
    (if pair
        (%%cadr pair)
      #f)))

(define (zen:product-descriptor-run descriptor)
  (let ((pair (%%assq 'run (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      #f)))

(define (zen:product-descriptor-switches descriptor)
  (let ((pair (%%assq 'switches (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      #f)))

(define (zen:product-descriptor-product descriptor)
  (let ((pair (%%assq 'product (%%cdr descriptor))))
    (if pair
        (%%cadr pair)
      #f)))

(define (zen:product-descriptor-test descriptor)
  (let ((pair (%%assq 'test (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      #f)))

(define (zen:product-descriptor-update descriptor)
  (let ((pair (%%assq 'update (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      '())))

(define (zen:product-descriptor-build descriptor)
  (let ((pair (%%assq 'build (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      #f)))

(cond-expand
  (cocoa
    (define (zen:product-descriptor-build-bundle descriptor)
      (let ((alist (zen:product-descriptor-build descriptor)))
        (if alist
            (let ((pair (%%assq (zen:product-descriptor-name descriptor) alist)))
              (if pair
                  (zen:getf (%%cdr pair) bundle:)
                #f))
          #f))))
  (else
    (define (zen:product-descriptor-build-bundle descriptor)
      #f)))

(define (zen:product-descriptor-library descriptor)
  (let ((pair (%%assq 'library (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      #f)))


(define (zen:product-descriptor-dependencies descriptor)
  (let ((pair (%%assq 'dependencies (%%cdr descriptor))))
    (if pair
        (%%cdr pair)
      '())))


(define (zen:cond-expanded-product-descriptor-update product-name product-descriptor)
  (zen:cond-expand-map (zen:ill-formed-field-error "update" product-name)
                        (zen:product-descriptor-update product-descriptor)))


(define (zen:cond-expanded-product-descriptor-dependencies product-name product-descriptor)
  (zen:cond-expand-map (zen:ill-formed-field-error "dependencies" product-name)
                        (zen:product-descriptor-dependencies product-descriptor)))


(define zen:Products-Table
  (%%make-table test: eq?))

(define zen:Products-Run-Table
  (%%make-table test: eq?))


(define (zen:register-run name proc)
  (%%table-set! zen:Products-Run-Table name proc))


(define (zen:registered-run name)
  (or (%%table-ref zen:Products-Run-Table name #f)
      (zen:error "Unable to find registered run: {s}" name)))


(define (zen:run-registered name)
  (let ((proc (zen:registered-run name)))
    (proc)))


(zen:define-variable zen:process-product
  #f)

(zen:define-variable zen:process-name
  #f)

(zen:define-variable zen:process-title
  #f)

(zen:define-variable zen:process-prefix
  #f)

(zen:define-variable zen:process-traits
  #f)

(zen:define-variable zen:process-icon
  #f)

(zen:define-variable zen:process-version
  #f)


(define (zen:current-process-product)
  zen:process-product)

(define (zen:current-process-name)
  zen:process-name)

(define (zen:current-process-name-set! name)
  (set! zen:process-name name))

(define (zen:current-process-title)
  zen:process-title)

(define (zen:current-process-title-set! title)
  (set! zen:process-title title))

(define (zen:current-process-prefix)
  zen:process-prefix)

(define (zen:current-process-prefix-set! prefix)
  (set! zen:process-prefix prefix))

(define (zen:current-process-traits)
  zen:process-traits)

(define (zen:current-process-traits-set! traits)
  (set! zen:process-traits traits))

(define (zen:current-process-icon)
  zen:process-icon)

(define (zen:current-process-icon-set! icon)
  (set! zen:process-icon icon))

(define (zen:current-process-version)
  zen:process-version)

(define (zen:current-process-version-set! version)
  (set! zen:process-version version))


(define (zen:current-process-present)
  (or (zen:current-process-title)
      (let ((name (zen:current-process-name)))
        (if name
            (%%symbol->string name)
          #f))))


(define (zen:register-product name #!key (title #f) (icon #f) (run #f) (test #f) (update #f) (build #f) (options #f) (library-options #f) (install #f) (deploy #f))
  (receive (package descriptor) (zen:find-product-descriptor name)
    (%%table-set! zen:Products-Table name (%%make-product name title icon run test update build options library-options install deploy package descriptor))))


(define (zen:get-product-descriptor name)
  (receive (package descriptor) (zen:find-product-descriptor name)
    (if (and package descriptor)
        (values package descriptor)
      (zen:error "Unable to find product: {s}" name))))


(define (zen:get-product name)
  (define (get-registered-product name)
    (or (%%table-ref zen:Products-Table name #f)
        (zen:error "Unable to find registered product: {s}" name)))

  (receive (package descriptor) (zen:get-product-descriptor name)
    (let ((name (zen:product-descriptor-name descriptor)) ; because of aliases
          (unit (zen:product-descriptor-product descriptor)))
      (if unit
          (begin
            (zen:load-unit unit)
            (get-registered-product name))
        (let ((title (zen:product-descriptor-title descriptor))
              (icon (zen:product-descriptor-icon descriptor)))
          (%%make-product name title icon
            #f
            #f
            zen:update-product-descriptor
            zen:build-product-descriptor
            #f
            #f
            #f
            #f
            package
            descriptor))))))


(define (zen:setup-product name)
  (let ((product (zen:get-product name)))
    (let ((descriptor (%%get-product-descriptor product)))
      (set! zen:process-product name)
      (set! zen:process-name name)
      (set! zen:process-title (or (%%get-product-title product) (zen:product-descriptor-title descriptor)))
      (set! zen:process-prefix (zen:product-descriptor-prefix descriptor))
      (set! zen:process-icon (or (%%get-product-icon product) (zen:product-descriptor-icon descriptor)))
      (let ((switches (zen:product-descriptor-switches descriptor)))
        (if switches
            (zen:add-kernel-runtime-switches switches))))
    (if zen:debugger
        (zen:load-debuggee))
    product))


#; ;; dynamic-dependencies
(define (zen:adjust-build-repository product)
  ;; dynamic repositories - build location mirrors the source location
  ;; we want to build in $BINARY/$REPO/$BRANCH
  ;; where: kernel.exe is in $BINARY/zen/master
  ;; where: source repo is in $SOURCE/$REPO/$BRANCH
  ;; zen:Build-Repository is the destination
  ;; zen:Repositories is used to locate which files are up-to-date
  (if (zen:getf zen:kernel-properties dynamic?:)
      (let ((product-source-directory (%%get-repository-directory (%%get-package-repository (%%get-product-package product))))
            (kernel-root-directory (path-directory (path-strip-trailing-directory-separator
                                                     (path-directory (path-strip-trailing-directory-separator
                                                                       zen:kernel-install))))))
        (let ((product-build-directory (zen:build-dynamic-path kernel-root-directory product-source-directory))
              (current-build-directory (%%get-repository-directory zen:Build-Repository)))
          (if (not (equal? product-build-directory current-build-directory))
              (let ((new-build-repository (zen:make-repository 'Build "lib" product-build-directory binary?: #t dynamic?: #t)))
                (zen:setup-repository new-build-repository)
                (set! zen:Repositories (cons new-build-repository (zen:remove zen:Build-Repository zen:Repositories)))
                (set! zen:Build-Repository new-build-repository)))))))


(define (zen:register-product-run name proc)
  (zen:register-run name proc))


(define (zen:run-product name)
  (define (run-product-descriptor descriptor)
    (let ((name (zen:product-descriptor-name descriptor))
          (run (zen:product-descriptor-run descriptor)))
      (if run
          (begin
            (for-each zen:load-unit run)
            (let ((proc (zen:registered-run name)))
              (proc descriptor)))
        (zen:error "Product is not runnable: {s}" name))))
  
  (let ((product (zen:setup-product name)))
    (let ((run (%%get-product-run product))
          (descriptor (%%get-product-descriptor product)))
      (if run
          (run descriptor)
        (run-product-descriptor descriptor)))))


(define (zen:test-product name)
  (define (test-product-descriptor descriptor)
    (let ((name (zen:product-descriptor-name descriptor))
          (test (zen:product-descriptor-test descriptor)))
      (if test
          (for-each zen:load-unit test)
        (zen:error "Product is not testable: {s}" name))))
  
  (let ((product (zen:setup-product name)))
    (let ((test (%%get-product-test product))
          (descriptor (%%get-product-descriptor product)))
      (if test
          (test descriptor)
        (test-product-descriptor descriptor)))))


(define (zen:ill-formed-field-error field-name product-name)
  (lambda ()
    (zen:error "Ill-formed {a} field in product descriptor for product {a}" field-name product-name)))


(define (zen:cond-expand-map error-proc updates)
  (define (apply-cond-expand exp)
    (cond ((and (%%pair? exp)
                (%%pair? (%%car exp)))
           (let ((clause (%%car exp)))
             (cond ((zen:feature-satisfied? (%%car clause))
                    (%%cdr clause))
                   ((%%eq? 'else (%%car clause))
                    (%%cdr clause))
                   (else
                    (apply-cond-expand (%%cdr exp))))))
          ((null? exp)
           '())
          (else
           (error-proc))))

  (define (expand-update update)
    (cond ((%%symbol? update)
           (%%list update))
          ((and (%%pair? update)
                (%%eq? 'cond (%%car update)))
           (apply-cond-expand (%%cdr update)))
          (else
           (error-proc))))
  
  (%%apply append (map expand-update updates)))


(define (zen:update-product name)
  (let ((product (zen:setup-product name)))
    #; ;; dynamic-dependencies
    (zen:adjust-build-repository product)
    (let ((update (%%get-product-update product))
          (descriptor (%%get-product-descriptor product)))
      (if update
          (update descriptor)
        (zen:update-product-descriptor descriptor)))))


(define (zen:update-product-descriptor descriptor)
  (let ((name (zen:product-descriptor-name descriptor)))
    (let ((update (zen:cond-expanded-product-descriptor-update name descriptor)))
      (for-each zen:build-unit update))))


(define (zen:build-product name)
  (let ((product (zen:setup-product name)))
    #; ;; dynamic-dependencies
    (zen:adjust-build-repository product)
    (let ((build (%%get-product-build product))
          (descriptor (%%get-product-descriptor product)))
      (zen:feedback "make {a}" name)
      (if build
          (build descriptor)
        (zen:build-product-descriptor descriptor))
      (if (or (zen:link-libraries?) (zen:link-static?))
          (zen:build-library-descriptor descriptor)))))


(define (zen:build-product-descriptor descriptor #!key (unit #f) (skip-references? #f) (force? #f))
  (define (build-product)
    (zen:update-product-descriptor descriptor)
    (let ((build (zen:product-descriptor-build descriptor)))
      (if build
          (for-each (lambda (obj)
                      (if (%%symbol? obj)
                          (zen:build-image obj)
                        (%%apply zen:build-image obj)))
                    build))))
  
  (if unit
      (zen:compile-unit unit skip-references?: skip-references? force?: force?)
    (build-product)))


(define (zen:build-library-descriptor descriptor)
  (let ((library (zen:product-descriptor-library descriptor)))
    (let ((options (or library '())))
      (zen:build-library (zen:product-descriptor-name descriptor) descriptor options: options))))


(define zen:product-download
  #f)

(define (zen:set-product-download download)
  (set! zen:product-download download))


(define (zen:cond-expand-remote remote)
  (declare (proper-tail-calls))
  (define (ill-formed)
    (zen:error "Ill-formed remote: {s}" remote))
  
  (define (unfulfilled)
    #f)
  
  (let loop ((scan remote))
       (if (%%null? scan)
           (unfulfilled)
         (let ((clause (%%car scan)))
           (cond ((%%string? clause)
                  (values #f clause))
                 ((%%pair? clause)
                  (if (or (%%null? (%%cdr clause))
                          (%%not (%%null? (%%cddr clause))))
                      (ill-formed)
                    (let ((feature (%%car clause))
                          (remote (%%cadr clause)))
                      (if (or (%%not (%%symbol? feature))
                              (%%not (%%string? remote)))
                          (ill-formed)
                        (if (zen:feature-satisfied? feature)
                            (values feature remote)
                          (loop (%%cdr scan)))))))
                 (else
                  (ill-formed)))))))


(define (zen:download-product name)
  (declare (proper-tail-calls))
  (let ((subproduct-table (%%make-table test: eq?)))
    (define (download name)
      (let ((product (zen:setup-product name)))
        (let ((descriptor (%%get-product-descriptor product)))
          (let ((remote (zen:product-descriptor-remote descriptor)))
            (if remote
                (let ((info (zen:cond-expand-remote remote)))
                  (if info
                      (receive (feature remote) info
                        (zen:load-download)
                        (zen:product-download descriptor feature remote))))))
          (for-each (lambda (subname)
                      (if (%%not (%%table-ref subproduct-table subname #f))
                          (begin
                            (%%table-set! subproduct-table subname #t)
                            (download subname))))
                    (receive (package descriptor) (zen:get-product-descriptor name)
                      (zen:cond-expanded-product-descriptor-dependencies name descriptor))))))
    
    (download name)))


(define (zen:install-product name)
  (let ((product (zen:setup-product name)))
    (let ((install (%%get-product-install product)))
      (if install
          (let ((descriptor (%%get-product-descriptor product)))
            (zen:feedback "install {a}" name)
            (install descriptor))))))


(define (zen:deploy-product name)
  (let ((product (zen:setup-product name)))
    (let ((deploy (%%get-product-deploy product)))
      (if deploy
          (let ((descriptor (%%get-product-descriptor product)))
            (deploy descriptor))))))


;; changed to 0 until parallel build is fully debugged
(define zen:jobs-default
  0)


;;;
;;;; Script
;;;


(define (zen:setup-script)
  (set! zen:process-product 'script)
  (set! zen:process-name 'script)
  (set! zen:process-title "Script")
  (if zen:debugger
      (zen:load-debuggee)))


;;;
;;;; Master
;;;


(define (zen:make-product name)
  (let ((subproduct-table (%%make-table test: eq?))
        (subproduct-table-mutex (%%make-mutex 'subproduct-table-mutex))
        (active-processes '())
        (free-processes '())
        (outdated-processes '())
        (process-mutex (%%make-mutex 'process-mutex))
        (process-condition-variable (%%make-condition-variable 'process-condition-variable))
        (listening-port (open-tcp-server 0))
        (output-mutex (%%make-mutex 'output-mutex))
        (jobs (or zen:jobs (zen:build-jobs) zen:jobs-default))
        (stop-build? #f))
    (define (key-product? name)
      (%%memq name '(contrib.irregex
                     zen
                     scheme)))
    
    (define (atomic-output line)
      (mutex-lock! output-mutex)
      (display line)
      (newline)
      (force-output)
      (mutex-unlock! output-mutex))
    
    (define (end-port/echo port/echo)
      (let ((established-port (%%car port/echo))
            (echo-thread (%%cdr port/echo)))
        (write '() established-port)
        (force-output established-port)
        (close-port established-port)
        (thread-join! echo-thread)))
    
    (define (grab-build-process)
      (mutex-lock! process-mutex)
      (let ((active-count (%%length active-processes)))
        (cond ((%%pair? free-processes)
               (let ((port/echo (%%car free-processes)))
                 (set! free-processes (%%cdr free-processes))
                 (mutex-unlock! process-mutex)
                 port/echo))
              ((%%fx< active-count jobs)
               (let ((process (open-process
                                (list
                                  path: (zen:install-path "zen")
                                  arguments: `(,(if (zen:worker-repl?)
                                                    "-:darQ"
                                                  "-:daqQ-")
                                               "-worker"
                                               "-link" ,(%%symbol->string zen:link)
                                               ,@(if (%%memq 'keep-c zen:compile-options) `("-keep-c") '())
                                               ,@(if (%%memq 'track-scheme zen:compile-options) `("-track-scheme") '())
                                               ,@(if (%%memq 'expansion zen:compile-options) `("-expansion") '())
                                               ,@(if (zen:build-target) `("-target" ,(symbol->string (zen:build-target))) '())
                                               ,@(if (zen:save-emit?) `("-emit") '())
                                               ,@(if (zen:build-repository) `("-build-repository" ,(zen:build-repository)) '())
                                               ,@(if (zen:zen-repository) `("-zen-repository" ,(zen:zen-repository)) '())
                                               ,@(if (zen:repositories) `("-repositories" ,(zen:repositories)) '())
                                               #; ;; dynamic-dependencies
                                               ,@(let ((dependencies (string-append (current-directory) ".dependencies")))
                                                   (if (file-exists? dependencies)
                                                       `("-dependencies" ,dependencies)
                                                     '()))
                                               ,@(if (zen:reporting?) `("-reporting") '())
                                               "-jobs" "1"
                                               "-port" ,(number->string (socket-info-port-number (tcp-server-socket-info listening-port))))
                                  stdin-redirection: #t
                                  stdout-redirection: #t
                                  stderr-redirection: #t))))
                 (let ((established-port (read listening-port))
                       (echo-thread (thread-start! (%%make-thread (lambda ()
                                                                    (let iter ()
                                                                         (let ((line (read-line process)))
                                                                           (if (not (eof-object? line))
                                                                               (begin
                                                                                 (atomic-output line)
                                                                                 (iter))))))))))
                   (let ((port/echo (%%cons established-port echo-thread)))
                     (set! active-processes (%%cons port/echo active-processes))
                     (mutex-unlock! process-mutex)
                     port/echo))))
              (else
               (mutex-unlock! process-mutex process-condition-variable)
               (grab-build-process)))))
    
    (define (build name)
      (let ((port/echo (grab-build-process)))
        (define (build-process-problem)
          (mutex-lock! process-mutex)
          (set! active-processes (zen:remove port/echo active-processes))
          (set! stop-build? #t)
          (mutex-unlock! process-mutex)
          (condition-variable-signal! process-condition-variable))
        
        (define (build-process-ended changes)
          (mutex-lock! process-mutex)
          (if (and (key-product? name) (%%not (%%null? changes)))
              (set! outdated-processes active-processes))
          (if (%%memq port/echo outdated-processes)
              (begin
                (set! active-processes (zen:remove port/echo active-processes))
                (set! outdated-processes (zen:remove port/echo outdated-processes))
                (end-port/echo port/echo))
            (set! free-processes (%%cons port/echo free-processes)))
          (mutex-unlock! process-mutex)
          (condition-variable-signal! process-condition-variable))
        
        (if stop-build?
            (build-process-ended #f)
          (let ((established-port (%%car port/echo)))
            (send-build-command established-port name)
            (let iter ()
                 (let ((form (read established-port)))
                   (if (eof-object? form)
                       (build-process-problem)
                     (let ((command (%%car form))
                           (arguments (%%cdr form)))
                       (case command
                         ((changes)
                          (build-process-ended arguments))
                         ((report)
                          (zen:report "{a}" arguments)
                          (iter))
                         ((exception)
                          (build-process-problem)
                          (iter))
                         (else
                          (error "Unknown master command" command)))))))))))
    
    (define (local-make name)
      (for-each (lambda (subname)
                  (if (%%not (%%table-ref subproduct-table subname #f))
                      (begin
                        (make subname)
                        (%%table-set! subproduct-table subname #t))))
                (receive (package descriptor) (zen:get-product-descriptor name)
                  (zen:cond-expanded-product-descriptor-dependencies name descriptor)))
      (zen:build-product name))
    
    (define (remote-make name)
      (for-each thread-join!
                (map (lambda (name)
                       (mutex-lock! subproduct-table-mutex)
                       (let ((thread (or (%%table-ref subproduct-table name #f)
                                         (let ((thread (thread-start! (%%make-thread (lambda ()
                                                                                       (make name))))))
                                           (%%table-set! subproduct-table name thread)
                                           thread))))
                         (mutex-unlock! subproduct-table-mutex)
                         thread))
                     (receive (package descriptor) (zen:get-product-descriptor name)
                       (zen:cond-expanded-product-descriptor-dependencies name descriptor))))
      (build name))
    
    (define (make name)
      (if (or (%%not jobs) (%%eqv? jobs 0) (zen:debug-build?))
          (local-make name)
        (remote-make name)))
    
    (define (send-build-command established-port name)
      (write `(build ,name) established-port)
      (newline established-port)
      (force-output established-port))
    
    (parameterize ((current-user-interrupt-handler
                     (lambda ()
                       (set! stop-build? #t)
                       (for-each end-port/echo active-processes)
                       (for-each end-port/echo free-processes)
                       (exit))))
      (make name))
    (for-each end-port/echo free-processes)
    (if stop-build? 1 0)))


;;;
;;;; Worker
;;;


(define zen:worker-port
  #f)

(define zen:worker-product
  #f)


(define (zen:worker-process port)
  (declare (proper-tail-calls))
  (let ((repl? (zen:worker-repl?))
        (previous-handler (current-exception-handler))
        (established-port (open-tcp-client port)))
    (define (build product)
      (set! zen:worker-product product)
      (zen:reset-changed-units)
      (zen:build-product product)
      (write (cons 'changes (zen:get-changed-units)) established-port)
      (force-output established-port))
    
    (set! zen:worker-port established-port)
    (let iter ()
         (let ((form (read established-port)))
           (if (or (eof-object? form)
                   (null? form))
               (close-port established-port)
             (let ((command (%%car form))
                   (arguments (%%cdr form)))
               (%%continuation-capture
                 (lambda (cont)
                   (with-exception-handler
                     (lambda (exc)
                       (if repl?
                           (begin
                             ;; closing the port would terminate the parent
                             ;; process making the console port unavailable
                             ;; (close-port established-port)
                             (write '(exception) established-port)
                             (force-output established-port)
                             (previous-handler exc))
                         (%%continuation-graft cont
                           (lambda ()
                             (if zen:worker-product
                                 (begin
                                   (display "*** ERROR BUILDING ")
                                   (display zen:worker-product)
                                   (display " -- ")))
                             (display-exception exc)
                             (force-output)
                             (exit)))))
                     (lambda ()
                       (case command
                         ((build)
                          (build (%%car arguments)))
                         (else
                          (error "Unknown worker command" command)))
                       (iter)))))))))))


;;;
;;;; Resource
;;;


(define zen:default-char-encoding
  'UTF)


(define (zen:resource-char-encoding resource)
  (or (%%get-package-char-encoding (%%get-resource-package resource))
      zen:default-char-encoding))


(define (zen:resource-package resource)
  (%%get-resource-package resource))


(define (zen:resource-pathname resource)
  (zen:package-root-pathname (%%get-resource-package resource)
    (zen:resource-package-pathname resource)))


(define (zen:resource-package-pathname resource)
  (let ((ext (%%get-resource-extension resource)))
    (if (%%not ext)
        (%%get-resource-path resource)
      (%%string-append (%%get-resource-path resource) "." ext))))


(define (zen:name->path resource-name)
  (zen:string-replace (%%symbol->string resource-name) #\. #\/))


(define (zen:path->name resource-name)
  (%%string->symbol (zen:string-replace resource-name #\/ #\.)))


(define (zen:binary-with-extension src extension)
  (let* ((bindir (zen:resource-build-dir src))
         (pathname (zen:resource-pathname src))
         (name-base (zen:pathname-base pathname)))
    (string-append bindir name-base extension)))


(define (zen:probe-numbered-pathname pathname n)
  (let ((candidate (string-append pathname (%%number->string n))))
    (if (%%not (file-exists? candidate))
        candidate
      (zen:probe-numbered-pathname pathname (%%fx+ n 1)))))


(define (zen:for-each-numbered-pathname pathname n0 proc)
  (let iter ((n n0))
       (let ((candidate (string-append pathname (%%number->string n))))
         (if (file-exists? candidate)
             (begin
               (iter (%%fx+ n 1))
               (proc candidate))))))


(define (zen:with-numbered-library pathname fresh? n0 proc)
  ;; quicky around libraries not currently supporting single-objects?
  ;; and is possible as libraries are only used for setup at the moment
  (let ((library (string-append pathname (%%number->string n0))))
    (proc library (file-exists? library)))
  #;
  (let iter ((n n0) (exists? #f))
       (let ((candidate (string-append pathname (%%number->string n))))
         (if (%%not (file-exists? candidate))
             (if fresh?
                 (proc candidate exists?)
               (if exists?
                   (proc (string-append pathname (%%number->string (%%fx- n 1))) exists?)
                 (proc (string-append pathname (%%number->string n0)) exists?)))
           (iter (%%fx+ n 1) #t)))))


(define (zen:product-library-name-base package path)
  (zen:relocate-product-library-name-base (%%get-package-repository package) package path))


(define (zen:relocate-product-library-name-base repository package path)
  (define (build-dir package)
    (let ((parent (%%get-package-parent package)))
      (zen:repository-pathname repository
        (%%string-append (if parent (%%string-append (%%get-package-library-path parent) "/") "")
                         (%%symbol->string (%%get-package-name package))))))
  
  (string-append (build-dir package)
                 "/"
                 path))


;;;
;;;; Manifest
;;;


(define (zen:manifest-pathname package resource)
  (zen:package-root-pathname package
                              (%%string-append (%%get-resource-path resource)
                                               "."
                                               zen:Manifest-Extension)))


(define (zen:digest-pathname package resource)
  (zen:package-root-pathname package
                              (%%string-append (%%get-resource-path resource)
                                               "."
                                               zen:Digest-Extension)))


;;;
;;;; Load
;;;


(define zen:load-indent
  (%%make-parameter 0))


(define (zen:load-resource unit-name loading resource . rest)
  (let ((quiet? (if (%%null? rest) #f (%%car rest))))
    (if (zen:unit-verbose?)
        (pretty-print unit-name))
    (zen:with-verbose (zen:load-verbose?) loading (zen:resource-pathname resource)
      (lambda ()
        (zen:load-file (%%list
                          path: (zen:resource-pathname resource)
                          char-encoding: (zen:resource-char-encoding resource))
                        quiet?)))))


(define zen:verbose-port
  #f)

(define (zen:set-verbose-port port)
  (set! zen:verbose-port port))


(define (zen:with-verbose flag action path proc)
  (let ((port (or zen:verbose-port (repl-output-port))))
    (define (verbose-load)
      (display (%%make-string (zen:load-indent) #\space) port)
      (display "; " port)
      (display action port)
      (display " " port)
      (display path port)
      (display "..." port)
      (newline port)
      (force-output port))
    
    (define (verbose-done)
      (display (%%make-string (zen:load-indent) #\space) port)
      (display "; done " port)
      (display "..." port)
      (newline port)
      (force-output port))
    
    (if flag
        (begin
          (verbose-load)
          (let ((result
                  (parameterize ((zen:load-indent (%%fx+ (zen:load-indent) 2)))
                    (proc))))
            (if (zen:done-verbose?)
                (verbose-done))
            result))
      (proc))))


(define zen:load-interpreted-hook
  #f)

(define (zen:get-load-interpreted-hook)
  zen:load-interpreted-hook)

(define (zen:set-load-interpreted-hook hook)
  (set! zen:load-interpreted-hook hook))


(define zen:unit-not-found-hook
  #f)

(define (zen:get-unit-not-found-hook)
  zen:unit-not-found-hook)

(define (zen:set-unit-not-found-hook hook)
  (set! zen:unit-not-found-hook hook))


;; quick hack so that once we switch repositories to target
;; we don't load any binary code anymore as it will be ios!
(define zen:TARGET-HACK?
  #f)

(define (zen:TARGET-HACK?-set! flag)
  (set! zen:TARGET-HACK? flag))


(define (zen:load-unit-src/bin unit-name #!key (force-source? #f))
  (let try-again ((first-try? #t))
    (zen:with-unit-resources unit-name #f
      (lambda (src obj bin load-proc obj-uptodate? bin-uptodate? lib-uptodate? manifest)
        (if zen:TARGET-HACK?
            (begin
              (set! obj #f)
              (set! bin #f)
              (set! load-proc #f)
              (set! obj-uptodate? #f)
              (set! bin-uptodate? #f)
              (set! lib-uptodate? #f)
              (set! manifest #f)))
        (parameterize ((zen:requested-unit-name unit-name)
                       (zen:requested-unit-resource (if bin-uptodate? bin src))
                       (zen:requested-pathname #f))
          (cond ((and lib-uptodate? (not force-source?))
                 (zen:increment-image-load-counter)
                 (zen:with-verbose (zen:load-verbose?) "loading lib" (symbol->string unit-name)
                   (lambda ()
                     (parameterize ((zen:walk-for 'runtime))
                       (load-proc)))))
                ((and bin-uptodate? (not force-source?))
                 (zen:increment-object-load-counter)
                 (let ((quiet? (or (%%not src) (let ((ext (%%get-resource-extension src)))
                                                 (and ext (%%string=? ext "zen"))))))
                   (parameterize ((zen:walk-for 'runtime))
                     (zen:load-resource unit-name "loading bin" bin quiet?))))
                (src
                 (if (or (%%not zen:load-interpreted-hook)
                         (%%not (zen:load-interpreted-hook unit-name)))
                     (begin
                       (zen:increment-interpreted-load-counter)
                       (let ((warn (zen:warn-interpreted?)))
                         (if warn
                             (case warn
                               ((error)
                                (zen:error "Loading {a} interpreted" unit-name))
                               ((stack)
                                (zen:feedback "Warning: Loading {a} interpreted" unit-name)
                                (pp (zen:current-load-stack)))
                               (else
                                (zen:feedback "Warning: Loading {a} interpreted" unit-name)
                                (if (and (%%pair? warn) (%%memq unit-name warn))
                                    (pp (zen:current-load-stack)))))))
                       (parameterize ((zen:walk-for 'interpret)
                                      (zen:generate-symbol-for "&")
                                      (zen:generate-symbol-context unit-name))
                         (zen:with-extension-reader (%%get-resource-extension src)
                           (lambda ()
                             (zen:load-resource unit-name "loading src" src)))))))
                (else
                 (if force-source?
                     (zen:error "Unable to find unit source: {s}" unit-name)
                   (if (and zen:unit-not-found-hook first-try?)
                       (begin
                         (zen:unit-not-found-hook unit-name)
                         (try-again #f))
                     (zen:error "Unable to find unit: {s}" unit-name))))))))))


(define (zen:unit-loadable? unit-name)
  (zen:with-unit-resources unit-name #f
    (lambda (src obj bin load-proc obj-uptodate? bin-uptodate? lib-uptodate? manifest)
      (or lib-uptodate? bin-uptodate? src))))


(define (zen:unit-obj-uptodate? unit-name)
  (zen:with-unit-resources unit-name #f
    (lambda (src obj bin load-proc obj-uptodate? bin-uptodate? lib-uptodate? manifest)
      obj-uptodate?)))


(define (zen:unit-source unit-name)
  (zen:with-unit-resources unit-name #f
    (lambda (src obj bin load-proc obj-uptodate? bin-uptodate? lib-uptodate? manifest)
      (and src (zen:resource-pathname src)))))


(define (zen:load-download)
  (zen:load-unit 'jas))


;;;
;;;; Evaluate
;;;


(define zen:evaluate-forms-hook
  #f)

(define (zen:get-evaluate-forms-hook)
  zen:evaluate-forms-hook)

(define (zen:set-evaluate-forms-hook hook)
  (set! zen:evaluate-forms-hook hook))


;;;
;;;; Console
;;;


(define zen:console-evaluate-hook
  #f)

(define (zen:get-console-evaluate-hook)
  zen:console-evaluate-hook)

(define (zen:set-console-evaluate-hook hook)
  (set! zen:console-evaluate-hook hook))


;;;
;;;; Build
;;;


(define (zen:resource-build-dir resource)
  (zen:relocate-resource zen:Build-Repository resource))


(define (zen:relocate-resource repository resource)
  (let ((package (%%get-resource-package resource))
        (dir (zen:pathname-dir (%%get-resource-path resource))))
    (zen:relocate-package-pathname repository package dir)))


;;;
;;;; State
;;;


(define zen:Unloaded-State
  #f)

(define zen:Loading-State
  '(loading))

(define zen:Loaded-State
  '(loaded))

(define zen:Error-State
  '(error))


;;;
;;;; Environment
;;;


(define zen:Environment
  (%%make-table test: eq?))


(define (zen:get-environment-table)
  zen:Environment)


(define (zen:get-environment-unit name)
  (%%table-ref zen:Environment name zen:Unloaded-State))


(define (zen:set-environment-unit name unit)
  (%%table-set! zen:Environment name unit))


;;;
;;;; Image Units
;;;


(define zen:Image-Units
  (%%make-table test: eq?))

(define (zen:get-image-unit name)
  (%%table-ref zen:Image-Units name #f))

(define (zen:set-image-unit name load-proc compile-time-hash)
  (%%table-set! zen:Image-Units name (%%make-image-unit load-proc compile-time-hash)))


;;;
;;;; Foreign
;;;


(define zen:foreign-libraries
  (%%make-table test: eq?))


(define (zen:get-foreign-libraries)
  zen:foreign-libraries)


(define (zen:register-foreign-libraries unit-name . libraries)
  (%%table-set! zen:foreign-libraries unit-name libraries))


(define (zen:registered-foreign-libraries unit-name)
  (%%table-ref zen:foreign-libraries unit-name #f))


(define (zen:logging-foreign-libraries unit-name)
  (if (and zen:kernel-debug-foreign? (zen:logging?))
      (let ((libraries (zen:registered-foreign-libraries unit-name)))
        (if libraries
            (zen:logging-line (zen:format ";**** libraries {l}" libraries))))))


;;;
;;;; Unit
;;;


(define zen:Load-Mutex
  (%%make-mutex 'load))


(zen:define-variable zen:Load-Thread
  #f)


(define zen:Load-Count
  0)


(define zen:current-load-stack
  (%%make-parameter '()))


(define zen:requested-unit-name
  (%%make-parameter #f))

(define zen:requested-unit-resource
  (%%make-parameter #f))

(define zen:requested-pathname
  (%%make-parameter #f))


(define zen:compiled-source
  (%%make-parameter #f))


(define (zen:get-load-mutex)
  zen:Load-Mutex)

(define (zen:get-load-thread)
  zen:Load-Thread)


(define (zen:call-with-load-lock thunk)
  (if (%%eq? zen:Load-Thread (current-thread))
      (thunk)
    (dynamic-wind
      (lambda ()
        (mutex-lock! zen:Load-Mutex)
        (set! zen:Load-Thread (current-thread)))
      thunk
      (lambda ()
        (set! zen:Load-Thread #f)
        (mutex-unlock! zen:Load-Mutex)))))


(define (zen:unit-loaded? unit-name)
  (%%eq? (zen:get-environment-unit unit-name) zen:Loaded-State))


(define (zen:circular-dependency-error unit-name unit-name-list)
  (define (take-until predicate list)
    (let loop ((in list)
               (out '()))
         (if (%%null? in)
             out
           (let ((elem (%%car in)))
             (if (predicate elem)
                 (%%cons elem out)
               (loop (%%cdr in)
                     (%%cons elem out)))))))
  
  (let ((circular-unit-list (append (take-until
                                      (lambda (name) (%%eq? unit-name name))
                                      unit-name-list)
                                    (%%list unit-name))))
    (zen:error "Circular dependency detected: {a}"
                (zen:join-strings (map symbol->string circular-unit-list)
                                   " -> "))))


(define zen:load-feedback-setup-count
  0)

(define zen:load-feedback-done-count
  0)


(define zen:load-feedback-port
  #f)


(define zen:load-feedback-mutex
  (make-mutex 'load-feedback))

(define (zen:with-load-feedback-mutex thunk)
  (mutex-lock! zen:load-feedback-mutex)
  (thunk)
  (mutex-unlock! zen:load-feedback-mutex))


(define (zen:load-feedback-setup port expected-count)
  (zen:with-load-feedback-mutex
    (lambda ()
      (set! zen:load-feedback-setup-count zen:Load-Count)
      (set! zen:load-feedback-port port)
      (write expected-count port)
      (newline port)
      (force-output port))))


(define (zen:load-feedback unit-name)
  (zen:with-load-feedback-mutex
    (lambda ()
      (if zen:load-feedback-port
          (with-exception-catcher
            (lambda (exc)
              (zen:load-feedback-close))
            (lambda ()
              (write unit-name zen:load-feedback-port)
              (newline zen:load-feedback-port)
              (force-output zen:load-feedback-port)))))))


(define (zen:load-feedback-done)
  (zen:with-load-feedback-mutex
    (lambda ()
      (if zen:load-feedback-port
          (begin
            (set! zen:load-feedback-done-count zen:Load-Count)
            (write '(done) zen:load-feedback-port)
            (newline zen:load-feedback-port)
            (force-output zen:load-feedback-port)
            (zen:load-feedback-close))))))


(define (zen:load-feedback-close)
  (with-exception-catcher
    (lambda (exc)
      #f)
    (lambda ()
      (close-port zen:load-feedback-port)))
  (set! zen:load-feedback-port #f))


(define (zen:load-unit unit-name)
  (if (%%symbol? unit-name)
      (let ((unit-state (zen:get-environment-unit unit-name)))
        (if (%%not (%%eq? unit-state zen:Loaded-State))
            (zen:call-with-load-lock ; unit-state might change while suspended
              (lambda ()
                (zen:load-feedback unit-name)
                (let ((unit-state (zen:get-environment-unit unit-name)))
                  (cond ((%%eq? unit-state zen:Loading-State)
                         (zen:circular-dependency-error unit-name (map cdr (zen:current-load-stack))))
                        ((or (%%eq? unit-state zen:Unloaded-State)
                             (%%eq? unit-state zen:Error-State))
                         (parameterize ((zen:current-load-stack (%%cons (%%cons ':load unit-name) (zen:current-load-stack))))
                           (dynamic-wind
                             (lambda ()
                               (zen:set-environment-unit unit-name zen:Loading-State))
                             (lambda ()
                               (zen:logging-foreign-libraries unit-name)
                               (zen:load-unit-src/bin unit-name force-source?: (%%eq? unit-state zen:Error-State))
                               (let ((unit-state (zen:get-environment-unit unit-name)))
                                 (if (%%procedure? unit-state)
                                     (unit-state unit-name)))
                               (zen:set-environment-unit unit-name zen:Loaded-State))
                             (lambda ()
                               (if (%%eq? (zen:get-environment-unit unit-name) zen:Loading-State)
                                   (zen:set-environment-unit unit-name zen:Error-State)
                                 (set! zen:Load-Count (%%fx+ zen:Load-Count 1)))))))))))))
    (zen:error "Unit name expected: {a}" unit-name)))


(define (zen:load-hook unit-name hook)
  (zen:call-with-load-lock
    (lambda ()
      (let ((unit-state (zen:get-environment-unit unit-name)))
        (if (%%eq? unit-state zen:Loaded-State)
            (zen:error "Unit already loaded: {a}" unit-name)
          (zen:set-environment-unit unit-name hook))))))


(define (zen:load-count)
  zen:Load-Count)


(define zen:current-script-arguments
  (%%make-parameter '()))


(define zen:load-script-hook
  #f)

(define (zen:get-load-script-hook)
  zen:load-script-hook)

(define (zen:set-load-script-hook hook)
  (set! zen:load-script-hook hook))


(define (zen:load-script path)
  (if (or (%%not zen:load-script-hook)
          (%%not (zen:load-script-hook path)))
      (parameterize ((zen:walk-for 'interpret)
                     (zen:generate-symbol-for "&")
                     (zen:generate-symbol-context (gensym))
                     (zen:requested-unit-name #f)
                     (zen:requested-unit-resource #f)
                     (zen:requested-pathname path))
        (zen:with-extension-reader (zen:pathname-extension path)
          (lambda ()
            (zen:load-file (%%list
                              path: path
                              char-encoding: 'UTF)
                            #t))))))


(define (zen:unload-unit unit-name)
  (if (mutex-lock! zen:Load-Mutex)
      (begin
        (zen:set-environment-unit unit-name zen:Unloaded-State)
        (mutex-unlock! zen:Load-Mutex))
    ;; reaquire mutex
    (zen:unload-unit unit-name)))


(define (zen:reload-unit unit-name)
  (zen:unload-unit unit-name)
  (zen:load-unit unit-name))


;;;
;;;; Service
;;;


(define zen:Services
  (%%make-table test: eq?))


(define (zen:register-service name thunk)
  (%%table-set! zen:Services name thunk))


(define (zen:find-service name)
  (let ((symbol/proc (%%table-ref zen:Services name #f)))
    (if (%%symbol? symbol/proc)
        (begin
          (zen:load-unit symbol/proc)
          (set! symbol/proc (%%table-ref zen:Services name #f))))
    (if symbol/proc
        (symbol/proc)
      #f)))


(define (zen:require-service name)
  (or (zen:find-service name)
      (error "Unknown service: {s}" name)))


;;;
;;;; Coupler
;;;


(define zen:Couplers
  (%%make-table test: eq?))


(define (zen:register-coupler name thunk)
  (%%table-set! zen:Couplers name
    (cons thunk
          (%%table-ref zen:Couplers name '()))))


(define (zen:get-couplers name)
  (%%table-ref zen:Couplers name '()))


(define (zen:activate-couplers name)
  (for-each (lambda (proc)
              (proc))
            (zen:get-couplers name))
  (%%table-clear zen:Couplers name))


;;;
;;;; Literal
;;;


(define zen:Literal-Constructors
  (%%make-table test: eq?))


(define (zen:register-literal-constructor name constructor-reference constructor)
  (%%table-set! zen:Literal-Constructors name (%%cons constructor-reference constructor)))


(define (zen:require-literal-constructor name)
  (or (%%table-ref zen:Literal-Constructors name #f)
      (zen:error "Cannot construct literals of type {s}" name)))


(zen:define-macro (zen:define-literal name constructor-reference)
  (receive (constructor-module constructor-name) (zen:break-reference constructor-reference)
    (let ((constructor-locator (zen:compose-reference constructor-module constructor-name)))
      `(zen:register-literal-constructor ',name ',constructor-reference
         (lambda (arguments)
           (zen:load-unit ',constructor-module)
           (%%apply (zen:global-ref ',constructor-locator) arguments))))))


(define (zen:construct-literal name arguments)
  (let ((constructor (%%cdr (zen:require-literal-constructor name))))
    (constructor arguments)))


(define zen:Literal-Walkers
  (%%make-table test: eq?))


(define (zen:register-literal-walker name walker-locator walker)
  (%%table-set! zen:Literal-Walkers name (%%cons walker-locator walker)))


(zen:define-macro (zen:define-literal-walker name walker-reference)
  (receive (walker-module walker-name) (zen:break-reference walker-reference)
    (let ((walker-locator (zen:compose-reference walker-module walker-name)))
      `(zen:register-literal-walker ',name ',walker-locator
         (lambda (arguments proc)
           (zen:load-unit ',walker-module)
           ((zen:global-ref ',walker-locator) arguments proc))))))


(define (zen:walk-literal name arguments proc)
  (let ((info (%%table-ref zen:Literal-Walkers name #f)))
    (if info
        (let ((walker (%%cdr info)))
          (walker arguments proc)))))


;;;
;;;; Reader
;;;


(define zen:Extension-Readers
  (%%make-table test: equal?))


(define (zen:get-extension-reader extension)
  (%%table-ref zen:Extension-Readers extension #f))


(define (zen:with-extension-reader extension thunk)
  (let ((readtable-getter (zen:get-extension-reader extension)))
    (if readtable-getter
        (parameterize ((current-readtable (readtable-getter)))
          (thunk))
      (thunk))))


(define (zen:with-resource-reader resource thunk)
  (zen:with-extension-reader (%%get-resource-extension resource) thunk))


(define (zen:register-reader-extension extension readtable-getter)
  (%%table-set! zen:Extension-Readers extension readtable-getter))


;;;
;;;; Scheme
;;;


(define zen:scheme-readtable
  (%%current-readtable))


(zen:register-reader-extension "scm"
  (lambda ()
    zen:scheme-readtable))


(zen:register-reader-extension "zen"
  (lambda ()
    (zen:load-unit 'zen.dialect)
    zen:zen-readtable)))
