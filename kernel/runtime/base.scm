;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Base
;;;


(block kernel.base


;;;
;;;; Exception
;;;


(define (zen:unimplemented . rest)
  (if (null? rest)
      (error "Unimplemented functionality")
    (error "Unimplemented functionality:" (car rest))))


;;;
;;;; Number
;;;


(define (zen:naturals from to)
  (let iter ((n (%%fx- to 1)) (lst '()))
       (if (%%fx< n from)
           lst
         (iter (%%fx- n 1) (%%cons n lst)))))


;;;
;;;; List
;;;


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


(define (zen:find-if predicate lst)
  (let iter ((scan lst))
    (if (null? scan)
        #f
      (let ((value (car scan)))
        (if (predicate value)
            value
          (iter (cdr scan)))))))


(define (zen:sort-list smaller l #!key (key #f))
  (declare (proper-tail-calls))
  (declare (optimize-dead-local-variables))
  (declare (inline))
  (declare (inlining-limit 1000))
  (define (apply-key key object)
    (if (not key)
        object
      (key object)))

  (define (merge-sort l)
    (define (merge l1 l2)
      (cond ((%%null? l1) l2)
            ((%%null? l2) l1)
            (else
             (let ((e1 (%%car l1)) (e2 (%%car l2)))
               (if (smaller (apply-key key e1) (apply-key key e2))
                   (%%cons e1 (merge (%%cdr l1) l2))
                 (%%cons e2 (merge l1 (%%cdr l2))))))))
    
    (define (split l)
      (if (or (%%null? l) (%%null? (%%cdr l)))
          l
        (%%cons (%%car l) (split (%%cddr l)))))
    
    (if (or (%%null? l) (%%null? (%%cdr l)))
        l
      (let* ((l1 (merge-sort (split l)))
             (l2 (merge-sort (split (%%cdr l)))))
        (merge l1 l2))))
  
  (merge-sort l))


;; stable merge sort from srfi-32 reference implementation
;; https://github.com/scheme-requests-for-implementation/srfi-32

(define (zen:sort-stable elt< lis)              ; A stable, natural merge sort
  
  (define (getrun lis) ; Pick off a contiguous run of sorted data.
    (let lp ((xs (%%cdr lis)) (prev (%%car lis)) (i 1))
         (if (%%pair? xs)
             (let ((x (%%car xs)))
               (if (elt< x prev) 
                   (values i xs)
                 (lp (%%cdr xs) x (%%fx+ i 1))))
           (values i xs))))
  
  (define (cmerge a la b lb)	; Counted merge.
    (let recur ((x (%%car a)) (a a) (la la)
                (y (%%car b)) (b b) (lb lb))
         (if (elt< y x)
             (%%cons y (let ((lb (%%fx- lb 1)))
                         (if (%%fx= lb 0)
                             a
                           (let ((b (%%cdr b)))
                             (recur x a la 
                               (%%car b) b lb)))))
           (%%cons x (let ((la (%%fx- la 1)))
                       (if (%%fx= la 0)
                           b
                         (let ((a (%%cdr a)))
                           (recur (%%car a) a la
                             y b lb))))))))
  
  (define (grow s ls ls2 u lw)	; The core of the sort algorithm.
    (if (or (%%fx<= lw ls) (%%not (%%pair? u)))
        (values s ls u)
      (let ((ls2 (let lp ((ls2 ls2))
                      (let ((ls2*2 (%%fx+ ls2 ls2)))
                        (if (%%fx<= ls2*2 ls) (lp ls2*2) ls2)))))
        ;; LS2 is now the largest power of two <= LS.
        (receive (lr u2) (getrun u)	; Get a run.
          (receive (t lt u3) (grow u lr 1 u2 ls2) ; Grow it.
            (grow (cmerge s ls t lt) (%%fx+ ls lt) ; Merge &
              (%%fx+ ls2 ls2) u3 lw))))))	     ;   loop.
  
  (define (first lis i)	; Nil-terminate LIS.
    (if (%%fx= 0 i)
        '()
      (%%cons (%%car lis) (first (%%cdr lis) (%%fx- i 1)))))
  
  (if (%%not (%%pair? lis))
      '()		; Note: (LENGTH LIS) or any constant 
    (receive (lr tail) (getrun lis)	; guaranteed to be greater can be used
      (let ((infinity #o100000000))	; in place of INFINITY.
        (receive (a la v) (grow lis lr 1 tail infinity)
          (first a la))))))


;;;
;;;; String
;;;


(define (zen:string-find str c #!optional (start 0))
  (declare (proper-tail-calls))
  (let ((len (%%string-length str)))
    (let iter ((n start))
      (cond ((%%fx>= n len)
             #f)
            ((%%char=? (%%string-ref str n) c)
             n)
            (else
             (iter (%%fx+ n 1)))))))


(define (zen:string-replace str old new)
  (declare (proper-tail-calls))
  (let ((cpy (string-copy str)))
    (let iter ((n (%%fx- (%%string-length cpy) 1)))
      (if (%%fx>= n 0)
          (begin
            (if (%%eqv? (%%string-ref cpy n) old)
                (%%string-set! cpy n new))
            (iter (%%fx- n 1)))))
    cpy))


(define (zen:string-starts-with? str target)
  (let ((sl (%%string-length str))
        (tl (%%string-length target)))
    (and (%%fx>= sl tl)
         (%%string=? (%%substring str 0 tl) target))))


(define (zen:string-search str target)
  (let ((sl (%%string-length str))
        (tl (%%string-length target)))
    (let iter ((n 0))
      (let ((end (%%fx+ n tl)))
        (if (%%fx<= end sl)
            (if (%%string=? (%%substring str n end) target)
                n
              (iter (%%fx+ n 1)))
          #f)))))


(define (zen:string-contains? str target)
  (if (zen:string-search str target)
      #t
    #f))


(define (zen:string-downcase str)
  (list->string (map char-downcase (string->list str))))


;;;
;;;; Symbol
;;;


(define (zen:global-bound? symbol)
  (and (%%global-var? symbol)
       (%%not (%%unbound? (%%global-var-ref symbol)))))

(define (zen:global-ref symbol)
  (%%global-var-ref symbol))

(define (zen:global-set! symbol value)
  (%%global-var-set! symbol value))

(define (zen:global-unbind! symbol)
  (%%global-var-unbind! symbol))


(define (zen:global-setting name default)
  (if (zen:global-bound? name)
      (zen:global-ref name)
    default))


(define (zen:break-reference identifier)
  (let ((str (%%symbol->string identifier)))
    (let ((n (or (zen:string-find str #\:)
                 (zen:string-find str #\#))))
      (if (%%not n)
          (values identifier #f)
        (values (%%string->symbol (%%substring str 0 n))
                (%%string->symbol (%%substring str (%%fx+ n 1) (%%string-length str))))))))


(define (zen:split-colon str)
  (let ((n (zen:string-find str #\#)))
    (if (%%not n)
        (values str #f)
      (values (%%substring str 0 n)
              (%%substring str (%%fx+ n 1) (%%string-length str))))))


(define (zen:unsafe-locator locator)
  (%%string->symbol (%%string-append "%%" (%%symbol->string locator))))


;;;
;;;; Pathname
;;;


(define (zen:pathname-type pathname)
  (let ((type (file-type pathname)))
    (if (eq? type 'regular)
        'file
      type)))

(define (zen:pathname-link? pathname)
  (let ((info (file-info pathname #f)))
    (%%eq? (file-info-type info) 'symbolic-link)))

(define zen:pathname-exists?
  file-exists?)


(define (zen:pathname-standardize path)
  (zen:string-replace path #\\ #\/))


(define (zen:pathname-normalize path #!optional (error? #t))
  (if (%%not (zen:pathname-exists? path))
      (if error?
          (zen:error "No such directory: {s}" path)
        #f)
    (let ((len (%%string-length path)))
      (let ((dir? (zen:string-ends-with? path "/")))
        (let ((normalized (path-normalize (if dir? (%%substring path 0 (%%fx- len 1)) path))))
          (let ((standardized (zen:pathname-standardize normalized)))
            (if (and dir? (%%not (zen:string-ends-with? standardized "/")))
                (string-append standardized "/")
              standardized)))))))


(define (zen:dirname-normalize dir)
  (if (zen:string-ends-with? dir "/")
      dir
    (string-append dir "/")))


(cond-expand
  (windows
    (define (zen:path=? path1 path2)
      (%%string-ci=? path1 path2)))
  (else
   (define (zen:path=? path1 path2)
     (%%string=? path1 path2))))


(define (zen:create-directory dir #!key (feedback #f))
  (if (%%not (file-exists? dir))
      (begin
        (if feedback
            (feedback "; creating {a}..." dir))
        (create-directory dir))))


(define (zen:create-directories dir #!key (feedback #f))
  (define (scan->directory scan)
    (%%string-append (zen:join-strings (%%reverse scan) "/") "/"))
  
  (let ((path (%%reverse (zen:split-string dir #\/))))
    (let iter ((scan (if (%%equal? (%%car path) "") (%%cdr path) path)))
      (if (%%not (%%null? scan))
          (begin
            (let ((parent (%%cdr scan)))
              (if (%%not (%%null? parent))
                  (let ((parent-dir (scan->directory parent)))
                    (if (zen:filesystem-allowed? parent-dir)
                        (iter parent)))))
            (let ((subdir (scan->directory scan)))
              (if (%%not (file-exists? subdir))
                  (zen:create-directory subdir feedback: feedback))))))))


(define (zen:directory-content directory)
  (directory-files (if (%%string? directory)
                       (%%list path: directory ignore-hidden: 'dot-and-dot-dot)
                     directory)))


(define (zen:directory-files directory)
  (zen:collect-if (lambda (name)
                     (%%eq? (zen:pathname-type (string-append directory name)) 'file))
                   (zen:directory-content directory)))


(define (zen:directory-directories directory)
  (zen:collect-if (lambda (name)
                     (%%eq? (zen:pathname-type (string-append directory name)) 'directory))
                   (zen:directory-content directory)))


(define (zen:directory-collect directory type)
  (zen:collect-if (lambda (name)
                     (%%eq? (zen:pathname-type (string-append directory name)) type))
                   (zen:directory-content directory)))


(define (zen:delete-directory directory #!optional (level 0) (delete-file? #f) (delete-directory? #f) (feedback #f))
  (if (zen:empty-directory directory level delete-file? delete-directory? feedback)
      (begin
        (if feedback
            (feedback directory level))
        (delete-directory directory)
        #t)
    #f))


(define (zen:empty-directory directory #!optional (level 0) (delete-file? #f) (delete-directory? #f) (feedback #f))
  (define (default-delete-file? file level)
    #t)
  
  (define (default-delete-directory? dir level)
    (zen:empty-directory dir level delete-file? delete-directory? feedback))
  
  (let ((empty? #t))
    (for-each (lambda (name)
                (let ((file (string-append directory name)))
                  (if ((or delete-file? default-delete-file?) file level)
                      (begin
                        (if feedback
                            (feedback file level))
                        (delete-file file))
                    (set! empty? #f))))
              (zen:directory-files directory))
    (for-each (lambda (name)
                (let ((dir (string-append directory name "/")))
                  (if ((or delete-directory? default-delete-directory?) dir (%%fx+ level 1))
                      (begin
                        (if feedback
                            (feedback dir level))
                        (delete-directory dir))
                    (set! empty? #f))))
              (zen:directory-directories directory))
    empty?))


(define (zen:copy-directory src dst #!key (copy? #t) (feedback #f))
  (define (copy src dst level)
    (if feedback
        (feedback src level))
    (let ((src-content (zen:directory-content src)))
      (zen:create-directories dst)
      (for-each (lambda (name)
                  (let ((sub-src (string-append src name))
                        (sub-dst (string-append dst name)))
                    (if (or (%%eq? copy? #t) (copy? name level))
                        (case (zen:pathname-type sub-src)
                          ((file)
                           (zen:copy-file sub-src sub-dst))
                          ((directory)
                           (copy (string-append sub-src "/") (string-append sub-dst "/") (%%fx+ level 1)))))))
                src-content)))
  
  (copy src dst 0))


(define (zen:platform-eol-encoding platform)
  (case platform
    ((windows) 'cr-lf)
    (else 'lf)))


;; work around "~" not always existing on windows
(cond-expand
  (windows
   (define zen:home-directory
     (let ((home-dir #f))
       (lambda ()
         (define (home-heuristic)
           (if zen:kernel-windows-homedir
               (if (zen:string-starts-with? zen:kernel-windows-homedir ".")
                   (string-append zen:kernel-install zen:kernel-windows-homedir)
                 (let ((dir zen:kernel-windows-homedir))
                   (if (%%not (file-exists? dir))
                       (create-directory dir))
                   dir))
             ;; gambit uses HOME to determine the location of ~
             (if (getenv "HOME" #f)
                 "~"
               (get-special-folder CSIDL_LOCAL_APPDATA))))
         
         (or home-dir
             (let ((home (home-heuristic)))
               (set! home-dir home)
               home))))))
  (else
   (define zen:home-directory
     (lambda ()
       "~"))))


;;;
;;;; Command Line
;;;


(define (zen:option? arg)
  (and (%%fx> (%%string-length arg) 0)
       (%%char=? (%%string-ref arg 0) #\-)))


(define (zen:option-name arg)
  (let ((len (%%string-length arg)))
    (let ((start (if (and (%%fx>= len 2) (%%equal? (%%substring arg 0 2) "--"))
                     2
                   1)))
      (%%substring arg start len))))


(define (zen:option=? arg option)
  (and (zen:option? arg)
       (%%equal? (zen:option-name arg) option)))


(define (zen:find-option name options #!optional (default #f))
  (let ((pair (%%assoc name options)))
    (if pair
        (%%cdr pair)
      default)))


(define zen:*command-arguments*
  #f)


(cond-expand
  (mac
    (define (zen:effective-command-argument? arg)
      (%%not (zen:string-starts-with? arg "-psn"))))
  (else
    (define (zen:effective-command-argument? arg)
      #t)))


(define (zen:command-executable)
  (%%car (command-line)))


(define (zen:command-arguments)
  (or zen:*command-arguments*
      (let ((arguments (zen:collect-if zen:effective-command-argument? (%%cdr (command-line)))))
        (set! zen:*command-arguments* arguments)
        arguments)))


(define zen:kernel-runtime-switches
  '("nosource"))


(define (zen:add-kernel-runtime-switch switch)
  (set! zen:kernel-runtime-switches (%%cons switch zen:kernel-runtime-switches)))


(define (zen:add-kernel-runtime-switches switches)
  (set! zen:kernel-runtime-switches (%%append switches zen:kernel-runtime-switches)))


(define (zen:split-command-line arguments switches options missing-argument-for-option cont)
  (declare (proper-tail-calls))
  (define (split-commands arguments)
    (let iter ((commands '()) (arguments arguments))
      (if (or (%%null? arguments)
              (zen:option? (%%car arguments)))
          (values (%%reverse commands) arguments)
        (iter (%%cons (%%car arguments) commands) (%%cdr arguments)))))
  
  (receive (commands arguments) (split-commands arguments)
    (let loop ((args arguments)
               (rev-options '()))
      (if (and (%%pair? args)
               (zen:option? (%%car args)))
          (let ((opt (zen:option-name (%%car args)))
                (rest (%%cdr args)))
            (cond ((%%member opt switches)
                   (loop rest
                         (%%cons (%%cons opt #t) rev-options)))
                  ((%%member opt options)
                   (if (%%pair? rest)
                       (loop (%%cdr rest)
                             (%%cons (%%cons opt (%%car rest)) rev-options))
                     (begin
                       (if missing-argument-for-option
                           (missing-argument-for-option opt))
                       (loop rest rev-options))))
                  (else
                   (cont commands (%%reverse rev-options) args))))
        (cont commands (%%reverse rev-options) args)))))


;;;
;;;; Platform
;;;


(define (zen:linux-family)
  (let ((os (caddr (system-type))))
    (let ((str (symbol->string os)))
      (cond ((eq? os 'linux-gnu)
             'linux)
            ((or (zen:string-starts-with? str "openbsd")
                 (zen:string-starts-with? str "freebsd")
                 (zen:string-starts-with? str "netbsd"))
             'bsd)
            (else
             #f)))))


;;;
;;;; Process
;;;


(define (zen:->open-process-settings path-or-settings)
  (if (%%string? path-or-settings)
      (%%list 'path: path-or-settings)
    path-or-settings))


(define (zen:invoke-process path-or-settings)
  (let ((port (open-process `(,@(zen:->open-process-settings path-or-settings)
                              stdin-redirection: #f
                              stdout-redirection: #f
                              stderr-redirection: #f))))
    (process-status port)))


(define (zen:call-process path-or-settings)
  (let ((code (zen:invoke-process path-or-settings)))
    (if (not (= code 0))
        (zen:error "Process failed ({a}): {s}" code path-or-settings))))


;;;
;;;; Feedback
;;;


(define (zen:feedback fmt-string . rest)
  (display (apply zen:format fmt-string rest))
  (newline)
  (force-output))


;;;
;;;; Report
;;;


(define zen:*reporting?*
  #f)


(define zen:*report-port*
  #f)


(define (zen:reporting?)
  zen:*reporting?*)


;; worker
(define (zen:setup-reporting)
  (set! zen:*reporting?* #t))


;; master
(define (zen:setup-report file)
  (set! zen:*reporting?* #t)
  (set! zen:*report-port* (open-output-file file)))


(define (zen:report fmt-string . rest)
  (let ((message (apply zen:format fmt-string rest)))
    (cond (zen:worker-port
           (write (cons 'report message) zen:worker-port)
           (force-output zen:worker-port))
          (zen:*report-port*
           (let ((port zen:*report-port*))
             (display message port)
             (newline port)
             (force-output port))))))


;;;
;;;; Crash
;;;


(define zen:crash-reporter
  #f)


(define (zen:set-crash-reporter proc)
  (set! zen:crash-reporter proc)))
