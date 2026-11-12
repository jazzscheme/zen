;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Build Core
;;;


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
;;;; Feedback
;;;


(define (zen:feedback fmt-string . rest)
  (display (apply zen:format fmt-string rest))
  (newline)
  (force-output))


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


(define zen:pathname-exists?
  file-exists?)


(define (zen:pathname-standardize path)
  (zen:string-replace path #\\ #\/))


(define (zen:pathname-normalize path #!optional (error? #t))
  (if (not (zen:pathname-exists? path))
      (if error?
          (zen:error "No such directory: {s}" path)
        #f)
    (let ((len (string-length path)))
      (let ((dir? (zen:string-ends-with? path "/")))
        (let ((normalized (path-normalize (if dir? (substring path 0 (- len 1)) path))))
          (let ((standardized (zen:pathname-standardize normalized)))
            (if (and dir? (not (zen:string-ends-with? standardized "/")))
                (string-append standardized "/")
              standardized)))))))


(define (zen:dirname-normalize dir)
  (if (zen:string-ends-with? dir "/")
      dir
    (string-append dir "/")))


(define (zen:file-needs-update? src dst)
  (or (not (file-exists? dst))
      (> (zen:file-last-modification-seconds src)
         (zen:file-last-modification-seconds dst))))


(define (zen:maybe-relativise-directory basedir rootdir targdir)
  (let ((targdir (zen:pathname-normalize targdir))
        (basedir (zen:pathname-normalize basedir))
        (rootdir (zen:pathname-normalize rootdir)))
    (let ((targlen (string-length targdir))
          (baselen (string-length basedir))
          (rootlen (string-length rootdir)))
      (if (and (>= baselen rootlen)
               (string=? (substring basedir 0 rootlen) rootdir)
               (>= targlen rootlen)
               (string=? (substring targdir 0 rootlen) rootdir))
          (let ((suffix (substring basedir rootlen baselen))
                (relative-dir #f))
            (let iter ((n (- (string-length suffix) 1)))
              (if (>= n 0)
                  (begin
                    (if (eqv? (string-ref suffix n) #\/)
                        (set! relative-dir (string-append (or relative-dir "") "../")))
                    (iter (- n 1)))))
            (values (string-append (or relative-dir "./") (substring targdir rootlen targlen)) #t))
        (values targdir #f)))))


(define (zen:relativise-directory basedir rootdir targdir)
  (receive (directory relative?) (zen:maybe-relativise-directory basedir rootdir targdir)
    directory))


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
