;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Readtable
;;;


(block kernel.readtable


(define (zen:make-zen-readtable)
  (let ((readtable (readtable-max-unescaped-char-set (%%make-standard-readtable) #\U0010ffff)))
    (zen:zenify-readtable! readtable)
    readtable))


(define (zen:zenify-readtable! readtable)
  (zen:readtable-named-char-table-set! readtable (%%append (zen:readtable-named-char-table readtable) zen:named-chars))
  (%%readtable-char-class-set! readtable #\[ #t zen:read-tensor)
  (%%readtable-char-class-set! readtable #\{ #t zen:read-literal)
  (%%readtable-char-class-set! readtable #\@ #t zen:read-comment)
  (%%readtable-char-sharp-handler-set! readtable #\" zen:read-delimited-string)
  (%%readtable-char-sharp-handler-set! readtable #\/ zen:read-syntax-string))


(define zen:named-chars
  '(("nul"                  . #\x00)
    ("home"                 . #\x01)
    ("enter"                . #\x03)
    ("end"                  . #\x04)
    ("info"                 . #\x05)
    ("backspace"            . #\x08)
    ("tab"                  . #\x09)
    ("page-up"              . #\x0B)
    ("page-down"            . #\x0C)
    ("escape"               . #\x1B)
    ("left-arrow"           . #\x1C)
    ("right-arrow"          . #\x1D)
    ("up-arrow"             . #\x1E)
    ("down-arrow"           . #\x1F)
    ("space"                . #\x20)
    ("exclamation-mark"     . #\x21)
    ("double-quote"         . #\x22)
    ("sharp"                . #\x23)
    ("ampersand"            . #\x26)
    ("quote"                . #\x27)
    ("open-parenthesis"     . #\x28)
    ("close-parenthesis"    . #\x29)
    ("times"                . #\x2A)
    ("plus"                 . #\x2B)
    ("comma"                . #\x2C)
    ("minus"                . #\x2D)
    ("period"               . #\x2E)
    ("slash"                . #\x2F)
    ("colon"                . #\x3A)
    ("semi-colon"           . #\x3B)
    ("equal"                . #\x3D)
    ("question-mark"        . #\x3F)
    ("at"                   . #\x40)
    ("open-bracket"         . #\x5B)
    ("backslash"            . #\x5C)
    ("close-bracket"        . #\x5D)
    ("exponential"          . #\x5E)
    ("underscore"           . #\x5F)
    ("backquote"            . #\x60)
    ("open-brace"           . #\x7B)
    ("vertical-bar"         . #\x7C)
    ("close-brace"          . #\x7D)
    ("tilde"                . #\x7E)
    ("delete"               . #\x7F)
    ("copyright"            . #\xA9)))


(define (zen:read-tensor re c)
  (declare (proper-tail-calls))
  (let ((port (zen:readenv-port re))
        (start-pos (%%readenv-current-filepos re)))
    (read-char port)
    (let ((lst (%%build-list re #t start-pos #\])))
      (zen:readenv-wrap re
                         (if (%%null? lst)
                             (%%vector)
                           (let ((first (%%car lst))
                                 (len (%%length lst)))
                             (if (%%f64vector? (zen:source-code first))
                                 (let ((vec (%%make-vector len)))
                                   (let loop ((lst lst) (n 0))
                                        (if (%%not (%%null? lst))
                                            (begin
                                              (%%vector-set! vec n (zen:source-code (%%car lst)))
                                              (loop (%%cdr lst) (%%fx+ n 1)))))
                                   `(tensor ,vec))
                               (let ((vec (%%make-f64vector len)))
                                 (let loop ((lst lst) (n 0))
                                      (if (%%not (%%null? lst))
                                          (begin
                                            (%%f64vector-set! vec n (%%exact->inexact (zen:source-code (%%car lst))))
                                            (loop (%%cdr lst) (%%fx+ n 1)))))
                                 vec))))))))


(define zen:in-expression-comment?
  (%%make-parameter #f))


(define zen:read-literals?
  (%%make-parameter #t))

(define zen:read-literal-hook
  (%%make-parameter #f))


(zen:define-variable zen:new-literal)


(define (zen:read-literal re c)
  (let ((port (zen:readenv-port re))
        (start-pos (%%readenv-current-filepos re)))
    (read-char port)
    (if (%%eqv? (peek-char port) #\@)
        (zen:error "Trying to read an unreadable literal")
      (let ((lst (%%build-list re #t start-pos #\})))
        (zen:readenv-wrap re
                           (cond ;; do not read
                                 ((or (%%not (zen:read-literals?)) (zen:in-expression-comment?))
                                  #f)
                                 ;; empty literal
                                 ((%%null? lst)
                                  (%%unspecified))
                                 ;; walk
                                 ((zen:walk-for)
                                  (zen:new-literal (%%car lst) (%%cdr lst)))
                                 ;; read
                                 (else
                                  (let ((name (%%car lst))
                                        (arguments (%%cdr lst))
                                        (hook (zen:read-literal-hook)))
                                    (or (and hook (hook name arguments))
                                        (zen:construct-literal name arguments))))))))))


(define (zen:read-comment re c)
  (let ((port (zen:readenv-port re)))
    (parameterize ((zen:in-expression-comment? #t))
      (read-char port)
      (let ((name (read port)))   ; comment name
        (if (%%symbol? name)
            (read port)           ; commented expr
          (zen:error "Invalid @ comment"))))
    (%%read-datum-or-label-or-none-or-dot re)))


(define (zen:read-delimited-string re next start-pos)
  (let ((port (zen:readenv-port re)))
    (read-char port)
    (let ((output (open-output-string)))
      (let iter ()
           (let ((c (read-char port)))
             (cond ((%%eof-object? c)
                    #f)
                   ((%%eqv? c #\\)
                    (let ((escaped (read-char port)))
                      (case escaped
                        ((#\()
                         (write-char #\x02 output)
                         (iter))
                        ((#\))
                         (write-char #\x03 output)
                         (iter))
                        (else
                         (write-char escaped output)
                         (iter)))))
                   ((and (%%eqv? c #\")
                         (%%eqv? (peek-char port) #\#))
                    (read-char port)
                    (zen:readenv-wrap re (get-output-string output)))
                   (else
                    (write-char c output)
                    (iter))))))))


(define (zen:read-syntax-string re next start-pos)
  (let ((port (zen:readenv-port re)))
    (read-char port)
    (let iter ()
         (let ((c (read-char port)))
           (cond ((%%eof-object? c)
                  #f)
                 ((%%eqv? c #\/)
                  (let ((output (open-output-string)))
                    (let iter ()
                         (let ((c (read-char port)))
                           (cond ((%%eof-object? c)
                                  #f)
                                 ((%%eqv? c #\/)
                                  (let sub ()
                                       (let ((next (read-char port)))
                                         (cond ((%%eqv? next #\/)
                                                (cond ((%%eqv? (peek-char port) #\#)
                                                       (read-char port)
                                                       (zen:readenv-wrap re (get-output-string output)))
                                                      (else
                                                       (write-char c output)
                                                       (sub))))
                                               (else
                                                (write-char c output)
                                                (write-char next output)
                                                (iter))))))
                                 (else
                                  (write-char c output)
                                  (iter)))))))
                 ((or (%%eqv? c #\newline) (%%eqv? c #\return))
                  (zen:error "Invalid syntax string"))
                 (else
                  (iter)))))))


(define zen:zen-readtable
  (zen:make-zen-readtable))


(define (zen:with-zen-readtable thunk)
  (parameterize ((current-readtable zen:zen-readtable))
    (thunk))))
