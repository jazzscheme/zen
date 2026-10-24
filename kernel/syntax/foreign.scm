;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Foreign Expansion
;;;


(block kernel.foreign


;; quicky duplicated code
(define enter-marker
  ";---> ")

(define exit-marker
  ";<--- ")


(zen:define-macro (c-include include)
  (let ((str (string-append "#include " include)))
    (if (%%not zen:kernel-debug-foreign?)
        `(c-declare ,str)
      `(begin
         (zen:logging-line ,(string-append enter-marker str))
         (c-declare ,str)
         (zen:logging-line ,(string-append exit-marker str))))))


(zen:define-macro (c-declaration name-or-declaration . rest)
  (let ((name (if (symbol? name-or-declaration) name-or-declaration 'anonymous))
        (declaration (if (symbol? name-or-declaration) (car rest) name-or-declaration)))
    (if (%%not zen:kernel-debug-foreign?)
        `(c-declare ,declaration)
      (let ((str (symbol->string name)))
        `(begin
           (zen:logging-line ,(string-append enter-marker str " declaration"))
           (c-declare ,declaration)
           (zen:logging-line ,(string-append exit-marker str " declaration")))))))


(zen:define-macro (c-initialization name-or-code . rest)
  (let ((name (if (symbol? name-or-code) name-or-code 'anonymous))
        (code (if (symbol? name-or-code) (car rest) name-or-code)))
    (if (%%not zen:kernel-debug-foreign?)
        `(c-initialize ,code)
      (let ((str (symbol->string name)))
        `(begin
           (zen:logging-line ,(string-append enter-marker str " initialization"))
           (c-initialize ,code)
           (zen:logging-line ,(string-append exit-marker str " initialization")))))))


(zen:define-macro (c-definition signature types result-type c-name scope . body)
  (if (%%not zen:kernel-debug-foreign?)
      `(c-define ,signature ,types ,result-type ,c-name ,scope
         ,@body)
    (let ((name (car signature)))
      `(c-define ,signature ,types ,result-type ,c-name ,scope
         (zen:logging-c->scheme ',name
           (lambda ()
             ,@body))))))


(zen:define-macro (c-function name types result-type c-name-or-code)
  (if (%%not zen:kernel-debug-foreign?)
      `(c-lambda ,types ,result-type ,c-name-or-code)
    (let ((variables (map (lambda (type)
                            (zen:generate-symbol))
                          types)))
      `(lambda ,variables
         (zen:logging-scheme->c ',name
           (lambda ()
             ((c-lambda ,types ,result-type ,c-name-or-code)
              ,@variables))
           ,@variables)))))


(zen:define-macro (c-external signature result-type . rest)
  (let ((name (car signature))
        (types (cdr signature)))
    (let ((c-name-or-code (if (null? rest) (symbol->string name) (car rest))))
      (if (%%not zen:kernel-debug-foreign?)
          `(define ,name
             (c-lambda ,types ,result-type ,c-name-or-code))
        (let ((variables (map (lambda (type)
                                (zen:generate-symbol))
                              types)))
          `(define ,name
             (lambda ,variables
               (zen:logging-scheme->c ',name
                 (lambda ()
                   ((c-lambda ,types ,result-type ,c-name-or-code)
                    ,@variables))
                 ,@variables))))))))


#; ;; quicky as this cannot refer to c-function???
(zen:define-macro (c-external signature result-type . rest)
  (let ((name (car signature))
        (types (cdr signature)))
    (let ((c-name-or-code (if (null? rest) (symbol->string name) (car rest))))
      `(define ,name
         (c-function ,types ,result-type ,c-name-or-code))))))
