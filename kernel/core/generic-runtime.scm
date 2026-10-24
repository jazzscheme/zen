;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Generic Runtime
;;;


(block core.generic-runtime


;;;
;;;; Specific
;;;


(define (zen:new-specific signature-proc implementation)
  (zen:allocate-specific signature-proc implementation '() '()))


;;;
;;;; Generic


(define (zen:new-generic locator dynamic-signature root-proc)
  (let* ((name (zen:identifier-name locator))
         (generic (zen:allocate-generic locator name #f '())))
    (zen:generic-make-root generic dynamic-signature root-proc)
    generic))


(define (zen:generic-reset generic root-proc)
  (zen:generic-make-root generic (zen:get-specific-dynamic-signature (zen:get-generic-root-specific generic)) root-proc))


(define (zen:generic-make-root generic dynamic-signature root-proc)
  (let ((root-specific (zen:new-specific dynamic-signature (or root-proc (lambda rest (apply zen:invalid-generic-call generic rest))))))
    (zen:set-generic-root-specific generic root-specific)
    (zen:set-generic-pending-specifics generic (%%cons root-specific (zen:get-generic-pending-specifics generic)))))


(define (zen:invalid-generic-call generic . rest)
  (let ((dynamic-parameters (let iter ((signature (zen:get-specific-dynamic-signature (zen:get-generic-root-specific generic)))
                                       (rest rest))
                                 (if (and (%%pair? signature) (%%pair? rest))
                                     (%%cons (zen:class-of (%%car rest))
                                             (iter (%%cdr signature) (%%cdr rest)))
                                   '()))))
    (zen:generic-error generic dynamic-parameters)))


(define (zen:generic-error generic signature)
  (let ((name (zen:get-generic-name generic))
        (root-signature (zen:get-specific-dynamic-signature (zen:get-generic-root-specific generic))))
    (zen:error "Generic {a} cannot take {a}" (%%cons name root-signature) signature)))



;;;
;;;; Register
;;;


(define (zen:register-specific generic specific)
  (zen:set-generic-pending-specifics generic (%%cons specific (zen:get-generic-pending-specifics generic))))


;;;
;;;; Update
;;;


(define (zen:process-pending-specifics generic)
  ;; root must be done first - already present in the hierarchy
  (zen:resolve-signature (zen:get-generic-root-specific generic))
  (for-each (lambda (specific)
              (zen:resolve-signature specific)
              (zen:insert/replace-specific generic specific))
            (%%reverse (zen:get-generic-pending-specifics generic)))
  (zen:set-generic-pending-specifics generic '()))


(define (zen:resolve-signature specific)
  (let ((signature/proc (zen:get-specific-dynamic-signature specific)))
    (%%when (%%procedure? signature/proc)
      (zen:set-specific-dynamic-signature specific (signature/proc)))))


(define (zen:insert/replace-specific generic specific)
  (define (put-best-first specifics)
    (let iter ((scan (%%cdr specifics))
               (best (%%car specifics))
               (others '()))
         (if (%%pair? scan)
             (let ((specific (%%car scan)))
               (if (zen:specific-better? specific best)
                   (iter (%%cdr scan) specific (%%cons best others))
                 (iter (%%cdr scan) best (%%cons specific others))))
           (%%cons best others))))
  (let* ((dynamic-signature (zen:get-specific-dynamic-signature specific))
         (matches (zen:gather-dynamic-signature-ancestors generic dynamic-signature)))
    (cond ((%%not matches)
           (zen:generic-error generic dynamic-signature))
          ((%%pair? matches)
           ;; new node - replace ancestor/descendant links with ancestor/specific specific/descendant links
           ;; new node - keep nextmethod specific as first ancestor
           (let ((ancestors (put-best-first matches))
                 (descendant-specifics '()))
             (zen:set-specific-ancestor-specifics specific ancestors)
             (for-each (lambda (ancestor)
                         (let* ((ancestor-signature (zen:get-specific-dynamic-signature ancestor))
                                (partition (zen:partition (zen:get-specific-descendant-specifics ancestor)
                                                           (lambda (descendant)
                                                             (let ((descendant-signature (zen:get-specific-dynamic-signature descendant)))
                                                               (%%eq? 'ordered (zen:dynamic-signature-compare descendant-signature dynamic-signature))))
                                                           assv))
                                (descendant-partition (%%assq #t partition))
                                (descendants (if descendant-partition (%%cdr descendant-partition) '()))
                                (brother-partition (%%assq #f partition))
                                (brothers (if brother-partition (%%cdr brother-partition) '())))
                           (for-each (lambda (descendant)
                                       (let ((best (%%car (zen:get-specific-ancestor-specifics descendant))))
                                         (cond ((%%eq? ancestor best)
                                                (zen:set-specific-ancestor-specifics descendant
                                                                                   (%%cons specific (%%cdr (zen:get-specific-ancestor-specifics descendant)))))
                                               ((zen:specific-better? specific best)
                                                (zen:set-specific-ancestor-specifics descendant
                                                                                   (%%cons specific (zen:remove! ancestor (zen:get-specific-ancestor-specifics descendant)))))
                                               (else
                                                (zen:set-specific-ancestor-specifics descendant
                                                                                   (%%cons best (%%cons specific (zen:remove! ancestor (%%cdr (zen:get-specific-ancestor-specifics descendant)))))))))
                                       (%%when (%%not (%%memq descendant descendant-specifics))
                                         (set! descendant-specifics (%%cons descendant descendant-specifics))))
                                     descendants)
                           (zen:set-specific-descendant-specifics ancestor (%%cons specific brothers))))
                       ancestors)
             (zen:set-specific-descendant-specifics specific descendant-specifics)))
          (else
           ;; node already exists - since lexical nextmethod uses new specific we cannot just copy implementation into old node
           (let ((perfect-match matches))
             (%%when (%%eq? perfect-match (zen:get-generic-root-specific generic))
               (zen:set-generic-root-specific generic specific))
             (let ((ancestors (zen:get-specific-ancestor-specifics perfect-match)))
               (zen:set-specific-ancestor-specifics specific ancestors)
               (let iter ((ancestors ancestors))
                    (if (%%pair? ancestors)
                        (if (%%eq? perfect-match (%%car ancestors))
                            (%%set-car! ancestors specific)
                          (iter (%%cdr ancestors))))))
             (let ((descendants (zen:get-specific-descendant-specifics perfect-match)))
               (zen:set-specific-descendant-specifics specific descendants)
               (let iter ((descendants descendants))
                    (if (%%pair? descendants)
                        (if (%%eq? perfect-match (%%car descendants))
                            (%%set-car! descendants specific)
                          (iter (%%cdr descendants)))))))))))


;;;
;;;; Compare
;;;


(define (zen:gather-dynamic-signature-ancestors generic dynamic-signature)
  (let ((perfect-match #f))
    (or (let iter ((specifics (%%list (zen:get-generic-root-specific generic)))
                   (partial-matches '()))
             (if (%%pair? specifics)
                 (let ((specific (%%car specifics)))
                   (case (zen:dynamic-signature-compare dynamic-signature (zen:get-specific-dynamic-signature specific))
                     ((equal)
                      (set! perfect-match specific)
                      #f)
                     ((ordered)
                      (let ((found-in-descendants (iter (zen:get-specific-descendant-specifics specific) partial-matches)))
                        (if perfect-match
                            #f
                          (iter (%%cdr specifics) (or found-in-descendants
                                                      (if (%%memq specific partial-matches)
                                                          partial-matches
                                                        (%%cons specific partial-matches)))))))
                     (else
                      (iter (%%cdr specifics) partial-matches))))
               (if (%%null? partial-matches)
                   #f
                 partial-matches)))
        perfect-match)))


(define (zen:dynamic-signature-compare descendant-signature ancestor-signature)
  (let iter ((descendant-signature descendant-signature)
             (ancestor-signature ancestor-signature)
             (match 'equal))
       (if (or (%%null? descendant-signature) (%%null? ancestor-signature))
           (if (and (%%null? descendant-signature) (%%null? ancestor-signature))
               match
             'unordered)
         (cond ((%%eq? (%%car descendant-signature) (%%car ancestor-signature))
                (iter (%%cdr descendant-signature) (%%cdr ancestor-signature) match))
               ((zen:subcategory? (%%car descendant-signature) (%%car ancestor-signature))
                (if (%%eq? match 'reverse-ordered)
                    'unordered
                  (iter (%%cdr descendant-signature) (%%cdr ancestor-signature) 'ordered)))
               ((zen:subcategory? (%%car ancestor-signature) (%%car descendant-signature))
                (if (%%eq? match 'ordered)
                    'unordered
                  (iter (%%cdr descendant-signature) (%%cdr ancestor-signature) 'reverse-ordered)))
               (else
                'unordered)))))

;; not used
(define (zen:dynamic-signature-equal? dynamic-signature1 dynamic-signature2)
  (let iter ((dynamic-signature1 dynamic-signature1)
             (dynamic-signature2 dynamic-signature2))
       (if (or (%%null? dynamic-signature1) (%%null? dynamic-signature2))
           (and (%%null? dynamic-signature1) (%%null? dynamic-signature2))
         (and (%%eq? (%%car dynamic-signature1) (%%car dynamic-signature2))
              (iter (%%cdr dynamic-signature1) (%%cdr dynamic-signature2))))))


(define (zen:specific-better? specific1 specific2)
  (let iter ((signature1 (zen:get-specific-dynamic-signature specific1))
             (signature2 (zen:get-specific-dynamic-signature specific2)))
       (or (%%fx> (%%get-class-level (%%car signature1)) (%%get-class-level (%%car signature2)))
           (iter (%%cdr signature1) (%%cdr signature2)))))


;;;
;;;; Debug
;;;


(define (zen:display-tree generic)
  (%%when (%%not (%%null? (zen:get-generic-pending-specifics generic)))
    (zen:process-pending-specifics generic))
  (let iterate ((specifics (%%list (zen:get-generic-root-specific generic)))
                (level 0))
       (for-each (lambda (specific)
                   (write (%%list level
                                  specific
                                  (zen:get-specific-dynamic-signature specific)
                                  (zen:get-specific-ancestor-specifics specific)
                                  (zen:get-specific-descendant-specifics specific)))
                   (newline))
                 specifics)
       (for-each (lambda (specific)
                   (write (%%list level specific))
                   (newline)
                   (iterate (zen:get-specific-descendant-specifics specific) (%%fx+ level 1)))
                 specifics)))


;;;
;;;; Dispatch
;;;


(define (zen:dispatch-from-root generic dynamic-classes)
  (let ((matches (zen:gather-dynamic-signature-ancestors generic dynamic-classes)))
    (cond ((%%not matches)
           (zen:generic-error generic dynamic-classes))
          ((%%pair? matches)
           (%%car matches))
          (else
           matches)))))
