;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Verbose
;;;


(declare
  (block)
  (standard-bindings)
  (extended-bindings)
  (not safe))


(##actlog-stop)


;; bongo
(define ##get-current-monotonic-time!
  (let ((u64vec (^#u64vector 0))
        (nanosecond (exact->inexact (expt 10 9))))
    (lambda (floats i)
      (##get-monotonic-time! u64vec 0)
      (let ((time (exact->inexact (^#u64vector-ref u64vec 0))))
        (^#f64vector-set! floats i (/ time nanosecond))))))


;;;
;;;; Time
;;;


(define zen:current-seconds
  ##current-time-point)


(define zen:current-monotonic
  (let ((f64vec (^#f64vector 0.)))
    (lambda ()
      (declare (not interrupts-enabled))
      (^#get-current-monotonic-time! f64vec 0)
      (^#f64vector-ref f64vec 0))))


#; ;; bongo
(define zen:current-monotonic-jiffies
  (let ((u64vec (^#u64vector 0)))
    (lambda ()
      (declare (not interrupts-enabled))
      (^#get-monotonic-jiffies! u64vec 0)
      (^#u64vector-ref u64vec 0))))


#; ;; bongo
(define zen:current-monotonic-frequency
  (let ((u64vec (^#u64vector 0)))
    (lambda ()
      (declare (not interrupts-enabled))
      (^#get-monotonic-frequency! u64vec 0)
      (^#u64vector-ref u64vec 0))))


#; ;; bongo
(define zen:current-monotonic-nanoseconds
  (let ((u64vec (^#u64vector 0)))
    (lambda ()
      (declare (not interrupts-enabled))
      (^#get-monotonic-nanoseconds! u64vec 0)
      (^#u64vector-ref u64vec 0))))


;;;
;;;; Boot
;;;


;; put here to be as early as possible in the boot
(define zen:kernel-boot
  (zen:current-seconds))

(define zen:kernel-boot-monotonic
  (zen:current-monotonic))


;;;
;;;; Verbose
;;;


(define zen:kernel-verbose?
  #f)


(define (zen:verbose-kernel unit)
  (if zen:kernel-verbose?
      (let ((port (console-port)))
        (display "; loading " port)
        (display unit port)
        (display "..." port)
        (newline port)
        (force-output port))))
