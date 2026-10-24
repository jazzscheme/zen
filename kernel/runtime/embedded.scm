;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Embedded
;;;


(block kernel.embedded


;;;
;;;; Launch
;;;


(c-define (launch-application boot) (char-string) void "launch_application" ""
  (zen:load-unit 'zen)
  (let ((boot (string->symbol boot)))
    (zen:load-unit boot)
    (zen:run-registered boot)))


;;;
;;;; Heartbeat
;;;


(c-define (zen:zen-heartbeat) () void "zen_heartbeat" "extern"
  (zen:thread-heartbeat!)
  (let ((thread-process 'zen.language.runtime:thread-process))
    (if (%%global-var? thread-process)
        (let ((func (%%global-var-ref thread-process)))
          (if (not (%%unbound? func))
              (func timeout: 0)))))))
