;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Zen Dialect
;;;


(module protected zen.dialect scheme


(require (zen.dialect.runtime)
         (zen.backend)
         (zen.backend.core))


(export (zen.dialect.kernel)
        (zen.dialect.syntax (phase syntax))
        (gambit (except break current-directory current-time error print seconds->time time->seconds time? trace unbreak untrace))))
