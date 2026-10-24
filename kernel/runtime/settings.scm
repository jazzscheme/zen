;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Settings
;;;


(block kernel.settings


;;;
;;;; Zen
;;;


(zen:define-setting zen:debug-expansion?
  #f)


;;;
;;;; Repositories
;;;


(zen:define-setting zen:build-repository
  #f)

(zen:define-setting zen:zen-repository
  #f)

(zen:define-setting zen:repositories
  #f)

(zen:define-setting zen:dependencies
  #f)

(zen:define-setting zen:absolutize-sources?
  #f)


;;;
;;;; Verbose
;;;


(zen:define-setting zen:unit-verbose?
  #f)

(zen:define-setting zen:load-verbose?
  #f)

(zen:define-setting zen:outline-verbose?
  #f)

(zen:define-setting zen:shape-verbose?
  #f)

(zen:define-setting zen:done-verbose?
  #f)


;;;
;;;; Interpret
;;;


(zen:define-setting zen:warn-interpreted?
  #f)


;;;
;;;; Walker
;;;


;; code walker warnings
(zen:define-setting zen:warnings?
  #f)

;; set to #f to debug the walker itself
(zen:define-setting zen:delay-reporting?
  #t)


;;;
;;;; Print
;;;


;; low-level setting to disable calling print methods
(zen:define-setting zen:use-print?
  #t)


;;;
;;;; Build
;;;


(zen:define-setting zen:debug-build?
  #f)

(zen:define-setting zen:build-configure
  #f)

(zen:define-setting zen:build-setup
  #f)

(zen:define-setting zen:build-binary-repositories
  #f)

(zen:define-setting zen:build-source-access?
  #t)

(zen:define-setting zen:build-zenini-access?
  #t)

(zen:define-setting zen:build-single-objects?
  #f)

(zen:define-setting zen:build-windows-homedir
  #f)

(zen:define-setting zen:build-link
  'objects)

(zen:define-setting zen:build-jobs
  #f)

(zen:define-setting zen:build-target
  #f)

(zen:define-setting zen:build-configuration
  #f)

(zen:define-setting zen:force-outlines?
  #f)

(zen:define-setting zen:dry-run?
  #f)

(zen:define-setting zen:save-emit?
  #f)


;;;
;;;; Debug
;;;


(zen:define-setting zen:debug-specializers
  '())


(zen:define-setting zen:force-interpreted?
  #f)


(zen:define-setting zen:use-debugger?
  #t)


(zen:define-setting zen:use-snapshot?
  #t)


;;;
;;;; Worker
;;;


(zen:define-setting zen:worker-repl?
  #f)


;;;
;;;; Profile
;;;


(zen:define-setting zen:profile-walker?
  #f)


;;;
;;;; Interface
;;;


(zen:define-setting zen:font-engine
  #f))
