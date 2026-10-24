;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Kernel Repository
;;;


(block kernel.repository


;;;
;;;; Repository
;;;


;; A repository is where packages are located. The system comes with the following already
;; present repositories: build and zen: Note that repository order is important as it
;; defines search precedence.


(zen:define-structure Repository () (constructor: %%make-repository predicate: zen:repository? accessors-type: macro)
  ((name              getter: generate setter: generate)
   (directory         getter: generate)
   (library-root      getter: generate)
   (library-directory getter: generate)
   (binary?           getter: generate)
   (packages-table    getter: generate setter: generate)
   (dependencies      getter: generate)
   (versions          getter: generate setter: generate)))


;;;
;;;; Package
;;;


;; A package is the deployment unit that groups together related resources. Packages are
;; discovered automatically and their order within their repository should not be relevant.


(zen:define-structure Package () (constructor: %%make-package predicate: zen:package? accessors-type: macro)
  ((repository    getter: generate)
   (name          getter: generate)
   (parent        getter: generate)
   (library-root  getter: generate)
   (library-path  getter: generate)
   (units-root    getter: generate)
   (units-path    getter: generate)
   (install       getter: generate)
   (char-encoding getter: generate)
   (products      getter: generate)
   (profiles      getter: generate setter: generate)
   (project       getter: generate)
   (title         getter: generate)
   (description   getter: generate)
   (authors       getter: generate)
   (stage         getter: generate)))


;;;
;;;; Product
;;;


;; A product is the runtime implementation of some user level entity that can be run, updated, built and installed.


(zen:define-structure Product () (constructor: %%make-product accessors-type: macro)
  ((name            getter: generate)
   (title           getter: generate)
   (icon            getter: generate)
   (run             getter: generate)
   (test            getter: generate)
   (update          getter: generate)
   (build           getter: generate)
   (options         getter: generate)
   (library-options getter: generate)
   (install         getter: generate)
   (deploy          getter: generate)
   (package         getter: generate)
   (descriptor      getter: generate)))

 
;;;
;;;; Resource
;;;


;; A resource is a triplet (package path . extension) representing a resource inside
;; a package. Compilation will use the path part to put the binary outputs under the
;; build subdir of the architecture directory to enable a cross-compilation scheme.


(zen:define-structure Resource () (constructor: %%make-resource accessors-type: macro)
  ((package     getter: generate)
   (path        getter: generate)
   (underscore? getter: generate)
   (extension   getter: generate)))


;;;
;;;; Image-Unit
;;;


(zen:define-structure Image-Unit () (constructor: %%make-image-unit accessors-type: macro)
  ((load-proc         getter: generate)
   (compile-time-hash getter: generate)))


;;;
;;;; Digest
;;;


(zen:define-structure Digest () (constructor: %%make-digest accessors-type: macro)
  ((pathname getter: generate setter: generate)
   (hash     getter: generate setter: generate)
   (seconds  getter: generate setter: generate)))


;;;
;;;; Manifest
;;;


(zen:define-structure Manifest () (constructor: %%make-manifest accessors-type: macro)
  ((name              getter: generate)
   (version           getter: generate setter: generate)
   (compile-time-hash getter: generate setter: generate)
   (source-digests    getter: generate setter: generate)
   (references        getter: generate setter: generate)))


;;;
;;;; Unit
;;;


(zen:define-syntax unit
  (lambda (form-src)
    (zen:expand-unit-source (%%cdr (zen:source-code form-src)))))


(zen:define-syntax require
  (lambda (form-src)
    (zen:expand-require (%%cdr (zen:source-code form-src))))))
