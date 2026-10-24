;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Foundation Dialect
;;;


(module foundation foundation.dialect


;;;
;;;; Digest
;;;


(native open-digest)
(native close-digest)
(native digest-update-subu8vector)
(native digest-string)
(native digest-substring)
(native digest-u8vector)
(native digest-subu8vector)
(native digest-file)


;;;
;;;; Kernel
;;;


(native zen:debug-core?)
(native zen:debug-user?)
(native zen:valid-conditional-requirement)
(native zen:conditional-satisfied?)
(native zen:process-conditional)
(native zen:cond-expand-features)
(native zen:cond-expand-features-set!)
(native zen:cond-expand-features-add!)
(native zen:feature-satisfied?)
(native zen:generate-symbol)
(native zen:simplify-begin)
(native zen:compose-reference)


;;;
;;;; Syntactic Closure
;;;


(native zen:make-syntactic-closure)
(native zen:syntactic-closure?)
(native zen:syntactic-closure-form)
(native zen:unwrap-syntactic-closure)
(native zen:strip-syntactic-closures)
(native zen:strip-source-info)
(native zen:identifier?)
(native zen:identifier=?)
(native zen:sc-macro-transformer)
(native zen:rsc-macro-transformer)
(native zen:er-macro-transformer)


;;;
;;;; Syntax
;;;


(native zen:source?)
(native zen:text-source?)
(native zen:source-code)
(native zen:source-locat)
(native zen:desourcify)
(native zen:desourcify-all)
(native zen:sourcify)
(native zen:sourcify-if)
(native zen:sourcify-deep)
(native zen:sourcify-deep-if)
(native zen:extract-location)
(native zen:present-source)
(native zen:save-emit-to)
(native zen:locat-container)
(native zen:locat-position)
(native zen:locat-start)
(native zen:locat-end)
(native zen:locat->container/line/col)
(native zen:locat->path/container/start/end&)
(native zen:container->path)
(native zen:position->filepos)
(native zen:filepos-line)
(native zen:filepos-col)


;;;
;;;; Table
;;;


(native zen:table-iterate)
(native zen:table-iterate-safe))
