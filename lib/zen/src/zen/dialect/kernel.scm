;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Zen Dialect Kernel
;;;


(module protected zen.dialect.kernel scheme


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
;;;; Backend
;;;


(native zen:new-backend)
(native zen:register-backend)
(native zen:register-emit)
(native zen:add-backend-binding)
(native zen:emit-namespace-statements)
(native zen:new-code)
(native zen:sourcified-form)
(native zen:sourcified-form2)
(native zen:codes-forms)


;;;
;;;; Binary
;;;


(native scan-s8)
(native put-s8)
(native scan-u8)
(native put-u8)
(native scan-s16)
(native put-s16)
(native scan-s16-big-endian)
(native put-s16-big-endian)
(native scan-u16)
(native put-u16)
(native scan-s32)
(native put-s32)
(native scan-u32)
(native put-u32)
(native scan-s32-big-endian)
(native put-s32-big-endian)
(native scan-u32-big-endian)
(native put-u32-big-endian)
(native scan-s64)
(native put-s64)
(native scan-s64-big-endian)
(native put-s64-big-endian)
(native scan-u64)
(native put-u64)
(native scan-float)
(native put-float)
(native scan-floats32!)
(native scan-floats64!)
(native scan-float-big-endian)
(native put-float-big-endian)
(native scan-double)
(native put-double)
(native scan-double-big-endian)
(native put-double-big-endian)
(native scan-c-string)
(native put-c-string)
(native scan-size-string)
(native put-size-string)
(native scan-utf-8-string)
(native put-utf-8-string)


;;;
;;;; Boolean
;;;


(native zen:boolean)


;;;
;;;; Call-Site
;;;


(native zen:new-call-site)


;;;
;;;; Category
;;;


(native zen:get-category-identifier)
(native zen:get-category-descendants)
(native zen:get-class-ascendant)


;;;
;;;; Char
;;;


(native zen:symbolic-char)
(native zen:char-symbol)


;;;
;;;; Closure
;;;


(native zen:closure?)
(native zen:closure-code)
(native zen:closure-length)
(native zen:closure-ref)
(native zen:closure-environment)


;;;
;;;; Configure
;;;


(native zen:enable-track-scheme?)
(native zen:enable-debug-garbage-collect?)


;;;
;;;; Continuation
;;;


(native zen:continuation-graft-no-winding)
(native zen:continuation-needs-winding?)
(native zen:continuation-parent)
(native zen:continuation-creator)
(native zen:continuation-locat)
(native zen:continuation-next)
(native zen:continuation-backtrace)


;;;
;;;; Core
;;;


(native zen:compile-unit)
(native zen:build-unit)
(native zen:build-image)
(native zen:build-library)
(native zen:for-each-subunit)
(native zen:load-manifest)


;;;
;;;; Debug
;;;


(native zen:run-loop?)
(native zen:terminal)
(native zen:terminal-string)
(native zen:terminal-line)
(native zen:terminal-newline)
(native zen:terminal-port)
(native zen:unimplemented)
(native zen:dump-stack)
(native zen:log-object)
(native zen:log-string)
(native zen:log-newline)
(native zen:close-log)
(native zen:object->vector)
(native zen:vector->object)
(native zen:object-copy)
(native zen:install-step-handler)


;;;
;;;; Devel
;;;


(native zen:subtype)


;;;
;;;; Development
;;;


;; These globals should really be some readtime mecanism like Gambit's #
;; Need to discuss with Marc Feeley about how to do that cleanly


(native ?)  (native get-?)  (native set-?)
(native ?a) (native get-?a) (native set-?a)
(native ?b) (native get-?b) (native set-?b)
(native ?c) (native get-?c) (native set-?c)
(native ?d) (native get-?d) (native set-?d)
(native ?e) (native get-?e) (native set-?e)
(native ?f) (native get-?f) (native set-?f)
(native ?g) (native get-?g) (native set-?g)
(native ?h) (native get-?h) (native set-?h)
(native ?i) (native get-?i) (native set-?i)
(native ?j) (native get-?j) (native set-?j)
(native ?k) (native get-?k) (native set-?k)
(native ?l) (native get-?l) (native set-?l)
(native ?m) (native get-?m) (native set-?m)
(native ?n) (native get-?n) (native set-?n)
(native ?o) (native get-?o) (native set-?o)
(native ?p) (native get-?p) (native set-?p)
(native ?q) (native get-?q) (native set-?q)
(native ?r) (native get-?r) (native set-?r)
(native ?s) (native get-?s) (native set-?s)
(native ?t) (native get-?t) (native set-?t)
(native ?u) (native get-?u) (native set-?u)
(native ?v) (native get-?v) (native set-?v)
(native ?w) (native get-?w) (native set-?w)
(native ?x) (native get-?x) (native set-?x)
(native ?y) (native get-?y) (native set-?y)
(native ?z) (native get-?z) (native set-?z)
(native ?0) (native get-?0) (native set-?0)
(native ?1) (native get-?1) (native set-?1)
(native ?2) (native get-?2) (native set-?2)
(native ?3) (native get-?3) (native set-?3)
(native ?4) (native get-?4) (native set-?4)
(native ?5) (native get-?5) (native set-?5)
(native ?6) (native get-?6) (native set-?6)
(native ?7) (native get-?7) (native set-?7)
(native ?8) (native get-?8) (native set-?8)
(native ?9) (native get-?9) (native set-?9)


(native %)  (native get-%)  (native set-%)
(native %a) (native get-%a) (native set-%a)
(native %b) (native get-%b) (native set-%b)
(native %c) (native get-%c) (native set-%c)
(native %d) (native get-%d) (native set-%d)
(native %e) (native get-%e) (native set-%e)
(native %f) (native get-%f) (native set-%f)
(native %g) (native get-%g) (native set-%g)
(native %h) (native get-%h) (native set-%h)
(native %i) (native get-%i) (native set-%i)
(native %j) (native get-%j) (native set-%j)
(native %k) (native get-%k) (native set-%k)
(native %l) (native get-%l) (native set-%l)
(native %m) (native get-%m) (native set-%m)
(native %n) (native get-%n) (native set-%n)
(native %o) (native get-%o) (native set-%o)
(native %p) (native get-%p) (native set-%p)
(native %q) (native get-%q) (native set-%q)
(native %r) (native get-%r) (native set-%r)
(native %s) (native get-%s) (native set-%s)
(native %t) (native get-%t) (native set-%t)
(native %u) (native get-%u) (native set-%u)
(native %v) (native get-%v) (native set-%v)
(native %w) (native get-%w) (native set-%w)
(native %x) (native get-%x) (native set-%x)
(native %y) (native get-%y) (native set-%y)
(native %z) (native get-%z) (native set-%z)
(native %0) (native get-%0) (native set-%0)
(native %1) (native get-%1) (native set-%1)
(native %2) (native get-%2) (native set-%2)
(native %3) (native get-%3) (native set-%3)
(native %4) (native get-%4) (native set-%4)
(native %5) (native get-%5) (native set-%5)
(native %6) (native get-%6) (native set-%6)
(native %7) (native get-%7) (native set-%7)
(native %8) (native get-%8) (native set-%8)
(native %9) (native get-%9) (native set-%9)


(native $)  (native get-$)  (native set-$)
(native $a) (native get-$a) (native set-$a)
(native $b) (native get-$b) (native set-$b)
(native $c) (native get-$c) (native set-$c)
(native $d) (native get-$d) (native set-$d)
(native $e) (native get-$e) (native set-$e)
(native $f) (native get-$f) (native set-$f)
(native $g) (native get-$g) (native set-$g)
(native $h) (native get-$h) (native set-$h)
(native $i) (native get-$i) (native set-$i)
(native $j) (native get-$j) (native set-$j)
(native $k) (native get-$k) (native set-$k)
(native $l) (native get-$l) (native set-$l)
(native $m) (native get-$m) (native set-$m)
(native $n) (native get-$n) (native set-$n)
(native $o) (native get-$o) (native set-$o)
(native $p) (native get-$p) (native set-$p)
(native $q) (native get-$q) (native set-$q)
(native $r) (native get-$r) (native set-$r)
(native $s) (native get-$s) (native set-$s)
(native $t) (native get-$t) (native set-$t)
(native $u) (native get-$u) (native set-$u)
(native $v) (native get-$v) (native set-$v)
(native $w) (native get-$w) (native set-$w)
(native $x) (native get-$x) (native set-$x)
(native $y) (native get-$y) (native set-$y)
(native $z) (native get-$z) (native set-$z)
(native $0) (native get-$0) (native set-$0)
(native $1) (native get-$1) (native set-$1)
(native $2) (native get-$2) (native set-$2)
(native $3) (native get-$3) (native set-$3)
(native $4) (native get-$4) (native set-$4)
(native $5) (native get-$5) (native set-$5)
(native $6) (native get-$6) (native set-$6)
(native $7) (native get-$7) (native set-$7)
(native $8) (native get-$8) (native set-$8)
(native $9) (native get-$9) (native set-$9)


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
;;;; Enumerator
;;;


(native zen:enumerator?)
(native zen:enumerator->symbol)
(native zen:symbol->enumerator)


;;;
;;;; Exception
;;;


(native zen:exception-reason)
(native zen:exception-detail)
(native zen:exception-locat)
(native zen:get-exception-hook)
(native zen:set-exception-hook)
(native zen:invoke-exception-hook)
(native zen:system-exception-hook)
(native zen:handle-exception-filter)
(native zen:catch-exception-filter)
(native zen:retry-exception-handler)
(native zen:dump-exception)
(native zen:new-walk-problems)


;;;
;;;; Exception-Detail
;;;


(native zen:new-exception-detail)


;;;
;;;; Field
;;;


(native zen:field? <object:bool>)
(native zen:field-name)
(native zen:category-field)
(native zen:add-field)


;;;
;;;; Fifo
;;;


(native zen:fifo->list)


;;;
;;;; Fixnum
;;;


(native zen:fixnum->flonum <fx:fl>)
(native zen:flonum->fixnum <fl:fx>)
(native zen:->flonum <:fl>)


;;;
;;;; Flonum
;;;


;; until proper call site casting of native calls

(native undocumented zen:sharp/sharp/fl+ <fl^fl:fl>)
(native undocumented zen:sharp/sharp/fl- <fl^fl:fl>)
(native undocumented zen:sharp/sharp/fl* <fl^fl:fl>)
(native undocumented zen:sharp/sharp/fl/ <fl^fl:fl>)

(native zen:flalloc <:fl>)
(native zen:flref)
(native zen:flset!)


;;;
;;;; Foreign
;;;


(native zen:still-obj-refcount-dec!)
(native zen:still-obj-refcount-inc!)
(native zen:still-copy)


;;;
;;;; Foundation
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
;;;; Heartbeat
;;;


(native zen:thread-heartbeat!)
(native zen:zen-heartbeat)


;;;
;;;; Host
;;;


(native zen:command-executable)
(native zen:command-arguments)


;;;
;;;; Identifier
;;;


(native zen:composite-identifier?)
(native zen:composite-reference?)
(native zen:compose-identifier)
(native zen:split-symbol)
(native zen:split-identifier)
(native zen:split-reference)
(native zen:break-reference)
(native zen:identifier-name <symbol>)
(native zen:reference-unit <symbol>)
(native zen:reference-name <symbol>)


;;;
;;;; Install
;;;


(cond-expand
  (windows
   (native CSIDL_PERSONAL)
   (native CSIDL_DESKTOPDIRECTORY)
   (native CSIDL_APPDATA)
   (native CSIDL_LOCAL_APPDATA)
   (native CSIDL_PROGRAM_FILES)
   (native get-special-folder))
  (else))


;;;
;;;; Integer
;;;


(native zen:arithmetic-shift-left <int^int:int>)
(native zen:arithmetic-shift-right <int^int:int>)


;;;
;;;; Kernel
;;;


(native zen:build-feedback)
(native zen:boot-directory)
(native zen:kernel-compiler)
(native zen:kernel-system)
(native zen:kernel-platform)
(native zen:kernel-processor)
(native zen:kernel-windowing)
(native zen:kernel-safety)
(native zen:kernel-optimize?)
(native zen:kernel-debug-environments?)
(native zen:kernel-debug-location?)
(native zen:kernel-debug-source?)
(native zen:kernel-debug-foreign?)
(native zen:kernel-mutable-bindings?)
(native zen:kernel-destination)
(native zen:kernel-features)
(native zen:kernel-properties)
(native zen:kernel-built)
(native zen:kernel-path)
(native zen:kernel-install)
(native zen:kernel-bundle-contents)
(native zen:kernel-bundle-root)
(native zen:kernel-bundle-install)
(native zen:kernel-root)
(native zen:kernel-source)
(native zen:kernel-source-root)
(native zen:kernel-source-accessible?)
(native zen:kernel-source-access?)
(native zen:kernel-version)
(native zen:kernel-boot)
(native zen:kernel-boot-monotonic)
(native zen:install-path)
(native zen:bundle-depth)
(native zen:get-update-version)
(native zen:get-update-targets)
(native zen:get-update-description)
(native zen:get-zen-version-number)
(native zen:zen-settings-directory)
(native zen:zen-settings-version)
(native zen:list->updates)
(native zen:home-directory)
(native zen:versioned-directory)
(native zen:path->container-override)
(native zen:platform-eol-encoding)
(native zen:load-debuggee-units)
(native zen:zen-product)
(native zen:zen-profile)
(native zen:unit-verbose?)
(native zen:load-verbose?)
(native zen:outline-verbose?)
(native zen:shape-verbose?)
(native zen:done-verbose?)
(native zen:warn-interpreted?)
(native zen:force-interpreted?)
(native zen:delay-reporting?)
(native zen:use-print?)
(native zen:use-debugger?)
(native zen:use-snapshot?)
(native zen:font-engine)
(native zen:compile-options)
(native zen:repositories-get)
(native zen:build-repository-get)
(native zen:load-repository)
(native zen:make-repository)
(native zen:install-repository)
(native zen:uninstall-repository)
(native zen:require-repository)
(native zen:find-repository)
(native zen:find-package)
(native zen:repository?)
(native zen:repository-name)
(native zen:repository-title)
(native zen:repository-directory)
(native zen:repository-library-root)
(native zen:repository-library-directory)
(native zen:repository-binary?)
(native zen:repository-dependencies)
(native zen:repository-packages)
(native zen:repository-find-package)
(native zen:repository-install-packages)
(native zen:repository-add-package)
(native zen:repository-remove-package)
(native zen:repository-pathname)
(native zen:package?)
(native zen:package-repository)
(native zen:package-name)
(native zen:package-root)
(native zen:package-units-root)
(native zen:package-pathname)
(native zen:package-root-pathname)
(native zen:package-directory)
(native zen:package-products)
(native zen:package-profiles)
(native zen:package-profiles-set!)
(native zen:package-project)
(native zen:package-title)
(native zen:package-description)
(native zen:package-authors)
(native zen:package-stage)
(native zen:resource-package)
(native zen:resource-path)
(native zen:resource-extension)
(native zen:split-version)
(native zen:present-version)
(native zen:load-package)
(native zen:load-debuggee)
(native zen:register-run)
(native zen:registered-run)
(native zen:run-registered)
(native zen:register-product)
(native zen:register-product-run)
(native zen:get-product-descriptor)
(native zen:find-product-descriptor)
(native zen:product-descriptor-named?)
(native zen:product-descriptor-name)
(native zen:product-descriptor-title)
(native zen:product-descriptor-description)
(native zen:product-descriptor-icon)
(native zen:product-descriptor-run)
(native zen:product-descriptor-product)
(native zen:product-descriptor-update)
(native zen:product-descriptor-build)
(native zen:product-descriptor-build-bundle)
(native zen:product-descriptor-dependencies)
(native zen:cond-expanded-product-descriptor-update)
(native zen:cond-expanded-product-descriptor-dependencies)
(native zen:run-product-descriptor)
(native zen:update-product-descriptor)
(native zen:build-product-descriptor)
(native zen:custom-compile/build)
(native zen:find-unit-product)
(native zen:find-unit-options)
(native zen:set-link-options)
(native zen:current-process-product)
(native zen:current-process-name)
(native zen:current-process-name-set!)
(native zen:current-process-title)
(native zen:current-process-title-set!)
(native zen:current-process-prefix)
(native zen:current-process-prefix-set!)
(native zen:current-process-traits)
(native zen:current-process-traits-set!)
(native zen:current-process-icon)
(native zen:current-process-icon-set!)
(native zen:current-process-version)
(native zen:current-process-version-set!)
(native zen:current-process-present)
(native zen:parent-directory)
(native zen:destination-directory)
(native zen:executable-extension)
(native zen:quote-pathname)
(native zen:quote-zen-pathname)
(native zen:zen-pathname)
(native zen:set-product-download)
(native zen:run-product)
(native zen:update-product)
(native zen:build-product)
(native zen:install-product)
(native zen:descendant-unit?)
(native zen:find-pathname-unit)
(native zen:find-unit-src)
(native zen:gather-profiles)
(native zen:make-profile)
(native zen:profile-name)
(native zen:profile-title)
(native zen:profile-appl)
(native zen:profile-unit)
(native zen:get-environment-table)
(native zen:get-environment-unit)
(native zen:unit-loaded?)
(native zen:unit-status)
(native zen:unit-uptodate-binary?)
(native zen:verbose-port)
(native zen:set-verbose-port)
(native zen:load-unit)
(native zen:load-count)
(native zen:load-hook)
(native zen:load-feedback-setup-count)
(native zen:load-feedback-done-count)
(native zen:load-feedback-setup)
(native zen:load-feedback-done)
(native zen:unload-unit)
(native zen:reload-unit)
(native zen:load-file)
(native zen:load-script)
(native zen:get-load-script-hook)
(native zen:set-load-script-hook)
(native zen:current-script-arguments)
(native zen:get-foreign-libraries)
(native zen:register-foreign-libraries)
(native zen:registered-foreign-libraries)
(native zen:get-load-mutex)
(native zen:get-load-thread)
(native zen:current-load-stack)
(native zen:get-modules-table)
(native zen:find-module)
(native zen:require-module)
(native zen:module-get)
(native zen:module-ref)
(native zen:module-set!)
(native zen:with-extension-reader)
(native zen:with-resource-reader)
(native zen:register-reader-extension)
(native zen:get-load-interpreted-hook)
(native zen:set-load-interpreted-hook)
(native zen:get-unit-not-found-hook)
(native zen:set-unit-not-found-hook)
(native zen:get-evaluate-forms-hook)
(native zen:set-evaluate-forms-hook)
(native zen:get-console-evaluate-hook)
(native zen:set-console-evaluate-hook)
(native zen:walk-literal)
(native zen:walk-for)
(native zen:register-service)
(native zen:find-service)
(native zen:require-service)
(native zen:register-coupler)
(native zen:get-couplers)
(native zen:activate-couplers)
(native zen:global-bound?)
(native zen:global-ref)
(native zen:global-set!)
(native zen:global-unbind!)
(native zen:global-setting)
(native zen:testing?)
(native zen:testing)
(native zen:generate-symbol-for)
(native zen:generate-symbol-context)
(native zen:image-load-counter)
(native zen:object-load-counter)
(native zen:interpreted-load-counter)
(cond-expand
  (mac
   (native zen:use-dlclose))
  (else))
(native zen:compiler-present?)
(native zen:compiler-name)
(native zen:compiler-extension)
(native zen:language-extension)
(native zen:codesign-required?)
(native zen:codesign-if)
(native zen:codesign)
(native zen:add-exit-job!)
(native zen:enable-crash-handler)
(native zen:disable-crash-handler)
(native zen:log-backtrace)
(native zen:set-crash-reporter)
(native zen:crash-process)
(native zen:invoke-process)
(native zen:call-process)
(native zen:feedback)
(native zen:reporting?)
(native zen:report)
(native zen:unit-loadable?)
(native zen:unit-obj-uptodate?)
(native zen:unit-source)
(native zen:split-command-line)
(native zen:quit)
(native zen:pkg-config)
(native zen:pkg-config-exists?)
(native zen:pkg-config-version)
(native zen:pkg-config-cflags)
(native zen:pkg-config-libs)
(native zen:file-executable?)
(native zen:file-permissions)
(native zen:file-permissions-set!)


;;;
;;;; List
;;;


(native zen:not-null?)
(native zen:listify)
(native zen:list-copy)
(native zen:last-pair)
(native zen:proper-list)
(native zen:sort-list)
(native zen:sort-stable)

;; until high-level optimized
(native zen:kernel-some?)
(native zen:kernel-every?)
(native zen:kernel-collect-if)


;;;
;;;; Logging
;;;


(native zen:logging?)
(native zen:set-logging?)


;;;
;;;; Memory
;;;


(native zen:gc)
(native zen:gc-count)
(native zen:gc-statistics)
(native zen:add-gc-interrupt-job!)
(native zen:clear-gc-interrupt-jobs!)
(native zen:last-gc-real-time)
(native zen:get-min-heap)
(native zen:set-min-heap!)
(native zen:get-max-heap)
(native zen:set-max-heap!)
(native zen:process-memory)
(native zen:symbols-memory)
(native zen:symbol-table)
(native zen:keyword-table)
(native zen:bytes-allocated!)
(native zen:classes-statistics)
(native zen:vector-size)
(native zen:f64vector-size)
(native zen:list-size)
(native zen:table-size)
(native zen:get-live-percent)
(native zen:raise-heap-overflow-exception)
(native zen:still8-minimum)
(native zen:still16-minimum)
(native zen:still32-minimum)
(native zen:still64-minimum)
(native MOVABLE0)
(native MOVABLE1)
(native MOVABLE2)
(native STILL)
(native PERM)
(native zen:memory-allocated?)
(native zen:memory-allocated-kind)
(native zen:memory-allocated-size)
(native zen:memory-size)
(native zen:gc-hash-table?)


;;;
;;;; Mutation
;;;


(native zen:loading-module)
(native zen:get-mutations)
(native zen:reset-mutations)
(native zen:register-mutation)


;;;
;;;; Number
;;;


(native zen:ratnum?)
(native zen:bignum?)


;;;
;;;; Object
;;;


(native zen:zen?)
(native zen:zenify)
(native zen:zenstruct?)
(native zen:zenstructify)
(native zen:new)
(native zen:nascent-new)
(native zen:class-of)
(native zen:object?)
(native zen:type?)
(native zen:category?)
(native zen:interface?)
(native zen:method?)
(native zen:is?)
(native zen:subtype?)
(native zen:subcategory?)
(native zen:subclass?)
(native zen:iterate-class-overrides)
(native zen:update-method)


;;;
;;;; Pathname
;;;


(native zen:pathname-type)
(native zen:pathname-link?)
(native zen:pathname-expand)
(native zen:pathname-normalize)
(native zen:file-last-access-seconds)
(native zen:file-last-modification-seconds)
(native zen:add-extension)
(native zen:copy-file)
(native zen:current-directory)
(native zen:with-current-directory)
(native zen:directory-content)
(native zen:directory-collect)


;;;
;;;; Port
;;;


(native zen:stdin-port)
(native zen:stdout-port)
(native zen:stderr-port)
(native zen:standard-input-port)
(native zen:standard-output-port)
(native zen:standard-error-port)
(native zen:unix-controlling-terminal?)
(native zen:terminal-available?)
(native zen:filesystem-authorize-terminal)
(native zen:eof-object)
(native zen:write-string)
(native zen:read-proper-line)
(native zen:read-source-all)
(native zen:read-source-first)
(native zen:read-literal-hook)
(native zen:output-port-width-set!)
(native zen:debug-port-setup-width)


;;;
;;;; Procedure
;;;


(native zen:procedure-name)
(native zen:procedure-name-set!)
(native zen:procedure-locat)


;;;
;;;; Property
;;;


(native zen:new-property)
(native zen:property-getter)
(native zen:property-setter)


;;;
;;;; Queue
;;;


(native zen:new-queue)
(native zen:queue-empty?)
(native zen:queue-length)
(native zen:enqueue)
(native zen:enqueue-list)
(native zen:dequeue)
(native zen:queue-list <any:list>)
(native zen:trim-queue)
(native zen:reset-queue)


;;;
;;;; Random
;;;


(native zen:random-integer-65536)


;;;
;;;; Readtable
;;;


(native zen:make-zen-readtable)
(native zen:make-standard-readtable)
(native zen:readtable-copy)
(native zen:readtable-char-delimiter?)
(native zen:readtable-char-delimiter?-set!)
(native zen:readtable-char-handler)
(native zen:readtable-char-handler-set!)
(native zen:readtable-char-sharp-handler)
(native zen:readtable-char-sharp-handler-set!)
(native zen:readtable-char-class-set!)
(native zen:readtable-paren-keyword-set!)
(native zen:readtable-bracket-keyword-set!)
(native zen:readtable-brace-keyword-set!)
(native zen:readtable-named-char-table)
(native zen:readtable-named-char-table-set!)
(native zen:readtable-escaped-char-table)
(native zen:readtable-escaped-char-table-set!)
(native zen:with-readtable)
(native zen:scheme-readtable)
(native zen:zen-readtable)
(native zen:with-zen-readtable)
(native zen:six-types)
(native zen:six-types-set!)
(native zen:print-marker)


;;;
;;;; Reference
;;;


(native zen:resolve-runtime-reference)
(native zen:serialize-runtime-reference)
(native zen:deserialize-runtime-reference)


;;;
;;;; Repl
;;;


(native zen:current-repl-context)
(native zen:repl-context-level)
(native zen:repl-context-depth)
(native zen:repl-context-cont)
(native zen:repl-context-initial-cont)
(native zen:repl-context-prev-level)
(native zen:repl-context-prev-depth)
(native zen:with-repl-context)
(native zen:inspect-repl-context)
(native zen:repl)
(native zen:repl-debug)
(native zen:eval-within-no-winding)
(native zen:repl-result-history-add)


;;;
;;;; Resource
;;;


(native zen:resource-package)
(native zen:resource-pathname)
(native zen:name->path)
(native zen:path->name)


;;;
;;;; Restricted
;;;


(native zen:filesystem-restrictable)
(native zen:filesystem-restricted?)
(native zen:filesystem-restrict)
(native zen:filesystem-unrestrict)
(native zen:filesystem-authorized)
(native zen:filesystem-authorize)
(native zen:filesystem-allowed?)
(native zen:validate-restricted)


;;;
;;;; Runtime
;;;


(native zen:get-object-slot)
(native zen:set-object-slot)
(native zen:find-slot-offset)
(native zen:dispatch)
(native zen:find-dispatch)
(native zen:call-into-abstract)
(native zen:get-core-classes)


;;;
;;;; Serial
;;;


(native zen:object->serial)
(native zen:serial->object)


;;;
;;;; Slot
;;;


(native zen:slot? <object:bool>)
(native zen:slot-value)
(native zen:set-slot-value)


;;;
;;;; Stack
;;;


(native zen:hidden-frame?)
(native zen:hidden-frame?-set!)
(native zen:get-continuation-stack)
(native zen:get-continuation-dynamic-environment)
(native zen:get-continuation-lexical-environment)
(native zen:get-continuation-location)
(native zen:interpreted-continuation?)


;;;
;;;; String
;;;


(native zen:string-find-reversed)
(native zen:string-starts-with?)
(native zen:string-ends-with?)
(native zen:split-string)
(native zen:join-strings)


;;;
;;;; Structure
;;;


(native zen:kind?)
(native zen:kind-id)
(native zen:kind-name)
(native zen:kind-flags)
(native zen:kind-super)
(native zen:kind-length)
(native zen:kind-fields)
(native zen:structure?)
(native zen:structure-kind)
(native zen:structure-ref)
(native zen:structure-set!)


;;;
;;;; Symbol
;;;


(native zen:generate-symbol)
(native zen:with-uniqueness)
(native zen:with-uniqueness-typed)


;;;
;;;; System
;;;


(native zen:switch?)
(native zen:switch-name)
(native zen:command-argument)
(native zen:command-argument?)
(native zen:find-option)
(native zen:exit-no-jobs)
(native zen:exit-jobs)
(native zen:add-exit-job!)
(native zen:clear-exit-jobs!)


;;;
;;;; Table
;;;


(native zen:table-iterate)
(native zen:table-iterate-safe)
(native zen:table-clear)
(native zen:map-table)
(native zen:table-entries)


;;;
;;;; Terminal
;;;


(native zen:set-terminal-title)
(native zen:bring-terminal-to-front)
(native zen:clear-terminal)


;;;
;;;; Thread
;;;


(native TERMINATE-INTERRUPT)
(native HEARTBEAT-INTERRUPT)
(native USER-INTERRUPT)
(native GC-INTERRUPT)
(native HIGH-LEVEL-INTERRUPT)
(native zen:interrupt-vector-set!)
(native zen:interrupts-enabled?)
(native zen:disable-interrupts!)
(native zen:enable-interrupts!)
(native zen:get-heartbeat-interval)
(native zen:set-heartbeat-interval!)
(native zen:thread-int!)
(native zen:thread-cont)
(native zen:thread-continuation)
(native zen:pristine-thread-continuation)
(native zen:thread-mutexes)
(native zen:write-timeout)


;;;
;;;; Thread With Stack
;;;


(native zen:thread-add-stack)
(native zen:make-thread-with-stack)


;;;
;;;; Time
;;;


(native zen:current-systime)
(native zen:systime?)
(native zen:systime->seconds)
(native zen:seconds->systime)
(native zen:current-seconds!)
(native zen:current-seconds <:fl>)
(native zen:current-monotonic <:fl>)
(native zen:current-monotonic-jiffies)
(native zen:current-monotonic-frequency)
(native zen:current-monotonic-nanoseconds)
(native zen:present-seconds)
(native zen:present-bytes)


;;;
;;;; Tracking
;;;


(native zen:tracking-allocations?)
(native zen:monitoring-allocations?)
(native zen:monitor-allocations-reset)
(native zen:monitor-allocations-start)
(native zen:monitor-allocations-stop)
(native zen:monitor-save-rate)
(native zen:monitor-capture-rate)
(native zen:monitor-allocation-rate)
(native zen:register-allocations)
(native zen:unregister-allocations)
(native zen:reset-allocations)
(native zen:registered-allocations)
(native zen:ordered-allocations)
(native zen:persist-allocations)
(native zen:unpersist-allocations)
(native zen:persisted-allocations-table)
(native zen:allocation-rank)
(native zen:allocation-size)
(native zen:allocation-thread)
(native zen:allocation-file)
(native zen:allocation-line)
(native zen:allocation-stack)


;;;
;;;; UDP
;;;


(native zen:udp-socket-tos)
(native zen:udp-socket-tos-set!)
(native zen:udp-socket-receive-buffer-size)
(native zen:udp-socket-receive-buffer-size-set!)
(native zen:udp-socket-send-buffer-size)
(native zen:udp-socket-send-buffer-size-set!)
(native zen:udp-socket-send-again-count)


;;;
;;;; Unspecified
;;;


(native zen:unspecified)
(native zen:unspecified?)
(native zen:specified?)


;;;
;;;; Values
;;;


(native zen:values?)
(native zen:values-ref)
(native zen:values-set!)


;;;
;;;; Vector
;;;


(native zen:vector-memq?)
(native zen:vector-memv?)


;;;
;;;; Walker
;;;


(native zen:analysis-mode?)
(native zen:analysis-data)
(native zen:call-walk-error)
(native zen:new-walk-context)
(native zen:specifier?)
(native zen:binding-specifier)
(native zen:parse-specifier)
(native zen:type->specifier)
(native zen:new-fixed-type)
(native zen:fixed-constructors-get)
(native zen:fixed-makers-get)
(native zen:add-primitive-type)
(native zen:primitive-patterns-get)
(native zen:primitive-predicates-get)
(native zen:requested-unit-name)
(native zen:requested-unit-resource)
(native zen:get-private-lookup)
(native zen:get-public-lookup)
(native zen:get-protected-lookup)
(native zen:get-catalog-table)
(native zen:get-catalog-entry)
(native zen:release-catalog-entries)
(native zen:outline-feedback)
(native zen:get-outline-not-found-hook)
(native zen:set-outline-not-found-hook)
(native zen:get-outline-hook)
(native zen:set-outline-hook)
(native zen:outline-unit)
(native zen:walk-unit)
(native zen:walk/outline-unit)
(native zen:walk-extended-definition-declaration)
(native zen:walk-extended-definition)
(native zen:walk-describe)
(native zen:generate-unit)
(native zen:emit-unit)
(native zen:check-unit)
(native zen:verify-unit)
(native zen:custom-compile-unit)
(native zen:expanding-unit)
(native zen:expand-unit)
(native zen:expand-form)
(native zen:expand-source)
(native zen:expand-to)
(native zen:expand-to-port)
(native zen:expand-to-file)
(native zen:expanding-script)
(native zen:expand-script)
(native zen:find-declaration)
(native zen:find-declaration-child)
(native zen:remove-declaration-child)
(native zen:reset-module-imported)
(native zen:get-unit/module-container)
(native zen:get-unit/module-requires))
