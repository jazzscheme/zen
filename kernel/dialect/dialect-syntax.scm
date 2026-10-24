;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Dialect Syntax
;;;


(block dialect.dialect-syntax


;;;
;;;; Dialect
;;;


(zen:define-virtual-syntax (zen:dialect-walker (zen:Dialect dialect)))
(zen:define-virtual-syntax (zen:dialect-wrap (zen:Dialect dialect) body))


;;;
;;;; Walk Binding
;;;


(zen:define-virtual-syntax (zen:walk-binding-lookup (zen:Walk-Binding binding) symbol source-declaration))
(zen:define-virtual-syntax (zen:walk-binding-referenced (zen:Walk-Binding binding)))
(zen:define-virtual-syntax (zen:walk-binding-validate-call (zen:Walk-Binding binding) walker resume source-declaration operator arguments form-src))
(zen:define-virtual-syntax (zen:walk-binding-validate-assignment (zen:Walk-Binding binding) walker resume source-declaration symbol-src))
(zen:define-virtual-syntax (zen:walk-binding-assignable? (zen:Walk-Binding binding)))
(zen:define-virtual-syntax (zen:walk-binding-walkable? (zen:Walk-Binding binding)))
(zen:define-virtual-syntax (zen:walk-binding-walk-form (zen:Walk-Binding binding) walker resume declaration environment form))
(zen:define-virtual-syntax (zen:walk-binding-expandable? (zen:Walk-Binding binding)))
(zen:define-virtual-syntax (zen:walk-binding-expand-form (zen:Walk-Binding binding) walker resume declaration environment form))
(zen:define-virtual-syntax (zen:emit-binding-symbol (zen:Walk-Binding binding) source-declaration environment))
(zen:define-virtual-syntax (zen:emit-binding-reference (zen:Walk-Binding binding) source-declaration walker resume environment))
(zen:define-virtual-syntax (zen:emit-binding-call (zen:Walk-Binding binding) binding-src arguments arguments-codes source-declaration walker resume environment))
(zen:define-virtual-syntax (zen:emit-inlined-binding-call (zen:Walk-Binding binding) arguments call source-declaration walker resume environment))
(zen:define-virtual-syntax (zen:emit-binding-assignment (zen:Walk-Binding binding) value source-declaration walker resume environment form-src))


;;;
;;;; Lexical Binding
;;;


(zen:define-virtual-syntax (zen:resolve-binding (zen:Lexical-Binding binding)))


;;;
;;;; Declaration
;;;


(zen:define-virtual-syntax (zen:compose-declaration-locator (zen:Declaration declaration)))
(zen:define-virtual-syntax (zen:lookup-declaration (zen:Declaration declaration) symbol access source-declaration))
(zen:define-virtual-syntax (zen:get-declaration-inclusions (zen:Declaration declaration)))
(zen:define-virtual-syntax (zen:get-nextmethod-signature (zen:Declaration declaration)))
(zen:define-virtual-syntax (zen:emit-declaration (zen:Declaration declaration) walker resume environment))
(zen:define-virtual-syntax (zen:expand-referenced-declaration (zen:Declaration declaration)))
(zen:define-virtual-syntax (zen:outline-generate (zen:Declaration declaration) output))
(zen:define-virtual-syntax (zen:outline-extract (zen:Declaration declaration) meta))


;;;
;;;; Declaration Reference
;;;


(zen:define-virtual-syntax (zen:resolve-reference (zen:Declaration-Reference declaration-reference) module-declaration))


;;;
;;;; Namespace
;;;


(zen:define-macro (zen:get-access-lookup namespace-declaration access)
  `(%%vector-ref (zen:get-namespace-declaration-lookups ,namespace-declaration) ,access))


;;;
;;;; Walker
;;;


(zen:define-virtual-syntax (zen:walker-declarations (zen:Walker walker)))
(zen:define-virtual-syntax (zen:walker-bindings (zen:Walker walker)))
(zen:define-virtual-syntax (zen:walk-form (zen:Walker walker) resume declaration environment form))
(zen:define-virtual-syntax (zen:walk-symbol (zen:Walker walker) resume declaration environment symbol-src))
(zen:define-virtual-syntax (zen:walk-symbol-assignment (zen:Walker walker) resume declaration environment symbol-src value form-src))
(zen:define-virtual-syntax (zen:validate-proclaim (zen:Walker walker) resume declaration form-src))
(zen:define-virtual-syntax (zen:runtime-export (zen:Walker walker) declaration))
(zen:define-virtual-syntax (zen:lookup-environment (zen:Walker walker) resume declaration environment symbol-src symbol))
(zen:define-virtual-syntax (zen:lookup-analyse (zen:Walker walker) declaration symbol-src referenced-declaration))


;;;
;;;; Parameter
;;;


(zen:define-virtual-syntax (zen:emit-check? (zen:Parameter parameter)))
(zen:define-virtual-syntax (zen:emit-parameter (zen:Parameter parameter) declaration walker resume environment))


;;;
;;;; Expression
;;;


(zen:define-virtual-syntax (zen:emit-expression (zen:Expression expression) declaration walker resume environment))
(zen:define-virtual-syntax (zen:emit-call (zen:Expression expression) arguments arguments-codes declaration walker resume environment)))
