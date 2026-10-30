;;;=============
;;;  ZenScheme
;;;=============
;;;
;;;; Zen Install
;;;


(unit zen/install


;;;
;;;; Literals
;;;


;; having the literal class as a literal is a bit strange but usefull for marshalling
(zen:define-literal Literal          zen/literals#construct-literal)
(zen:define-literal Boxed            zen/literals#construct-boxed)
(zen:define-literal Point            zen/literals#construct-point)
(zen:define-literal Point$fl$        zen/literals#construct-point$fl$)
(zen:define-literal Distance         zen/literals#construct-distance)
(zen:define-literal Dimension        zen/literals#construct-dimension)
(zen:define-literal Dimension$fl$    zen/literals#construct-dimension$fl$)
(zen:define-literal Cell             zen/literals#construct-cell)
(zen:define-literal Rect             zen/literals#construct-rect)
(zen:define-literal Rect$fl$         zen/literals#construct-rect$fl$)
(zen:define-literal Range            zen/literals#construct-range)
(zen:define-literal Exception-Detail zen/literals#construct-exception-detail)
(zen:define-literal Walk-Location    zen/literals#construct-walk-location))
