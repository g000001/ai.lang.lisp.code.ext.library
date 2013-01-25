(in-package :ai.lang.lisp.code.ext.library.internal)

;;; From Bill York <york@parc.xerox.com>
(defun funcallable-p (thing)
  (etypecase thing
    (symbol (fboundp thing))
    (function T)))
