;;;; ai.lang.lisp.code.ext.library.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)

(defsystem :ai.lang.lisp.code.ext.library
  :serial t
  :depends-on (:fiveam)
  :components ((:file "package")
               (:file "cl-utilities")
               (:file "extensions")
               (:file "interesting-ex")
               (:file "string-to-list")
               (:file "func_p")
               (:file "more")))

(defmethod perform ((o test-op) (c (eql (find-system :ai.lang.lisp.code.ext.library))))
  (load-system :ai.lang.lisp.code.ext.library)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :ai.lang.lisp.code.ext.library.internal :ai.lang.lisp.code.ext.library))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

