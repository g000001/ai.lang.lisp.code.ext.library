(in-package :ai.lang.lisp.code.ext.library.internal)

(defun nth-from-end (n list)
  (let* ((length (length list))
	 (delta (- length n)))
    (unless (minusp delta)
      (nth delta list))))
