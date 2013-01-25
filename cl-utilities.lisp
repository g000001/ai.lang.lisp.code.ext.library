;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: UT; Base: 10; -*-
;;;_____________________________________________________________________________
;;;
;;;                       System: Common Lisp Utilities
;;;                       Module: MACROS
;;;
;;; Copyright (c): Forschungsgruppe DRUID, Hubertus Hohl
;;;                Universitaet Stuttgart
;;;
;;; File: /usr/local/lisp/xit/cl-utilities/cl-utilities.lisp
;;; File Creation Date: 11/18/91 15:29:14
;;; Last Modification Time: 12/16/92 09:16:07
;;; Last Modification By: Juergen Herczeg
;;;
;;;
;;; Changes (worth to be mentioned):
;;; ================================
;;;
;;; 07/29/1992 (Juergen) Function find-file has been extended to also accept
;;;                      full pathnames as the first argument, which may
;;;                      include directory, filename, and extension.
;;;                      The second and third argument (directories and 
;;;                      extensions) are now optional.  Extensions default
;;;                      to the list of the binary file type, e.g. "fasl",
;;;                      and the "lisp" file type, e.g. "lisp", in this
;;;                      order.
;;; 12/10/1992 (Hubertus) Macro ADD-HOOK: made default value for key argument 
;;;                       IF-NEEDED consistent with ADD-*-HOOK definitions.
;;;_____________________________________________________________________________

(in-package :ai.lang.lisp.code.ext.library.internal)


;;;
;;;  This file contains general multi-purpose macros and function definitions
;;;  


;_______________________________________________________________________________
;
;                               Points
;_______________________________________________________________________________

(defstruct (point (:constructor point (x y)))
  x y)

(defun point-= (thing1 thing2)
  (and (point-p thing1) (point-p thing2)
       (= (point-x thing1) (point-x thing2))
       (= (point-y thing1) (point-y thing2))))


(defun point-add (p1 p2)
  (point (+ (point-x p1) (point-x p2))
	 (+ (point-y p1) (point-y p2))))

(defun point-sub (p1 p2)
  (point (- (point-x p1) (point-x p2))
	 (- (point-y p1) (point-y p2))))

(defun point-incr (p1 p2)
  (incf (point-x p1) (point-x p2))
  (incf (point-y p1) (point-y p2))
  p1)

(defun point-decr (p1 p2)
  (decf (point-x p1) (point-x p2))
  (decf (point-y p1) (point-y p2))
  p1)

(defun point-incr-xy (p x y)
  (incf (point-x p) x)
  (incf (point-y p) y)
  p)

(defun point-decr-xy (p x y)
  (decf (point-x p) x)
  (decf (point-y p) y)
  p)



;_______________________________________________________________________________
;
;                               Regions
;_______________________________________________________________________________


(defstruct (region (:constructor region (x y w h)) (:include point))
  w h)

(defun region-= (thing1 thing2)
  (and (region-p thing1) (region-p thing2)
       (= (region-x thing1) (region-x thing2))
       (= (region-y thing1) (region-y thing2))
       (= (region-w thing1) (region-w thing2))
       (= (region-h thing1) (region-h thing2))))


(defun region-endx (reg)
  (+ (region-x reg) (region-w reg)))

(defun region-endy (reg)
  (+ (region-y reg) (region-h reg)))

(defun region-pos (reg)
  (point (region-x reg) (region-y reg)))

(defun region-size (reg)
  (point (region-w reg) (region-h reg)))

(defun region-ps (p s)
  (region (point-x p) (point-y p)
	  (point-x s) (point-y s)))

(defun region-corner (reg)
  (point (region-endx reg) (region-endy reg)))

(defun region-center (reg)
  (point (+ (point-x reg) (floor (region-w reg) 2))
	 (+ (point-y reg) (floor (region-h reg) 2))))

(defun region-containp (reg p)
  (region-containp-xy reg (point-x p) (point-y p)))

(defun region-containp-xy (reg x y)
  (and (>= x (region-x reg))
       (< x (region-endx reg))
       (>= y (region-y reg))
       (< y (region-endy reg))))

(defun region-between (p1 p2)
  (region (point-x p1) (point-y p1)
	  (- (point-x p2) (point-x p1)) (- (point-y p2) (point-y p1)))) 

(defun region-union (reg1 reg2)
  (region-between (point (min (region-x reg1) (region-x reg2))
			 (min (region-y reg1) (region-y reg2)))
		  (point (max (region-endx reg1) (region-endx reg2))
			 (max (region-endy reg1) (region-endy reg2)))))
			 
  


;_______________________________________________________________________________
;
;                            Basic Macros
;_______________________________________________________________________________

(define-modify-macro maxf (&rest maxima) max)
(define-modify-macro minf (&rest minima) min)

(defmacro ignoring-errors (&rest forms)
  `(let ((results
	  (multiple-value-list
	      (ignore-errors ,.forms))))    
     (cond ((car results) (apply #'values results))
	   (t (locally (declare (special **last-error**))
		(setq **last-error** (second results)))
	      (apply #'values results)))))

(defmacro remove-hook (foo hooks)
  ;; If HOOKS is a setf-able place expression then foo will be removed 
  `(setq ,hooks (remove ,foo ,hooks :test #'equal)))

(defmacro add-hook (foo hooks &key position (if-needed t))
  ;; If HOOKS holds a list foo will be added according
  ;; to the specified position (default: at the end).
  ;; In case IF-NEEDED is not nil FOO is not added if already on the list.
  `(unless (and ,if-needed (find ,foo ,hooks :test #'equal))
     (cond ((consp ,position)
	    (case (car, position)
	      (:after (let ((found (member (cadr ,position) ,hooks :test #'equal)))
			(if found
			    (setf (cdr found) (cons ,foo (cdr found)))
			  (push ,foo ,hooks))))
	      (otherwise
	       (warn "Position option ~S unknown. Using :front." ,position))))
	   (t (case ,position
		(:front (push ,foo ,hooks))
		((nil :end) (setf ,hooks (append ,hooks (list ,foo))))
		(otherwise
		 (warn "Position option ~S unknown. Using :front." ,position)))))
     ,hooks))


;_____________________________________________________________________________
;
;                            File Utilities
;_____________________________________________________________________________

;;(defun find-file (filename directory extensions)
;;  (let (foundpath dirpath)
;;      (dolist (dir (if (listp directory)
;;		       directory
;;		     (list directory)))
;;	(setq dirpath (pmds::make-source-pathname filename dir))
;;	(dolist (extension extensions)
;;	  (when (setq foundpath
;;		    (probe-file
;;		     (make-pathname :type extension
;;				    :defaults dirpath)))
;;	    (return-from find-file foundpath)))))
;;  nil)



(defun find-file (filename
		  &optional directories
			    (extensions
			     (list #-pmds "lisp"
                                   #+pmds (cdr pmds::*current-lisp-file-types*)
                                   #-pmds "fasl"
				   #+pmds (car pmds::*current-lisp-file-types*))))
  (let (foundpath path)
      (dolist (dir (if (consp directories)
		       directories
		     (list directories)))
	(dolist (extension (if (consp extensions)
			       extensions
			     (list extensions)))
	  (setq path
	      (apply #'make-pathname
		     (delete nil
			     (append
			      (if dir
				  (list #+symbolics :raw-name
					#-symbolics :name
					filename
					:defaults dir)
				(list :defaults filename))
			      (when extension
				(list :type extension))))))
	  (when (pathname-name path)
	    (setq foundpath (probe-file path)))
	  (when foundpath
	    (return-from find-file foundpath)))))
  nil)




(defun system-pathname (system-name relative-pathname)
  "return absolute pathname namestring for the pathname relative 
   to the system directory specified by SYSTEM-NAME."
  #+pmds (namestring
          (merge-pathnames relative-pathname
                           (pmds::system-definition-default-directory
                            (pmds::get-system-definition system-name))))
  #+asdf (asdf:system-relative-pathname system-name 
                                        relative-pathname))


;_____________________________________________________________________________
;
;                         UNIX shell commands
;_____________________________________________________________________________

(defun shell-command (&rest args)
  ;; run a UNIX shell command specified by args
  #+allegro (apply #'excl::run-shell-command args)
  #+(and (not allegro) asdf) (apply #'asdf:run-shell-command args))
  
;_______________________________________________________________________________
;
;                            Conversion Functions
;_______________________________________________________________________________

(defvar *compile-lambda-expression* t
  "determines whether lambda-expressions given to CONVERT-TO-FUNCTION
   should be compiled.")


(defun convert-to-function (value)
  #+(and allegro-version>= (version>= 4 0))
  (if (and *compile-lambda-expression*
	   (listp value)
	   (eq (car value) 'lambda))
      (compile nil value)
    (coerce value 'function))	
  #-(and allegro-version>= (version>= 4 0))
  (cond ((symbolp value)
	 (if (and (fboundp value)
		  (not (macro-function value))
		  (not (special-operator-p value)))
	     (symbol-function value)
	   (error "Can't coerce ~S to type FUNCTION." value)))
	((and (listp value)
	      (eq (car value) 'lambda))
	 (if *compile-lambda-expression*
	     (compile nil value)
	   (eval `#',value)))
	((functionp value) value)
	(t
	 (error "Can't coerce ~S to type FUNCTION." value))))
		  
(defun convert-to-string (value)
  (if (stringp value)
      value
    (let ((new-string (format nil "~A" value)))
	(string-downcase new-string))))

(defun convert-to-readable-string (value)
  (if (stringp value)
      value
    (let ((new-string (format nil "~S" value)))
	(string-downcase new-string))))

(defun convert-from-string (string)
  (read-from-string string nil nil))



;;;_______________________________________
;;;
;;;    Querying the user
;;;_______________________________________

(defun query-string (query-string &key special default
					(test nil)
					(trim-chars '(#\Space #\Tab)))
  "query user."
  (let ((input nil))
    (loop 
      (setq input
	(progn (if default
		   (format *query-io* "~&~a (~a) " query-string default)
		 (format *query-io* "~&~a " query-string))
	       (string-trim trim-chars
			    (read-line *query-io*))))
      (when (and default (string= input ""))
	(setq input default))
      (when (or (and test (funcall test input))
		(string/= input ""))
	(when special
	  (set special input))
	(return
	  input)))))

;_______________________________________________________________________________
;
;                            Utility Functions
;_______________________________________________________________________________

(defun delete-all (sublist list)
  (delete-if #'(lambda (slot) (member slot sublist :test #'eq))
	     list))
