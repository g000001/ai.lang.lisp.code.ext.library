;;; -*- Mode: LISP; Syntax: Common-lisp; Package: (EXTENSIONS); Base: 10 -*-
;;; Wed Feb 27 12:06:32 1991 by Mark Kantrowitz <mkant@GLINDA.OZ.CS.CMU.EDU>
;;; extensions.lisp

;;; ****************************************************************
;;; Extensions to Common Lisp **************************************
;;; ****************************************************************
;;;
;;; This file is a collection of extensions to Common Lisp. 
;;;
;;; It is a combination of the CL-LIB package copyleft by Brad Miller
;;; <miller@cs.rochester.edu> and a similar collection by
;;; Mark Kantrowitz <mkant+@cs.cmu.edu>.
;;;
;;; Please send bug reports and additions to mkant@cs.cmu.edu
;;;
;;; The following functions were originally from CL-LIB:
;;;   let-if, factorial, update-alist, truncate-keywords, while,
;;;   defclass-x, copy-hash-table, defflag, round-to, extract-keyword
;;;   let*-non-null, mapc-dotted-list, 
;;;   mapcar-dotted-list, mapcan-dotted-list, some-dotted-list, 
;;;   every-dotted-list, msetq, mlet, mapatoms, read-delimited-string
;;;   <baldwin@cs.geneseo.edu>:
;;;   remove-keywords, force-string, prefix?,
;;;   elapsed-time-in-seconds, bit-length, flatten, 
;;;   sum-of-powers-of-two-representation, 
;;;   difference-of-powers-of-two-representation,
;;;   ordinal-string, between, 
;;;   <quiroz@cs.rochester.edu>:
;;;   cond-binding-predicate-to, dosequence
;;;   More brad:
;;;   macro-indent-rule, parser-error, reverse-alist, fast-union,
;;;   fast-intersection, true-list-p, progfoo, with-rhyme, mv-progfoo
;;;   comment, xor, eqv, nand, nor
;;;
;;;   By Barmar: 
;;;   scheme-stream, cons-scheme-stream, ss-head, ss-tail, scheme-delay,
;;;   scheme-force
;;;
;;;   By Kerry Koitzsch:
;;;   get-compiled-function-name
;;;
;;; The following functions were contributed by Mark Kantrowitz:
;;;   circular-list, dofile, seq-butlast, seq-last, firstn, in-order-union,
;;;   parse-with-delimiter, parse-with-delimiters, string-search-car,
;;;   string-search-cdr, parallel-substitute, lisp::nth-value,
;;;   parse-with-string-delimiter, parse-with-string-delimiter*,
;;;   member-or-eq, number-to-string, null-string, time-string.
;;;   list-without-nulls, cartesian-product, cross-product, permutations
;;;   powerset, occurs, split-string, format-justified-string, 
;;;   eqmemb, neq, car-eq, dremove, displace, tailpush, explode,
;;;   implode, crush, listify-string, and-list, or-list, lookup,
;;;   make-variable, variablep, make-plist, make-keyword, y-or-n-p-wait.
;;;   noting-progress, note-progress, quotify-list
;;;   
;;; From Larry Stead <lstead@breeze.bellcore.com>: when-bind
;;;
;;; From Vincent Keunen <keunen@milou.nrb.be>: retain-keywords
;;;
;;; From "Fernando D. Mato Mira" <matomira@ligsg13.epfl.ch>: external-symbols
;;;
;;;
;;; The GNU General Public License agreement is included by reference.
;;; Share and Enjoy!
;;;

;; (in-package "EXTENSIONS" :nicknames '("EXT" "CL-LIB"))

(in-package :ai.lang.lisp.code.ext.library.internal)


;;; Uncomment this to make the extensions accessible from the Lisp package
;;; without the EXT prefix.
;(in-package "LISP")

;;; ********************************
;;; Sets ***************************
;;; ********************************
;;; list-without-nulls
;;; cross-product
;;; cartesian-product
;;; permutations

(defun list-without-nulls (list)
  "Returns a copy of list with all null elements removed."
  (let* ((head (list nil))
         (tail head))
    (loop
     (if (null list)
	 (return-from list-without-nulls (cdr head))
	 (when (car list)
	   (rplacd tail (list (car list)))
	   (setf tail (cdr tail))))
     (setf list (cdr list)))))

(defun cartesian-product (set1 set2)
  "Returns the cross product of two sets."
  (let ((result ()))
    (dolist (elt1 set1)
      (dolist (elt2 set2)
        (push (cons elt1 elt2) result)))
    result))

(defun cross-product (&rest lists)
  "Returns the cross product of a set of lists."
  (labels ((cross-product-internal (lists)
	     (if (null (cdr lists))
		 (mapcar #'list (car lists))
		 (let ((cross-product (cross-product-internal (cdr lists)))
		       (result '()))
		   (dolist (elt-1 (car lists))
		     (dolist (elt-2 cross-product)
		       (push (cons elt-1 elt-2) result)))
		   result))))
    (cross-product-internal lists)))

(defun permutations (items)
  "Given a list of items, returns all possible permutations of the list."
  (let ((result nil))
    (if (null items)
        '(nil)
        (dolist (item items result)
          (dolist (permutation (permutations (remove item items)))
            (push (cons item permutation) result))))))

(defun powerset (list)
  "Given a set, returns the set of all subsets of the set."
  (let ((result (list nil)))
    (dolist (item list result)
      (dolist (subset result)
	(push (cons item subset) result)))))

#|
(defun powerset (set)
  (if (null set) (list nil)
      (let ((powerset (powerset (rest set))))
	(dolist (subset powerset)
	  (push (cons (first set) subset)
		powerset))
	powerset)))
|#

;; Fast versions of the commonlisp union and intersection, that want 
;; and return sorted lists.

(defun fast-union (list1 list2 predicate &key (test #'eql) (key #'identity))
  "Like Union (but no support for test-not) should be faster because list1
   and list2 must be sorted. Fast-Union is a Merge that handles duplicates. 
   Predicate is the sort predicate."
  (declare (type list list1 list2))
  (let (result
	(wlist1 list1)
	(wlist2 list2))
    (while (and wlist1 wlist2)
      (cond
	((funcall test (funcall key (car wlist1)) (funcall key (car wlist2)))
	 (push (pop wlist1) result)
	 (pop wlist2))
	((funcall predicate
		  (funcall key (car wlist1))
		  (funcall key (car wlist2)))
	 (push (pop wlist1) result))
	(t
	 (push (pop wlist2) result))))
    (cond
      (wlist1
       (nconc (nreverse result) wlist1))
      (wlist2
       (nconc (nreverse result) wlist2))
      (t 
       (nreverse result)))))

(defun fast-intersection (list1 list2 predicate
				&key (test #'eql) (key #'identity))
  "Like Intersection (but no support for test-not) should be faster
   because list1 and list2 must be sorted. Fast-Intersection is a variation
   on Merge that handles duplicates. Predicate is the sort predicate."
  (declare (type list list1 list2))
  (let (result
	(wlist1 list1)
	(wlist2 list2))
    (while (and wlist1 wlist2)
      (cond
	((funcall test (funcall key (car wlist1)) (funcall key (car wlist2)))
	 (push (pop wlist1) result)
	 (pop wlist2))
	((funcall predicate 
		  (funcall key (car wlist1))
		  (funcall key (car wlist2)))
	 (pop wlist1))
	(t
	 (pop wlist2))))
    (nreverse result)))


;;; ********************************
;;; List Structure *****************
;;; ********************************
(defun Flatten (L)
  "Flattens list L, i.e., returns a single list containing the
   same atoms as L but with any internal lists 'dissolved'. For example,
   (flatten '(a (b c) d))  ==>  (a b c d)
   Recursively flattens components of L, according to the following rules:
    - an atom is already flattened.
    - a list whose CAR is also a list is flattened by appending the
      flattened CAR to the flattened CDR (this is what dissolves internal
      lists).
    - a list whose CAR is an atom is flattened by just flattening the CDR
      and CONSing the original CAR onto the result.
   These rules were chosen with some attention to minimizing CONSing."

  (cond ((null L) '())
	((atom L) L)
	(t ;; (consp L)
	 (if (consp (car L))
	     (append (Flatten (car L)) (Flatten (cdr L)))
	     (cons (car L) (Flatten (cdr L)))))
	#|(t L)|#))

#|
(defun flatten (list)
  (cond ((null list) nil)
	((atom list) (list list))
	(t (append (flatten (car list))
		   (flatten (cdr list))))))
|#

(defun circular-list (&rest list)
  "Creates a circular list of the arguments. Handy for use with 
   the list mapping functions. For example, 
     (mapcar #'+ '(1 2 3 4 5) (circular-list 3)) --> (4 5 6 7 8)
     (mapcar #'+ '(1 2 3 4 5) (circular-list 0 1)) --> (1 3 3 5 5)"
  (setf list (copy-list list))
  (setf (cdr (last list)) list)
  list)

(defun occurs (elt lst)
  "Returns T if ELT occurs somewhere in LST's tree structure."
  (cond ((null lst)
         nil)
        ((consp lst)
         ;; This walks down the tree structure of LST.
         (or (occurs elt (car lst))
             (occurs elt (cdr lst))))
        ((atom lst)
         ;; If we are at a leaf, test if ELT is the same as the leaf.
         (eq lst elt))))

(defun firstn (list &optional (n 1))
  "Returns a new list the same as List with only the first N elements."
  (cond ((> n (length list)) list)
	((< n 0) nil)
	(t (ldiff list (nthcdr n list)))))

(defun in-order-union (list1 list2)
  "Append and remove duplicates. Like union, but the objects are
   guarranteed to stay in order."
  (remove-duplicates (append list1 list2) :from-end t))

;;; yep, verrry similar to the one in zetalisp, 
;;; so on the symbolics we use that one instead.
#-symbolics
(declaim (ftype (function (function &optional package atom) (values))  mapatoms))
#-symbolics
(defun mapatoms (func &optional (package *package*) (inherited-symbols-too t))
  "Maps the passed function over all symbols in the package, and if 
   inherited-symbols-too is non-nil, then over those symbols as well.
   Note that the function may be called more than one time on a symbol."
  (do-symbols (sym package)
    (if (or inherited-symbols-too
	    (eq package (symbol-package sym)))
	(funcall func sym))))

(defun true-list-p (term)
  "Returns t if the term is a non-dotted list. Note that nil is a true list."
  (declare (optimize (speed 3) (safety 0)))
  (and (listp term)
       (not (cdr (last term)))))

;;; ********************************
;;; Sequences **********************
;;; ********************************
(defun seq-butlast (sequence &optional (n 1))
  (let* ((length (length sequence))
	 (delta (- length n)))
    (when (plusp delta)
      (subseq sequence 0 delta))))

(defun seq-last (sequence &optional (n 1))
  (let* ((length (length sequence))
	 (delta (- length n)))
    (when (plusp delta)
      (subseq sequence delta length))))

(defmacro dosequence ((var sequence &optional result) &BODY body)
  "Like DOLIST, except that iteration is over any sequence, not
   necessarily a list.
     Syntax is (DOSEQUENCE (var sequence [result]) . body).
     Do body with VAR bound to each member of SEQUENCE, then return result
     form or NIL."
  #+lispm(DECLARE (ZWEI:INDENTATION 1 1))
  (check-type var symbol)
  (let ((iter-index (gensym))
        #|(iter-limit (gensym))|#
	;; iter-sequence is necessary to avoid evaluating sequence
	;; multiple times.
	(iter-sequence (gensym)))
    `(let ((,var nil)
	   (,iter-sequence ,sequence))
       (dotimes (,iter-index (length ,iter-sequence) ,result)
	 (setq ,var (elt ,iter-sequence ,iter-index))
	 ,@body))))

(defun prefix? (prefix seq)
  "Prefix? - Checks to see if Prefix is really a prefix of Seq. Returns
   T if it is, NIL otherwise. Just checks that Prefix is no longer than
   Seq, then checks to see if the the initial subsequence of Seq that is
   the same length as Prefix is equal to Prefix. Prefix is a real prefix
   if and only if both conditions hold."
  (and (<= (length prefix) (length seq))
       (equalp (subseq seq 0 (length prefix)) prefix)))

(defun between (lo hi)
  "Generates a list of integers between Lo and Hi, inclusive. 
   Straightforward recursive definition, i.e., result is Lo consed onto
   integers from Lo+1 to Hi, unless Lo is greater than Hi in which case
   result is nil."
  (declare (integer Lo Hi))
  (when (<= Lo Hi)
    (cons lo (between (+ lo 1) hi))))

;;; ********************************
;;; Strings ************************
;;; ********************************
(defun ordinal-string (n)
  "Generates a string representing N as an ordinal number 
   (i.e., 1st, 2nd, etc.). Works by printing N and the appropriate
   suffix to a string - N is printed in decimal, the suffix is looked 
   up based on the last digit of N (i.e., N mod 10)."
  (declare (integer n))
  (let ((last-digit (mod (abs n) 10))
	(last-2-digits (mod (abs n) 100)))
    (declare (integer last-digit))
    (format nil "~d~a"
	    n
	    (cond ((or (= last-2-digits 11)
		       (= last-2-digits 12)
		       (= last-2-digits 13))         "th")
		  ((= last-digit 1)                  "st")
		  ((= last-digit 2)                  "nd")
		  ((= last-digit 3)                  "rd")
		  (t                                 "th")))))

(defun string-search-car (character-bag string)
  "Returns the part of the string before the first of the delimiters in 
   CHARACTER-BAG and the delimiter."
  (let* ((delimiter nil)
	 (delimiter-position (position-if #'(lambda (character)
					      (when (find character 
							  character-bag)
						(setq delimiter character)))
					  string)))
    (values (subseq string 0 delimiter-position)
	    delimiter)))

(defun string-search-cdr (character-bag string)
  "Returns the part of the string after the first of the delimiters in 
   CHARACTER-BAG, if any, and the delimiter. If none of the delimiters 
   are found, returns NIL and NIL."
  (let* ((delimiter nil)
	 (delimiter-position (position-if #'(lambda (character)
					      (when (find character 
							  character-bag)
						(setq delimiter character)))
					 string)))
    (if delimiter-position
	(values (subseq string (1+ delimiter-position))
		delimiter)
	;; Maybe this should be "" instead of NIL?
	(values nil delimiter))))

(defun parse-with-delimiter (line &optional (delim #\newline))
  "Breaks LINE into a list of strings, using DELIM as a 
   breaking point."
  ;; what about #\return instead of #\newline?
  (let ((pos (position delim line)))
    (cond (pos
           (cons (subseq line 0 pos)
                 (parse-with-delimiter (subseq line (1+ pos)) delim)))
          (t
           (list line)))))

(defun parse-with-delimiters (line &optional (delimiters '(#\newline)))
  "Breaks LINE into a list of strings, using DELIMITERS as a 
   breaking point."
  ;; what about #\return instead of #\newline?
  (let ((pos (position-if #'(lambda (character) (find character delimiters))
			    line)))
    (cond (pos
           (cons (subseq line 0 pos)
                 (parse-with-delimiters (subseq line (1+ pos)) delimiters)))
          (t
           (list line)))))

;;; subst:sublis::substitute:?  -- cl needs a parallel-substitute for
;;; performing many substitutions in a sequence in parallel.
(defun parallel-substitute (alist string)
  "Makes substitutions for characters in STRING according to the ALIST. 
   In effect, PARALLEL-SUBSTITUTE can perform several SUBSTITUTE
   operations simultaneously."
  (declare (simple-string string))
  ;; This function should be generalized to arbitrary sequences and
  ;; have an arglist (alist sequence &key from-end (test #'eql) test-not
  ;; (start 0) (count most-positive-fixnum) end key).
  (if alist
      (let* ((length (length string))
	     (result (make-string length)))
	(declare (simple-string result))
	(dotimes (i length)
	  (let ((old-char (schar string i)))
	    (setf (schar result i)
		  (or (second (assoc old-char alist :test #'char=))
		      old-char))))
	result)
      string))

(defun parse-with-string-delimiter (delim string &key (start 0) end)
  "Returns up to three values: the string up to the delimiter DELIM
   in STRING (or NIL if the field is empty), the position of the beginning
   of the rest of the string after the delimiter, and a value which, if
   non-NIL (:delim-not-found), specifies that the delimiter was not found."
  (declare (simple-string string))
  ;; Conceivably, if DELIM is a string consisting of a single character,
  ;; we could do this more efficiently using POSITION instead of SEARCH.
  ;; However, any good implementation of SEARCH should optimize for that
  ;; case, so nothing to worry about.
  (setq end (or end (length string)))
  (let ((delim-pos (search delim string :start2 start :end2 end))
	(dlength (length delim)))
    (cond ((null delim-pos)		
	   ;; No delimiter was found. Return the rest of the string,
	   ;; the end of the string, and :delim-not-found.
	   (values (subseq string start end) end :delim-not-found))
	  ((= delim-pos start)		
	   ;; The field was empty, so return nil and skip over the delimiter.
	   (values nil (+ start dlength)))
	  ;; The following clause is subsumed by the last cond clause,
	  ;; and hence should probably be eliminated.
	  (t				
	   ;; The delimiter is in the middle of the string. Return the
	   ;; field and skip over the delimiter. 
	   (values (subseq string start delim-pos)
		   (+ delim-pos dlength))))))

(defun parse-with-string-delimiter* (delim string &key (start 0) end
					   include-last)
  "Breaks STRING into a list of strings, each of which was separated
   from the previous by DELIM. If INCLUDE-LAST is nil (the default),
   will not include the last string if it wasn't followed by DELIM
   (i.e., \"foo,bar,\" vs \"foo,bar\"). Otherwise includes it even if
   not terminated by DELIM. Also returns the final position in the string."
  (declare (simple-string string))
  (setq end (or end (length string)))
  (let (result)
    (loop
     (if (< start end)
	 (multiple-value-bind (component new-start delim-not-found)
	     (parse-with-string-delimiter delim string :start start :end end)
	   (when delim-not-found 
	     (when include-last
	       (setq start new-start)
	       (push component result))
	     (return))
	   (setq start new-start)
	   (push component result))
	 (return)))
    (values (nreverse result) 
	    start)))

(defun split-string (string &key (item #\space) (test #'char=))
  ;; Splits the string into substrings at spaces.
  (let ((len (length string))
	(index 0) result)
    (dotimes (i len
		(progn (unless (= index len)
			 (push (subseq string index) result))
		       (reverse result)))
      (when (funcall test (char string i) item)
	(unless (= index i);; two spaces in a row
	  (push (subseq string index i) result))
	(setf index (1+ i))))))

(defun extract-strings (string &optional (delimiters '(#\newline #\space
						       #\return #\tab)))
  "Breaks STRING into a list of strings, using DELIMITERS as a 
   breaking point."
  (let* ((begin (position-if-not #'(lambda (character)
				     (find character delimiters))
				 string))
	 (end   (when begin
		  (position-if #'(lambda (character)
				   (find character delimiters))
			       string :start begin))))
    (cond ((and begin end)
           (cons (subseq string begin end)
                 (extract-strings (subseq string (1+ end)) delimiters)))
	  (begin
	   (list (subseq string begin)))
          (t
           nil))))

#|
(defun format-justified-string (prompt contents 
				       &optional (stream *standard-output*))
  (format stream (concatenate 'string
			      "~%" prompt "-~{~<~%" prompt " ~1,80:; ~A~>~^~}")
	  (split-string contents))
  (finish-output stream))
|#

(defun format-justified-string (prompt contents &optional (width 80)
				       (stream *standard-output*))
  (let ((prompt-length (+ 2 (length prompt))))
    (cond ((< (+ prompt-length (length contents)) width)
	   (format stream "~%~A- ~A" prompt contents))
	  (t
	   (format stream "~%~A-" prompt)
	   (do* ((cursor prompt-length)
		 (contents (split-string contents) (cdr contents))
		 (content (car contents) (car contents))
		 (content-length (1+ (length content)) (1+ (length content))))
	       ((null contents))
	     (cond ((< (+ cursor content-length) width)
		    (incf cursor content-length)
		    (format stream " ~A" content))
		   (t
		    (setf cursor (+ prompt-length content-length))
		    (format stream "~%~A  ~A" prompt content)))))))
  (finish-output stream))


(defun number-to-string (number &optional (base 10))
  (cond ((zerop number) "0")
	((eql number 1) "1")
	(t
	 (do* ((len (1+ (truncate (log number base)))) 
	       (res (make-string len))
	       (i (1- len) (1- i))
	       (q number)		; quotient
	       (r 0))			; residue
	     ((zerop q)			; nothing left
	      res)
	   (declare (simple-string res)
		    (fixnum len i r))
	   (multiple-value-setq (q r) (truncate q base))
	   (setf (schar res i) 
		 (schar "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" r))))))

(defun null-string (string &optional (start 0) end)
  "Returns T if STRING is the null string \"\" between START and END."
  (unless end (setf end (length string)))
  (string-equal string "" :start1 start :end1 end))

(defun force-string (thing)
  "Generates a string representation of Thing. This representation
   is the print name for symbols, otherwise whatever 'coerce' can do (which may
   be to generate an error sometimes)."
  (cond ((symbolp thing)  (symbol-name thing))
	(t  (coerce thing 'string))))

#-symbolics
(declaim (ftype (function (list &optional stream atom t)
                          (values string t character))
                read-delimited-string))
;;; This function was inspired by a similar function on the Symbolics lisp
;;; machine. In fact, if we are on a symbolics, use that one. It will return
;;; more values, but that's ok. Plus we get the input editor "for free :-)"
;;; collect the characters until we hit a delimiter or eof, then turn it
;;; into a string and return!
#-symbolics
(defun read-delimited-string (delimiters &optional (stream *standard-input*)
					 (eof-error-p t) eof-value)
  "Read a stream until one of the delimiters (a list of characters) is found.
   Returns the characters so read until the delimiter as a string, plus the
   additional values: EOF-VALUE, which is as passed if eof was reached, and
   the delimiter that caused termination of the string. If EOF-ERROR-P is 
   non-nil (the default), then an EOF causes an error to be signalled instead
   of returning EOF-VALUE."
  (declare (type list delimiters)
	   (type stream stream))
  (let (char-list)
    (declare (dynamic-extent char-list))
    (do ((peeked-char (peek-char nil stream eof-error-p :eof)
		      (peek-char nil stream eof-error-p :eof)))
	((or (member peeked-char delimiters) (eq peeked-char :eof))
	 (values (coerce (nreverse char-list) 'string)
		 (if (eq peeked-char :eof) eof-value) peeked-char))
      (push (read-char stream t)
	    ;; it should be good, else peek-char would have gotten the error.
	    ;; so go for it.
	    char-list))))

;;; ********************************
;;; Time ***************************
;;; ********************************
(defun elapsed-time-in-seconds (base now)
  "Returns the time in seconds that has elapsed between Base and Now.
   Just subtracts Base from Now to get elapsed time in internal time units,
   then divides by the number of internal units per second to get seconds."
  (coerce (/ (- now base)
	     internal-time-units-per-second)
	  'float))

(defun time-string (&optional universal-time)
  (unless universal-time (setf universal-time (get-universal-time)))
  (multiple-value-bind (secs min hour date month year dow)
      (decode-universal-time universal-time)
    (format nil "~@:(~A ~A-~A-~A ~2,'0d:~2,'0d:~2,'0d~)"
	    (svref '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") dow)
	    date 
	    (svref '#(0 "Jan" "Feb" "Mar" "Apr" "May"
			"Jun" "Jul" "Aug" "Sep" "Oct"
			"Nov" "Dec")
		   month)
	    (mod year 100)
	    hour min secs)))

;;; ********************************
;;; Math ***************************
;;; ********************************
(declaim (ftype (function (fixnum) #+sbcl (values integer &optional)
                                   #-sbcl integer)
                factorial))
(defun factorial (n)
  "Compute the factorial of an integer"
  (cond ((minusp n)
	 (cerror "Compute -(~D!) instead  I can't do -~D!" n n)
	 (factorial (- n)))
	(t
	 (do ((x n (1- x))
	      (result 1))
	     ((zerop x) result)
	   (declare (fixnum x))
	   (setq result (* x result))))))

;;; replace this eventually with declaim...
(declaim (ftype (function (number &optional number) number) round-to))
(defun round-to (number &optional (divisor 1))
  "like round, but returns the resulting number"
  (* (round number divisor) divisor))

(defun Bit-Length (N)
  "Computes the number of bits needed to represent integer N.
   Assumes that 0 requires 1 bit to represent, positive numbers require
   floor(log(N))+1 bits, and negative numbers require one bit more than
   their positive counterparts (for the sign bit). This treatment of
   negative integers is a little bit arbitrary, but seems as good as
   anything else."
  (cond ((= N 0)  1)
	((< N 0)  (+ (Bit-Length (- N)) 1))
	((> N 0)  (+ (floor (log N 2)) 1))))

(defun Sum-of-Powers-of-Two-Representation (N)
  "Figures out how to represent N as a sum of powers of two. Returns a 
   list of exponents, the idea being the N is the sum over E in this list
   of two raised to the E-th power. Requires N to be a positive integer,
   so that all exponents in the result list are integers."
  (declare (integer N))
  (assert (> N 0))
  (do ( (I 0 (+ I 1))
        (Exps '() (if (logbitp I N)
		      (cons I Exps)
		      Exps)) )
      ((>= I (integer-length N))  Exps)
    (declare (integer I) (list Exps))))

(defun Difference-of-Powers-of-Two-Representation (N)
  "Figures out how to represent N as the difference of a sequence of
   powers of 2 (e.g., 2^e1 - 2^e2 - ...). Returns a list of exponents,
   with e1 as the last and the others in some arbitrary order. Requires 
   N to be an integer greater than 0, which simplifies the code but
   isn't absolutely necessary. Starts by figuring out the smallest power
   of two greater than or equal to N - this exponent becomes e1. 
   Remaining exponents are just those of the greater power of two minus N."
  (declare (integer N))
  (assert (> N 0))
  (let* ((E1 (ceiling (log N 2)))
	 (Next-Power (expt 2 E1)))
    (declare (integer E1 Next-Power))
    (if (= Next-Power N)
	(list E1)
	(append (Sum-of-Powers-of-Two-Representation (- Next-Power N))
		(list E1)))))


;;; ********************************
;;; Keywords ***********************
;;; ********************************
(declaim (ftype (function (keyword list &optional t &key) t) grab-key))
(defun extract-keyword (key arglist &optional (default nil)
			    &key (no-value nil))
  "Searches the arglist for keyword key, and returns the following mark,
   or the default if supplied. If no-value is non-nil, then if nothing follows
   the key it is returned."
  (declare (type list arglist)
	   (type t default)
	   (type keyword key)
	   (optimize (speed 3) (safety 0)))
  (let ((binding (member key arglist)))
    (cond ((and (null binding) no-value)
	   no-value)
	  ((cdr binding)
	   (cadr binding))
	  (t
	   default))))

(declaim (ftype (function (list) list) truncate-keywords))
(defun truncate-keywords (input-list)
  "Many functions take multiple arguments, via &rest, that can cause
   problems when keyword arguments are also supplied. This function
   truncates a list at the first top-level keyword. Thus, '(A B C :FOO D)
   is returned as (A B C). Note that the new list is freshly consed to 
   avoid any stack problems with destroying a &rest argument."
  (declare (type list input-list)
	   (optimize (speed 3) (safety 0)))
  (ldiff input-list (member-if #'keywordp input-list)))

(defun remove-keywords (keys things)
  "Removes all keyword arguments from THINGS. 
   The idea is that Things represents an &rest argument list to a function
   that also wants to take keyword parameters, and so the keywords are
   embedded in the interesting arguments. This function filters out the
   keywords and the values associated with them, laving just the interesting
   stuff. Note that for purposes of this function a 'keyword argument'
   is defined to be any symbol appearing in Keys and the immediately
   following element of Things. Works by CDRing down Things, looking
   for members of Keys. As we go, we copy elements of Things to a result
   list, except when elements of Keys are found, in which case we skip
   two elements of Things (if there are that many left)."
  ;; contributed by baldwin@cs.rochester.edu
  (cond ((null Things) nil)
	((member (car things) keys)  
	 (when (> (length things) 2)
	     (remove-keywords keys (cddr things))))
	(t  
	 (cons (car things)
	       (remove-keywords keys (cdr things))))))

(defun retain-keywords (keys things)
  "Retains only some keyword arguments from THINGS."
  (cond ((null Things) nil)
	((member (car things) keys)
	 (cons (car things)
	       (cons (cadr things)
		     (retain-keywords keys (cddr things)))))
	(t  
	 (when (> (length things) 2)
	       (retain-keywords keys (cddr things))))))

;;; ********************************
;;; Association Lists **************
;;; ********************************
(deftype alist () 'list)

(defmacro update-alist (item value alist &key (test '#'eql) (key '#'identity))
  "If alist already has a value for Key, it is updated to be Value. 
   Otherwise the passed alist is updated with key-value added as a new pair."
  (let ((entry (gensym)))
    `(let ((,entry (assoc ,item ,alist :test ,test :key ,key)))
       (if ,entry
	   (progn (setf (cdr ,entry) ,value)
		  ,alist)
	   (setf ,alist (acons ,item ,value ,alist))))))

(defun reverse-alist (alist &key (test #'eql))
  "Takes an alist of uniqe keys and non-unique values, and 
   returns an alist of unique keys based on the values, whose 
   values are lists of the original keys."
  (let (result)
    (dolist (x alist)
      (let ((assoc-result (assoc (cdr x) result :test test)))
	(if assoc-result
	    (setf (cdr assoc-result) (cons (car x) (cdr assoc-result)))
	    (setq result (acons (cdr x) (list (car x)) result)))))
    result))

;;; ********************************
;;; Syntactic Sugar ****************
;;; ********************************
(defmacro msetq (vars value)
  #+lispm(declare (compiler:do-not-record-macroexpansions)
                  (zwei:indentation 1 1))
 `(multiple-value-setq ,vars ,value))

(defmacro mlet (vars value &body body)
  #+lispm(declare (compiler:do-not-record-macroexpansions)
                  (zwei:indentation 1 3 2 1))
  `(multiple-value-bind ,vars ,value ,@body))

;;; should really use parse-body to avoid using "declare" options in
;;; non-let clause.
(defmacro let-if (condition bindings &body body)
  "Binds let arguments only if condition is non-nil, and evaluates body
   in any case."
  `(if ,condition
       (let ,bindings
	 ,@body)
       (progn ,@(if (eq (caar body) 'declare) (cdr body) body))))

(defmacro when-bind ((symbol predicate) &body body)
  "Binds the symbol to predicate and executes body only if predicate
   is non-nil."
  `(let ((,symbol ,predicate))
     (when ,symbol
       ,@body)))

(defmacro while (test &body body)
  "Keeps invoking the body while the test is true;
   test is tested before each loop."
  (let ((end-test (gensym))
	(loop (gensym)))
    `(block nil
       (tagbody (go ,end-test) 
		,loop
		,@body
		,end-test
		(unless (null ,test) (go ,loop))
		(return)))))

(defmacro let*-non-null (bindings &body body)
  "Like let*, but if any binding is made to NIL, the let*-non-null 
   immediately returns NIL."
  #+lispm(declare lt:(arg-template ((repeat let)) declare . body))
  `(block lnn
	  (let* ,(mapcar
		  #'(lambda (entry)
		      ;; If it isn't a list, it's getting a nil binding,
		      ;; so generate a return. Otherwise, wrap with test.
		      (if (atom entry)
			  `(,entry (return-from lnn nil))
			  `(,(car entry) 
			    (or ,@(cdr entry)
				(return-from lnn nil))))) bindings)
	    ,@body)))

(defmacro cond-binding-predicate-to (symbol &rest clauses)
  "(COND-BINDING-PREDICATE-TO SYMBOL . CLAUSES)                      [macro]
   A COND-like macro.  The clauses are exactly as in COND.  In the body
   of a clause, the SYMBOL is lexically bound to the value returned by the
   test.  Example: 
     (cond-binding-predicate-to others
       ((member 'x '(a b c x y z))
        (reverse others)))
   evaluates to
      (x y z)."
  #+lispm  (declare (zwei:indentation 0 3 1 1))
  ;; Contributed by quiroz@cs.rochester.edu 
  ;; with slight modifications by miller@cs.rochester.edu
  (check-type symbol symbol)
  `(let (,symbol)
     (cond ,@(mapcar #'(lambda (clause)
                         `((setf ,symbol ,(first clause))
                           ,@(rest clause)))
		     clauses))))

(defmacro if* (condition true &rest false)
  `(if ,condition ,true (progn ,@false)))

;;; ********************************
;;; Dotted Lists *******************
;;; ********************************
(defmacro mapc-dotted-list (fn &rest lists)
  "Like normal Mapc, but handles dotted lists, and will apply the
   fn to the dotted argument, unless it is NIL"
  `(block mdl
	  (maplist #'(lambda (&rest x)
		       (declare (dynamic-extent x)) ;; 3/8/91 - bwm
		       (apply ,fn (mapcar #'car x))
		       ;; is cdr an atom
		       (when (some #'(lambda (y)
				       (and (atom (cdr y))
					    (cdr y))) ;not nil
				   x)
			 (apply ,fn (mapcar #'cdr x))
			 (return-from mdl (values))))
		   ,@lists)
	  (values)))

(defmacro mapcar-dotted-list (fn &rest lists)
  "Like normal Mapcar, but handles dotted lists, and will apply the
   fn to the dotted argument, unless it is NIL"
  (let ((return-val (gensym));; 3/8/91 - bwm
	(lastcdr (gensym)))
    `(let (,return-val ,lastcdr)
       (block mdl 
	      (maplist #'(lambda (&rest x)
			   (declare (dynamic-extent x));; 3/8/91 - bwm
			   (push (apply ,fn (mapcar #'car x)) ,return-val)
			   ;; is cdr an atom
			   (when (some #'(lambda (y)
					   (and (atom (cdr y)) 
						(cdr y))) ;not nil
				       x)
			     (setq ,lastcdr (apply ,fn (mapcar #'cdr x)))
			     (return-from mdl nil)))
		       ,@lists))
       (nconc (nreverse ,return-val) ,lastcdr))))

(defmacro mapcan-dotted-list (fn &rest lists)
  "Like normal Mapcan, but handles dotted lists, and will apply the
   fn to the dotted argument, unless it is NIL"
  (let ((return-val (gensym)));; 3/8/91 - bwm
    `(let (,return-val)
       (block mdl
	      (maplist #'(lambda (&rest x)
			   (declare (dynamic-extent x));; 3/8/91 - bwm
			   (setq ,return-val
				 (nconc ,return-val
					(apply ,fn (mapcar #'car x))))
			   ;; is cdr an atom
			   (when (some #'(lambda (y)
					   (and (atom (cdr y))
						(cdr y))) ;not nil
				       x)
			     (setq ,return-val 
				   (nconc ,return-val
					  (apply ,fn (mapcar #'cdr x))))
			     (return-from mdl (values))))
		       ,@lists))
       ,return-val)))

(defmacro some-dotted-list (fn &rest lists)
  "Like normal Some, but handles a single dotted list, and will apply
   the fn to the dotted argument, unless it is NIL"
  `(block sdl
	  (maplist #'(lambda (&rest x)
		       (declare (dynamic-extent x)) ;; 3/8/91 - bwm
		       (if (apply ,fn (mapcar #'car x))
			   (return-from sdl t)
			   ;; is cdr an atom
			   (when (some #'(lambda (y)
					   (and (atom (cdr y))
						(cdr y))) ;not nil
				       x)
			     (if (apply ,fn (mapcar #'cdr x))
				 (return-from sdl t)
				 (return-from sdl nil)))))
		   ,@lists)
	  ;; fell thru maplist w/o return
	  nil))

(defmacro every-dotted-list (fn &rest lists)
  "Like normal Every, but handles dotted lists, and will apply the
   fn to the dotted arguments, unless they are (all) NIL."
  `(block edl
	  (maplist #'(lambda (&rest x)
		       (declare (dynamic-extent x)) ;; 3/8/91 - bwm
		       (if (apply ,fn (mapcar #'car x))
			   ;; is cdr an atom
			   (when (some #'(lambda (y)
					   (and (atom (cdr y))
						(cdr y)))
				       x)
			     (if (apply ,fn (mapcar #'cdr x))
				 (return-from edl t)
				 (return-from edl nil)))
			   (return-from edl nil)))
		   ,@lists)
	  ;; fell thru maplist w/o return
	  t))


;;; ********************************
;;; Hash Tables ********************
;;; ********************************
(defun copy-hash-table (hash-table)
  #+symbolics (cli::copy-table hash-table)
  #-symbolics
  (progn
    (let ((new (make-hash-table
		:test (hash-table-test hash-table)
		:size (hash-table-size hash-table)
		:rehash-size (hash-table-rehash-size hash-table)
		:rehash-threshold (hash-table-rehash-threshold hash-table))))
      (maphash #'(lambda (key entry)
                   (setf (gethash key new) entry))
               hash-table)
      new)))

;;; ********************************
;;; CLOS ***************************
;;; ********************************
#+EXPLORER (eval-when (compile load eval) (proclaim '(declaration lt::arg-template)))

#+symbolics
(DEFMACRO DEFCLASS-X (TYPE SUPERTYPES SLOTS . STUFF)
  "Extended defclass, also creates a TYPE-P function and MAKE-TYPE function, like defstuct did."
  `(eval-when (compile load eval)
     (clos:DEFCLASS ,TYPE ,SUPERTYPES ,SLOTS ,@STUFF)
     (DEFUN ,(INTERN (CONCATENATE 'STRING (STRING TYPE) "-P")) (TERM)
       (TYPEP TERM ',TYPE))
     (DEFUN ,(INTERN (CONCATENATE 'STRING "MAKE-" (STRING TYPE))) (&REST ARGS)
       (APPLY 'CLOS:MAKE-INSTANCE ',TYPE ARGS))))

#-symbolics
(DEFMACRO DEFCLASS-X (TYPE SUPERTYPES SLOTS . STUFF)
  "Extended defclass, also creates a TYPE-P function and MAKE-TYPE function, like defstuct did."
  `(eval-when (compile load eval)
     (DEFCLASS ,TYPE ,SUPERTYPES ,SLOTS ,@STUFF)
     (DEFUN ,(INTERN (CONCATENATE 'STRING (STRING TYPE) "-P")) (TERM)
       (TYPEP TERM ',TYPE))
     (DEFUN ,(INTERN (CONCATENATE 'STRING "MAKE-" (STRING TYPE))) (&REST ARGS)
       (APPLY 'MAKE-INSTANCE ',TYPE ARGS))))

;;; this is to support a field in a clos structrure called "flags", which 
;;; is bit encoded. The testname can be used to see if the
;;; bit (defined by flagname - a constant) is set. It can also be setf to
;;; set or clear it. The type is the type of structure this
;;; test will handle, allowing multiple encodings of the flags field for
;;; different structures.
#+symbolics
(DEFMACRO DEFFLAG (TESTNAME (TYPE FLAGNAME))
  `(PROGN (CLOS:DEFMETHOD ,TESTNAME ((TERM ,TYPE))
	    (LOGTEST ,FLAGNAME (FLAGS TERM)))
	  (CLOS:DEFMETHOD (CLOS:SETF ,TESTNAME) (NEW-FLAG (TERM ,TYPE))
	    (CLOS:SETF (FLAGS TERM)
		       (IF NEW-FLAG
			   (LOGIOR (FLAGS TERM) ,FLAGNAME)
			   (LOGAND (FLAGS TERM) (LOGNOT ,FLAGNAME)))))))
#-symbolics
(DEFMACRO DEFFLAG (TESTNAME (TYPE FLAGNAME))
  `(PROGN (DEFMETHOD ,TESTNAME ((TERM ,TYPE))
	    (LOGTEST ,FLAGNAME (FLAGS TERM)))
	  (DEFMETHOD (SETF ,TESTNAME) (NEW-FLAG (TERM ,TYPE))
	    (SETF (FLAGS TERM)
		  (IF NEW-FLAG
		      (LOGIOR (FLAGS TERM) ,FLAGNAME)
		      (LOGAND (FLAGS TERM) (LOGNOT ,FLAGNAME)))))))



;;; ********************************
;;; misc ***************************
;;; ********************************
(defun eqmemb (item list &key (test #'equal))
  "Checks whether ITEM is either equal to or a member of LIST."
  (if (listp list)
      (member item list :test test)
      (funcall test item list)))

(defun neq (x y)
  "not eq"
  (not (eq x y)))

(defun car-eq (x y)
  "Checks whether Y is eq to the car of X."
  (and (listp x) ; consp?
       (eq (car x) y)))

(defun dremove (item list)
  "Destructive remove which replaces the original list with the list
   that results when ITEM is deleted from LIST."
  ;; This is safe only because of the way delete works.
  (displace list (delete item list :test #'eq)))

(defun displace (list val)
  "Replaces LIST with VAL by destructively modifying the car and cdr of LIST.
   Warning: VAL must not share list structure with LIST or you'll be sorry."
  (when list
    ;; Can't alter NIL.
    (rplaca list (car val))
    (rplacd list (cdr val))))

(defun tailpush (item list)
  "Pushes ITEM onto the tail of LIST. Does not work if the list is null."
  (when list
    (rplacd (last list) (list item))))

(defun explode (symbol)
  (map 'list #'identity (symbol-name symbol)))

(defun implode (list &optional (package *package*))
  (intern (map 'string #'identity list) package))

(defun crush (a b &optional (package *package*))
  (implode (append (explode a) (explode b)) package))

(defun listify-string (string)
  "Turns a string into a list of symbols."
  (let ((eof (gensym))
	(result nil)
	(start 0)
	item)
    (loop
     (multiple-value-setq (item start)
	 (read-from-string string nil eof :start start))
     (when (eq item eof)
       (return result))
     (setq result (nconc result (list item))))))

#|
(defun listify (sent)
  (if (listp sent) sent
      (read-from-string
        (concatenate 'string "(" sent ")"))))
|#

(defun and-list (list)
  (dolist (item list t)
    (unless item
      (return nil))))

(defun or-list (list)
  (dolist (item list nil)
    (unless item
      (return t))))

(defun lookup (symbol environment)
  (dolist (frame environment)
    (let ((binding (assoc symbol frame)))
      (when binding
	(return (cdr binding))))))


;;; we define a variable to be a symbol of the form ?NAME, i.e., a
;;; symbol whose first character is #\?.
(defun make-variable (x)
  (make-symbol (format nil "?~a" x)))

(defun variablep (item)
  "Returns T if ITEM is a variable, namely a symbol of the form ?NAME,
   whose first character is a question-mark."
  (and (symbolp item)
       (char= (char (symbol-name item) 0)
              #\?)))

(defmacro dofile ((var filename &optional return-form) &body body)
  "Opens the specified file for input, reads successive lines 
   from the file, setting the specified variable <var> to 
   each line. When end of file is reached, the value of <return-form>
   is returned."
  ;; After an idea by Eric Nyberg.
  (let ((eof (gensym "EOF"))
	(stream (gensym "STREAM")))
    `(with-open-file (,stream ,filename :direction :input)
       (do ((,var (read-line ,stream nil ,eof)
		  (read-line ,stream nil ,eof)))
	   ((eq ,var ,eof)
	    ,return-form)
	 ,@body))))


#-ANSI-CL
(unless (fboundp 'lisp::nth-value)
  ;; NTH-VALUE is a CLtL2 addition, so not every lisp has it yet.
  ;; This definition conses a lot, so we shouldn't use it in time-critical
  ;; situations. This definition is taken from CLtL2.
  (defmacro cl::nth-value (n form)
    "Returns the nth value of the values returned by form."
    `(nth ,n (multiple-value-list ,form)))
  (export 'lisp::nth-value "LISP"))

(defun make-plist (keys data &optional (plist '()))
  "Constructs a property list from keys and data (addition to plist)."
  (cond ((and (null data) (null keys))
	 plist)
	((or  (null data) (null keys))
	 (error "The lists of keys and data are of unequal length."))
	(t
	 (list* (car keys)
		(car data)
		(make-plist (cdr keys) (cdr data) plist)))))

(defvar *keyword-package* (find-package 'keyword))
(defun make-keyword (symbol)
  (intern (symbol-name symbol) *keyword-package*))

(defun quotify-list (list)
  "Quotes every element of the list."
  (if (not (consp list)) list
      (cons (list 'quote (car list))
	    (quotify-list (cdr list)))))


;;; ********************************
;;; Y-OR-N-P-WAIT ******************
;;; ********************************
;;; See file query.lisp.

;;; ********************************
;;; Noting Progress ****************
;;; ********************************
(defmacro noting-progress ((&optional (width 70)) &body body)
  "Every time NOTE-PROGRESS is called within the body of a NOTING-PROGRESS
   macro, it prints out a dot. Every width number of dots it also outputs
   a carriage return."
  (let ((dot-counter (gensym "DOT")))
    `(let ((,dot-counter 0))
       (declare (special ,dot-counter))
       (flet ((note-progress ()
		(incf ,dot-counter)
		(when (> ,dot-counter ,width)
		  (setf ,dot-counter 0)
		  (terpri))
		(princ #\.)))
	 ,@body))))

;;; ********************************
;;; Indentation ********************
;;; ********************************
;; additions for other versions of lisp are welcome!
#+allegro
(defmacro macro-indent-rule (symbol what)
  #-allegro-version>=
  nil
  #+allegro-version>=                   ; must be 4.1 or later
  `(add-initialization
    ,(format nil "lep init for ~A" symbol)
    '(lep::eval-in-emacs 
      ,(concatenate 'string 
	"(put '"
	(string-downcase (string symbol))
	" 'fi:lisp-indent-hook "
	(string-downcase (format nil "'~S)" what))))
    '(:lep)))

(defmacro parser-error (stream format-string &rest format-args)
  #+symbolics
  `(zl:::sys:read-error ,stream ,format-string ,@format-args)
  #+explorer
  (declare (ignore stream))
  #+explorer
  `(cerror :no-action nil 'sys:read-error-1 ,format-string ,@format-args)
  #-(or symbolics explorer)
  `(error (concatenate 'string
		       "Parse error: "
		       ,format-string
		       "~@[ at file position ~D~]") 
          ,@format-args
          (and (typep ,stream 'file-stream)
               (file-position ,stream))))

#+symbolics (macro-indent-rule parser-error (like format))

;;; ********************************
;;; Scheme Stream ******************
;;; ********************************

;;; Scheme-Stream (thanks to barmar@think.com)
;;; 
;;; Why use these instead of generators & series? Well, you can use both.
;;; Note that generators discard their output, while scheme-streams cache
;;; their output. What constitutes a tail-form isn't specified. So it's
;;; valid to put, e.g. a Water's generator there, or at least a fn that
;;; conses up a new scheme-stream whose head is the result of the call on
;;; the generator, and whose tail is another call on this fn.

(defstruct scheme-stream
  head
  tail
  (tail-closure-p t))

(defmacro cons-scheme-stream (head tail-form)
  `(make-scheme-stream :head ,head
		       :tail #'(lambda () ,tail-form)))

(defun ss-head (stream)
  "Return the head of scheme stream Stream. If Stream is not a stream, 
   returns NIL (to allow the usual car of nil)."
  (if (scheme-stream-p stream)
      (scheme-stream-head stream)))     ;return nil for non-streams

(defun ss-tail (stream)
  "Return the tail of the scheme stream Stream. Invokes lazy evaluation 
   if needed. If stream is not a scheme stream, return NIL (allows the
   usual cdr of nil)."
  (cond
    ((not (scheme-stream-p stream))
     nil)
    ((scheme-stream-tail-closure-p stream)
     (setf (scheme-stream-tail-closure-p stream) nil
	   (scheme-stream-tail stream) (funcall (scheme-stream-tail stream))))
    (t (scheme-stream-tail stream))))

;; scheme force/delay model

(defstruct scheme-delay
  (first-time-p t)
  value)

(defmacro scheme-delay (form)
  `(make-scheme-delay :value #'(lambda () ,form)))

(defun scheme-force (delay)
  (cond ((scheme-delay-first-time-p delay)
	 (setf (scheme-delay-first-time-p delay) nil
	       (scheme-delay-value delay)
	       (funcall (scheme-delay-value delay))))
	(t (scheme-delay-value delay))))

;;; ********************************
;;; ProgFoo ************************
;;; ********************************
;;; OK, how many times have you written code of the form
;;;
;;; (let ((retval (mumble)))
;;;    (setf (slot retval) bletch)
;;;    (setf (slot retval) barf)
;;;    retval)
;;;
;;; or things of the sort? More than you care to remember most likely. Enter
;;; the utterly useful PROGFOO. Think of it as a PROG1 with the value being
;;; bound to FOO. inside it's extent Lexically, of course.

(defmacro progfoo (special-term &body body)
  `(let ((foo ,special-term))
     ,@body
     foo))

#+symbolics (macro-indent-rule progfoo (like prog1))

(defmacro with-rhyme (body)
  "Well, there must be rhyme OR reason, and we now admit there is no reason,
   so... Used to flag silly constructs that may need to be rewritten for best
   effect."
  body)

#+symbolics (macro-indent-rule with-rhyme (like progn))

;;; and for common lisp fans of multiple values... FOO is the first value,
;;; you can access all the values as MV-FOO. returns the multiple values, 
;;; like multiple-values-prog1

(defmacro mv-progfoo (special-term &body body)
  `(let* ((mv-foo (multiple-value-list ,special-term))
          (foo (car mv-foo)))
     ,@body
     (values-list mv-foo)))

#+symbolics (macro-indent-rule mv-progfoo (like multiple-value-prog1))

;;; ********************************
;;; Miscellaneous ******************
;;; ********************************
;;; from the net
;;; From: Kerry Koitzsch <kerry@erg.sri.com>

(defun GET-COMPILED-FUNCTION-NAME (fn)
  "Returns the symbol name of a function. Covers the six major CL vendors."
  #+lispm
  (when (si:lexical-closure-p fn)
    (return-from get-compiled-function-name nil))
  (etypecase fn 
    (symbol fn)
    (compiled-function #+sbcl (sb-impl::%fun-name fn)
                       #+(:and :cmu (not :cmu20)) (kernel:%function-header-name fn)
                       #+(:and cmu cmu20) (kernel:%function-name fn)
                       #+:mcl(ccl::function-name fn)
                       #+lispm(si:compiled-function-name fn)
                       #+akcl(system::compiled-function-name fn)
                       #+lucid
                       (when (sys:procedurep fn)
                         (sys:procedure-ref fn SYS:PROCEDURE-SYMBOL))
                       #+excl (xref::object-to-function-name fn)
                       #+lispworks (system::function-name fn)
                       )))

;;; back to miller@cs.rochester.edu
;;; This may seem like a silly macro, but used inside of other macros or
;;; code generation facilities it is very useful - you can see 
;;; comments in the (one-time) macro expansion!

(defmacro comment (&rest anything)
  "Expands into nothing"
  (declare (ignore anything)))

;;; ********************************
;;; Boolean Ops ********************
;;; ********************************

;;; define boolean control extensions to and, or, not... the bit operators
;;; are there, but not the more general short-circuting ones. 
;;; Some of these will only make sense on two operands, and many can't short
;;; circuit (exclusive ops, for instance).

(defmacro xor (&rest predicates)
  "True only if exactly one predicate is true. Short circutes when it finds
   a second one is true. Returns the true predicate"
  (let ((result (gensym))
        (temp (gensym))
        (block-name (gensym)))
    `(block ,block-name
       (let ((,result ,(car predicates))
             ,temp)
         ,@(let (code-result)
             (dolist (pred (cdr predicates))
               (push `(cond
			((and (setq ,temp ,pred)
			      ,result)
			 (return-from ,block-name nil))
			(,temp
			 (setq ,result ,temp)))
                     code-result))
             (nreverse code-result))
         ,result))))
         
#+symbolics (macro-indent-rule xor (like and))

(defmacro eqv (&rest predicates)
  "True iff all predicates produce the same result according to eql, or
   passed :test (exclusive nor if binary)"
  (let ((result (gensym))
        (real-preds (truncate-keywords predicates))
        (test-key (extract-keyword :test predicates #'eql))
        (block-name (gensym)))
    `(block ,block-name
       (let ((,result ,(car real-preds)))
	 ,@(let (code-result)
	     (dolist (pred (cdr real-preds))
	       (push `(if (not (funcall ,test-key ,pred ,result))
			  (return-from ,block-name nil))
		     code-result))
	     (nreverse code-result))
	 (or ,result t)))))

#+symbolics (macro-indent-rule eqv (like and))

(defmacro nand (&rest predicates)
  "True only if all predicates are not true. Short circutes when it finds
   one is false."
  (let ((block-name (gensym)))
    `(block ,block-name
       ,@(let (code-result)
           (dolist (pred predicates)
             (push `(if (not ,pred)
                        (return-from ,block-name t))
                   code-result))
	   (nreverse code-result))
       nil)))
         
#+symbolics (macro-indent-rule nand (like and))

(defmacro nor (&rest predicates)
  "True only if all predicates are false. Short circutes when it finds a one
   is true."
  (let ((block-name (gensym)))
    `(block ,block-name
       ,@(let (code-result)
           (dolist (pred predicates)
             (push `(if ,pred
                        (return-from ,block-name nil))
                   code-result))
	   (nreverse code-result))
       t)))
         
#+symbolics (macro-indent-rule nor (like and))

;;; ********************************
;;; ********************************
;;; ********************************

(defun external-symbols (package)
  (let ((l))
    (do-external-symbols (s package l) 
      (push s l))))

;;; ********************************
;;; Login/Logout *******************
;;; ********************************
#|
;;; Useful for login/logout. Replace *logout-list* with si:logout-list
;;; on Symbolics.
(defun flatten-pairs (list)
  (cond ((null list) nil)
	(t (append (car list)
		   (flatten-pairs (cdr list))))))

(defmacro setq-bindings (&rest vars)
  `(setq ,@(flatten-pairs (mapcar #'(lambda (var)
				      (list var  `',(eval var)))
				  vars))))

(defvar *logout-list* nil)
(defmacro login-setq (&body body)
  (let ((unbinding-clause (cons 'setq-bindings
				(do ((clauses body (cddr clauses))
				     result)
				    ((null clauses) result)
				  (push (car clauses) result)))))
    `(progn
       (push ',(macroexpand unbinding-clause) *logout-list*)
       ,(cons 'setq body))
    ))
|#

;;; *EOF*
