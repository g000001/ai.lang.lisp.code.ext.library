; This is a little collection of interesting examples, utilities, and
; one brain-bender. The file may be loaded into LISP (but only the last
; of the four definitions of transpose will remain).
; Tom Kramer
; kramer@cme.nist.gov
; 2/15/93

; The symbol "==>" used below means "returns"

;**************************************************************************
; Here are four ways to transpose a matrix represented as a list of lists.
; The second one is perverted and awkward but fun. The last is the best.
; These illustrate interation with "prog" and "do" vs. recursion and the
; use of mapcar.

; (setq example '((A B C D) (E F G H) (I J K L)))

; Then (transpose example) ==> ((A E I) (B F J) (C G K) (D H L)).
; Also, (transpose nil) ==> nil.

(in-package :ai.lang.lisp.code.ext.library.internal)

#|(defun transpose (matrix)
  (mapcar #'(lambda (x) (prog1 (mapcar #'first matrix)
			       (setq matrix (mapcar #'rest matrix))))
	  (first matrix)))|#


(defun transpose (matrix)
  (prog (result temp)
	(setq temp (first matrix))
	loop1
	(cond (temp
	       (push nil result)
	       (setq temp (rest temp))
	       (go loop1)))
	loop2
	(cond ((null temp)
	       (cond ((null (setq temp (pop matrix)))
		      (return result)))))
	(setq result (append (rest result)
			     (list (append (first result)
					   (list (pop temp))))))
	(go loop2)))


#|(defun transpose (matrix)
  (do ((result (mapcar #'(lambda (x) nil) (first matrix))
	       (mapcar #'(lambda (x) (append x (list (pop row)))) result))
       (row (pop matrix) (pop matrix)))
      ((null row) result)))|#


#|(defun transpose (matrix)
  (cond ((first matrix)
	 (cons (mapcar #'first matrix)
	       (transpose (mapcar #'rest matrix))))))|#

;**************************************************************************

; BRAIN-BENDER

; Given a positive integer argument, what does this function "bleep" do,
; how does it work, and how long will it take to compute (bleep 6) on your
; computer?

(defun bleep (arg)
  (cond ((eq arg 1) 1)
	((numberp arg)
	 (bleep (bleep (list arg (list (list (- arg 1)))))))
	((and (numberp (car arg)) (zerop (car arg))) nil)
	((and (numberp (car arg)) (numberp (cadr arg)))
	 (apply #'+ (mapcar #'(lambda (zz)
                                (declare (ignore zz))
                                (bleep (cdr arg))) arg)))
	((and (numberp (car arg)) (null (cdr arg))) 1)
	((numberp (car arg))
	 (cons (cadr arg) (bleep (list (1- (car arg)) (cadr arg)))))
	((listp (caar arg)) (bleep (mapcar #'bleep arg)))
	((not (eq (caar arg) 1))
	 (bleep (bleep (list (bleep (caar arg))
			     (list (list (- (caar arg) 1)))))))
	(t 1)))

; ANSWER

; This is a horribly inefficient way to calculate the factorial of an
; integer.

; When calculating n factorial:
; It makes a list (call it list_A) of n copies of another list (call it
; list_B).  List_B consists of (n-1) factorial elements, each of which
; is (n-2) factorial.

; To waste as much time as possible (and for no other reason), the
; function computes the factorial of the length of each copy of list_B 
; (the length being (n-1) factorial) by a second embedded method, (which
; essentially counts by ones).

; Then computes the factorial of the length (which is n) of list_A by the
; second method.

; For example, in computing 4!
; List_B is a (2 2 2 2 2 2),
; so List_A is ((2 2 2 2 2 2) (2 2 2 2 2 2) (2 2 2 2 2 2) (2 2 2 2 2 2))
; The length of List_B is 6, so the function counts to 720 (which is 6!)
; four times. Then it counts to 24 (which is 4!).

; In computing 6!, List_B has 120 elements, each of which is 24, and List_A
; is 6 copies of List_B. The function counts to 120! six times, which
; is the only significant time user. However long it will take your
; computer to do that is a good estimate of the time it takes to
; compute (bleep 6). For any existing computer, this is many times the
; age of the universe, of course.

; The function willingly accepts the following argument forms:
; 1. A number n, e.g. 3
;    Returns n factorial, but cannot compute it in a reasonable amount of
;    time for anything greater than n=4.  An attempt to compute (bleep 5)
;    was aborted after 12 hours.
; 2. A list of numbers, e.g. (3 45 2 1 -4 1.3) where the first is not zero.
;    Returns the factorial of the length of the list.
; 3. A list of the form (n arg) where n is a non-negative integer, and arg
;    is a not a number,  e.g. (3 ((2)))
;    Returns a list of n copies of liz, e.g. (((2)) ((2)) ((2)))
;    unless n is zero, in which case it returns nil.
; 4. A list of the form (((n)) ((n)) ((n)) ... ((n))), where n is an
;    integer e.g. (((2)) ((2)) ((2)))
;    Returns factorial of the length of the list, but computes the
;    factorial of the factorial of each integer in the process.
; 5. A list of the form ((n)), e.g. ((3))
;    Returns (n!)!, e.g. (bleep '((3))) = (3!)! = 6! = 720.

; The general approach to constructing this monstrosity was to figure
; out the logic (?) of it, determine how to represent the data so that
; the representation at each stage was differentiated from the
; representation at any other stage, and have the function test the
; representation to determine what to do next. For aesthetic reasons,
; the only number appearing in the function is 1.

; For amusement, or a difficult assignment for a student, try breaking
; bleep up into logical component functions, each of which handles only
; one or two data types for its argument or arguments.

;****************************************************************************

; This is just an interesting example of the power of recursion.

; This is an odometer with a variable number of wheels and a variable
; base. It prints numbers, starting with 000...0 and going up to xx...x
; where x is one less than the base and the number of digits in a reading
; is one more than the "subs" argument. Each number is printed on a
; new line. 

; Examples:
; (ODOMETER 2 5 '(0)) prints binary numbers from 0 to 63
; (ODOMETER 10 1 '(0)) prints decimal numbers from 0 to 99
; (ODOMETER 10 2 '(3 1 9)) prints decimal numbers from 31900 to 31999

; Just for fun, the entire odometer is in a single function.
; The function actually represents only a single wheel of an odometer.
; Since only one wheel is represented, one of the arguments passed (setting)
; which is a list of digits, has to tell it what numbers are on the
; superior wheels. The last element of setting is the current setting
; of the wheel represented.

; The base is the (base) argument. The number of subordinate wheels
; is (subs).

; The function returns t when done.

; The base should be not be greater than 10 or the printing gets weird.
; This could be fixed by using something other than princ to print the
; digits.

; The value of setting with which the function is called should be '(0)
; to start printing at 00...0 . If it is '(x), the printing starts
; at x00...0

; This function it not good for much itself, but programs requiring
; odometers might find the code useful.

(defun odometer (base subs setting)
  (cond ((eq (first (last setting)) base))
	((zerop subs)
	 (mapc #'princ setting)
	 (terpri)
	 (odometer base 0 (append (butlast setting)
				  (list (1+ (first (last setting)))))))
	(t
	 (odometer base (1- subs) (append setting (list 0)))
	 (odometer base subs (append (butlast setting)
				     (list (1+ (first (last setting)))))))))

; **************************************************************************

; push2 is a simple utility which is quite convenient in some situations.

; This builds a list by adding an item at the second element of the list.
; This enables the modification of the list when it is passed as an
; argument. When a list is built this way, it can be passed around without
; being global. The first element of the list is presumably a dummy, which
; will be discarded or ignored when the list is used.

(defun push2 (item liz)
  (rplacd liz (cons item (cdr liz))))

; The liz argument must be a non-null list.

; For example

; (setq liz '(a)) ==> (a) 
; (push2 'b liz) ==> (a b)
; liz ==> (a b)
; (push2 'c liz) ==> (a c b)
; liz ==> (a c b)

; **************************************************************************

; This is set of functions (fetch, place, putlink, pluck), for handling
; property lists which may be trees. A couple auxiliary functions are
; also defined.

;				 FETCH

; Fetch retrieves values from property lists which may be trees,
; "fetch" will go as far down the tree as you like. Fetch also works
; with the property lists of symbols.

; Example 1 - a disembodied property list
;	(setq jack '(jack toolbox (toolbox tool1 saw tool2 hammer) age 21))
; 	(fetch jack 'toolbox 'tool2) ==> hammer

; Example 2 - a property list of a symbol
;       (setf (symbol-plist 'jill)
;             '(toolbox (toolbox tool1 saw tool2 hammer) age 21))
;       (fetch 'jill 'toolbox 'tool2) ==> hammer

; Fetch takes any number of arguments. The arguments are all evaluated. 
; Each argument should be one level deeper in the tree than the previous one.
; The first argument should evaluate to a disembodied property list or
; a symbol.  All other arguments should evaluate to the names of properties.

; Fetch is perfectly happy with arguments naming properties that don't exist.
; It just returns nil.  For example,
;           (fetch jack 'car 'make) ==> nil
;           (fetch 'jill 'car 'make) ==> nil

; Fetch can deal with a combination of disembodied property lists and
; property lists of symbols.

; Example 3
; suppose we give "hammer" some properties
; (setf (symbol-plist 'hammer) '(color brown weight 2))
; now, although jack and the property list of jill have not changed,
; (fetch jack 'toolbox 'tool2 'weight) ==> 2
; (fetch 'jill 'toolbox 'tool2 'weight) ==> 2

(defun fetch (item &rest argl)
  (prog ()
	loop
	(cond ((null argl) (return item)))
	(setq item (cond ((listp item)
			  (list-get item (pop argl)))
			 (t
			  (get item (pop argl)))))
	(go loop)))

(defun list-get (liz indicator)
  (cond ((not (typep liz 'list))
	 (error "Non-list first argument to list-get."))
	((not (or (symbolp indicator) (integerp indicator)))
	 (error "Non-symbol, non-integer indicator in list-get"))
	(t
	 (list-get-aux (cdr liz) indicator))))

(defun list-get-aux (liz indicator)
  (prog ()
	loop
	(cond ((null liz) (return nil))
	      ((eq (car liz) indicator) (return (cadr liz))))
	(setq liz (cddr liz))
	(go loop)))

;                             PLACE

; Place is used to put values into disembodied property lists or to
; put values in the property lists of symbols.
; Place goes along a path specified by the arguments through as many levels
; of the list as you like.

; Example; If jack is as set above
;    (place jack 'toolbox 'tool2 'rasp)
; results in jack being changed:
;    jack ==> (jack toolbox (toolbox tool1 saw tool2 rasp) age 21)

; Place takes three or more arguments. The arguments are all evaluated.
; Each argument should be one level deeper in the property list than
; the previous one.  Each argument but the first and last should evaluate to
; a symbol or integer which is a property.  The first argument may evaluate
; to a symbol or a list.

; Any list that place has to deal with must have an odd number of
; members.  New properties go after the first element.  The function does
; NOT check for an odd number of members, and it will do things you
; probably don't want done to a list with an even number of members.

; Place checks that there are no null arguments (except possibly
; the last one), and that there are at least three arguments.

; If any check fails, the function causes a break in the program requiring
; operator intervention and prints an appropriate error message.

; Place adds to the property list in an appropriate way,
; either replacing a branch or an end node, or constructing and adding a
; new branch.  The auxiliary function "listup" is called to build a new
; branch or select a new end node.

; Note that place adds a COPY of its last argument to the property
; list of its first argument.  This ensures that if the last argument is
; changed later, the property list will not be changed.  This also means
; that "place" cannot be used to link existing lists together.  The function
; "putlink" has been written to do that.  It is identical to "place"
; except that the copying feature has been removed.  The documentation for
; putlink gives some examples of how the action of putlink differs from that
; of place.

(defun place (liz &rest argl)
  (prog (item prop value)
	(cond ((member nil (butlast argl))
	       (error "Null middle argument to place."))
	      (( < (length argl) 2)
	       (error "Too few arguments to place.")))
	(setq item argl)

	checkloop
	(cond ((not (or (symbolp (car item)) (integerp (car item))))
	       (error "Non-symbol, non-integer middle argument to place."))
	      ((cddr item)
	       (setq item (cdr item))
	       (go checkloop)))

	(cond ((symbolp liz)   ; make liz an even list
	       (setq value (symbol-plist liz))
	       (cond ((null value) ; if no plist, make one
		      (setf (symbol-plist liz)
			    (list (car argl) (listup (copy-tree argl))))
		      (return (cadr (symbol-plist liz))))
		     (t
		      (setq liz (cons 'dummy value)))))
	      ((and liz (listp liz)))
	      (t
	       (error "First argument to place not a list or symbol.")))

	outer-loop               ; new level of tree
	(setq value (cdr liz)
	      item  value
	      prop (pop argl))
	(cond ((null value)
	       (rplacd liz (list prop
				 (listup (cons prop (copy-tree argl)))))
	       (return (caddr liz))))

	inner-loop               ; check this level of the tree
	(cond ((null item)
	       (insert-first value prop
			     (listup (cons prop (copy-tree argl))))
	       (return (cadr value)))
	      ((eq prop (car item))
	       (setq value (cadr item))
	       (cond ((or (atom value) (null (cdr argl)))
		      (rplaca (cdr item)
			      (listup (cons prop (copy-tree argl))))
		      (return (cadr item)))
		     (t
		      (setq liz value)
		      (go outer-loop))))
	      (t
	       (setq item (cddr item))
	       (go inner-loop)))))


(defun listup (liz)
  (cond ((null (cdr liz)) nil)
	((equal (length liz) 2) (cadr liz))
	((equal (length liz) 3) liz)
	(t (list (car liz) (cadr liz) (listup (cdr liz))))))

(defun insert-first (liz prop value)
  (rplacd liz (nconc (list value (car liz)) (cdr liz)))
  (rplaca liz prop))

; Suppose the property list of vw is set up before each example to be:
; (setf (symbol-plist 'vw) '(name vw type (type subtype sedan color green)))

; Example 1. This is a simple replacement of a leaf node.
;      (place 'vw 'type 'subtype 'convertible) ==> convertible
;      Then (symbol-plist 'vw) ==>
;                      (name vw type (type subtype convertible color green))

; Example 2. In this example we build an entirely new branch.
;      (place 'vw 'windows 'rear 'shading 'yes) ==>
;                      (windows rear (rear shading yes))
;      Then (symbol-plist 'vw) ==>(windows (windows rear (rear shading yes))
;			  name vw type (type subtype sedan color green))

; Example 3. The last argument to place may be a list.
;      (place 'vw 'type 'subtype '(subtype a b)) ==> (subtype a b)
;      Then (symbol-plist 'vw) ==>
;                    (name vw type (type subtype (subtype a b) color green)

; Example 4. The last argument to place may be nil.
;      (place 'vw 'type nil) ==> nil
;      Then (symbol-plist 'vw) ==> (name vw type nil)

; Example 5. The following call to place is bad because of the "nil" argument
;     which is not the last argument.
;     (place 'vw 'type nil 'oops) ==> Error: Null argument to place. 

; Example 6. The following call to place is bad because of too few arguments.
;     (place 'vw 'oops) ==> Error: Too few arguments to "place". 

; Example 7. The following call to place is bad because of the list argument
;      which is not the last argument.      
;      (place 'vw 'name '(name first joe) 'second 'schmidt) ==>
; 		Error: Non-symbol, non-integer middle argument to "place".


;                           PUTLINK

(defun putlink (liz &rest argl)
  (prog (item prop value)
	(cond ((member nil (butlast argl))
	       (error "Null middle argument to putlink."))
	      (( < (length argl) 2)
	       (error "Too few arguments to putlink.")))
	(setq item argl)

	checkloop
	(cond ((not (or (symbolp (car item)) (integerp (car item))))
	       (error "Non-symbol, non-integer middle argument to putlink."))
	      ((cddr item)
	       (setq item (cdr item))
	       (go checkloop)))

	(cond ((symbolp liz)   ; make liz an even list
	       (setq value (symbol-plist liz))
	       (cond ((null value) ; if no plist, make one
		      (setf (symbol-plist liz)
			    (list (car argl) (listup argl)))
		      (return (cadr (symbol-plist liz))))
		     (t
		      (setq liz (cons 'dummy value)))))
	      ((and liz (listp liz)))
	      (t
	       (error "First argument to putlink not a list or symbol.")))

	outer-loop               ; new level of tree
	(setq value (cdr liz)
	      item  value
	      prop (pop argl))
	(cond ((null value)
	       (rplacd liz (list prop (listup (cons prop argl))))
	       (return (caddr liz))))

	inner-loop               ; check this level of the tree
	(cond ((null item)
	       (insert-first value prop (listup (cons prop argl)))
	       (return (cadr value)))
	      ((eq prop (car item))
	       (setq value (cadr item))
	       (cond ((or (atom value) (null (cdr argl)))
		      (rplaca (cdr item) (listup (cons prop argl)))
		      (return (cadr item)))
		     (t
		      (setq liz value)
		      (go outer-loop))))
	      (t
	       (setq item (cddr item))
	       (go inner-loop)))))

; Example 1.  What happens if value of the last argument is changed later.

; a. If place is used
;     (setq liz '(a b c)) ==> (a b c)
;     (setq newend '(e f g)) ==> (e f g)
;     (place liz 'b newend) ==> (e f g)
;     liz ==> (a b (e f g))
;     (rplaca newend 'x) ==> (x f g)
;     newend ==> (x f g)
;     liz ==> (a b (e f g))

; b. If putlink is used
;      (setq liz '(a b c)) ==> (a b c) 
;      (setq newend '(e f g)) ==> (e f g) 
;      (putlink  liz 'b newend) ==> (e f g) 
;      liz ==> (a b (e f g)) 
;      (rplaca newend 'x) ==> (x f g)
;      newend ==> (x f g)
;      liz ==> (a b (x f g))

; Example 2.  Place will not crosslink a property list.  Putlink will.
; In this case, when putlink is used to crosslink the list, a change in
; a value in one of the linked branches changes the corresponding value
; in another branch.

; a. (setq liz '(liz prop1 (prop1 a (a b c)) prop2 nil)) ==>
;				(liz prop1 (prop1 a (a b c)) prop2 nil)
;    (place liz 'prop2 'a (fetch liz 'prop1 'a)) ==> (prop2 a (a b c))
;    liz ==> (liz prop1 (prop1 a (a b c)) prop2 (prop2 a (a b c)))
;    (place liz 'prop1 'a 'b 3) ==> 3
;    liz ==> (liz prop1 (prop1 a (a b 3)) prop2 (prop2 a (a b c)))

; b. (setq liz '(liz prop1 (prop1 a (a b c)) prop2 nil)) ==> 
;                                (liz prop1 (prop1 a (a b c)) prop2 nil)
;    (putlink liz 'prop2 'a (fetch liz 'prop1 'a)) ==> (prop2 a (a b c))
;    liz ==> (liz prop1 (prop1 a (a b c)) prop2 (prop2 a (a b c)))
;    (place liz 'prop1 'a 'b '3) ==> 3
;    liz ==> (liz prop1 (prop1 a (a b 3)) prop2 (prop2 a (a b 3)))

; Example 3. Place will not make a list which is a member of itself.
; Putlink will. If your LISP does not stop printing at certain depth of
; nesting, part b will print until you stop it.

; a. (setq liz '(a b c)) ==> (a b c)
;    (place liz 'b liz) ==> (a b c)
;    liz ==> (a b (a b c))

; b. (setq liz '(a b c)) ==> (a b c)
;    (putlink liz 'b liz) ==> (a b (a b (a b (a b (a b #)))))
;    liz ==> (a b (a b (a b (a b (a b (a b (a b #)))))))
;    In this example, printing stops at depth 5.

;                         PLUCK

; Pluck removes a property and its value from
; a property list (either the plist of an atom or a disembodied property
; list).  Pluck, however, goes down along a path specified in the arguments
; as far as you like into the list.  In this regard it is like "fetch" and
; "place".  Pluck takes any number of arguments, all of which are evaluated.
; The first argument must evaluate to  either an atom with a property list
; or to a disembodied property list.  The last argument must evaluate to the
; property to be removed.  The middle arguments must evaluate to atoms which
; are property names.

; The value returned by pluck is t if something was plucked and nil if not.

(defun pluck (&rest argl &aux liz)
  (setq liz (apply 'fetch (butlast argl)))
  (cond ((oddp (length liz))              ; be sure the list handed to remf
	 (setq liz (cons (gensym) liz))   ; has an even number of entries
	 (remf liz (car (last argl))))
	((eq (car liz) (car (last argl))) ; need to do this since remf will
	 (rplaca liz (caddr liz))         ; not alter structure if property
	 (rplacd liz (cdddr liz))         ; is at head of list
	 t)
	(t
	 (remf liz (car (last argl))))))  ; if even list and prop in middle

; Example 1. (setq liz '(a b (b f g h k) d e)) ==> (a b (b f g h k) d e)
;            liz ==> (a b (b f g h k) d e)
;            (pluck liz 'b 'h) ==> t
;            liz ==> (a b (b f g) d e)
;            (pluck liz 'b) ==> t
;            liz ==> (a d e)

;****************************************************************************
;                             MAPT

; If val is nil or missing, mapt returns a list of all those elements of
; the list "liz" for which (func element) is non-nil.

; If val is non-nil, mapt returns a list of all values of (func element)
; which are non-nil, in the same order as the elements.

; I don't know how the rest of the world gets along without this function.

(defun mapt (func liz &optional val)
  (cond (val
	 (mapcan #'(lambda (x)
		     (cond ((setq val (funcall func x)) (list val)))) liz))
	(t
	 (mapcan #'(lambda (x)
		     (cond ((funcall func x) (list x)))) liz))))


; Example 1 - (mapt #'numberp '(a 1 4 b c 2.3)) ==> (1 4 2.3)

; Example 2 - (mapt #'(lambda (item) (cond ((numberp item) (1+ item))))
;                    '(a 1 4 b c 2.3) t) ==> (2 5 3.3)

;****************************************************************************
;                             PUSHEND

; This function is like push, except it adds a new-item at the end of
; a list, not the beginning, and the second argument (list-end) is a
; pointer to the last item on the list, not the first.  list-end is
; reset so that it points at the new last item.  To make use of this
; function, some other variable is normally set to point at the front
; of the list.

; Using this function to add items at the end of a long list is much
; faster than nconc'ing the list with a list of the new item, because
; nconc'ing requires traversing the entire list.

(defmacro push-end (new-item list-end)
  `(nconc ,list-end (setq ,list-end (list ,new-item))))

; Example:
; (setq liz '(1 2 3)) => (1 2 3)
; (setq lass (last liz)) => (3)
; (push-end 4 lass) => (3 4)     -- the returned value is not usually used
; liz => (1 2 3 4)
; (push-end 5 lass) => (4 5)
; liz => (1 2 3 4 5)
