;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :ai.lang.lisp.code.ext.library
  (:use)
  (:export                              ;cl-utilities
   :point :copy-point :point-x :point-y :point-= :point-add :point-sub
   :point-incr :point-decr :point-incr-xy :point-decr-xy :region :copy-region
   :region-x :region-y :region-w :region-h :region-endx :region-endy :region-pos
   :region-size :region-ps :region-corner :region-center :region-containp
   :region-containp-xy :region-between :region-union :maxf :minf :ignoring-errors
   :remove-hook :add-hook :find-file :system-pathname :shell-command
   :*compile-lambda-expression* :convert-to-function :convert-to-string
   :convert-to-readable-string :convert-from-string :query-string :delete-all)
  (:export                              ;Extensions to Common Lisp
   :list-without-nulls :cartesian-product :cross-product :permutations :powerset
   :flatten :circular-list :occurs :firstn :in-order-union :seq-butlast :seq-last
   :dosequence :true-list-p :fast-union :fast-intersection :mapatoms
   :read-delimited-string :prefix? :between :ordinal-string :parse-with-delimiter
   :parse-with-delimiters :string-search-car :string-search-cdr
   :parallel-substitute :parse-with-string-delimiter
   :parse-with-string-delimiter* :split-string :format-justified-string
   :number-to-string :null-string :force-string :elapsed-time-in-seconds
   :time-string :factorial :round-to :bit-length
   :sum-of-powers-of-two-representation
   :difference-of-powers-of-two-representation :extract-keyword
   :truncate-keywords :remove-keywords :retain-keywords :update-alist
   :reverse-alist :msetq :mlet :let-if :when-bind :while :let*-non-null
   :cond-binding-predicate-to :if* :mapc-dotted-list :mapcar-dotted-list
   :mapcan-dotted-list :some-dotted-list :every-dotted-list :copy-hash-table
   :defclass-x :defflag :eqmemb :neq :car-eq :dremove :displace :tailpush
   :explode :implode :crush :listify-string :listify :and-list :or-list :lookup
   :make-variable :variablep :dofile :make-plist :*keyword-package* :make-keyword
   :noting-progress :note-progress :quotify-list :macro-indent-rule :parser-error
   :scheme-stream :cons-scheme-stream :ss-head :ss-tail :scheme-delay
   :scheme-force :progfoo :foo :with-rhyme :mv-progfoo :get-compiled-function-name
   :comment :xor :eqv :nand :nor :external-symbols)
  (:export                              ;interesting-ex
   :transpose :bleep :odometer :push2 :fetch :place :putlink :pluck
   :mapt :push-end)
  (:export                              ;
   :list-from-string :list-from-string* :string->symbol-list)
  (:export                              ;funcallable-p
   :funcallable-p :nth-from-end))


(defpackage :ai.lang.lisp.code.ext.library.internal
  (:use :ai.lang.lisp.code.ext.library :cl :fiveam))


;;; *EOF* 
