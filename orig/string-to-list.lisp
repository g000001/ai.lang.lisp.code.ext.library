From crabapple.srv.cs.cmu.edu!cantaloupe.srv.cs.cmu.edu!rochester!rutgers!psuvax1!news.ecn.bgu.edu!uxa.ecn.bgu.edu!news.ils.nwu.edu!aristotle.ils.nwu.edu!will Tue Dec 22 17:18:49 EST 1992
Article: 8856 of comp.lang.lisp
Path: crabapple.srv.cs.cmu.edu!cantaloupe.srv.cs.cmu.edu!rochester!rutgers!psuvax1!news.ecn.bgu.edu!uxa.ecn.bgu.edu!news.ils.nwu.edu!aristotle.ils.nwu.edu!will
From: will@aristotle.ils.nwu.edu (William Fitzgerald)
Newsgroups: comp.lang.lisp
Subject: Re: Use of format or pprint
Message-ID: <1992Dec21.224457.23526@ils.nwu.edu>
Date: 21 Dec 92 22:44:57 GMT
References: <1gtulrINN3gr@early-bird.think.com>
Sender: usenet@ils.nwu.edu (Mr. usenet)
Distribution: usa
Organization: The Institute for the Learning Sciences
Lines: 71
Nntp-Posting-Host: aristotle.ils.nwu.edu
X-Newsreader: Tin 1.1 PL5

Barry Margolin (barmar@think.com) wrote:
: 
: You'll have to write your own routine to convert the single-string form
: into a list of strings.  Common Lisp has nothing that will parse the string
: into words and treat them separately for this purpose (the only thing in CL
: that deals with words is STRING-CAPITALIZE).
: 

Here are some functions I've written to convert strings into lists of
various sorts. Examples follow at the end.

(defun list-from-string (string
                         &key 
                         (start 0) 
                         (char-bag '(#\Space))
                         (test #'(lambda (ch)
                                   (not (member ch char-bag 
                                                :test 'char=))))
                         (post-process 'identity))
  (let ((pos (position-if test string :start start)))
    (if pos 
      (list-from-string* string :start  pos :char-bag char-bag
                         :test test :post-process post-process)
      nil)))

(defun list-from-string* (string
                          &key 
                          (start 0) 
                          (char-bag '(#\Space))
                          (test #'(lambda (ch)
                                    (not (member ch char-bag :test 'char=))))
                          (post-process 'identity))
  (let* ((pos (position-if-not test string :start start))
         (new-pos (if pos (position-if test string :start pos) nil)))
    (cond
     ((and pos new-pos)
      (cons (funcall post-process (subseq string start pos))
            (list-from-string* string :start new-pos :char-bag char-bag
                               :test test :post-process post-process)))
     (pos (list (funcall post-process (subseq string start pos))))
     
     (t (list (funcall post-process (subseq string start)))))))


(defmethod string->symbol-list ((s string) &optional (package *package*))
  (list-from-string s :post-process
                    #'(lambda (str) 
                        (intern (nstring-upcase str) package))
                    :test 'alphanumericp))



#|


(list-from-string  "chris dan ski elaine nick") --> 
  ("chris" "dan" "ski" "elaine" "nick")

(list-from-string  "chris dan ski elaine nick" 
                   :post-process 'nstring-capitalize) -->
  ("Chris" "Dan" "Ski" "Elaine" "Nick")

(list-from-string "chris! dan! ski! elaine! nick!"
                  :char-bag '(#\Space #\!)) -->
  ("chris" "dan" "ski" "elaine" "nick")

(string->symbol-list "chris dan ski elaine nick")-->
 (CHRIS DAN SKI ELAINE NICK)


|#


