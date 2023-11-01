(in-package om)

(defun time-tag ()
  (multiple-value-bind (sec min hour date month year day daylight-p zone)
      (get-decoded-time)
    (declare (ignore day daylight-p zone))
    (format nil "~4,'0D-~2,'0D-~2,'0D_~2,'0D:~2,'0D:~2,'0D" year month date hour min sec)))

;; 
;; orchidea output is [... [..] ..]
;;

(defun parse-[-delmited-string-to-list (string)
  "return a n-level list of []-delimited elts in string"
  (let ((list-string (substitute #\) #\] (substitute #\( #\[ string))))
    (with-input-from-string (str list-string)
      (loop for seg = (read str nil nil) then (read str nil nil)
	      while seg collect seg))))



;; from cl-cookbook:

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
            while pos)))


;; convenience while debugging graphic patches

(defun setq-symbol (symbol data)
  (eval `(setq ,symbol (quote ,data))))


;; collect arbitrary slot-value, along the lines of lvel, ldur etc.

(defgeneric l-slot (class slot))

(defmethod l-slot ((self chord) (slot symbol))
  (if (slot-exists-p (first (inside self)) slot)
      (mapcar #'(lambda (note)
		  (funcall slot note))
	      (inside self))
      (print (format nil "no-such-slot: ~A in class: ~A " slot self))))

(defmethod l-slot ((self chord-seq) (slot symbol))
  (mapcar #'(lambda (chord) (l-slot chord slot)) (inside self)))

(defmethod l-slot ((self list) (slot symbol))
  (mapcar #'(lambda (x) (l-slot x slot)) self))

(defun collect-string-items (string &optional (separation-string " "))
  "collect items from string separated by space"
  (labels ((collect-string-items-rec (str out)
	     (let ((end (position separation-string str :test #'string=)))
	       (if end
		   (let ((item (subseq str 0 end)))
		     (collect-string-items-rec (subseq str (1+ end))
					       (cons item out)))
		   (nreverse (remove "" (cons str out) :test #'string=))))))
    (collect-string-items-rec string '())))
    
;;; (collect-string-items "  3 n fire to tre")

