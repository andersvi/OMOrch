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



(defmethod l-slot ((class chord) (slot symbol))
  (if (slot-exists-p (first (inside self)) slot)
      (mapcar #'(lambda (note)
		  (funcall slot note))
	      (inside self))
      (print (format nil "no-such-slot: ~A in class: ~A " slot class))))


(defmethod l-slot ((class chord-seq) (slot symbol))
  (if (slot-exists-p (first (inside self)) slot)
      (mapcar #'(lambda (crd)
		  (l-slot crd slot))
	      (inside self))))
