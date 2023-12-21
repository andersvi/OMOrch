(in-package :omorch)

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
  "collect items from string separated by some string"
  (labels ((collect-string-items-rec (str out)
	     (let ((end (position separation-string str :test #'string=)))
	       (if end
		   (let ((item (subseq str 0 end)))
		     (collect-string-items-rec (subseq str (1+ end))
					       (cons item out)))
		   (nreverse (remove "" (cons str out) :test #'string=))))))
    (collect-string-items-rec string '())))



;;;
;;;
;;; utils to grab instruments from config-file or -string
;;; 
;;;

(defun read-instrument-list-from-config-file (conf-file &optional (line-tag "orchestra"))
  (with-open-file (c conf-file :direction :input)
     (find-instruments-in-stream c line-tag)))

(defun read-instrument-list-from-string (string &optional (line-tag "orchestra"))
  (with-input-from-string (c string)
    (find-instruments-in-stream c line-tag)))


;; uses lw:find-regexp-in-string :
(defun find-instruments-in-stream (c line-tag)
  (let ((orc-string (loop for line = (read-line c nil) then (read-line c nil)
			  while line
			  do (when
				 (lw:find-regexp-in-string (string+ "^" line-tag ".+") line)
			       (return line)))))
    (when orc-string
      (collect-instruments-from-string orc-string line-tag))))

(defun collect-instruments-from-string (orc-string tag)
  (multiple-value-bind (pos length)
      (lw:find-regexp-in-string (string+ "^" tag ".+") orc-string)
    (when pos
      (let ((resten (subseq orc-string (+ 1 pos (length tag)) (+ pos length))))
	(subseq resten (position-if-not  #'lw:whitespace-char-p resten))))))

(defun parse-instruments-from-config (config &key (orchestra-tag "orchestra"))
  "config can be string or file on disk"
  (let ((instruments (cond ((om::file-readable-p config) (read-instrument-list-from-config-file config orchestra-tag))
			   ((stringp config) (read-instrument-list-from-string  config orchestra-tag))
			   (t nil))))
    (or instruments
	(error (format nil "no orchestra found in ~S" config)))))

(defun substitute-in-splitted-string (split new old string)
  "substitute 'new' for 'old' in string splitted by 'split'"
  (let* ((old-str-list (lw::split-sequence split string))
	 (new-str-list (substitute new old old-str-list :test #'string-equal)))
    (reduce #'(lambda (s acc) (string+ s split acc))
	    new-str-list)))
