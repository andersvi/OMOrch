(in-package om)

(defstruct orch-output
  (orchestration)
  (segments)
  )

(defstruct orch-segment
  (onset-ms)
  (solutions))

(defstruct orch-solution
  (id)
  (notes))

(defstruct orch-note
  (duration-ms)
  (instrument)
  (style)
  (pitch-name)
  (dynamic)
  (instance)
  (sample-path)
  )

(defun parse-orchidea-output (lines) 
  (let ((parsed (convert-to-struct lines)))
    (print parsed)
    (mki 'chord-seq)))

(defun convert-to-struct (lines)
  ;;; add an extra enclosing set of brackets so we can parse the whole file as one list
  (let ((orchestra-line (car lines))
        (solution-lines (cdr lines)))
    (let ((segment-list (parse-square-bracketed (list-join `("[" ,@solution-lines "]") " ")))
          (orchestration (butlast (cddr (lw::split-sequence '(#\Space) orchestra-line)))))

      (make-orch-output
       :orchestration orchestration
       :segments (mapcar #'parse-segment segment-list)))))

;;; replace square brackets with parentheses, and convert the string to a nested list
(defun parse-square-bracketed (str) 
  (let ((new-string (replace-brackets str)))
    (read-from-string new-string)))

(defun replace-brackets (str) 
  (reduce #'(lambda (str subst-tuple) (substitute (car subst-tuple) (cdr subst-tuple) str))
          '(( #\( . #\[ ) 
            ( #\) . #\] ))
          :initial-value str))

(defun parse-segment (lis)
  (print (list 'parse-segment lis))
  (make-orch-segment 
   :onset-ms (second lis)
   :solutions (mapcar #'parse-solution (third lis)))
  )

(defun parse-solution (lis)
  (print (list 'parse-solution lis))
  (make-orch-solution
   :id (second lis)
   :notes (mapcar #'parse-note (third lis))))

(defun parse-note (lis)
  (make-orch-note
   :duration-ms (nth 1 lis)
   :instrument (nth 2 lis)
   :style (nth 3 lis)
   :pitch-name (nth 4 lis)
   :dynamic (nth 5 lis)
   :instance (nth 6 lis)
   :sample-path (nth 7 lis)))

    




