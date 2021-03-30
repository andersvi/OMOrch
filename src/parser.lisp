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
  (detune)
  )

(defun parse-orchidea-output (lines)
  ;;; add an extra enclosing set of brackets so we can parse the whole file as one list
  (let ((orchestra-line (car lines))
        (solution-lines (cdr lines)))
    (let ((segment-list (parse-square-bracketed (list-join `("[" ,@solution-lines "]") " ")))
          (orchestration (cdr (parse-square-bracketed orchestra-line))))

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
  (make-orch-segment 
   :onset-ms (second lis)
   :solutions (mapcar #'parse-solution (cddr lis)))
  )

(defun parse-solution (lis)
  (make-orch-solution
   :id (second lis)
   :notes (mapcar #'parse-note (cddr lis))))

(defun parse-note (lis)
  (make-orch-note
   :duration-ms (nth 1 lis)
   :instrument (nth 2 lis)
   :style (nth 3 lis)
   :pitch-name (nth 4 lis)
   :dynamic (nth 5 lis)
   :instance (nth 6 lis)
   :sample-path (nth 7 lis)
   :detune (nth 8 lis)))

    




