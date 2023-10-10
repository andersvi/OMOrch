(in-package om)

(defun parse-orchidea-output (orch-output-score-file)
  (let ((orch-output (parse-[-delmited-string-to-list
		      (om-read-file orch-output-score-file))))
    (make-orch-output :orchestration (car orch-output)
		      :segments (mapcar #'parse-segment (cdr orch-output)))))

(defun parse-segment (segment)
  "segment: (segment 0 334 (solution 1 (note) (note) ...))"
  (make-orch-segment 
   :onset-ms (nth 1 segment)
   :duration (nth 2 segment)
   :solution (mapcar #'parse-solution (nthcdr 3 segment))))

(defun parse-solution (lis)
  (make-orch-solution
   :id (second lis)
   :notes (mapcar #'parse-note (cddr lis))))

(defun parse-note (lis)
  (make-orch-note
   :duration-ms (round (nth 1 lis))
   :instrument (nth 2 lis)
   :style (nth 3 lis)
   :pitch-name (string (nth 4 lis))
   :dynamic (string (nth 5 lis))
   :instance (nth 6 lis)
   :sample-path (string (nth 7 lis))
   :detune (nth 8 lis)))

