(in-package om)

(defun parse-orchidea-output (orch-output-struct)
  (let ((orch-output (parse-[-delmited-string-to-list orch-output-struct)))
    (make-orch-output :orchestration (car orch-output)
		      :segments (mapcar #'parse-segment (cdr orch-output)))))

(defun parse-segment (segment)
  "segment: (segment 0 334 (solution 1 (note) (note) ...))"
  (make-orch-segment 
   :onset (nth 1 segment)
   :duration (nth 2 segment)
   :solution (parse-solution (nth 3 segment))))

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
   :midic (n->mc (orch-note-2-om-note (string (nth 4 lis))))
   :dynamic (get-velocity-from-orch-note-dynamic (string (nth 5 lis)))
   :vel (get-velocity-from-orch-note-dynamic (string (nth 5 lis)))
   :instance (nth 6 lis)
   :sample-path (string (nth 7 lis))
   :detune (nth 8 lis)))



