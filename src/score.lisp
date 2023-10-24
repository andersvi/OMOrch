(in-package :om)


;;;
;;; transfer output from orchidea to various OM-classes
;;; (chord, chord-seq, multi-seq, poly, voice...)
;;;

(defun orch-output->chord-seq (orch-output)     
  "return a chord-seq with one chord for each segment in output"
  (let ((onsets (mapcar #'onset (segments orch-output)))
	(chords (loop for seg in (segments orch-output)
		      collect (objfromobjs (notes (solution seg))
					   (make-instance 'chord)))))
    ;; :lmidic seems to work,  why not initargs :inside or :chords ?
    (make-instance 'chord-seq :lonset onsets :lmidic chords)))

(defmethod objfromobjs ((self orchestration) (out chord-seq))
  (let ((orch (orch-output self)))
    (orch-output->chord-seq orch)))




