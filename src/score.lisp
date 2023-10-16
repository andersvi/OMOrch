(in-package :om)


;;;
;;; transfer output from orchidea to various OM-classes
;;; (chord, chord-seq, multi-seq, poly, voice...)
;;;

(defun orch-output->chord-seq (orch-output)     
  "return a chord-seq with one chord of orch-notes per segment"
  (let ((onsets (mapcar #'onset (segments orch-output)))
	(chords (loop for seg in (segments orch-output)
		      collect (objfromobjs (notes (solution seg))
					   (mki 'chord)))))
    (mki 'chord-seq :lonset onsets :lmidic chords)))





