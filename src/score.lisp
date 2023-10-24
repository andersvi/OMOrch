(in-package :om)


;;; 
;;; transfer output from orchidea to relevant OM-classes
;;; (chord-seq, multi-seq)
;;;

;;; 
;;; ORCHESTRATION -> CHORD-SEQ
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


;;; 
;;; ORCHESTRATION -> MULTI-SEQ
;;; 
;;; set up list of "note-allocators", one per instrument
;;;
;;; 	1) setup: one allocator per instrument, to grab and pop relevant note from stack
;;; 	2) setup: stack w. notes and onset from output
;;; 	3) for each allocator, loop through stack:
;;; 
;;; 	   a) check if this note can be played by this instrument at this onset:
;;; 
;;; 	      + if this note's 'onset' is >=  current allocators 'next-possible-onset:
;;; 
;;; 		A. collect this note, pop it off the input stack
;;; 		B. update next-possible-onset = onset + note-duration
;;; 
;;; 	      + if not, leave it on stack, for consideration by next allocator
;;; 
;;; 	   b) return allocator and modified stack
;;; 
;;;	4) finally; return list of chord-seqs, one for each allocator
;;; 




