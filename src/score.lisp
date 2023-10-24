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


;; UTILS:
;; 
;; modify instrument-list, separate divisi instruments by voice-numbers,
;; return list of strings, e.g ("Gtr" "Gtr") -> ("Gtr-1" "Gtr-2")
;; 

(defun inst+voice-to-string (ins+voice)
  (format nil "~A-~A" (car ins+voice) (cdr ins+voice)))

(defun add-voice-nr-to-instruments (instruments)
  ;; add voice-no to separate more than 1 voices using the same instrument-class:
  (let ((instruments-+-voices '()))
    (loop for ins in instruments
	  do (let ((voice-no
		     (1+ (count-if #'(lambda (a) (string-equal (car a) ins))
				   instruments-+-voices))))
	       ;; uses cons here - ("GTR" . 1) - to support future changes of repr.
	       (push (cons ins voice-no) instruments-+-voices)))
    ;; instruments-+-voices
    (nreverse (mapcar #'(lambda (ins+voice)
			  (inst+voice-to-string ins+voice))
		      instruments-+-voices))))

