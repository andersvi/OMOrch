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



;;; 
;;; allocators: one per voice in ensemble
;;; 

(defclass voice-allocator () 
  ((instrument-name :initarg :name :accessor instrument-name :initform "")
   (name-w-voice :initarg :name-w-voice :accessor name-w-voice :initform "")
   (this-onset :initarg :this-onset :accessor this-onset :initform 0)
   (next-possible-onset :initarg :next-possible-onset :accessor next-possible-onset :initform 0)
   (input-onsets :initarg :input-onsets :accessor input-onsets :initform '())
   (lonsets :initarg :lonsets :accessor lonsets :initform '())
   (duration :initarg :duration :initarg dur :accessor duration :initform 0)
   (note-seq :initarg :note-seq :accessor note-seq :initform nil)))

(defun set-up-allocators-for-orchestration (orchestration)
  (loop for ins in (instruments orchestration)
	for ins+voice in (add-voice-nr-to-instruments (instruments orchestration)) ;careful! we need whole list
	for onset in (mapcar #'onset (segments (orch-output orchestration)))
	collect (make-instance 'voice-allocator
			       :name ins
			       :name-w-voice ins+voice)))


;; 
;; consider each orch-note for: a) instrument, b) available, ie not already playing
;; 

;; allow round-off-errors (ms) in input float onsets from parser

(defparameter *orch-onset-roundoff-threshold-ms* 2)

(defun should-i-play-this? (note-struct allocator)
  ;; predicate: from note - return boolean
  (let* ((note (car note-struct))
	 (onset (cdr note-struct))
	 (dur (slot-value note 'dur))
	 (instrument (slot-value note 'instrument))
	 (name-w-voice (slot-value allocator 'name-w-voice))
	 (allocator-instrument (instrument-name allocator))
	 (next-possible-onset (slot-value allocator 'next-possible-onset))
	 (prev-finished? (>= (+ onset *orch-onset-roundoff-threshold-ms*) next-possible-onset))
	 ;; instrument names are currently strings, possibly w. different case
	 (playable? (and (string-equal instrument allocator-instrument)	
			 prev-finished?)))
    playable?))

;; push all notes to a single stack

(defun orch-push-note-to-stack (orchestration)
  (let ((segs (segments (orch-output orchestration))))
    (loop for segment-notes in (mapcar #'(lambda (seg) (notes (solution seg))) segs)
	  for onset in (mapcar #'onset segs)
	  ;;output flat list of all '(note . onset):
	  append (loop for note in segment-notes
		       collect (cons note onset)))))

;; pop off note from stack when handled by an allocator

(defmacro orch-pop-note-from-stack (note stack)
  `(remove ,note ,stack :test #'(lambda (a b) (equalp a b)) :count 1))

;; update allocator with the selected note

(defun update-allocator (alloc note-w-onset)
  ;; add this note to this allocators seq
  (let* ((note (car note-w-onset))			    ; note-struct = '(note . onset)
	 (onset (cdr note-w-onset))
	 (dur (slot-value note 'dur)))
    (progn
      (setf (note-seq alloc) (append (note-seq alloc) (list note)))
      (setf (slot-value alloc 'next-possible-onset) (+ onset dur))
      (setf (slot-value alloc 'this-onset) onset)
      (setf (slot-value alloc 'duration) dur)
      (setf (slot-value alloc 'lonsets)  (append (slot-value alloc 'lonsets) (list onset))))))
