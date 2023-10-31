(in-package :om)

;; 
;; SAVE, EXPORT VARIOUS ORCH OUTPUT, TO BE ABLE TO LOAD BACK IN
;; 

;; TODO omng-save 'orchestrate-class

;; save div. orch-structure floating around in OMs editors ()
;; in orch-related - chord-seqs may be filled with chords of orch-notes, or orch-chords (with orch-notes):
;; 
;; omng-save for editors including orch-notes (chord, chord-seq, multi-seq, poly, voice)
;; test approaches:

;;   1) new class 'orch-chord', with special omng-save
;;   
;;   2) omng-save around-methods for chord-seq and chord:
;;	set up temp chord(seq), fill with input orch-notes
;;   
;;; 
;;; approach:
;;;
;;;	1) add to omng-save for chord-seq, chord, w. :around
;;;	2) cons up the inits for any orch-chords inside
;;;	3) add the orch-related to the original chord-seq, chord
;;;

(defun orch-find-if-orch-note (c)
  (find-if #'(lambda (c) (equal (class-of c) (find-class 'orch-note)))
	   (inside c)))

;; (orch-find-if-orch-type-chord (first (inside bbb)))
;; (orch-find-if-orch-type-chord (first (inside (mki 'chord-seq))))

(defmethod omng-save :around ((self chord-seq) &optional (values? nil))
  ;; make sure any orch-data - is saved and added back in load-form
  (if (find-if #'(lambda (c) (orch-find-if-orch-note c)) (inside self))
      (let ((chords (omng-save (inside self))))
	`(let ((new-cseq ,(call-next-method)))
	   (setf (lmidic new-cseq) ,chords)
	   new-cseq))
      (call-next-method)))

;; (remove-method #'omng-save (find-method #'omng-save  '(:around) (list (find-class 'chord))))

(defmethod omng-save :around ((self chord) &optional (values? nil))
  ;; make sure any orch-notes inside are saved, and added back in load-form
  (if (orch-find-if-orch-note self)
      (let ((notes (omng-save (inside self))))
	`(let ((new-chord ,(call-next-method)))
	   (setf (inside new-chord) ,notes)
	   new-chord))
      (call-next-method)))

#|
(progn
  (setq cs (mki 'chord-seq))
  (setf (inside cs) (list (mki 'orch-chord) (mki 'orch-chord :lmidic '(7000))))
  (inside cs))

(progn
  (setq cs (mki 'chord-seq))
  (setf (lmidic cs)
	(list (mki 'orch-note) (mki 'orch-note :midic 70000)))
  (inside cs))


(omng-save (inside bbb))
(lonset (eval (omng-save bbb)))
(omng-save (first (inside bbb)))
(pprint (omng-save cs))

(eval (omng-save cs))

(lmidic bbb)
(lmidic (eval (omng-save bbb)))

(lmidic bbb)
(l-slot bbb 'instrument)

(let ((a (eval (omng-save bbb)))
      (b bbb))
  (mapcar #'(lambda (slot) (list (l-slot a slot)
				 (l-slot b slot)))
	  '(instrument dynamic)))

(((("FL" "GTR" "GTR" "GTR" "GTR") ("GTR" "GTR" "GTR"))
  (("FL" "GTR" "GTR" "GTR" "GTR") ("GTR" "GTR" "GTR")))
 ((("FF" "FF" "MF" "MF" "MF") ("FF" "MF" "MF"))
  (("FF" "FF" "MF" "MF" "MF") ("FF" "MF" "MF"))))

|#

