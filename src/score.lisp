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


;; (defun solution->chord (notes orchestration)
;;   (let ((channels
;;          (loop with used-channels = '() 
;;                for note in notes
;;                for channel = (loop for sym in orchestration
;;                                    for ch from 1
;;                                    when (and (equal (instrument note) sym)
;;                                              (not (find ch used-channels)))
;;                                    return ch)
;;                do (push channel used-channels)
;;                collect channel)))
    
;;     (progn
;;       (print (loop for note in notes
;; 		   collect
;; 		   (list (detune note)
;; 			 (pitch-name note)
;; 			 (orch-note-2-om-note (pitch-name note))
;; 			 (n->mc (orch-note-2-om-note (pitch-name note))))))
      

;;       ;; (mapcar #'(lambda (note)
;;       ;; 		(print (format nil
;;       ;; 			       "(pitch-name note): ~a (detune note) ~A ~%"
;;       ;; 			       (pitch-name note)
;;       ;; 			       (detune note))))
;;       ;;         notes)
    
;;       ;; (cerror "cont" "solution->chord (notes orchestration)")

;;       (make-instance 'chord 
;;                      :lmidic (mapcar #'(lambda (n)
;; 					 (+ (n->mc (orch-note-2-om-note (pitch-name n)))
;; 					    (detune n)))
;;                                      notes)
;;                      :lchan channels)

;;       ;; (make-instance 'chord 
;;       ;;                :lmidic (loop for note in notes
;;       ;; 				   collect (+ (n->mc (orch-note-2-om-note (orch-note-pitch-name note)))
;;       ;; 					      (detune note)))
;;       ;;                :lchan channels)
      
;;       )))





;; reference: (midi-number (pitch) , onset-time(ms), duration(ms), velocity, channel)

;; (defun orch-output->mf-info (orch-output)
;;   (correct-channels (sort (mapcan #'(lambda (segment) 
;; 				      (mapcar #'(lambda (note) 
;; 						  (note->mf-note note (onset segment) (orchestration orch-output)))
;; 					      (notes (solution segment))))
;; 				  (segments orch-output))
;; 			  #'< 
;; 			  :key #'second)))

;; (defun correct-channels (mf-info)
;;   "corrects simultaneous notes on the same channel by getting the next available channel. Provided the overlap is caused by adjacent instruments with the same name, and overlaps have been avoided already in previous steps (like, in the orchidea executable) then this should work fine."
;;   (loop with notes-playing = '()
;;         for note in mf-info
;;         for new-channel = (loop for chan from (fifth note)
;;                                 when (not (find chan (mapcar 'fifth notes-playing)))
;;                                 return chan)
;;         for corrected-note = `(,@(subseq note 0 4) ,new-channel)
;;         do (setf notes-playing
;;                  (append (remove-if #'(lambda (np) (<= (+ (second np) (third np)) (second note)))
;;                                     notes-playing)
;;                          (list corrected-note)))
;;         collect corrected-note))

;; (defun note->mf-note (note onset orchestration)
;;   (list (* (n->mc (orch-note-2-om-note (pitch-name note))) 0.01)
;;         onset
;;         (duration-ms note)
;;         (get-velocity-from-orch-note-dynamic (dynamic note))
;;         (1+ (position (instrument note) orchestration))))

(defun orch-output->multi-seq (orch-output)
  (mf-info->multi-seq (orch-output->mf-info orch-output)))

(defun mf-info->multi-seq (mf-info)
  (let ((grouped (sort (group-by mf-info 'fifth)
                       #'<
                       :key #'car)))
    (make-instance 'multi-seq
                   :chord-seqs (mapcar #'(lambda (entry) 
                                           (let ((zipped (mat-trans (cdr entry))))
                                             (make-instance 'chord-seq
                                                            :lmidic (om* (first zipped) 100)
                                                            :lonset (second zipped)
                                                            :ldur (third zipped)
                                                            :lvel (fourth zipped)
                                                            :lchan (fifth zipped))))

                                       grouped))))

(defun group-by (lis fn)
  (reduce #'(lambda (result elt)
              (let ((group-key (funcall fn elt)))
                (unless (find-if #'(lambda (entry) (equal (car entry) group-key))
                                 result)
                  (push `(,group-key . ()) result))
                (push elt (cdr (assoc group-key result)))
                result))
          lis
          :initial-value nil))

