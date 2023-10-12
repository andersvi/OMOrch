(in-package :om)


;;;
;;; transfer output from orchidea to various OM-classes
;;; (chord, chord-seq, multi-seq, poly, voice...)
;;;


(defun orch-note-name-list-until-unvalid_ (lst)
  "return part of orch-note before potentially invalid part - '+ or '_"
  (subseq lst 0
	  (position-if #'(lambda (c) (member c '(#\_ #\+)))
		       lst)))

(defun orch-note-2-om-note  (notestring)
  "parse orchidea-type note-string, return valid OM note string."
  (let* ((orch-note-string-list (coerce (string notestring) 'list))
	 (valid-part-of-note-name (orch-note-name-list-until-unvalid_ orch-note-string-list))
	 (om-note-string-list (substitute-if #\+
					     #'(lambda (c) (or (member c '(#\q #\Q) )))
					     valid-part-of-note-name)))
    
    (if (equalp (car om-note-string-list) #\N)
	"C3" ;; just return default for OMs note-class
	(coerce om-note-string-list 'string))))

#|

("D+6_A+5_D+4")


(orch-note-2-om-note  'N)
(n->mc (orch-note-2-om-note  'N))
(orch-note-2-om-note  'a\#4+c4)
(orch-note-2-om-note "Dq6_Aq5_Dq4")
(n->mc (orch-note-to-om-note (orch-note-before-_ "Dq6_Aq5_Dq4")))
(n->mc "D+6")
(n->mc (orch-note-2-om-note 'Dq6_Aq5_Dq4 ))
(orch-note-2-om-note "Dq6" )
(n->mc (orch-note-2-om-note "a#q4" ))
(n->mc (orch-note-2-om-note "Dq6" ))
(n->mc (orch-note-2-om-note 'Dq4))
(n->mc (orch-note-2-om-note 'DQ4))
(n->mc (orch-note-2-om-note "DQ4"))
(n->mc (orch-note-2-om-note "Dq4"))

|#

(defun solution->chord (notes orchestration)
  (let ((channels
         (loop with used-channels = '() 
               for note in notes
               for channel = (loop for sym in orchestration
                                   for ch from 1
                                   when (and (equal (instrument note) sym)
                                             (not (find ch used-channels)))
                                   return ch)
               do (push channel used-channels)
               collect channel)))
    
    (progn
      (print (loop for note in notes
		   collect
		   (list (detune note)
			 (pitch-name note)
			 (orch-note-2-om-note (pitch-name note))
			 (n->mc (orch-note-2-om-note (pitch-name note))))))
      

      ;; (mapcar #'(lambda (note)
      ;; 		(print (format nil
      ;; 			       "(pitch-name note): ~a (detune note) ~A ~%"
      ;; 			       (pitch-name note)
      ;; 			       (detune note))))
      ;;         notes)
    
      ;; (cerror "cont" "solution->chord (notes orchestration)")

      (make-instance 'chord 
                     :lmidic (mapcar #'(lambda (n)
					 (+ (n->mc (orch-note-2-om-note (pitch-name n)))
					    (detune n)))
                                     notes)
                     :lchan channels)

      ;; (make-instance 'chord 
      ;;                :lmidic (loop for note in notes
      ;; 				   collect (+ (n->mc (orch-note-2-om-note (orch-note-pitch-name note)))
      ;; 					      (detune note)))
      ;;                :lchan channels)
      
      )))

(defun orch-output->chord-seq (orch-output)     
  "return a chord-seq with one chord for each segment in output"
  (let ((onsets (mapcar #'onset (segments orch-output)))
	(chords (loop for seg in (segments orch-output)
		      collect (objfromobjs (notes (solution seg))
					   (mki 'chord)))))
    (mki 'chord-seq :lonset onsets :lmidic chords)))



;; reference: (midi-number (pitch) , onset-time(ms), duration(ms), velocity, channel)

(defun orch-output->mf-info (orch-output)
  (correct-channels (sort (mapcan #'(lambda (segment) 
				      (mapcar #'(lambda (note) 
						  (note->mf-note note (onset segment) (orchestration orch-output)))
					      (notes (solution segment))))
				  (segments orch-output))
			  #'< 
			  :key #'second)))

(defun correct-channels (mf-info)
  "corrects simultaneous notes on the same channel by getting the next available channel. Provided the overlap is caused by adjacent instruments with the same name, and overlaps have been avoided already in previous steps (like, in the orchidea executable) then this should work fine."
  (loop with notes-playing = '()
        for note in mf-info
        for new-channel = (loop for chan from (fifth note)
                                when (not (find chan (mapcar 'fifth notes-playing)))
                                return chan)
        for corrected-note = `(,@(subseq note 0 4) ,new-channel)
        do (setf notes-playing
                 (append (remove-if #'(lambda (np) (<= (+ (second np) (third np)) (second note)))
                                    notes-playing)
                         (list corrected-note)))
        collect corrected-note))

(defun note->mf-note (note onset orchestration)
  (list (* (n->mc (orch-note-2-om-note (pitch-name note))) 0.01)
        onset
        (duration-ms note)
        (get-velocity-from-orch-note-dynamic (dynamic note))
        (1+ (position (instrument note) orchestration))))






;; orch-added-dynamics: '("ffpp" "fp" "N" "ppff" "ppmfpp")

(defun get-velocity-from-orch-note-dynamic (orch-dyn)
  (let ((dyn-list (coerce orch-dyn 'list))
	(dyn-keyword (intern (string-upcase orch-dyn) :keyword)))
    (cond ((equal dyn-keyword :n)
	   (progn (print (string+ "using velocity 0 for: " orch-dyn))
		  0))
	  ((member dyn-keyword *dynamics-symbols-list* :test #'(lambda (a b) (equal a (car b))))
	   (get-vel-from-dyn dyn-keyword))
	  ((get-vel-from-dyn
	    (intern (string-upcase
		     (coerce (loop with init = (first dyn-list)
				   repeat 3 ;;max 3 ppp or fff
				   for v in dyn-list
				   while (equal v init)
				   collect v)
			     'string))
		    :keyword)))
	  (t
	   (progn (print (string+ "setting fallback velocity = 0, none found for: " orch-dyn))
		  0)))))


;; (defun get-velocity-from-orch-note-dynamic (dynamic) 
;;   (let ((checked-dyn (force-om-dyns dynamic))
;; 	(dyn (intern dynamic :keyword)))
;;     (cond ((equal dyn :n)
;; 	   (progn (print (string+ "using velocity 0 for: " dynamic))
;; 		  0))
;; 	  ((get-vel-from-dyn dyn))
;; 	  (t
;; 	   (progn (print (string+ "setting fallback velocity = 0, none found for: " dynamic))
;; 		  0)))))


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

