(in-package om)

(defun orch-output->chord-seq (orch-output)     
  (make-instance 'chord-seq 
                 :lmidic (mapcar #'(lambda (segment) 
                                     (solution->chord (orch-solution-notes 
                                                       (car (orch-segment-solutions segment)))
                                                      (orch-output-orchestration orch-output))) 
                                 (orch-output-segments orch-output))
                 :lonset (mapcar #'orch-segment-onset-ms (orch-output-segments orch-output))))

(defun solution->chord (notes orchestration)
  (let ((channels
         (loop with used-channels = '() 
               for note in notes
               for channel = (loop for sym in orchestration
                                   for ch from 1
                                   when (and (equal (orch-note-instrument note) sym)
                                             (not (find ch used-channels)))
                                   return ch)
               do (push channel used-channels)
               collect channel)))
      
    (make-instance 'chord 
                   :lmidic (mapcar #'(lambda (note) 
                                       (+ (n->mc (orch-note-pitch-name note)) (orch-note-detune note)))
                                   notes)
                   :lchan channels)))


;; reference: (midi-number (pitch) , onset-time(ms), duration(ms), velocity, channel)

(defun orch-output->mf-info (orch-output)
  (sort (mapcan #'(lambda (segment) 
                    (mapcar #'(lambda (note) 
                                (note->mf-note note (orch-segment-onset-ms segment) (orch-output-orchestration orch-output)))
                            (orch-solution-notes (car (orch-segment-solutions segment)))))
                (orch-output-segments orch-output))
        #'< 
        :key #'second))

(defun note->mf-note (note onset orchestration)
  (list (* (n->mc (orch-note-pitch-name note)) 0.01)
        onset
        (orch-note-duration-ms note)
        (get-velocity-from-orch-note-dynamic (orch-note-dynamic note))
        1))
        
(defun get-velocity-from-orch-note-dynamic (dynamic) 
  (or (get-vel-from-dyn (intern (string-upcase dynamic) :keyword))
      (progn ()
        (print (string+ "using fallback velocity 64, none found for: " dynamic))
        64)))




                             