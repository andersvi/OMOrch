(in-package om)

(defun orch-output->chord-seq (orch-output)     
  (mki 'chord-seq 
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
      
    (mki 'chord 
         :lmidic (mapcar #'(lambda (note) 
                             (+ (n->mc (orch-note-pitch-name note)) (orch-note-detune note)))
                         notes)
         :lchan channels)))


                             