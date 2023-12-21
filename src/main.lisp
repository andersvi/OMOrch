(in-package :omorch)

;;;;;;;;;;; main orchestrate method ;;;;;;;;;;;;;




;;;
;;; POSSIBLY: avoid loading (the same) database for each run using cli?  Ask Carmine-Emanuel for ffi-api
;;; 

;;; method orchestrate returns an instance of class 'orchestration


(defmethod! orchestrate ((target sound) (config orch-config))
  :initvals '(nil nil)
  :indoc '("target sound object" "an orch-config")
  :icon 451
  :doc "Generate orchestration from a target sound and an instance of orch-config  Returns an instance of orchestration.
"
  :numouts 1

  
  (cond ((not *orch-sol-db-file*) (error "db file not set, use orch-set-db-file-and-sound-path function"))
	((not *orch-path-to-orchestrate*) (error "orchestrate binary not set, use #'orch-set-path-to-orchestrate"))
	((not (and (= (om::sample-rate target) 44100) (= (om::sample-size target) 16))) (error "orchestrate: target file must be 44.1k, 16 bit, RIFF")))
	   
  (let* ((target-sound (om::filename target))
	 (output-basename (pathname-name target-sound))
	 (output-dir (namestring (make-pathname 
				  :directory (append (pathname-directory om::*om-tmpfiles-folder*)
						    
						     (if *orch-overwrite-previous-run*
							 (list "omorch")
							 (list (string+ "omorch-" (time-tag))))
						     (list output-basename)))))
	 (output-sound (format nil "~A~A-solution.wav" output-dir output-basename)) ;current orchidea supports only RIFF/wav, 44.1k
	 (output-orchestration (format nil "~A~A.orchestration.txt" output-dir output-basename))
	 (config-file-name (namestring (tmp-config-file config)))
	 (instruments (collect-string-items (instruments config) " ")))
      
    (om::create-directory output-dir)
      
    (progn
      (print (format nil "output-dir: ~A" output-dir))
      (print (format nil "config-file: ~A" config-file-name))
      (print (format nil "output-basename: ~A" output-basename))
      (print (format nil "instruments: ~A" instruments))
      (print (format nil "output-orchestration ~A" output-orchestration))
      (print (format nil "output-sound ~A" output-sound))

      (let ((cmd (format nil "cd ~S && ~S ~S ~S"
			 output-dir
			 (namestring *orch-path-to-orchestrate*)
			 (namestring (om::filename target))
			 config-file-name
			 )))



	(progn
	  (om::om-cmd-line cmd)

	  (rename-file (string+ output-dir "connection.wav") output-sound)
	  (rename-file (string+ output-dir "connection.txt") output-orchestration)
    
	  ;; 
	  ;; return an orchestration-object
	  ;; 


	  (let* ((orch-struct (om::om-read-file output-orchestration))
		 (orch-output (parse-orchidea-output orch-struct))
		 (output-sound (objfromobjs output-sound (make-instance 'sound)))
		 ;; (output-instrument-list (collect-string-items instrument-pool " "))
		 )
	    
	    
	    (make-instance 'orchestration
			   :target-sound target
			   :output-sound output-sound
			   :orch-output orch-output
 			   :command-line cmd
			   :config-file config-file-name
			   ;; needs list of instruments to pass to various score-editors
			   :instruments instruments

			   )))))))


;; recall 'original' orchestration-instance:

(defmethod objfromobjs ((self orch-output) (out orchestration))
  (make-instance 'orchestration :orch-output self))

(defmethod objfromobjs ((self orchestration) (out orch-output))
  (orch-output self))

;; read input from external files and place in orch-output instance

(defmethod objfromobjs ((orch-output-file pathname) (out orch-output))
  ;; "expects an ***.orchestration.txt file (output from 'orchestrate' binary)"
  (when orch-output-file
    (let ((orch-struct (om::om-read-file orch-output-file)))
      (parse-orchidea-output orch-struct))))

(defun read-orchestration-file (orch-file)
  "reads an orchidea output-file (xxx.orchestration.txt) stored on disk.  Returns an instance of 'orch-output"
  (let* ((file (or orch-file (om::om-choose-file-dialog :prompt "select an orchidea output-file (xxx.orchestration.txt)"))))
    (when file
      (parse-orchidea-output (om::om-read-file file)))))
