(in-package :om)

;;;;;;;;;;; main orchestrate method ;;;;;;;;;;;;;




;;;
;;; POSSIBLY: avoid loading (the same) database for each run using cli?  Ask Carmine-Emanuel for ffi-api
;;; 

;;; method orchestrate returns an instance of class 'orchestration


(defmethod! orchestrate ((target sound)  (config orch-config))
  :initvals '(nil *orchidea-default-ensemble* 0.7 *orchidea-config-template-path* nil)
  :indoc '("target sound object"
	   "config is an instance of the orch-config class")
  :icon 451
  :doc "Generate orchestration from a target sound and an instance of orch-config  Returns an instance of orchestration.
"
  :numouts 1

  
  (cond ((not *orchidea-db-file*) (error "db file not set, use orchidea-set-db-file-and-sound-path function"))
	((not *orchidea-executable-path*) (error "orchidea binary not set, use orchidea-set-executable-path function"))
	((not (and (= (sample-rate target) 44100) (= (sample-size target) 16))) (error "orchestrate: target file must be 44.1k, 16 bit, RIFF")))
	   
  (let (

	;; set up various input and output filenames
	(output-dir (namestring (make-pathname 
				 :directory (append (pathname-directory *om-tmpfiles-folder*)
						    (if *orch-overwrite-previous-run*
							(list "omorch")
							(list (string+ "omorch-" (time-tag)))))))))
    

    (create-directory output-dir)

    (let* ((target-sound (filename target))
	   (output-basename (pathname-name target-sound))
	   (output-sound (format nil "~A~A-solution.wav" output-dir output-basename)) ;current orchidea supports only RIFF/wav, 44.1k
	   (output-orchestration (format nil "~A~A.orchestration.txt" output-dir output-basename))
	   )
      
      
      (let ((config-file-name (namestring (tmp-config-file config)))
	    (instruments (collect-string-items (instruments config) " ")))
	

	(progn
	  (print (format nil "output-dir: ~A" output-dir))
	  (print (format nil "config-file: ~A" config-file-name))
	  (print (format nil "output-basename: ~A" output-basename))
	  (print (format nil "instruments: ~A" instruments))
	  (print (format nil "output-orchestration ~A" output-orchestration))
	  (print (format nil "output-sound ~A" output-sound))

	  ;; TODO: allow control of all options in config from here
      
    
	  (let ((cmd (format nil "cd ~S && ~S ~S ~S"
			     output-dir
			     (namestring *orchidea-executable-path*)
			     (namestring (filename target))
			     config-file-name
			     )))



	    (progn
	      (om-cmd-line cmd)

	      (rename-file (string+ output-dir "connection.wav") output-sound)
	      (rename-file (string+ output-dir "connection.txt") output-orchestration)
    
	      ;; 
	      ;; return an orchestration-object
	      ;; 


	      (let* ((orch-struct (om-read-file output-orchestration))
		     (orch-output (parse-orchidea-output orch-struct))
		     (output-sound (objfromobjs output-sound (mki 'sound)))
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

			       )))))))))


;; recall 'original' orchestration-instance:

(defmethod objfromobjs ((self orch-output) (out orchestration))
  (make-instance 'orchestration :orch-output self))

(defmethod objfromobjs ((self orchestration) (out orch-output))
  (orch-output self))

;; read input from external files and place in orch-output instance

(defmethod objfromobjs ((orch-output-file pathname) (out orch-output))
  ;; "expects an ***.orchestration.txt file (output from 'orchestrate' binary)"
  (when orch-output-file
    (let ((orch-struct (om-read-file orch-output-file)))
      (parse-orchidea-output orch-struct))))

(defun read-orchestration-file (orch-file)
  "reads an orchidea output-file (xxx.orchestration.txt) stored on disk"
  (let* ((file (or orch-file (om-choose-file-dialog :prompt "select an orchidea output-file (xxx.orchestration.txt)"))))
    (when file
      (parse-orchidea-output (om-read-file file)))))
