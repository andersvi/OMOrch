(in-package :om)

;;;;;;;;;;; orchestrate method ;;;;;;;;;;;;;


(defun orch-set-up-orch-config-file (&key
				       (orchestration *orchidea-default-orchestration*)
				       (template-file *orchidea-config-template-path*)
				       (output-config-file "orch.config.txt")
				       (db-sound-path (derive-sound-path-from-db-file))
				       (onsets-threshold 2))
  (with-open-file (o output-config-file :direction :output :if-does-not-exist :create :if-exists :supersede)
    (with-open-file (s template-file)
      (loop for line = (read-line s nil)
	    while line
	    do
	       (progn
		 (setf line (replace-all line "__DB_FILE__"		(namestring *orchidea-db-file*)))
		 (setf line (replace-all line "__SOUND_PATH__"		(namestring db-sound-path)))
		 (setf line (replace-all line "__ORCHESTRA__"		orchestration))
		 (setf line (replace-all line "__ONSETS_THRESHOLD__"	(prin1-to-string onsets-threshold))))
	       (write-line line o)))
    output-config-file))


;;;
;;; POSSIBLY: is there a way to avoid loading (the same) database for each run?
;;; 

;;; method orchestrate returns an instance of class 'orchestration

(defmethod! orchestrate ((target sound) (instrument-pool string) (onsets-threshold number))
  :initvals '(nil *orchidea-default-orchestration* 0.7)
  :indoc '("source target object" "instrument abbreviations (space-delimited string)" "onsets threshold (ex. static = 2, dynamic = 0.1)")
  :icon 451
  :doc (format nil "Generate orchestration from source sample and database, return instance of orchestration.  Be sure to set appropriate various global orch-*** parameters")
  :numouts 1

  (cond ((not *orchidea-db-file*) (error "db file not set, use orchidea-set-db-file-and-sound-path function"))
	((not *orchidea-executable-path*) (error "orchidea binary not set, use orchidea-set-executable-path function")))
  (unless (and (= (sample-rate target) 44100) (= (sample-size target) 16)) (error "orchestrate: target file must be 44.1k, 16 bit"))
  (let ((db-sound-path (namestring (derive-sound-path-from-db-file)))

	;; set up various input and output filenames
	(output-dir (namestring (make-pathname 
				 :directory (append (pathname-directory *om-tmpfiles-folder*)
						    (list (string+ "orchidea-" (time-tag))))))))
    

    (create-directory output-dir)

    (let* ((target-sound (filename target))
	   (output-basename (pathname-name target-sound))
	   (output-sound (format nil "~A~A.wav" output-dir output-basename)) ;current orchidea supports only RIFF/wav
	   (output-orchestration (format nil "~A~A.orchestration.txt" output-dir output-basename))
	   (config-file (format nil "~A~A.config.txt" output-dir output-basename)))

      (print (format nil "output-dir: ~A" output-dir))
      (print (format nil "config-file: ~A" config-file))
      (print (format nil "output-basename: ~A" output-basename))
      (print (format nil "output-orchestration ~A" output-orchestration))
      (print (format nil "output-sound ~A" output-sound))

      ;; TODO: allow control of all options in config from here
    
      (let* ((conf-file (orch-set-up-orch-config-file :output-config-file config-file
						      :orchestration instrument-pool
						      :onsets-threshold onsets-threshold
						      :template-file *orchidea-config-template-path*
						      :db-sound-path db-sound-path))
	     (cmd (format nil "cd ~A && ~A ~A ~A"
			  output-dir
			  (namestring *orchidea-executable-path*)
			  (namestring (filename target))
			  conf-file
			  )))

	(progn

	  (om-cmd-line cmd)

	  (rename-file (string+ output-dir "connection.wav") output-sound)
	  (rename-file (string+ output-dir "connection.txt") output-orchestration)
    
	  ;; 
	  ;; return some useful output from call:
	  ;; 
	  (let* ((orch-struct (om-read-file output-orchestration))
		 (orch-output (parse-orchidea-output orch-struct))
		 (orch-config (objfromobjs conf-file (mki 'textfile)))
		 (output-sound (objfromobjs output-sound (mki 'sound)))
		 (output-instrument-list (collect-string-items instrument-pool " ")))
	    
	    (make-instance 'orchestration
			   :target-sound target
			   :output-sound output-sound
			   :orch-output orch-output
			   :instruments output-instrument-list
			   :command-line cmd
			   :config orch-config
			   :onsets-threshold onsets-threshold)))))))

;; recall 'original' orchestration-instance:

(defmethod objfromobjs ((self orch-output) (out orchestration))
  (make-instance 'orchestration :orch-output self))


