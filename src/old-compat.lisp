;;; 
;;; trying to keep some for minimal backwards compat?
;;;

;; 
;; 
;; removed from lib 23.11:
;; remove template-based approach, instead use orch-config class
;;
;; probably most of global vars below have new names,  check globals.lisp
;; 

(defparameter *orch-default-config-template*
  (namestring (make-pathname :directory (pathname-directory *orch-path-to-orchestrate*)
			     :name "config_template.txt")))

(defmethod! orch-set-default-config-template ((path string))
  :initvals (list *orch-default-config-template*)
  :icon 451
  :indoc '("path to a valid orchidea config template file")
  :doc "set path to a valid orchidea config template file"
  (setf *orch-default-config-template* (or path (file-chooser))))





(defun orch-set-up-orch-config-file (&key
				       (instruments *orch-default-ensemble*)
				       (input-config *orchidea-config-template-path*)
				       (onsets-threshold 1.0)
				       input-config-string
				       (db-file (namestring *orch-sol-db-file*))
				       (db-sound-path (derive-sound-path-from-db-file))
				       (output-config-file (orch-tmp-config-file))
				       
				       )
  "Set up a valid config file based on template and some passed parameters, return the name of a xxx.config.txt file on disk to pass to 'orchestrate'"
  (let ((config-string (or input-config-string (orch-config-string input-config))))
    (with-open-file (o output-config-file :direction :output :if-does-not-exist :create :if-exists :supersede)
      (progn
	(setf config-string (replace-all config-string "__DB_FILE__"		db-file))
	(setf config-string (replace-all config-string "__SOUND_PATH__"		(namestring db-sound-path)))
	(setf config-string (replace-all config-string "__ORCHESTRA__"		instruments))
	(setf config-string (replace-all config-string "__ONSETS_THRESHOLD__"	(prin1-to-string onsets-threshold))))
      (write-string config-string o))
    output-config-file))


(defun orch-config-string (file)
  "input: filename, textfile or pathname ; return a config string to write to a .config-file on disk"
  (cond ((equal (type-of file) 'textfile) (om-buffer-text (buffer-text file)))
	((or (stringp file) (pathnamep file))
	 (om-read-file file))
	(t (error "no .config-file or template provided"))))

(defmethod! orch-config-from-template ((self pathname)
				       &key
				       (instruments *orch-default-ensemble*)
				       (output-config-file (orch-tmp-config-file))
				       (db-file *orch-sol-db-file*)
				       (onsets-threshold 1.0))
  :initvals nil
  :indoc '("textfile, pathname or string" "ensemble" "output config file" "db-sound-path" "onsets-threshold" )
  :doc "Expects a template as input, and sets up a valid .config file in out-files/*,
to pass as argument in the call to 'orchestrate'.

Default values:
	[instruments: *orch-default-ensemble*],
	[db-file: *orch-sol-db-file*],
	[onsets-threshod: 1.0]
	[output-config-file: (orch-tmp-config-file)]

Returns an instance of orch-config
"
  :icon 451
  (let* ((db-sound-path (derive-sound-path-from-db-file (namestring db-file)))
	 (tmp-file (orch-set-up-orch-config-file :input-config self
						 :output-config-file output-config-file
						 :instruments instruments
						 :db-file (namestring db-file)
						 :db-sound-path db-sound-path
						 :onsets-threshold onsets-threshold
						 )))
    
    (make-instance 'orch-config :tmp-config-file tmp-file :instruments instruments)))
  

(defmethod! orch-config-from-template ((self string)
				       &key
				       (instruments *orch-default-ensemble*)
				       (output-config-file (orch-tmp-config-file))
				       (db-file *orch-sol-db-file*)
				       (onsets-threshold 1.0))
  (let* ((db-sound-path (derive-sound-path-from-db-file (namestring db-file)))
	 (tmp-file (orch-set-up-orch-config-file :input-config self
						 :output-config-file output-config-file
						 :instruments instruments
						 :db-file (namestring db-file)
						 :db-sound-path db-sound-path
						 :onsets-threshold onsets-threshold)))
    (make-instance 'orch-config :tmp-config-file tmp-file :instruments instruments)))

(defmethod! orch-config-from-template ((self textfile)
				       &key
				       (instruments *orch-default-ensemble*)
				       (output-config-file (orch-tmp-config-file))
				       (db-file *orch-sol-db-file*)
				       (onsets-threshold 1.0))
  (let* ((config-string (om-buffer-text (buffer-text self)))
	 (db-sound-path (derive-sound-path-from-db-file (namestring db-file)))
	 (tmp-file (orch-set-up-orch-config-file :input-config-string config-string
						 :output-config-file output-config-file
						 :instruments instruments
						 :db-file (namestring db-file)
						 :db-sound-path db-sound-path
						 :onsets-threshold onsets-threshold)))
    (make-instance 'orch-config :tmp-config-file tmp-file :instruments instruments)))

(defmethod! orch-config-from-template (self
				       &key
				       (instruments *orch-default-ensemble*)
				       (output-config-file (orch-tmp-config-file))
				       (db-file *orch-sol-db-file*)
				       (onsets-threshold 1.0))
  (orch-config-from-template *orchidea-config-template-path*))
