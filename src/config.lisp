(in-package :om)
;;;
;;;
;;; class: orch-config
;;;
;;;		- parameters
;;;		- template
;;;		- textfile object
;;;		- filename
;;;
;;;


(defparameter *orch-run-config-file* "orch.config")

(defun orch-tmp-config-file () (tmpfile *orch-run-config-file*))

;; 
;; TODO: provide more sensible initforms here?  Currently just grabbed from random .config-file...
;; 
(defclass orch-config ()
  ((textfile-object :type textfile :initform nil :accessor textfile-object :initarg :textfile-object)
   (tmp-config-file :type :string :initform nil :accessor tmp-config-file :initarg :tmp-config-file)
   (instruments :type :list :initform nil :accessor instruments :initarg :instruments)
   (config-string :type :string :initform nil :accessor config-string  :initarg :config-string)
   (db_files :type string :initform nil :accessor db_files :initarg :db_files)  			
   (sound_paths :type string :initform nil :accessor sound_paths :initarg :sound_paths)  			
   (orchestra :type list :initform nil :accessor orchestra :initarg :orchestra)  
   (styles :type list :initform nil :accessor styles :initarg :styles)  
   (pop_size :type number :initform 500 :accessor pop_size :initarg :pop_size)  			
   (max_epochs :type number :initform 500 :accessor max_epochs :initarg :max_epochs)  			
   (pursuit :type number :initform 0 :accessor pursuit :initarg :pursuit)  				
   (xover_rate :type number :initform 0.8 :accessor xover_rate :initarg :xover_rate)  			
   (mutation_rate :type number :initform 0.01 :accessor mutation_rate :initarg :mutation_rate)  			
   (sparsity :type number :initform 0.001 :accessor sparsity :initarg :sparsity)  			
   (positive_penalization :type number :initform .5 :accessor positive_penalization :initarg :positive_penalization)  		
   (negative_penalization :type number :initform 10 :accessor negative_penalization :initarg :negative_penalization)  		
   (hysteresis :type number :initform 0 :accessor hysteresis :initarg :hysteresis)   
   (regularization :type number :initform 0 :accessor regularization :initarg :regularization)   
   (segmentation :type string :initform "flux" :accessor segmentation :initarg :segmentation)  			
   (partials_window :type number :initform 32768 :accessor partials_window :initarg :partials_window)  			
   (partials_filtering :type number :initform .2 :accessor partials_filtering :initarg :partials_filtering)  		
   (onsets_threshold :type number :initform 0.3 :accessor onsets_threshold :initarg :onsets_threshold)  		
   (onsets_timegate :type number :initform .6 :accessor onsets_timegate :initarg :onsets_timegate)  			
   (export_solutions :type number :initform 10 :accessor export_solutions :initarg :export_solutions)  		
   (t60 :type number :initform 2.8 :accessor t60 :initarg :t60)  				
   (dry_wet :type number :initform .4 :accessor dry_wet :initarg :dry_wet)))



(defmethod! orch-config ((self textfile))
  :initvals '(*orch-default-config-path*)
  :indoc '("textfile, pathname or string")
  :doc "Set up an instance of orch-config, and writes a temporary *.config to pass as argument in the call to 'orchestrate'.
Name of tmp-file isi stored in  *orch-run-config-file*
If input is a textfile object, orch-config writes its content to that file.
If input is a string or pathname, orch-config copies that file to the tmp file.
"
  :icon 451
  (let* ((config-string (om-buffer-text (buffer-text self)))
	 (tmp-file-name (orch-tmp-config-file))
	 (instruments (parse-instruments-from-config config-string :orchestra-tag "orchestra")))
    (with-open-file (o tmp-file-name :direction :output :if-does-not-exist :create :if-exists :supersede)
      (write-string config-string o))
    (make-instance 'orch-config
		   :textfile-object self
		   :config-string config-string
		   :tmp-config-file tmp-file-name
		   :instruments instruments)))

(defmethod! orch-config ((self pathname))
  :icon 451
  (let ((tmp-file-name (orch-tmp-config-file))
	(instruments (parse-instruments-from-config (namestring self) :orchestra-tag "orchestra")))
    (copy-file self (orch-tmp-config-file))
    (make-instance 'orch-config :tmp-config-file tmp-file-name :instruments instruments)))

(defmethod! orch-config ((self string))
  :icon 451
  (let* ((tmp-file-name (orch-tmp-config-file))
	 (instruments (parse-instruments-from-config self :orchestra-tag "orchestra")))
    (copy-file self tmp-file-name)
    (make-instance 'orch-config :tmp-config-file tmp-file-name :instruments instruments)))

(defmethod! orch-config (self)
  :icon 451
  (make-instance 'orch-config
		 :input-config *orch-default-config-path*
		 :tmp-config-file (orch-tmp-config-file)))

(defmethod objfromobjs ((self orch-config) (tf textfile))
  (objfromobjs (tmp-config-file self) (make-instance 'textfile)))



