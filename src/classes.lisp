(in-package :om)

;;
;; replace original structs with CLOS classes, perhaps use some OMs built-in general functionality?
;;

(defclass! orch-output ()
  ((orchestration :accessor orchestration :initarg :orchestration :initform nil)
   (segments :accessor segments :initarg :segments :initform nil)))

(defun make-orch-output (&key orchestration segments)
  (make-instance 'orch-output :orchestration orchestration :segments segments))

(defclass! orch-segment ()
  ((onset :accessor onset :initarg :onset :initform 0)
   (duration :accessor duration :initarg :duration :initform 0)
   (solution :accessor solution :initarg :solution :initform nil)))

(defun make-orch-segment (&key onset solution duration)
  (make-instance 'orch-segment :onset onset :solution solution :duration duration))

(defclass! orch-solution (chord)
  ;; subclass of chord?
  ((id :accessor id :initarg :id :initform nil)
   (notes :accessor notes :initarg :notes :initform nil)))

(defun make-orch-solution (&key id notes)
  (make-instance 'orch-solution :id id :notes notes))

;;
;; subclassing OM's regular note,
;; 
;; allow the orig orch-struct :duration-ms together with om::note :dur
;; 

(defclass! orch-note (note)
  (
   ;; add accessors for om::note's dur:
   (dur :accessor duration-ms :initarg :duration-ms)
   (instrument :accessor instrument :initarg :instrument :initform nil)
   (style :accessor style :initarg :style :initform nil)
   (pitch-name :accessor pitch-name :initarg :pitch-name :initform nil)
   (dynamic :accessor dynamic :initarg :dynamic :initform nil)
   (instance :accessor instance :initarg :instance :initform nil)
   (sample-path :accessor sample-path :initarg :sample-path :initform nil)
   (detune :accessor detune :initarg :detune :initform 0)))


(defun make-orch-note (&rest initargs
		       &key duration-ms instrument style pitch-name dynamic instance sample-path detune
		       &allow-other-keys)
  (apply #'make-instance
	 'orch-note 
	 :dur (round duration-ms)
	 :instrument instrument
	 :style style 
	 :pitch-name pitch-name
	 :midic (n->mc (orch-note-2-om-note pitch-name))
	 :dynamic dynamic 
	 :instance instance 
	 :sample-path sample-path 
	 :detune detune
	 initargs))

;; 
;; main orchestration class:
;; class to contain outputs from call to orchestrate
;; slots store instance of inputs and outputs
;; use orch-output->chord-seq, orch-output->multi-seq and others
;;
;; method orchestrate returns an instance of class 'orchestration
;; 
   

;; TODO: consider using omNG-box-value on this class instead of separate call on #'orchestration

(defclass! orchestration (container)
  ((target-sound :accessor target-sound :accessor target :accessor orch-target :initarg :target :type sound :initform nil)
   (output-sound :accessor output-sound :accessor orch-sound :initarg :output-sound :type sound :initform nil)
   (orchestration :accessor orchestration :accessor orch-orchestration  :initarg :orchestration :type string :initform nil )
   (command-line :accessor command-line :accessor orch-command-line :initarg :command-line :type string :initform nil)
   (instruments :accessor instruments :accessor orch-instruments :initarg :instruments :type list :initform nil)
   (config :accessor config :accessor orch-config :initarg :config :type textfile :initform nil)
   (onsets-threshold :accessor onsets-threshold :initarg :onsets-threshold :type number :initform 1 ))
  (:documentation "main orchestration class, stores call and results from orchestrate method"))


