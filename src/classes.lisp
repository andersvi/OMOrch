(in-package :om)

;; 
;; main orchestration class:
;; class to contain outputs from call to orchestrate
;; slots store instance of inputs and outputs
;; use orch-output->chord-seq, orch-output->multi-seq and others
;;
;; method orchestrate returns an instance of class 'orchestration
;; 

(defclass! orchestration (container)
  ((target-sound :accessor target-sound :accessor target :accessor orch-target :initarg :target :initarg :target-sound :type sound :initform nil)
   (output-sound :accessor output-sound :accessor orch-sound :initarg :output-sound :type sound :initform nil)
   (orch-output  :accessor orch-output  :initarg :orch-output :type string :initform nil )
   (command-line :accessor command-line :accessor orch-command-line :initarg :command-line :type string :initform nil)
   (instruments :accessor instruments :accessor orch-instruments :initarg :instruments :type list :initform nil)
   (config :accessor config :accessor orch-config :initarg :config :type textfile :initform nil)
   (onsets-threshold :accessor onsets-threshold :initarg :onsets-threshold :type number :initform 1 ))
  (:documentation "main orchestration class, stores call and results from orchestrate method"))

(defclass! orch-output ()
  ((orchestration :accessor orchestration :initarg :orchestration :initform nil)
   (instruments :accessor instruments :accessor orch-instruments :initarg :instruments :type list :initform nil)
   (segments :accessor segments :initarg :segments :initform nil)))

(defun make-orch-output (&key orchestration instruments segments)
  (make-instance 'orch-output :orchestration orchestration :instruments instruments :segments segments))

;; hierarchy in orchestrate output:
;; 
;; [ segment 4272.45 417.959
;; 	[ solution 2
;;  		[ note 417.959 Fl cre_dec C5 ppmfpp N /Winds/Flute/crescendo_to_decrescendo/Fl-cre_dec-C5-ppmfpp-N-R100u.wav 0 ]
;; 		[ note 417.959 Fl cre_dec C5 ppmfpp N /Winds/Flute/crescendo_to_decrescendo/Fl-cre_dec-C5-ppmfpp-N-R100u.wav 0 ]
;; 		[ note 957.846 Gtr ord F4 mf 4c /PluckedStrings/Guitar/ordinario/Gtr-ord-F4-mf-4c-T11d.wav 0 ]
;; 		[ note 417.959 Gtr pizz_bartok C3 ff 5c /PluckedStrings/Guitar/pizzicato_bartok/Gtr-pizz_bartok-C3-ff-5c-N.wav 0 ]
;;
;;
;; (segment onset dur
;;	(solution N
;;		(note dur instrument technique .....)
;;		(note dur instrument technique .....)
;;		...))



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
;; 

(defclass! orch-note (note)
  ((instrument :accessor instrument :initarg :instrument :initform nil)
   (style :accessor style :initarg :style :initform nil)
   (pitch-name :accessor pitch-name :initarg :pitch-name :initform nil)
   (dynamic :accessor dynamic :initarg :dynamic :initform nil)
   (instance :accessor instance :initarg :instance :initform nil)
   (sample-path :accessor sample-path :initarg :sample-path :initform nil)
   (detune :accessor detune :initarg :detune :initform 0)))

(defmethod initialize-instance :after ((self orch-note) &rest args)
  (setf (slot-value self 'pitch-name) (mc->n (slot-value self 'midic))))


(defun make-orch-note (&rest initargs
		       &key instrument style pitch-name dynamic instance sample-path detune
		       &allow-other-keys)
  (apply #'make-instance
	 'orch-note 
	 :instrument instrument
	 :style style 
	 :pitch-name pitch-name
	 :midic (n->mc (orch-note-2-om-note pitch-name))
	 :dynamic dynamic 
	 :instance instance 
	 :sample-path sample-path 
	 :detune detune
	 initargs))

