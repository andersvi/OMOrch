(in-package :om)


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
  (:documentation "main orchestration class, stores call and results from orchestrate method")
  (:icon 451))


(defmethod omng-save ((self orchestration) &optional (values? nil))
  `(let ((target-sound ,(omng-save (target-sound self)))
	 (output-sound ,(omng-save (output-sound self)))
	 (orch-output ,(omng-save (orch-output self)))
	 (command-line ,(omng-save (command-line self)))
	 (instruments ,(omng-save (instruments self)))
	 (config ,(omng-save (config self)))
	 (onsets-threshold ,(omng-save (onsets-threshold self))))
     (make-instance 'orchestration
		    :target-sound target-sound
		    :output-sound output-sound
		    :orch-output orch-output
		    :command-line command-line
		    :instruments instruments
		    :config config)))

(defclass! orch-output ()
  ((ensemble :accessor ensemble :initarg :ensemble :initform nil)
   (instruments :accessor instruments :accessor orch-instruments :initarg :instruments :type list :initform nil)
   (segments :accessor segments :initarg :segments :initform nil))
  (:icon 451))

(defun make-orch-output (&key ensemble instruments segments)
  (make-instance 'orch-output :ensemble ensemble :instruments instruments :segments segments))

(defmethod omng-save ((self orch-output) &optional (values? nil))
  `(let ((segs ,(omng-save (segments self)))
	 (ensemble ,(omng-save (ensemble self))))
     (make-instance 'orch-output :segments segs :ensemble ensemble)))



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

(defmethod omng-save ((self orch-solution) &optional (values? nil))
  `(let ((notes ,(omng-save (notes self))))
     (make-instance 'orch-solution :notes notes)))

;; just in case it might be needed somewhere, in addition to orch-segment above.  Dont know if its useful yet, perhaps
;; because closer to regular 'chord'-class?

(defclass! orch-chord (chord)
  ;; class to carry orch-note info through to chord-seqs, omng-save etc.:
  ((orch-notes :initarg :orch-notes :accessor orch-notes :initform (list (mki 'orch-note)))))

;; TODO: handle slots 'inside' and 'orch-notes' as aliases in some way

(defmethod initialize-instance :after ((self orch-chord) &rest args)
  (setf (inside self) (orch-notes self)))

(defmethod omng-save ((self orch-chord) &optional (values? nil))
  `(let ((orch-notes ,(omng-save (orch-notes self))))
     (make-instance 'orch-chord :orch-notes orch-notes)))

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


(defun make-orch-note (&rest initargs
		       &key instrument style pitch-name dynamic instance sample-path detune
		       &allow-other-keys)
  (apply #'make-instance
	 'orch-note 
	 :instrument instrument
	 :style style 
	 :pitch-name pitch-name
	 :midic (n->mc (orch-note-string-2-om-note-string pitch-name))
	 :dynamic dynamic 
	 :instance instance 
	 :sample-path sample-path 
	 :detune detune
	 initargs))

