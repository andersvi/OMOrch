(in-package :omorch)


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

(defclass* orchestration (om::container)
  ((target-sound :accessor target-sound :accessor target :initarg :target :initarg :target-sound :type sound :initform nil)
   (output-sound :accessor output-sound :accessor orch-sound :initarg :output-sound :type sound :initform nil)
   (orch-output  :accessor orch-output  :initarg :orch-output :type string :initform nil )
   (command-line :accessor command-line :accessor orch-command-line :initarg :command-line :type string :initform nil)
   (instruments :accessor instruments :accessor orch-instruments :initarg :instruments :type list :initform nil)
   (config-file :accessor config-file  :initarg :config-file :type string :initform nil))
  
  (:documentation "main orchestration class, stores call and results from orchestrate method")
  (:icon 451))

(defmethod omng-save ((self orchestration) &optional (values? nil))
  `(let ((target-sound ,(omng-save (target-sound self)))
	 (output-sound ,(omng-save (output-sound self)))
	 (orch-output ,(omng-save (orch-output self)))
	 (command-line ,(omng-save (command-line self)))
	 (instruments ,(omng-save (instruments self)))
	 (config-file ,(omng-save (config-file self))))
     (make-instance 'orchestration
		    :target-sound target-sound
		    :output-sound output-sound
		    :orch-output orch-output
		    :command-line command-line
		    :instruments instruments
		    :config-file config-file)))


(defclass! orch-output ()
  ((instruments :accessor instruments :accessor orch-instruments :initarg :instruments :type list :initform nil)
   (orch-segments :accessor orch-segments :initarg :orch-segments :initform nil))
  (:documentation "orch-output stores the 'score' from orchestration")
  (:icon 451))

(defun make-orch-output (&key instruments orch-segments)
  (make-instance 'orch-output :instruments instruments :orch-segments orch-segments))

(defmethod omng-save ((self orch-output) &optional (values? nil))
  `(let ((segs ,(omng-save (orch-segments self)))
	 (instruments ,(omng-save (instruments self))))
     (make-instance 'orch-output :orch-segments segs :instruments instruments)))


(defclass! orch-segment ()
  ((onset :accessor onset :initarg :onset :initform 0)
   (duration :accessor duration :initarg :duration :initform 0)
   (solution :accessor solution :initarg :solution :initform nil))
  (:documentation "'segment' level from orchestration")
  (:icon 451))

(defun make-orch-segment (&key onset solution duration)
  (make-instance 'orch-segment :onset onset :solution solution :duration duration))

(defclass! orch-solution (chord)
  ;; subclass of chord
  ((id :accessor id :initarg :id :initform nil)
   (notes :accessor notes :initarg :notes :initform nil))
  (:documentation "'solution' level from orchestration")
  (:icon 451))

(defun make-orch-solution (&key id notes)
  (make-instance 'orch-solution :id id :notes notes))

(defmethod omng-save ((self orch-solution) &optional (values? nil))
  `(let ((notes ,(omng-save (notes self))))
     (make-instance 'orch-solution :notes notes)))


;; just in case it might be needed somewhere, in addition to orch-segment above.  Dont know if its useful yet, perhaps
;; because closer to regular 'chord'-class?

(defclass! orch-chord (chord)
  ;; class to carry orch-note info through to chord-seqs, omng-save etc.:
  ((orch-notes :initarg :orch-notes :accessor orch-notes :initform (list (make-instance 'orch-note))))
  (:documentation "subclass of chord, w slots for various metadata from Orchidea")
  (:icon 451))

;; TODO: handle slots 'inside' and 'orch-notes' as aliases in some way

(defmethod initialize-instance :after ((self orch-chord) &rest args)
  (setf (inside self) (orch-notes self)))

(defmethod omng-save ((self orch-chord) &optional (values? nil))
  `(let ((orch-notes ,(omng-save (orch-notes self))))
     (make-instance 'orch-chord :orch-notes orch-notes)))

;; specialize omng-save for chord to save orch-notes

(defmethod omng-save :around ((self chord) &optional (values? nil))
  ;; make sure any orch-notes inside are saved, and added back in load-form
  (if (orch-find-if-orch-note self)
      (let ((notes (omng-save (inside self))))
	`(let ((new-chord ,(call-next-method)))
	   (setf (inside new-chord) ,notes)
	   new-chord))
      (call-next-method)))

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
   (detune :accessor detune :initarg :detune :initform 0))
  (:documentation "subclass of note, w slots for various metadata from Orchidea")
  (:icon 451))


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

