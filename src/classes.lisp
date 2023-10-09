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
  ((onset-ms :accessor onset-ms :initarg :onset-ms :initform 0)
   (duration :accessor duration :initarg :duration :initform 0)
   (solutions :accessor solutions :initarg :solutions :initform nil)))

(defun make-orch-segment (&key onset-ms solutions duration)
  (make-instance 'orch-segment :onset-ms onset-ms :solutions solutions :duration duration))

(defclass! orch-solution ()
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
   (detune :accessor detune :initarg :detune :initform nil)))


(defun make-orch-note (&rest initargs
		       &key instrument style pitch-name dynamic instance sample-path detune
		       &allow-other-keys)
  (apply #'make-instance
	 'orch-note 
	 :instrument instrument
	 :style style 
	 :pitch-name pitch-name 
	 :dynamic dynamic 
	 :instance instance 
	 :sample-path sample-path 
	 :detune detune
	 initargs))


;;; previous structs defined in om-orchidea:


;; (defstruct orch-output
;;   (orchestration)
;;   (segments)
;;   )

;; (defstruct orch-segment
;;   (onset-ms)
;;   (solutions))

;; (defstruct orch-solution
;;   (id)
;;   (notes))

;; (defstruct orch-note
;;   (duration-ms)
;;   (instrument)
;;   (style)
;;   (pitch-name)
;;   (dynamic)
;;   (instance)
;;   (sample-path)
;;   (detune)
;;   )

