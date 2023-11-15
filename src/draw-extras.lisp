(in-package :om)

;;; 
;;; handle drawing of extras from orchidea output in score editors
;;;
;;; - playing style, dynamics, others - stored as slots in orch-note
;;;
;;; - draw, edit, output, input...
;;;
;;; - note, chord, chord-seq, multi-seq
;;;
;;; usage: make instances of OM's various 'xxx-extra'-classes, then #'add-extra' these into note's 'extra-obj-list
;;;

(defvar *orch-extras-list* nil)				    ;holding instances
(defvar *orch-extras-assoc-list* '()) 			    ;lookup to use in prefs and when saving ws:

(defparameter *orch-draw-extras* t "should i draw dynamics marks, style-text indications etc. in editors?")

(defclass orch-extra ()
  ((name :accessor name :initarg :name :type :symbol :initform nil )
   (func :accessor func :initarg :func :initform nil)
   ;; to present in prefs etc.
   (print-name :accessor print-name :initarg :print-name :type :string :initform nil)
   ;;wether to draw this extra or no
   (active? :accessor active? :initarg :active? :initform nil :type :boolean)))

(defmethod omng-save ((self orch-extra) &optional (values? nil))
  (let ((*print-readably* t))
    `(let ((name ,(omng-save (name self)))
	   (func ,(function-lambda-expression (func self)))
	   (print-name ,(omng-save (print-name self)))
	   (active? ,(omng-save (active? self))))
       (make-instance 'orch-extra :name name :func func :print-name print-name :active? active?))))

(defmethod print-object ((obj orch-extra) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s ~s" (name obj) (active? obj))))

;; setup extra-func and add to global list

(defmacro orch-add-extra-func (name func print-name &optional (active? nil))
  `(let ((extra-func (make-instance 'orch-extra
				    :name ,name
				    :func ,func
				    :print-name ,print-name
				    :active? ,active?)))
     (pushnew extra-func *orch-extras-list* :test #'(lambda (a b) (equal (name a) (name b))))
     (pushnew (cons (name extra-func) ,active?) *orch-extras-assoc-list* :key #'car)))


;; (defun orch-is-extra-active? (print-name)
;;   (let ((extra (find print-name *orch-extras-list*
;; 		     :key #'print-name
;; 		     :test #'string-equal)))
;;     (active? extra)))

(defun orch-is-extra-active? (name)
  (cdr (assoc name *orch-extras-assoc-list*)))

(defun orch-set-extra-active! (name &optional (val t))
  (let* ((extra (find name *orch-extras-list* :key #'name))
	 (state val))
    (setf (active? extra) state)
    (setf (cdr (assoc name *orch-extras-assoc-list*)) state)))


(defun orch-add-dynamic-sign (note)
  ;; add dynamic sign close to note
  (let ((dyn (mki 'vel-extra
		  :dynamics (get-dyn-from-vel (vel note))
		  :deltay 1.4)))
    (add-extra note dyn nil t)))

(orch-add-extra-func 'dynamics #'orch-add-dynamic-sign "Dynamics" t)

(defun orch-add-style-text (note)
  ;; text style-indication at close to note
  (let ((text (make-instance 'text-extra
			     :thetext (format nil "~S" (style note))
			     :deltay -3)))
    (add-extra note text nil t)))

(orch-add-extra-func 'styles #'orch-add-style-text "Styles" t)


;; called when filling orch-notes into relevant editors

(defun orch-add-extras-to-note (note)
  (loop for xtr in *orch-extras-list*
	when (active? xtr)
	  do (setf note (funcall (func xtr) note))
	finally (return note)))



;; *orch-extras-list*
;; (second *orch-extras-list*)
;; (setf (active? (second *orch-extras-list*)) t)

;; (setf (active? (first *orch-extras-list*)) nil)
;; (setf (active? (first *orch-extras-list*)) t)
;; (active? (first *orch-extras-list*))

;; (let ((n (mki 'orch-note)))
;;   (orch-add-extras-to-note n)
;;   )



#|

;; 
;; Possibly? or just messy? - add special note-heads, staccato, accent etc if useful?, or just rely on text for style?
;; 

functions returning various note-heads:

head-1/4harm head-1/2harm
head-1/4sq head-1/2sq head-1/4tri head-1/2tri head-1/4circ head-1/2circ
head-cross head-carre head-losange head-rect
head-triangle head-cercle

(defun orch-add-special-note-head (note)
(let ((head (mki 'head-extra
:thehead            (head-cross)
:deltax             0
:deltay             0
:object             nil
:levelsize          1
:graphic-frame      nil
:uord               1
)
))
(add-extra note head nil t)))

|#

;;;
;;; add extras to note, taking care to return copy of note

