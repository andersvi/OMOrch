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
;;; usage: make instances of OM's various 'xxx-extra'-classes, then #'add-extra' these to note
;;;

(defun orch-add-dynamic-sign (note)
  ;; add dynamic sign close to note
  (let ((dyn (mki 'vel-extra
		  :dynamics (get-dyn-from-vel (vel note))
		  :deltay 1.4)))
    (add-extra note dyn nil t)))

(defun orch-add-style-text (note)
  ;; text style-indication at close to note
  (let ((text (make-instance 'text-extra
			     :thetext (format nil "~S" (style note))
			     :deltay -3)))
    (add-extra note text nil t)))


;; main function, called when setting up and filling relevant editors

(defun orch-add-extras-to-note (n)
  ;; recursively funcall functions to add extras to note
  (labels ((add-rec (n extras)
	     (if (not extras)
		 n
		 (add-rec (funcall (car extras) n) (cdr extras)))))
    (add-rec n *orch-extras-list*)))

;; (let ((n (mki 'orch-note)))
;;   (orch-add-extras-to-note n))



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

