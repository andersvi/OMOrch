(in-package :om)

;; various global variables for om-orchidea


(defparameter *orchidea-executable-path*
  (make-pathname :directory (append (butlast (pathname-directory *load-pathname*)) (list "bin")) 
                 :name "orchestrate"))

(defmethod! orchidea-set-executable-path ((path string))
  :initvals '("")
  :icon 451
  :indoc '("path to orchestrate binary")
  :doc "set path to orchestrate binary"
  (setf *orchidea-executable-path* (or path (file-chooser))))


(defparameter *orchidea-config-template-path*
  (make-pathname :directory (pathname-directory *orchidea-executable-path*)
                 :name "config_template.txt"))

(defmethod! orchidea-set-config-template ((path string))
  :initvals nil
  :icon 451
  :indoc '("path to orchidea config template file")
  :doc "set path to orchidea config template file"
  (setf *orchidea-config-template-path* (or path (file-chooser))))


(defparameter *orchidea-db-file* nil)
(defparameter *orchidea-sound-path* nil)

(defun derive-sound-path-from-db-file ()
  (let ((root (first (lw::split-sequence (list #\.) (pathname-name *orchidea-db-file*)))))
    (namestring
     (make-pathname :directory (append (pathname-directory *orchidea-db-file*) (list root))))))

(defmethod! orchidea-set-db-file-and-sound-path ((path string))
  :initvals nil
  :icon 451
  :indoc '("path to .spectrum.db file")
  :doc "set path to .spectrum.db (database sound folder must be adjacent)"
  (progn
    (setf *orchidea-db-file* (or path (file-chooser)))
    (setf *orchidea-sound-path*
	  (let ((root (first (lw::split-sequence (list #\.) (pathname-name *orchidea-db-file*)))))
	    (namestring
	     (make-pathname :directory (append (pathname-directory *orchidea-db-file*) (list root))))))))

(defparameter *orchidea-default-orchestration*
  "Fl Fl Ob Ob ClBb ClBb Bn Bn Hn Hn TpC TpC Tbn Tbn BTb Vn Vn Va Va Vc Vc Cb Cb")


(defparameter *orch-draw-extras* t "should i draw dynamics marks, style-text indications etc. in editors?")



;; various functions to funcall on a note to draw style, dynamics, other metadata, see src/draw-extras.lisp:

(defvar *orch-extras-list* '())
;; (setf *orch-extras-list* '(orch-add-style-text orch-add-dynamic-sign))
(setf *orch-extras-list* '(orch-add-style-text ))
