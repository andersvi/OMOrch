(in-package :omorch)


(defun test-orchestrate-binary ()
  ;;returns 0 if found, 1 if not
  (sys:run-shell-command "bash -l -c 'which orchestrate'"))


(defun find-orchestrate-path ()
  ;;outputs orchestrate binary unix path
  (multiple-value-bind (out pid)
      #+unix(sys:run-shell-command "bash -l -c 'which orchestrate'"
                                   :wait nil
                                   :output :stream
                                   :error-output nil)
     #+win32(sys:run-shell-command "where Orchestrate.exe"
                                  :wait nil
                                  :output :stream
                                  :error-output nil)
    (declare (ignore pid))
    (with-open-stream (out out)
      (values (read-line out)))))

(defun orchestrate? ()
  ;;tests if orchestrate-binary is installed"
  (let ((test (test-orchestrate-binary)))
    (if (not (= 1 test))
	(find-orchestrate-path)
	"ORCHSTRATE BINARY NOT FOUND")))


;; various global variables for OMOorch
(defparameter *orch-path-to-orchestrate*
  (namestring
   (make-pathname :directory (append (butlast (pathname-directory *load-pathname*)) (list  "resources" "bin")) 
                  :name "orchestrate")))

(defmethod! orch-set-path-to-orchestrate ((path string))
  :initvals (list *orch-path-to-orchestrate*)
  :icon 451
  :indoc '("path to orchestrate binary")
  :doc "set path to orchestrate binary"
  (setf *orch-path-to-orchestrate* (or path (file-chooser))))

(defparameter *orch-overwrite-previous-run* t
  "whether to overwrite output from previous calls in out-files/omorch-***")

(defparameter *orch-default-config-path*
  (namestring (make-pathname :directory (pathname-directory *orch-path-to-orchestrate*)
			     :name "dynamic_orchestration.txt")))

(defmethod! orch-set-default-config ((path string))
  :initvals (list *orch-default-config-path*)
  :icon 451
  :indoc '("path to default config file")
  :doc "set path to default config file"
  (setf *orch-default-config-path* (or path (file-chooser))))

(defparameter *orch-sol-db-file*
  (namestring (make-pathname :directory (pathname-directory *orch-path-to-orchestrate*)
			     :name "SET_SOL_DB-FILE.db")))


(defvar *orch-sol-sound-directory*)


(defmethod! orch-set-db-file-and-sound-path ((path string))
  :initvals *orch-sol-db-file*
  :icon 451
  :indoc '("path to .spectrum.db file")
  :doc "set path to .spectrum.db (database sound folder must be adjacent)"
  (progn
    (setf *orch-sol-db-file* (or path (file-chooser)))
    (setf *orch-sol-sound-directory*
	  (let ((root (first (lw::split-sequence (list #\.) (pathname-name *orch-sol-db-file*)))))
	    (namestring
	     (make-pathname :directory (append (pathname-directory *orch-sol-db-file*) (list root))))))))

;; TODO set this to something  more sensible
;; (orch-set-db-file-and-sound-path "/NOT/YET/SET")



(defparameter *orch-default-ensemble*
  "Fl Fl Ob Ob ClBb ClBb Bn Bn Hn Hn TpC TpC Tbn Tbn BTb Vn Vn Va Va Vc Vc Cb Cb")



