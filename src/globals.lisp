(in-package :om)


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


;; various global variables for om-orchidea
(defparameter *orchidea-executable-path*
  (namestring
   (make-pathname :directory (append (butlast (pathname-directory *load-pathname*)) (list  "resources" "bin")) 
                  :name "orchestrate")))

(defmethod! orchidea-set-executable-path ((path string))
  :initvals (list *orchidea-executable-path*)
  :icon 451
  :indoc '("path to orchestrate binary")
  :doc "set path to orchestrate binary"
  (setf *orchidea-executable-path* (or path (file-chooser))))

(defparameter *orch-overwrite-previous-run* t
  "whether to overwrite output from previous calls in out-files/omorch-***")

(defparameter *orchidea-config-template-path*
  (namestring (make-pathname :directory (pathname-directory *orchidea-executable-path*)
			     :name "config_template.txt")))

(defmethod! orchidea-set-config-template ((path string))
  :initvals (list *orchidea-config-template-path*)
  :icon 451
  :indoc '("path to orchidea config template file")
  :doc "set path to orchidea config template file"
  (setf *orchidea-config-template-path* (or path (file-chooser))))

(defparameter *orchidea-db-file*
  (namestring (make-pathname :directory (pathname-directory *orchidea-config-template-path*)
			     :name "SET_ORCHIDEA_DB-FILE.db")))


(defvar *orchidea-sound-path*)


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

(defun derive-sound-path-from-db-file (db-file)
  (let ((root (first (lw::split-sequence (list #\.) (pathname-name db-file)))))
    (namestring
     (make-pathname :directory (append (pathname-directory db-file) (list root))))))

;; TODO set this to something  more sensible
(orchidea-set-db-file-and-sound-path "/NOT/YET/SET")




(defparameter *orchidea-default-ensemble*
  "Fl Fl Ob Ob ClBb ClBb Bn Bn Hn Hn TpC TpC Tbn Tbn BTb Vn Vn Va Va Vc Vc Cb Cb")



