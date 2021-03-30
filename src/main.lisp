(in-package om)

(defvar *db-file* nil)
(defvar *sound_path* nil)
(defvar *config-template-path*
        (make-pathname :directory (pathname-directory *executable-path*)
                   :name "config_template.txt"))

(defun set-executable-path ()
    (setf *executable-path* (om::file-chooser)))

(defun set-db-file (path)
  (setf *db-file* (or path (om::file-chooser))))

(defparameter *default-orchestration* "Fl Fl Ob Ob ClBb ClBb Bn Bn Hn Hn TpC TpC Tbn Tbn BTb Vn Vn Va Va Vc Vc Cb Cb")

;;;;;;;;;;; orchestrate method ;;;;;;;;;;;;;

(defmethod! orchestrate ((sound sound) (orchestration string) (dynamic t))
  :initvals '(nil *default-orchestration* nil)
  :indoc '("source sound object" "instrument abbreviations (space-delimited string)" "nil for status, t for dynamic")
  :icon 451
  :doc "generate orchestration from source sample and database"
  :numouts 2
  :menuins '((2 (("static" nil)
                 ("dynamic" t))))
                 
    (when (null *db-file*) (error "db file not set, use set-db-file function"))
    (let ((tmp-dir (make-pathname :directory (append (pathname-directory *om-tmpfiles-folder*) (list (string+ "tmp-" (prin1-to-string (om-random 10000000 99999999)))))))
          (db-sound-path (derive-sound-path-from-db-file))
          (output-basename (string+ (pathname-name (filename sound)) "-orch-" (prin1-to-string (om-random 10000000 99999999)))))

      (om-cmd-line (string+ 
                    "mkdir " (namestring tmp-dir)
                    " && "
                    "cd " (namestring tmp-dir)
                    " && "
                    "sed 's/__DB_FILE__/" (escape-slashes (namestring *db-file*)) "/' " (namestring *config-template-path*) " > orch.txt"
                    " && "
                    "sed -i '' 's/__SOUND_PATH__/" (escape-slashes (namestring db-sound-path)) "/' orch.txt"
                    " && "
                    "sed -i '' 's/__ORCHESTRA__/" orchestration "/' orch.txt"
                    " && "
                    "sed -i '' 's/__ONSETS_THRESHOLD__/" (if dynamic "0.1" "2") "/' orch.txt"
                    " && "
                    (namestring *executable-path*) " '" (namestring (filename sound)) "' orch.txt"
                    " && "
                    "mv connection.wav ../" output-basename ".wav" 
                    " && " 
                    "mv connection.txt ../" output-basename ".txt" 
                    " && " 
                    " cd .. && rm -rf " (namestring tmp-dir)
          ))

      (let ((lines (get-file (tmpfile (string+ output-basename ".txt")))))
        (values 
          (tmpfile (string+ output-basename ".wav")) 
          (parse-orchidea-output lines)))))



(defun derive-sound-path-from-db-file ()
  (let ((root (first (lw::split-sequence (list #\.) (pathname-name *db-file*)))))
    (make-pathname :directory (append (pathname-directory *db-file*) (list root)))))