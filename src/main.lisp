(in-package om)

(defvar *executable-path* 
    (make-pathname :directory (append (butlast (pathname-directory *load-pathname*)) (list "bin")) 
                   :name "orchestrate"))

(defvar *db-file* nil)
(defvar *sound_path* nil)
(defvar *config-template-path*
        (make-pathname :directory (pathname-directory *executable-path*)
                   :name "config_template.txt"))

(defun set-executable-path ()
    (setf *executable-path* (om::file-chooser)))

(defmethod! set-db-file ((path string))
  :initvals '("")
  :icon 451
  :indoc '("path to .spectrum.db file")
  :doc "set path to .spectrum.db (database sound folder must be adjacent)"
  (setf *db-file* (or path (file-chooser))))

(defparameter *default-orchestration* "Fl Fl Ob Ob ClBb ClBb Bn Bn Hn Hn TpC TpC Tbn Tbn BTb Vn Vn Va Va Vc Vc Cb Cb")

;;;;;;;;;;; orchestrate method ;;;;;;;;;;;;;

(defmethod! orchestrate ((sound sound) (orchestration string) (onsets-threshold number) (output-format t) &key quantizer)
  :initvals '(nil *default-orchestration* 0.1 :chord-seq)
  :indoc '("source sound object" "instrument abbreviations (space-delimited string)" "onsets threshold (ex. static = 2, dynamic = 0.1)" "score-format")
  :icon 451
  :doc "generate orchestration from source sample and database"
  :numouts 2
  :menuins '((3 (("struct" :struct)
                 ("mf-info" :mf-info)
                 ("chord-seq" :chord-seq)
                 ("multi-seq" :multi-seq)
                 ("poly" :poly))))
                
                 
    (when (null *db-file*) (error "db file not set, use set-db-file function"))
    (unless (and (= (sample-rate sound) 44100) (= (sample-size sound) 16)) (error "sound file must be 44.1k, 16 bit"))
    (let ((tmp-dir (make-pathname 
                    :directory (append (pathname-directory *om-tmpfiles-folder*)
                                       (list (string+ "tmp-" (prin1-to-string (om-random 10000000 99999999)))))))
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
                    "sed -i '' 's/__ONSETS_THRESHOLD__/" (prin1-to-string onsets-threshold) "/' orch.txt"
                    " && "
                    (namestring *executable-path*) " '" (namestring (filename sound)) "' orch.txt"
                    " && "
                    "mv connection.wav ../" output-basename ".wav" 
                    " && " 
                    "mv connection.txt ../" output-basename ".txt" 
                    " && " 
                    " cd .. && rm -rf " (namestring tmp-dir)
          ))

      (let* ((lines (get-file (tmpfile (string+ output-basename ".txt"))))
             (orch-output (parse-orchidea-output lines)))
        
        (values 
          (tmpfile (string+ output-basename ".wav")) 
          (case output-format
            (:struct (list orch-output)) ;; a list so we can instantiate it in a patch
            (:mf-info (orch-output->mf-info orch-output))
            (:chord-seq (orch-output->chord-seq orch-output))
            (:multi-seq (orch-output->multi-seq orch-output))
            (:poly (orch-output->poly orch-output quantizer)))))))


(defun derive-sound-path-from-db-file ()
  (let ((root (first (lw::split-sequence (list #\.) (pathname-name *db-file*)))))
    (make-pathname :directory (append (pathname-directory *db-file*) (list root)))))