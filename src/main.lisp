(in-package :om)



(defvar *orchidea-executable-path*
    (make-pathname :directory (append (butlast (pathname-directory *load-pathname*)) (list "bin")) 
                   :name "orchestrate"))

(defmethod! orchidea-set-executable-path ((path string))
  :initvals '("")
  :icon 451
  :indoc '("path to orchestrate binary")
  :doc "set path to orchestrate binary"
  (setf *orchidea-executable-path* (or path (file-chooser))))

;; (defvar *sound_path* nil)  ; not used anywhere?

(defvar *orchidea-config-template-path*
        (make-pathname :directory (pathname-directory *orchidea-executable-path*)
                   :name "config_template.txt"))

(defmethod! orchidea-set-config-template ((path string))
  :initvals nil
  :icon 451
  :indoc '("path to orchidea config template file")
  :doc "set path to orchidea config template file"
  (setf *orchidea-config-template-path* (or path (file-chooser))))


(defvar *orchidea-db-file* nil)

(defmethod! set-db-file ((path string))
  :initvals nil
  :icon 451
  :indoc '("path to .spectrum.db file")
  :doc "set path to .spectrum.db (database sound folder must be adjacent)"
  (setf *orchidea-db-file* (or path (file-chooser))))

(defparameter *orchidea-default-orchestration* "Fl Fl Ob Ob ClBb ClBb Bn Bn Hn Hn TpC TpC Tbn Tbn BTb Vn Vn Va Va Vc Vc Cb Cb")

;;;;;;;;;;; orchestrate method ;;;;;;;;;;;;;

(defmethod! orchestrate ((sound sound) (orchestration string) (onsets-threshold number) (output-format t) &key quantizer)
  :initvals '(nil *orchidea-default-orchestration* 0.1 :chord-seq)
  :indoc '("source sound object" "instrument abbreviations (space-delimited string)" "onsets threshold (ex. static = 2, dynamic = 0.1)" "score-format")
  :icon 451
  :doc "generate orchestration from source sample and database"
  :numouts 2
  :menuins '((3 (("struct" :struct)
                 ("mf-info" :mf-info)
                 ("chord-seq" :chord-seq)
                 ("multi-seq" :multi-seq)
                 ("poly" :poly))))
                
                 
  (when (null *orchidea-db-file*) (error "db file not set, use set-db-file function"))
  (unless (and (= (sample-rate sound) 44100) (= (sample-size sound) 16)) (error "sound file must be 44.1k, 16 bit"))
  (let ((tmp-dir (make-pathname 
                  :directory (append (pathname-directory *om-tmpfiles-folder*)
                                     (list (string+ "tmp-" (prin1-to-string (om-random 10000000 99999999)))))))
        (db-sound-path (derive-sound-path-from-db-file))
        (output-basename (string+ (pathname-name (filename sound)) "-orch-" (prin1-to-string (om-random 10000000 99999999)))))

    (let ((cmd (string+ 
                "mkdir -p " (namestring tmp-dir)
                " && "
                "cd " (namestring tmp-dir)
                " && "
                "sed 's/__DB_FILE__/" (escape-slashes (namestring *orchidea-db-file*)) "/' " (namestring *orchidea-config-template-path*) " > orch.txt"
                " && "
                "sed -i 's/__SOUND_PATH__/" (escape-slashes (namestring db-sound-path)) "/' orch.txt"
                " && "
                "sed -i 's/__ORCHESTRA__/" orchestration "/' orch.txt"
                " && "
                "sed -i  's/__ONSETS_THRESHOLD__/" (prin1-to-string onsets-threshold) "/' orch.txt"
                " && "
                (namestring *orchidea-executable-path*) " '" (namestring (filename sound)) "' orch.txt"
                " && "
                "mv connection.wav ../" output-basename ".wav" 
                " && " 
                "mv connection.txt ../" output-basename ".txt" 
                " && " 
                "mv orch.txt ../" output-basename ".orch.txt" 
                " && " 
                " cd .. && rm -rf " (namestring tmp-dir)
		)))
      ;; (break)
      ;; (print cmd)
      (om-cmd-line cmd))

    (let* ((lines (get-file (tmpfile (string+ output-basename ".txt"))))
           (orch-output (parse-orchidea-output lines)))
        
      ;; (cerror "her" "der")
      ;; (print (setq linjene lines))
      ;; (print (setq orkut orch-output))
      (values 
       (tmpfile (string+ output-basename ".wav")) 
       (case output-format
         (:struct (list orch-output)) ;; a list so we can instantiate it in a patch
         (:mf-info (orch-output->mf-info orch-output))
         (:chord-seq (orch-output->chord-seq orch-output))
         (:multi-seq (orch-output->multi-seq orch-output))
         (:poly (orch-output->poly orch-output quantizer)))))))


(defun derive-sound-path-from-db-file ()
  (let ((root (first (lw::split-sequence (list #\.) (pathname-name *orchidea-db-file*)))))
    (make-pathname :directory (append (pathname-directory *orchidea-db-file*) (list root)))))
