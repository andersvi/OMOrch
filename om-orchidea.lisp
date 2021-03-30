
(in-package :om)

(defparameter *src-files* '(
  "utils"
  "parser"
  "main"
  ))

(defun lib-src-file (name)
  (make-pathname :directory (append (pathname-directory *load-pathname*) (list "src")) 
                 :name name))

(defvar *executable-path* 
    (make-pathname :directory (append (pathname-directory *load-pathname*) (list "bin")) 
                   :name "orchestrate"))

(mapc #'(lambda (filename) 
          (compile&load (namestring (lib-src-file filename))))
      *src-files*)

(unless (fboundp 'om::set-lib-release) (defmethod om::set-lib-release (version &optional lib) nil))
(om::set-lib-release 0.1)


;(om::fill-library '((nil nil (soundtarget orchestra orc-solutionset orc-solution) 
;                         (orchestrate submit-orchestra submit-target) nil)
;                    ;("Constraints" nil nil (orc-size num-notes) nil)
;                    ("Server utils" nil nil (run-orchidee quit-orchidee check-orchidee orchidee-version reset-orchidee) nil)
;                    ("Spectrum Operators" nil nil (stretch-vps-freqs filter-vps-freqs set-amplitudes) nil))
;                  (find-library "OM-Orchidee"))

(print "
;;;===================================================
;;; OM-ORCHIDEA WIP
;;; Geof Holbrook
;;;===================================================
")

