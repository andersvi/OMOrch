;;;
;;; Time-stamp: <2023-11-07 13:13:44 andersvi>
;;; 

(in-package :om)

(defun lib-src-file (name)
  (make-pathname :directory (append (pathname-directory *load-pathname*) (list "src")) 
                 :name name))

(let* ((srcdir (if *load-pathname*
		   (append (pathname-directory *load-pathname*) '("src"))
		   '(:relative "src")			    ;while developing/debugging
		   ))
       (OMOrch-files '("package"
		       "classes"
		       "globals"
		       "utils"
		       "parser"
		       "config"
		       "draw-extras"
		       "score"
		       "main"
		       "omorch-prefs"
		       )))
  (mapc #'(lambda (f)
	    (compile&load (make-pathname :directory srcdir :name f)))
	OMOrch-files))



(import '(

	  OMOrch::*orch-path-to-orchestrate*
	  OMOrch::find-orchestrate-path
	  OMOrch::orch-set-path-to-orchestrate
	  OMOrch::orchestrate?



	  OMOrch::*orch-sol-db-file*
	  OMOrch::*orch-sol-sound-directory*
	  OMOrch::orch-set-db-file-and-sound-path

	  OMOrch::*orch-default-config-path*
	  OMOrch::*orch-run-config-file*
	  OMOrch::*orch-overwrite-previous-run*
	  OMOrch::*orch-onset-roundoff-threshold-ms*


	  OMOrch::*orch-default-ensemble*
	  OMOrch::*orch-draw-extras*
	  OMOrch::*orch-extras-list*
	  OMOrch::*orch-extras-assoc-list*

	  OMOrch::orchestrate

	  OMOrch::orchestration
	  OMOrch::target-sound
	  OMOrch::read-orchestration-file

	  OMOrch::orch-output

	  OMOrch::export_solutions
	  OMOrch::orch-solution
	  OMOrch::orch-sound

	  OMOrch::parse-orchidea-output

	  OMOrch::orch-configuration
	  OMOrch::orch-config
	  OMOrch::hysteresis

	  OMOrch::parse-instruments-from-config
	  OMOrch::inst+voice-to-string

	  OMOrch::orch-tmp-config-file
	  OMOrch::tmp-config-file


	  OMOrch::orch-note
	  OMOrch::make-orch-note
	  OMOrch::orch-notes
	  OMOrch::orch-chord

	  OMOrch::orch-output->chord-seq
	  OMOrch::orchestration->multi-seq

	  OMOrch::orch-note-string-2-om-note-string

	  OMOrch::orch-segments
	  OMOrch::orch-extra
	  OMOrch::orch-segment

	  OMOrch::add-voice-nr-to-instruments
	  OMOrch::orch-command-line
	  ))



(defvar *OMOrch-version* '1.1.1)
(defvar *OMOrch-date* '2023-11-28)

; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------

(om::fill-library
 '((nil nil
    (orchestration orch-output orch-configuration)
    (orchestrate))
   ("Utils" nil (orch-note orch-segment orch-solution) (parse-instruments-from-config) ))
 (find-library "OMOrch"))





(set-lib-release *OMOrch-version* (find-library "OMOrch"))

(print
 (format nil "
;; ==============================================================
;;  OMOrch - an OM library using Cella's Orchidea orchestration tool
;;  Version:	~A
;;  Date:	~A
;;  Sources:    https://github.com/andersvi/OMOrch
;;  Authors:	Anders Vinjar
;; ==============================================================
"
	 *OMOrch-version*
	 *OMOrch-date*))

;; generate html doc:
;; (om::gen-lib-reference (exist-lib-p "OMOrch"))
