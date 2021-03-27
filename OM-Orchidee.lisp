;--------------------------------------------------
;
; OM-ORCHESTRATION
; J. Bresson - IRCAM
; Updated 15/10/2007
;
;--------------------------------------------------


(in-package :om)

(require-library "OMChroma")

;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

(setf *soinitfile* *load-pathname*)

(defun lib-src-file (name)
  (make-pathname :directory (append (pathname-directory *soinitfile*) (list "sources")) 
                 :name name))

(defvar *SO-files* nil)
(setf *SO-files* '("orchidee-client"
                   "database"
                   "orc-preferences"
                   "cible"
                   "vps-operators"
                   "orchestre"
                   "contraintes"
                   "solver"
                   "solutions"
                   ))

(defvar *orcobject-pict* nil)
(setf *orcobject-pict* (om-load-and-store-picture 
                        "orceditorpict" 'full 
                        (make-pathname :directory (append (pathname-directory *soinitfile*)
                                                          (list "resources" "pict")))))

;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(mapc #'(lambda (filename) 
          (compile&load (namestring (lib-src-file filename))))
      *SO-files*)


;--------------------------------------------------
;filling packages
;--------------------------------------------------

(unless (fboundp 'om::set-lib-release) (defmethod om::set-lib-release (version &optional lib) nil))


;;; 1.4 = minor fix compat w/ OM-Tristan (mixer slot in solution)
(om::set-lib-release 1.4)


(om::fill-library '((nil nil (soundtarget orchestra orc-solutionset orc-solution) 
                         (orchestrate submit-orchestra submit-target) nil)
                    ;("Constraints" nil nil (orc-size num-notes) nil)
                    ("Server utils" nil nil (run-orchidee quit-orchidee check-orchidee orchidee-version reset-orchidee) nil)
                    ("Spectrum Operators" nil nil (stretch-vps-freqs filter-vps-freqs set-amplitudes) nil))
                  (find-library "OM-Orchidee"))


; (gen-lib-reference "OM-Orchidee")

(print "
;;;===================================================
;;; OM-Orchidée (c) IRCAM 2010-2011
;;; computer-aided orchestration client for Orchidée
;;;===================================================
")

;;;======================
;;; RELEASE NOTES
;;;======================
;;; 1.1
;;; - OSC default port setup adapted to Orchidee 1.0 (forum)
;;; - Port binding messages and server message display update
;;; - Database update system
;;; - Tutorial patches updated
;;; - Amplitude table for target synthesis
;;;
;;; 1.2
;;; Fix compatibility with OM 6.6

