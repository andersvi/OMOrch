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
       (OMOrch-files '(;; "package"
		       "classes"
		       "globals"
		       "utils"
		       "parser"
		       "draw-extras"
		       "score"
		       "main"
		       "omorch-prefs"
		       )))
  (mapc #'(lambda (f)
	    (compile&load (make-pathname :directory srcdir :name f)))
	OMOrch-files))


(defvar *OMOrch-version* '0.2)
(defvar *OMOrch-date* '2023-10-10)

(let ((funcs '(orchestrate))
      (p (find-library "OMOrch")))
  (AddGenFun2Pack funcs p))

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
