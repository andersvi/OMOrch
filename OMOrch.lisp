(in-package :om)

(let* ((srcdir (if *load-pathname*
		   (append (pathname-directory *load-pathname*) '("src"))
		   '(:relative "src")			    ;while developing/debugging
		   ))
       (OMOrch-files '("package"
			    "classes"
			    "globals"
			    ;; "om-sharp-compat"
			    "utils"
			    "parser"
			    "score"
			    "main"
			    )))
  (mapc #'(lambda (f)
	    (compile&load (make-pathname :directory srcdir :name f)))
	OMOrch-files))


(defparameter omorch::*OMOrch-version* '0.2)
(defparameter omorch::*OMOrch-date* '2023-10-10)

(let ((funcs '(orchestrate))
      (p (find-library "OMOrch")))
  (AddGenFun2Pack funcs p))

(set-lib-release omorch::*OMOrch-version* (find-library "OMOrch"))

(print
 (format nil "
;; ==============================================================
;; OM Orchidea Library
;; Fork of Geof Holbrooks OMOrch library
;;  Version:	~A
;;  Date:	~A
;;  Sources:    https://github.com/andersvi/OMOrch
;;  Authors:	Anders Vinjar
;; ==============================================================
"
	 omorch::*OMOrch-version*
	 omorch::*OMOrch-date*))

;; generate html doc:
;; (om::gen-lib-reference (exist-lib-p "OMOrch"))

(defparameter *src-files* '(
			    "package"
			    "classes"
			    "globals"
			    ;; "om-sharp-compat"
			    "utils"
			    "parser"
			    "score"
			    "main"
			    ))

(defun lib-src-file (name)
  (make-pathname :directory (append (pathname-directory *load-pathname*) (list "src")) 
                 :name name))


#|
(mapc #'(lambda (filename) 
          (compile&load (namestring (lib-src-file filename))))
      *src-files*)

|#

;; (unless (fboundp 'om::set-lib-release) (defmethod om::set-lib-release (version &optional lib) nil))
;; (om::set-lib-release 0.1)
