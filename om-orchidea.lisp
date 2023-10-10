(in-package :om)

(let* ((srcdir (if *load-pathname*
		   (append (pathname-directory *load-pathname*) '("src"))
		   '(:relative "src")			    ;while developing/debugging
		   ))
       (om-orchidea-files '("package"
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
	om-orchidea-files))


(defparameter omorch::*om-orchidea-version* '0.2)
(defparameter omorch::*om-orchidea-date* '2023-10-10)

(let ((funcs '(orchestrate))
      (p (find-library "om-orchidea")))
  (AddGenFun2Pack funcs p))

(set-lib-release omorch::*om-orchidea-version* (find-library "om-orchidea"))

(print
 (format nil "
;; ==============================================================
;; OM Orchidea Library
;; Fork of Geof Holbrooks om-orchidea library
;;  Version:	~A
;;  Date:	~A
;;  Sources:    https://github.com/andersvi/om-orchidea
;;  Authors:	Anders Vinjar
;; ==============================================================
"
	 omorch::*om-orchidea-version*
	 omorch::*om-orchidea-date*))

;; generate html doc:
;; (om::gen-lib-reference (exist-lib-p "om-orchidea"))

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
