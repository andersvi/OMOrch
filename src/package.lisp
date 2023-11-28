
(defpackage "OMOrch"
  (:nicknames :omorch)
  (:use :common-lisp :om)
  )

(import '(om::defmethod!
	  om::defclass!
	  om::defmethod*
	  om::defclass*
	  om::inside
	  om::objfromobjs)
	:omorch)


