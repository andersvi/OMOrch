(in-package :omorch)


;;
;; various translations from orch-style syntax into something which
;; fits OM-classes and methods
;; 

(defun orch-note-name-list-until-unvalid (lst)
  "return part of orch-note before potentially invalid part - '+ or '_"
  (subseq lst 0
	  (position-if #'(lambda (c) (member c '(#\_ #\+)))
		       lst)))

(defun orch-note-string-2-om-note-string  (notestring &optional (default "A0"))
  "parse orchidea-type note-string, return valid OM note string."
  ;; orch-added-dynamics: '("ffpp" "fp" "N" "ppff" "ppmfpp")
  (let* ((orch-note-string-list (coerce (string notestring) 'list))
	 (valid-part-of-note-name (orch-note-name-list-until-unvalid orch-note-string-list))
	 (om-note-string-list (substitute-if #\+
					     #'(lambda (c) (or (member c '(#\q #\Q) )))
					     valid-part-of-note-name)))
    
    (if (equalp (car om-note-string-list) #\N)
	default ;; just return default for OMs note-class
	(coerce om-note-string-list 'string))))


;; extend om's standard list w. some potential used by 'orchestrate':

(defvar *orch-dynamics-symbols-list* (append om::*dynamics-symbols-list* 
					     '((:ffpp nil 115)
					       (:fp nil 100)
					       (:N nil 0)
					       (:ppff nil 40)
					       (:ppmfpp nil 60))))

(defun orch-get-vel-from-dyn (dyn-keyword)
  (let ((om::*dynamics-symbols-list*
	  (append om::*dynamics-symbols-list* *orch-dynamics-symbols-list*)))
    (om::get-vel-from-dyn dyn-keyword)))


(defun get-velocity-from-orch-note-dynamic (orch-dyn)
  "support extra dynamics from orch: - ffpp fp N ppff ppmfpp pppppp..."
  (let* (;; (dyn-list (coerce orch-dyn 'list))
	 (dyn-keyword (intern (string-upcase orch-dyn) :keyword)))
    (cond ((equal dyn-keyword :n)
	   (progn (print (string+ "using velocity 0 for: " orch-dyn))
		  0))
	  ((member dyn-keyword *orch-dynamics-symbols-list* :test #'(lambda (a b) (equal a (car b))))
	   (orch-get-vel-from-dyn dyn-keyword))
	  (t
	   (progn (print (string+ "setting fallback velocity = 0, none found for: " orch-dyn))
		  0)))))

(defun parse-orchidea-output (orch-output-struct)
  (let ((orch-output (parse-[-delmited-string-to-list orch-output-struct)))
    (make-orch-output :instruments (car orch-output)
		      :orch-segments (mapcar #'parse-segment (cdr orch-output)))))

(defun parse-segment (segment)
  "segment: (segment 0 334 (solution 1 (note) (note) ...))"
  (make-orch-segment 
   :onset (nth 1 segment)
   :duration (nth 2 segment)
   :solution (parse-solution (nth 3 segment))))

(defun parse-solution (lis)
  (make-orch-solution
   :id (second lis)
   :orch-notes (mapcar #'parse-note (cddr lis))))


(defun parse-note (lis)
  (let ((pitch-name (string (nth 4 lis)))
	(dynamic-string (string (nth 5 lis))))
    (make-orch-note
     :dur (round (nth 1 lis))
     :instrument (string (nth 2 lis))
     :style (nth 3 lis)
     :pitch-name pitch-name
     :midic (n->mc (orch-note-string-2-om-note-string pitch-name))
     :dynamic dynamic-string
     :vel (get-velocity-from-orch-note-dynamic dynamic-string)
     :instance (nth 6 lis)
     :sample-path (string (nth 7 lis))
     :detune (nth 8 lis))))



