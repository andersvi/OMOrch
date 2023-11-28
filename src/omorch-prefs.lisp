;;; OMOrch
;;; 
;;; Yet Another OM orch** -lib!  (YAOMO!)
;;; 
;;; provide preferences pane with some defaults:


(in-package :omorch)


(defmethod get-external-name ((module (eql 'omorch))) "OMOrch")
(defmethod get-external-icon ((module (eql 'omorch)))
  (and (find-library "OMOrch") (list 451 (find-library "OMOrch"))))


(defmethod get-external-module-path ((module (eql 'omorch)) modulepref)
  (om::get-pref modulepref :omorch))

(defmethod set-external-module-path ((module (eql 'omorch)) modulepref path)
  (om::set-pref modulepref :omorch path))

(defmethod save-external-prefs ((module (eql 'omorch))) 
  `(:orchestrate ,(om::om-save-pathname *orch-path-to-orchestrate*)))

(defmethod put-external-preferences ((module (eql 'omorch)) moduleprefs)
  (when (om::get-pref moduleprefs :orch)
    (setf *orch-path-to-orchestrate* (om::find-true-external (om::get-pref moduleprefs :omorch))))
  t)
      

;========================================================================================================================
;         PREFERENCES PANEL MODULE
;=========================================================================================================================

;; (progn
;;   (om::set-pref (om::find-pref-module :omorch) :omorch-default-ensemble *orch-default-ensemble*)
;;   (om::set-pref (om::find-pref-module :omorch) :orch-default-config-path *orch-default-config-path*)
;;   (om::set-pref (om::find-pref-module :omorch) :orch-sol-db-file *orch-sol-db-file*)
;;   (om::set-pref (om::find-pref-module :omorch) :omorch-default-extras *orch-extras-assoc-list*))
;;   (om::set-pref (om::find-pref-module :omorch) :omorch-overwrite-previous-runs *orch-overwrite-previous-run*)



(defmethod get-def-vals ((iconID (eql :omorch)))
   (list 
    :orch-orchestrate-program *orch-path-to-orchestrate*
    :orch-default-config-path *orch-default-config-path*
    :orch-sol-db-file *orch-sol-db-file*
    :omorch-default-ensemble *orch-default-ensemble*
    :omorch-default-extras *orch-extras-assoc-list*
    :omorch-overwrite-previous-runs *orch-overwrite-previous-run*
    ))


(defmethod put-preferences ((iconID (eql :omorch)))
  (let* ((modulepref (om::find-pref-module iconID)))
    (setf *orch-path-to-orchestrate* (om::get-pref modulepref :orch-orchestrate-program))
    (setf *orch-sol-db-file* (om::get-pref modulepref :orch-sol-db-file))
    (setf *orch-default-ensemble* (om::get-pref modulepref :omorch-default-ensemble))
    (setf *orch-extras-assoc-list* (om::get-pref modulepref :omorch-default-extras))
    (setf *orch-overwrite-previous-run* (om::get-pref modulepref :omorch-overwrite-previous-runs))))

(defmethod save-pref-module ((iconID (eql :omorch)) item)
   (list iconID `(list 
		  :orch-orchestrate-program ,*orch-path-to-orchestrate*
		  :orch-sol-db-file ,*orch-sol-db-file*
		  :omorch-default-ensemble ,*orch-default-ensemble*
		  :omorch-default-extras ',*orch-extras-assoc-list*
		  :omorch-overwrite-previous-runs ,*orch-overwrite-previous-run*
		  )
	 om::*om-version*))

(defclass orch-extras-view (om-view) 
  ((object :initform nil :initarg :object :accessor object)))

(defmethod om-component-border ((self orch-extras-view)) :line)

(defmethod initialize-instance :after ((self orch-extras-view) &rest initargs)
  (let ((textdims (om::om-make-point 150 24)))
    (om::om-add-subviews self
                     (om::om-make-dialog-item 'om-check-box (om::om-make-point 10 5) textdims "Styles" 
                                          :checked-p (orch-is-extra-active? 'styles)
                                          :di-action (om::om-dialog-item-act item 
						       (progn
							 (orch-set-extra-active! 'styles (om::om-checked-p item))
							 (om::set-pref (object self) :omorch-default-extras *orch-extras-assoc-list*))))
		     
		     
    
		     (om::om-make-dialog-item 'om-check-box (om::om-make-point 10 25) textdims "Dynamics" 
					  :checked-p (orch-is-extra-active? 'dynamics)
					  :di-action (om::om-dialog-item-act item 
						       (progn
							 (orch-set-extra-active! 'dynamics (om::om-checked-p item))
							 (om::set-pref (object self) :omorch-default-extras *orch-extras-assoc-list*)))))))

(defmethod make-new-pref-scroll  ((num (eql :omorch)) modulepref)
  (let ((thescroll (om::om-make-view 'preference-pane
                                 :pref-id num
                                 :name "OMOrch"
                                 :size (om::get-pref-scroll-size)
                                 :position (om::om-make-point 0 0)
                                 :font om::*controls-font* 
                                 :bg-color om::*om-light-gray-color*
                                 ))
        (l1 50)
	(l2 (round (om::om-point-h (om::get-pref-scroll-size)) 1.3))
	(l3 (- (om::om-point-h (om::get-pref-scroll-size)) 60))
        (posy 0)
	(dy1 20)
	(dy2 30)
        outtxt)
    
    
    (om::om-add-subviews thescroll 

		     (om::om-make-dialog-item 'om-static-text (om::om-make-point l1 (incf posy 20)) (om::om-make-point 200 30) "OMOrch prefs"
                                          :font om::*om-default-font2b*)

		     (om::om-make-dialog-item 'om-static-text (om::om-make-point (+ l1 100) posy) (om::om-make-point l3 30)
					  "Check docs included w. the CLI version of Orchidea"
                                          :font om::*controls-font*)

                     
		     ;; orchestrate executable:

		     (om::om-make-dialog-item 'om-static-text  (om::om-make-point l1 (incf posy dy2)) (om::om-make-point (- l2 50) 30)
					  "Path to Orchidea's 'orchestrate' executable:"
                                          :font om::*controls-font*)
                     
                     (om::om-make-view 'om-icon-button
                                   :icon1 "folder" :icon2 "folder-pushed"
                                   :position (om::om-make-point l2 posy)
				   :size (om::om-make-point 26 25)
                                   :action (om::om-dialog-item-act item
                                             (declare (ignore item))
                                             (let ((file (om::om-choose-file-dialog :prompt "choose orchestrate executable")))
                                               (when file
                                                 (om::om-set-dialog-item-text outtxt (om::om-namestring file))
                                                 (om::set-pref modulepref :orch-orchestrate-program file)))))

                     
		     (setq outtxt (om::om-make-dialog-item 'om-static-text  (om::om-make-point (+ l1 20) (incf posy dy1)) (om::om-make-point l3 45)
                                                       (om::om-namestring (om::get-pref modulepref :orch-orchestrate-program))
                                                       :font om::*om-default-font1*))
		     
		     
		     ;; db-file, and sound-file

		     (om::om-make-dialog-item 'om-static-text  (om::om-make-point l1 (incf posy dy1)) (om::om-make-point (- l2 50) 30)
					  (format nil "Path to your SOL's ~S file (db-sound folder must be adjacent):" "XXX.spectrum.db")
                                          :font om::*controls-font*)
                     
                     (om::om-make-view 'om-icon-button
                                   :icon1 "folder" :icon2 "folder-pushed"
                                   :position (om::om-make-point l2 posy)
				   :size (om::om-make-point 26 25)
                                   :action (om::om-dialog-item-act item
                                             (declare (ignore item))
                                             (let ((file (om::om-choose-file-dialog :prompt "select a SOL xxx.spectrum.db-file:")))
                                               (when file
                                                 (progn
						   (orch-set-db-file-and-sound-path (om::om-namestring file))
						   (om::om-set-dialog-item-text outtxt (om::om-namestring file)))
						 (om::set-pref modulepref :orch-sol-db-file file)
						 
						 ))))

                     
		     (setq outtxt (om::om-make-dialog-item 'om-static-text  (om::om-make-point (+ l1 20) (incf posy dy1)) (om::om-make-point l3 45)
                                                       (om::om-namestring (om::get-pref modulepref :orch-sol-db-file))
                                                       :font om::*om-default-font1*))



		     		     ;; default config

		     (om::om-make-dialog-item 'om-static-text  (om::om-make-point l1 (incf posy (* 1 dy1))) (om::om-make-point (- l2 50) 30)
					  "Path to default config file:"
                                          :font om::*controls-font*)
                     
                     (om::om-make-view 'om-icon-button
                                   :icon1 "folder" :icon2 "folder-pushed"
                                   :position (om::om-make-point l2 posy)
				   :size (om::om-make-point 26 25)
                                   :action (om::om-dialog-item-act item
                                             (declare (ignore item))
                                             (let ((file (om::om-choose-file-dialog :prompt "select a valid config file:")))
                                               (when file
                                                 (progn
						   (orch-set-default-config  (om::om-namestring file))
						   (om::om-set-dialog-item-text outtxt (om::om-namestring file)))
						 (om::set-pref modulepref :orch-default-config-path file)
						 ))))

                     
		     (setq outtxt (om::om-make-dialog-item 'om-static-text  (om::om-make-point (+ l1 20) (incf posy dy1)) (om::om-make-point l3 45)
                                                       (om::om-namestring (om::get-pref modulepref :orch-default-config-path))
                                                       :font om::*om-default-font1*))



		     ;; DEFAULT ORCHESTRATION PARAMS

		     (om::om-make-dialog-item 'om-static-text (om::om-make-point l1 (incf posy 50)) (om::om-make-point 200 30) "Default OMOrch parameters:"
                                          :font om::*om-default-font2b*)


		     (om::om-make-dialog-item 'om-static-text  (om::om-make-point l1 (incf posy dy1)) (om::om-make-point l3 200)
					  "Default orchestra, valid choices depends on the db in use:"
                                          :font om::*controls-font*)

                     
                     (om::om-make-dialog-item 'om-editable-text (om::om-make-point (+ l1 40) posy)
                                          (om::om-make-point (- l3 l1 40) 100)
                                          (om::get-pref modulepref :omorch-default-ensemble)
                                          :after-action (om::om-dialog-item-act item 
                                                          (om::set-pref modulepref :omorch-default-ensemble (om::om-dialog-item-text item)))
                                          :font om::*controls-font*
                                          )



		     ;; Default extras to draw in editors
		     (om::om-make-dialog-item 'om-static-text  (om::om-make-point l1 (incf posy 80)) (om::om-make-point (- l2 50) 30)
					  "Metadata to draw as 'extras' in editors :"
                                          :font om::*controls-font*)

		     (om::om-make-view 'orch-extras-view ;; :background om::*om-light-gray-color*
						     :position (om::om-make-point (- l2 150) posy)
						     :size (om::om-make-point 150 60) 
						     :object modulepref)

		     ;; should i keep previous output in out-file/omorch-[timetag]?
		     (om::om-make-dialog-item 'om-static-text  (om::om-make-point l1 (incf posy 70)) (om::om-make-point (- l2 50) 30)
					  "Overwrite output from previous calls to orchestrate?:"
                                          :font om::*controls-font*)

                     (om::om-make-dialog-item 'om-check-box (om::om-make-point (- l2 150) posy) (om::om-make-point 20 20) ""
                                          :font om::*controls-font*
                                          :checked-p (om::get-pref modulepref :omorch-overwrite-previous-runs)
                                          :di-action (om::om-dialog-item-act item 
                                                       (om::set-pref modulepref :omorch-overwrite-previous-runs (om::om-checked-p item))))
		     )
    ;; (setf posy 0)
    ;; (om::om-add-subviews thescroll)
    thescroll))




(defun add-omorch-preferences ()
  (om::push-pref-module (list :omorch (get-def-vals :omorch))))

(add-omorch-preferences)

;set and load tab in om preferences panel 
(om::pushr :omorch om::*pref-order*)
