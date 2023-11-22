;;; OMOrch
;;; 
;;; Yet Another OM orch** -lib!  (YAOMO!)
;;; 
;;; provide preferences pane with some defaults:


(in-package :om)


(defmethod get-external-name ((module (eql 'omorch))) "OMOrch")
(defmethod get-external-icon ((module (eql 'omorch)))
  (and (find-library "OMOrch") (list 451 (find-library "OMOrch"))))


(defmethod get-external-module-path ((module (eql 'omorch)) modulepref)
  (get-pref modulepref :omorch))

(defmethod set-external-module-path ((module (eql 'omorch)) modulepref path)
  (set-pref modulepref :omorch path))

(defun set-orchestrate-path ()
  (set-pref (find-pref-module :externals) :omorch (pathname (find-orchestrate-path))))

(defmethod save-external-prefs ((module (eql 'omorch))) 
  `(:orchestrate ,(om-save-pathname *orchidea-executable-path*)))

(defmethod put-external-preferences ((module (eql 'omorch)) moduleprefs)
  (when (get-pref moduleprefs :orch)
    (setf *orchidea-executable-path* (find-true-external (get-pref moduleprefs :omorch))))
  t)
      

;========================================================================================================================
;         PREFERENCES PANEL MODULE
;=========================================================================================================================

;; *orchidea-config-template-path*
;; *orchidea-db-file*
;; *orchidea-sound-path*



;; (progn
;;   (set-pref (find-pref-module :omorch) :orchidea-sound-path (pathname (derive-sound-path-from-db-file)))
;;   (set-pref (find-pref-module :omorch) :orchidea-config-template-path *orchidea-config-template-path*)
;;   (set-pref (find-pref-module :omorch) :omorch-default-ensemble *orchidea-default-ensemble*)
;;   (set-pref (find-pref-module :omorch) :orchidea-default-config-path *orchidea-default-config-path*)
;;   (set-pref (find-pref-module :omorch) :orchidea-db-file *orchidea-db-file*)
;;   (set-pref (find-pref-module :omorch) :omorch-default-extras *orch-extras-assoc-list*))
;;   (set-pref (find-pref-module :omorch) :omorch-overwrite-previous-runs *orch-overwrite-previous-run*)



(defmethod get-def-vals ((iconID (eql :omorch)))
   (list 
    :orchidea-orchestrate-executable *orchidea-executable-path*
    :orchidea-default-config-template-path *orchidea-default-config-template-path*
    :orchidea-default-config-path *orchidea-default-config-path*
    :orchidea-db-file *orchidea-db-file*
    :orchidea-sound-path *orchidea-sound-path*
    :omorch-default-ensemble *orchidea-default-ensemble*
    :omorch-default-extras *orch-extras-assoc-list*
    :omorch-overwrite-previous-runs *orch-overwrite-previous-run*
    ))


(defmethod put-preferences ((iconID (eql :omorch)))
  (let* ((modulepref (find-pref-module iconID)))
    (setf *orchidea-executable-path* (get-pref modulepref :orchidea-orchestrate-executable))
    (setf *orchidea-default-config-template-path* (get-pref modulepref :orchidea-default-config-template-path))
    (setf *orchidea-db-file* (get-pref modulepref :orchidea-db-file))
    (setf *orchidea-sound-path* (get-pref modulepref :orchidea-sound-path))
    (setf *orchidea-default-ensemble* (get-pref modulepref :omorch-default-ensemble))
    (setf *orch-extras-assoc-list* (get-pref modulepref :omorch-default-extras))
    (setf *orch-overwrite-previous-run* (get-pref modulepref :omorch-overwrite-previous-runs))))

(defmethod save-pref-module ((iconID (eql :omorch)) item)
   (list iconID `(list 
		  :orchidea-orchestrate-executable ,*orchidea-executable-path*
		  :orchidea-default-config-template-path ,*orchidea-default-config-template-path*
		  :orchidea-db-file ,*orchidea-db-file*
		  :orchidea-sound-path ,*orchidea-sound-path*
		  :omorch-default-ensemble ,*orchidea-default-ensemble*
		  :omorch-default-extras ',*orch-extras-assoc-list*
		  :omorch-overwrite-previous-runs ,*orch-overwrite-previous-run*
		  )
	 *om-version*))

(defclass orch-extras-view (om-view) 
  ((object :initform nil :initarg :object :accessor object)))

(defmethod om-component-border ((self orch-extras-view)) :line)

(defmethod initialize-instance :after ((self orch-extras-view) &rest initargs)
  (let ((textdims (om-make-point 150 24)))
    (om-add-subviews self
                     (om-make-dialog-item 'om-check-box (om-make-point 10 5) textdims "Styles" 
                                          :checked-p (orch-is-extra-active? 'styles)
                                          :di-action (om-dialog-item-act item 
						       (progn
							 (orch-set-extra-active! 'styles (om-checked-p item))
							 (set-pref (object self) :omorch-default-extras *orch-extras-assoc-list*))))
		     
		     
    
		     (om-make-dialog-item 'om-check-box (om-make-point 10 25) textdims "Dynamics" 
					  :checked-p (orch-is-extra-active? 'dynamics)
					  :di-action (om-dialog-item-act item 
						       (progn
							 (orch-set-extra-active! 'dynamics (om-checked-p item))
							 (set-pref (object self) :omorch-default-extras *orch-extras-assoc-list*)))))))

(defmethod make-new-pref-scroll  ((num (eql :omorch)) modulepref)
  (let ((thescroll (om-make-view 'preference-pane
                                 :pref-id num
                                 :name "OMOrch"
                                 :size (get-pref-scroll-size)
                                 :position (om-make-point 0 0)
                                 :font *controls-font* 
                                 :bg-color *om-light-gray-color*
                                 ))
        (l1 50)
	(l2 (round (om-point-h (get-pref-scroll-size)) 1.3))
	(l3 (- (om-point-h (get-pref-scroll-size)) 60))
        (posy 0)
	(dy1 20)
	(dy2 30)
        outtxt)
    
    
    (om-add-subviews thescroll 

		     (om-make-dialog-item 'om-static-text (om-make-point l1 (incf posy 20)) (om-make-point 200 30) "OMOrch prefs"
                                          :font *om-default-font2b*)

		     (om-make-dialog-item 'om-static-text (om-make-point (+ l1 100) posy) (om-make-point l3 30)
					  "Check docs included w. the CLI version of Orchidea"
                                          :font *controls-font*)

                     
		     ;; orchestrate executable:

		     (om-make-dialog-item 'om-static-text  (om-make-point l1 (incf posy dy2)) (om-make-point (- l2 50) 30)
					  "Path to Orchidea's 'orchestrate' executable:"
                                          :font *controls-font*)
                     
                     (om-make-view 'om-icon-button
                                   :icon1 "folder" :icon2 "folder-pushed"
                                   :position (om-make-point l2 posy)
				   :size (om-make-point 26 25)
                                   :action (om-dialog-item-act item
                                             (declare (ignore item))
                                             (let ((file (om-choose-file-dialog :prompt "choose orchestrate executable")))
                                               (when file
                                                 (om-set-dialog-item-text outtxt (om-namestring file))
                                                 (set-pref modulepref :orchidea-orchestrate-executable file)))))

                     
		     (setq outtxt (om-make-dialog-item 'om-static-text  (om-make-point (+ l1 20) (incf posy dy1)) (om-make-point l3 45)
                                                       (om-namestring (get-pref modulepref :orchidea-orchestrate-executable))
                                                       :font *om-default-font1*))
		     
		     
		     ;; db-file, and sound-file

		     (om-make-dialog-item 'om-static-text  (om-make-point l1 (incf posy dy1)) (om-make-point (- l2 50) 30)
					  (format nil "Path to orchidea ~S file (database sound folder must be adjacent):" "XXX.spectrum.db")
                                          :font *controls-font*)
                     
                     (om-make-view 'om-icon-button
                                   :icon1 "folder" :icon2 "folder-pushed"
                                   :position (om-make-point l2 posy)
				   :size (om-make-point 26 25)
                                   :action (om-dialog-item-act item
                                             (declare (ignore item))
                                             (let ((file (om-choose-file-dialog :prompt "select a SOL xxx.spectrum.db-file:")))
                                               (when file
                                                 (progn
						   (orchidea-set-db-file-and-sound-path (om-namestring file))
						   (om-set-dialog-item-text outtxt (om-namestring file)))
						 (set-pref modulepref :orchidea-db-file file)
						 (set-pref modulepref :orchidea-sound-path (derive-sound-path-from-db-file file))
						 ))))

                     
		     (setq outtxt (om-make-dialog-item 'om-static-text  (om-make-point (+ l1 20) (incf posy dy1)) (om-make-point l3 45)
                                                       (om-namestring (get-pref modulepref :orchidea-db-file))
                                                       :font *om-default-font1*))




		     ;; default orchestrate config-template

		     (om-make-dialog-item 'om-static-text  (om-make-point l1 (incf posy (* 1 dy1))) (om-make-point (- l2 50) 30)
					  "Path to default orchidea config template:"
                                          :font *controls-font*)
                     
                     (om-make-view 'om-icon-button
                                   :icon1 "folder" :icon2 "folder-pushed"
                                   :position (om-make-point l2 posy)
				   :size (om-make-point 26 25)
                                   :action (om-dialog-item-act item
                                             (declare (ignore item))
                                             (let ((file (om-choose-file-dialog :prompt "select an orchidea config template:")))
                                               (when file
                                                 (progn
						   (orchidea-set-config-template (om-namestring file))
						   (om-set-dialog-item-text outtxt (om-namestring file)))
						 (set-pref modulepref :orchidea-default-config-template-path file)
						 ))))

                     
		     (setq outtxt (om-make-dialog-item 'om-static-text  (om-make-point (+ l1 20) (incf posy dy1)) (om-make-point l3 45)
                                                       (om-namestring (get-pref modulepref :orchidea-default-config-template-path))
                                                       :font *om-default-font1*))

		     		     ;; default config

		     (om-make-dialog-item 'om-static-text  (om-make-point l1 (incf posy (* 1 dy1))) (om-make-point (- l2 50) 30)
					  "Path to default config file:"
                                          :font *controls-font*)
                     
                     (om-make-view 'om-icon-button
                                   :icon1 "folder" :icon2 "folder-pushed"
                                   :position (om-make-point l2 posy)
				   :size (om-make-point 26 25)
                                   :action (om-dialog-item-act item
                                             (declare (ignore item))
                                             (let ((file (om-choose-file-dialog :prompt "select a valid config file:")))
                                               (when file
                                                 (progn
						   (orchidea-set-default-config  (om-namestring file))
						   (om-set-dialog-item-text outtxt (om-namestring file)))
						 (set-pref modulepref :orchidea-default-config-path file)
						 ))))

                     
		     (setq outtxt (om-make-dialog-item 'om-static-text  (om-make-point (+ l1 20) (incf posy dy1)) (om-make-point l3 45)
                                                       (om-namestring (get-pref modulepref :orchidea-default-config-path))
                                                       :font *om-default-font1*))



		     ;; DEFAULT ORCHESTRATION PARAMS

		     (om-make-dialog-item 'om-static-text (om-make-point l1 (incf posy 50)) (om-make-point 200 30) "Default OMOrch parameters:"
                                          :font *om-default-font2b*)


		     (om-make-dialog-item 'om-static-text  (om-make-point l1 (incf posy dy1)) (om-make-point l3 200)
					  "Default orchestra, valid choices depends on the db in use:"
                                          :font *controls-font*)

                     
                     (om-make-dialog-item 'om-editable-text (om-make-point (+ l1 40) posy)
                                          (om-make-point (- l3 l1 40) 100)
                                          (get-pref modulepref :omorch-default-ensemble)
                                          :after-action (om-dialog-item-act item 
                                                          (set-pref modulepref :omorch-default-ensemble (om-dialog-item-text item)))
                                          :font *controls-font*
                                          )



		     ;; Default extras to draw in editors
		     (om-make-dialog-item 'om-static-text  (om-make-point l1 (incf posy 80)) (om-make-point (- l2 50) 30)
					  "Metadata to draw as 'extras' in editors :"
                                          :font *controls-font*)

		     (om-make-view 'orch-extras-view ;; :background *om-light-gray-color*
						     :position (om-make-point (- l2 150) posy)
						     :size (om-make-point 150 60) 
						     :object modulepref)

		     ;; should i keep previous output in out-file/omorch-[timetag]?
		     (om-make-dialog-item 'om-static-text  (om-make-point l1 (incf posy 70)) (om-make-point (- l2 50) 30)
					  "Overwrite output from previous calls to orchestrate?:"
                                          :font *controls-font*)

                     (om-make-dialog-item 'om-check-box (om-make-point (- l2 150) posy) (om-make-point 20 20) ""
                                          :font *controls-font*
                                          :checked-p (get-pref modulepref :omorch-overwrite-previous-runs)
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :omorch-overwrite-previous-runs (om-checked-p item))))
		     )
    ;; (setf posy 0)
    ;; (om-add-subviews thescroll)
    thescroll))




(defun add-omorch-preferences ()
  (push-pref-module (list :omorch (get-def-vals :omorch))))

(add-omorch-preferences)

;set and load tab in om preferences panel 
(pushr :omorch *pref-order*)
