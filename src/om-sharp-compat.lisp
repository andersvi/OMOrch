;;; loaded for om-sharp only ;;;;;;;

(defparameter *om-tmpfiles-folder* (get-pref-value :files :tmp-file))

(defmethod filename ((sound sound)) (file-pathname sound))