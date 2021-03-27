(in-package om)

(defun list-join (lis delimiter) 
  (format nil (string+ "~{~A~^" delimiter "~}") lis))

(defun escape-slashes (str) 
  (let ((new-char-list '()))
    (coerce (mapcan #'(lambda (char)                
                        (if (eql char #\/) (list #\\ #\/) (list char)))
                    (coerce str 'list))
            'string)))

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))