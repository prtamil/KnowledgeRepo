(defun Disp-File(fil)
  (let ((in (open fil :if-does-not-exist nil)))
    (when in
      (iter (for line = (read-line in nil))
            (while line)
            (format t "~a~%" line))
      (close in))))
