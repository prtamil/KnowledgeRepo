
(defparameter *l* '(1 2 3 4 5 6 7 8))

;;Python itertools cycle logic
(defun looplst (lst)
  (let ((n 0) (res nil))
    (lambda ()
      (if (> n (1- (length lst)))
	  (setf n 0))
      (setf res (nth n lst))
      (incf n)res)))


