(let ((n '(("tn" "erode")("tn" "kvp")("tn" "pudur")("tn" "metup")("up" "noida")("up" "zilla")("kl" "cochi")("kl" "erna")("kl" "kuda")))
      (ht (make-hash-table :test 'equal)))
  (loop for (x y) in n
	:do (push y (gethash x ht)))
  (maphash #'(lambda (k v) (format t "~a,~a~%" k v)) ht))
