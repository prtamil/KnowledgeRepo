(make-dispatch-macro-character #\%)

(defmacro defreader (left right parms &body body)
  `(ddfn ,left ,right #'(lambda ,parms ,@body)))

(let ((rpar (get-macro-character #\) )))
  (defun ddfn (left right fn)
    (set-macro-character right rpar)
    (set-dispatch-macro-character #\% left
                                  #'(lambda (stream char1 char2)
                                      (declare (ignore char1 char2))
                                      (apply fn
                                             (read-delimited-list right stream t))))))

(defreader #\( #\) (hash op &rest stuff)
  (let ((var-hash (gensym)))
    (cond ((or (eq op '->) (eq op '-->))
           ;; "GET" operations
           (cond ((= 1 (length stuff))
                  (setq stuff (car stuff))
                  `(gethash ,stuff ,hash))
                 (t
                  `(let ((,var-hash ,hash))
                     (values ,@(mapcar #'(lambda(key)
                                           `(gethash ,key ,var-hash))
                                       stuff))))))
          ((or (eq op '<-) (eq op '<--))
           ;; "SET" operations
           (cond ((= 3 (length stuff))
                  `(let ((,var-hash ,hash))
                     (setf (gethash ,(first stuff) ,var-hash) ,(third stuff))
                     ,var-hash))
                 (t
                  (setq stuff
                        (loop :for (key nil value) :on stuff :by #'cdddr
                           :collecting (list key value)))
                  `(let ((,var-hash ,hash))
                     ,@(loop :for c :in stuff
                          :collecting
                          `(setf (gethash ,(first c) ,var-hash) ,(second c)))
                     ,var-hash)))))))
