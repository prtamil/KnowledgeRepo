Hashtable to count LetterFreq
==============================
The above code is quite specific to ASCII characters. If you want to do the same for any possible character, you can use a hash-table.

(defun letter-freq (file)
  (with-open-file (stream file)
    (let ((str (make-string (file-length stream)))
          (ht (make-hash-table)))
      (read-sequence str stream)
      (loop :for char :across str :do
        (incf (gethash char ht 0)))
      (maphash (lambda (k v)
                 (format t "~@C: ~D~%" k v))
               ht))))

~@C format directive prints the charaсter as if by prin1.
