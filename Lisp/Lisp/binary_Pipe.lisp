 7 down vote accepted
	

I got a working answer from Raymond Toy on comp.lang.lisp. His solution was for CMUCL, but it worked with the essentially identical RUN-PROGRAM function on the closely related SBCL, and with minor changes it will work on CCL as well, because CCL's RUN-PROGRAM is basically a clone of the one from CMUCL/SBCL.

The secret, as it were, is to set up the ls process first, and then provide its output stream to the grep process as input, like so:

(defun piping-test2 () 
  (let ((ls-process (run-program "/bin/ls" '() 
                                 :wait nil 
                                 :output :stream))) 
    (unwind-protect 
        (with-open-stream (s (process-output ls-process)) 
          (let ((grep-process (run-program "/usr/bin/grep" '("lisp") 
                                          :input s 
                                          :output :stream))) 
            (when grep-process 
              (unwind-protect 
                  (with-open-stream (o (process-output grep-process)) 
                    (loop 
                       :for line := (read-line o nil nil) 
                       :while line 
                       :collect line)) 
                (process-close grep-process))))) 
      (when ls-process (process-close ls-process))))) 

I also experimented with omitting the :WAIT NIL argument from the RUN-PROGRAM call for ls, and it worked just as well.




COMP.lang.lisp
==============
Raymond Toy 	
Sign in to reply
More message actions
3/1/10
On 2/28/10 1:26 PM, Pillsy wrote:
> (defun piping-test ()
>   (let ((grep-process (run-program "/usr/bin/grep" '("lisp")
>                                    :input :stream
>                                    :output :stream)))
>     (unwind-protect
>          (with-open-stream (s (process-input grep-process))
>            (let ((ls-process (run-program "/bin/ls" '()
>                                           :output s)))
>              (when ls-process
>                (unwind-protect
>                     (with-open-stream (o (process-output grep-process))
>                       (loop
>                          :for line := (read-line o nil nil)
>                          :while line
>                          :collect line))
>                  (process-close ls-process)))))
>       (when grep-process (process-close grep-process)))))

Try this one.  It works for me with CMUCL; perhaps it works with SBCL.
It basically starts ls first before starting grep.

(defun piping-test2 ()

  (let ((ls-process (run-program "/bin/ls" '()
                                   :wait nil
                                   :output :stream)))
    (unwind-protect
         (with-open-stream (s (process-output ls-process))

           (let ((grep-process (run-program "/usr/bin/grep" '("lisp")
                                          :input s
                                          :output :stream)))
             (when grep-process

               (unwind-protect
                    (with-open-stream (o (process-output grep-process))
                      (loop
                         :for line := (read-line o nil nil)
                         :while line
                         :collect line))
                 (process-close grep-process)))))
      (when ls-process (process-close ls-process)))))

Ray

	Mirko Vukovic 	
Sign in to reply
More message actions
3/2/10
- show quoted text -

Confirmed on on RHEL5 (slightly different paths) and SBCL (only added
sb-ext package qualifiers)

(let ((ls-process (sb-ext:run-program "/bin/ls" '()

                                      :wait nil
                                      :output :stream)))
  (unwind-protect
       (with-open-stream (s (sb-ext:process-output ls-process))
         (let ((grep-process (sb-ext:run-program "/bin/grep" '("lisp")

                                                 :input s
                                                 :output :stream)))
           (when grep-process
             (unwind-protect
                  (with-open-stream (o (sb-ext:process-output grep-process))

                    (loop
                       :for line := (read-line o nil nil)
                       :while line
                       :collect line))
               (sb-ext:process-close grep-process)))))
    (when ls-process (sb-ext:process-close ls-process))))

Thanks,

Mirko
