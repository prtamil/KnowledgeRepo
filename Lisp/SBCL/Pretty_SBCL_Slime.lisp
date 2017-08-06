;;Pretty object Printing in SLIME Repl
;;======================================
;  + We can use it to print Additional Info
;  +We Can Right-Click which user defined menu where we can print interesting info

  (defmethod swank::menu-choices-for-presentation ((ob fixnum))
    (list (list "English" (lambda (choice object id) (format t "~r~%" object)))))

; + After adding this code type any fix num in repl
; + We can see the Repl menu as English
; + Click it will print the english word for Repl
 
