Customize Clozure commonlisp Prompt
====================================
Clozure CL’s default prompt is “? “.  You can customize this by setting ccl:*listener-prompt-format*   to a format control of your choice.
Note that a format control “string” can be a function.  Here’s an example that makes the prompt contain the current package name.


(defun prompt-formatter (stream break-level)
  (princ (package-name *package*) stream)
  (if (plusp break-level)
    (format stream " ~d > " break-level)
    (write-string "> " stream)))

To start using this, do

(setq ccl:*listener-prompt-format* #'prompt-formatter)
