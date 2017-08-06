Download From SVN
=================
svn co http://svn.clozure.com/publicsvn/openmcl/release/1.9/linuxx86/ccl

Recompile CCL
=============

> ./lx86cl --no-init
? (rebuild-ccl :full t)
? (quit)


Add .emacs
==============

(set-language-environment "utf-8")

;;; Note that if you save a heap image, the character
;;; encoding specified on the command line will be preserved,
;;; and you won't have to specify the -K utf-8 any more.
(setq inferior-lisp-program "/usr/local/bin/ccl64 -K utf-8")

(require 'slime)
(setq slime-net-coding-system 'utf-8-unix)
(slime-setup '(slime-fancy))
 

Add Quicklisp
=============
/home/tamil/SoftWares/Lisp/ccl/ccl/lx86cl -l 
/home/tamil/SoftWares/Lisp/ccl/ccl/ccl-init.lisp -K utf-8

contents-of-ccl-init.lisp
-------------------------
;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "~/quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))


