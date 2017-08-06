
________________________________________________________________________________Dir Walk -> Python
========


import os, fnmatch

for (dirpath, dirnames, filenames) in os.walk("c:\\"):
    for filename in filenames:
        if fnmatch.fnmatch(filename, "*.txt"):
            print dirpath + filename


________________________________________________________________________________
Dir Walk -> CommonLisp
======================

(defun mapc-dir-tree(fn dir)
           (dolist (entry (cl-fad:list-directory dir))
             (when (cl-fad:directory-pathname-p entry)
               (mapc-dir-tree fn entry))
             (funcall fn entry)))


(defun find-txt(fname)
           (when (equal (pathname-type fname) "txt")
             (write-line (namestring fname))))

(mapc-dir-tree #'find-txt "c://")

________________________________________________________________________________
Tokenizer - nltk Python
=======================

from nltk.tokenize import RegexpTokenizer

tokenizer = RegexpTokenizer("[\w']+")
s = "good boy can't worry."
tokenizer.tokenize(s)

CookBook First Chapter cool.
----------------------------

________________________________________________________________________________
