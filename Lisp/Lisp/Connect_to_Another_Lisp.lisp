If we want to connect to another instance of lisp
we need to use slime-connect

1. Server
========
  (ql:quickload "swank")
  (swank:create-server :port 4005)

 IF connect between ssh
 ======================
 (ql:quickload "swank")
 (setf swank::*loopback-interface* "192.168.0.5")
  (swank:create-server :port 4005)
  
2. Emacs
========
  M-x slime-connect


