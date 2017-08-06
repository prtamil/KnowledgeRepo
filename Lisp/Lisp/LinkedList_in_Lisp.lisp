 Linked List in CommonLisp
****************************

Introduction
==============
Just Wanted to Write Linked List in Common Lisp
Common Lisp itself has List data structure inbuilt . But for curiosity
I decided like C Like Structres
Preparation

+ Tried Iterate package for my looping needs but CL's Default loop is far better

Structure of Node
====================

    (defstruct node  
      (data 0)  
      (lnk nil))  

Filling Nodes of List just with numerical data


  (defun fillnode(uptoN)
           (let ((newl nil)
                 (tail nil))
             (loop for x from 1 to uptoN
                  do (if (equal newl nil)
                         (progn
                           (setf newl (make-node :data x
                                                 :lnk nil))
                           (setf tail newl))
                         (progn
                           (setf (node-lnk tail) (make-node :data x
                                                            :lnk nil))
                           (setf tail (node-lnk tail))))) newl))
             
 
 Printing List Data
=====================


   (defun printlst(lst)
	   (let ((l lst))
	     (loop while (not (equal l nil))
		do(progn
		    (format t "~a =>" (node-data l))
		    (setf l (node-lnk l))))
	     (format t "~%")))

Copying List and Why this is important
=======================================

By default Common lisp is pass-by-value language but when we pass arrays, hashes, user defined structrues we can modify as its reference 
the data inside array,hash,defstructs are modifiable would reflect source in order to avoid it we need to have copy function which makes 
copy of original. then we operate on Copy function

There are Two functions 
1. Copy-LinkedList (iterative) 
2. Clone-lst (recursive)

Copy-Linked List Iterative
---------------------------

  (defun copy-linkedlist(lst)
    (let ((curr lst)
          (newl nil)
          (tail nil))
      (loop while (not (equal curr nil))
         do (progn
              (if (equal newl nil)
                  (progn
                    (setf newl (make-node :data (node-data curr)
                                          :lnk nil))
                    (setf tail newl))
                  (progn
                    (setf (node-lnk tail) (make-node :data 0
                                                     :lnk nil))
                    (setf tail (node-lnk tail))
                    (setf (node-data tail) (node-data curr))
                    (setf (node-lnk tail) nil)))
              (setf curr (node-lnk curr))))newl))  
                    
Clone Linked List Recursive
---------------------------
      

    (defun clone-lst(lst)  
      (let ((res nil))  
        (if (equal lst nil)  
            nil  
          (progn  
            (setf res (make-node :data (node-data lst)))  
            (setf (node-lnk res) (clone-lst (node-lnk lst)))  
            res))))  



Reversing Linked List
======================


  (defun reverse-lst-cln(lst)
    (let* ((rf  (clone-lst lst))
           (rl rf)
           (tmp nil)
           (prev nil))
      (loop while (not (equal rl nil))
         do (progn
              (setf tmp (node-lnk rl))
              (setf (node-lnk rl) prev)
              (setf prev rl)
              (setf rl tmp)))prev))
  
  
  (defun reverse-lst-cpy(lst)
    (let* ((rf  (copy-linkedlist lst))
           (rl rf)
           (tmp nil)
           (prev nil))
      (loop while (not (equal rl nil))
         do (progn
              (setf tmp (node-lnk rl))
              (setf (node-lnk rl) prev)
              (setf prev rl)
              (setf rl tmp)))prev)) 
  


Test Functions
================
  
    (defun tst(uptox)  
      (let ((xx (fillnode uptox)))  
        (printlst xx)  
        (format t "~%")  
        (printlst (reverse-lst-cln  xx))  
        (format t "~%")  
         (printlst xx)))   
      
    (defun tst1(uptox)  
      (let ((xx (fillnode uptox)))  
        (printlst (reverse-lst-cln xx))))  
      
      
    (defun tst2(uptox)  
      (let ((xx (fillnode uptox)))  
        (printlst (reverse-lst-cpy xx))))  
          
Conclusion
===========

 In conclusion i have learned pass-by-value , pass-by-reference, linked list concepts and have confident in Common lisp 
