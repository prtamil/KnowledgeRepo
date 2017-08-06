 CommonLisp Reference are Same as C Pointers
============================================

CommonLisp Reference = C Pointers
==================================
When we are passing function parameters as references in commonlisp it actually behaves as C Pointers

  
  (defstruct node  
      (data 0)  
      (lnk nil))  
  (defparameter *nd* (make-node :data 10 :lnk nil))  
  (defun tst(nd)  
   (setf (node-lnk nd) (make-node :data 20 :lnk nil)))  
  (tst *nd*)  

In above code the global variables lnk will get added. As we pass by reference the node will become



    (NODE :data 10  
          :lnk (NODE :data 20  
                     :lnk nil))  

so even if we use LET the reference value will get changed such as



    (defun tst(nd)  
     (let ((n nd))  
     (setf (node-lnk n) (make-node :data 20 :lnk nil))))  
      
    (tst *nd*)  

We get the same Message as above. the above code is equal to below C Code



    int func(int *d)  
    {  
      *d = 12;  
    }  

the Content of d pointer get changed which tells the above scenario

Scenario II
============
Now if we tried to assign a new value to pointer itself it will not get reflected for ex like below c code


  
  int *f;  
  int func(int *d)  
  {  
    int *tmp = new int;  
    *tmp = 24;  
    d = tmp  
  }  
  func(f);  

The same will happen with commonlisp also. Try below code it will not change the pointer


  
  (defun func(nd)  
    (let ((n nd))  
      (setf n (make-node :data 20 :lnk nil))))  
  (func *nd*)  

in abvoe lisp code the *nd* will never be changed . or changes are lost. 
 
