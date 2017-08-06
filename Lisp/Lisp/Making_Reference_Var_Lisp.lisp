Making Reference Variables in CommonLisp
==========================================
In Lisp you can make your own way make variable references, if you want to. The simplest approach is like this:
  
  (defstruct reference getter setter)  
  
  (defmacro ref (place)  
    (let ((new-value (gensym)))  
      `(make-reference :getter (lambda () ,place)  
                       :setter (lambda (,new-value)  
                                 (setf ,place ,new-value)))))  
  
  (defun dereference (reference)  
    (funcall (reference-getter reference)))  
  
  (defun (setf dereference) (new-value reference)  
    (funcall (reference-setter reference) new-value))  
  
Usage
=====
And then you can use it:

    (defun increase-by-one (var-ref)  
      (incf (dereference var-ref)))  
      
    (defun test-inc-by-one (n)  
      (let ((m n))  
        (increase-by-one (ref m))  
        (values m n)))  
      
>    (test-inc-by-one 10) => 11, 10  

   
