Call by Reference / Call by Value in Common Lisp
================================================

Call by Common Lisp
====================
It is a minor nuisance in my opinion that Common Lisp is a pass-by-value language. Or, rather, it is a pass-by-reference. 
Or, rather, it is a rather confusing mixture of the two. For example; take the following bit of code:

  (defvar *list* nil)  
    
  (defun add-to-list (item list)  
    (push item list))  
    
  (add-to-list 'a *list*)  
    
  > *list*  


What do you think the outcome is? It's of course nil, because Lisp is pass-by-value. Now check this out:

    (defvar *hash* (make-hash-table))  
      
    (defun add-to-hash (key item hash)  
      (setf (gethash key hash) item))  
      
    (add-to-hash 'x 'a *hash*)  
      
    (gethash 'x *hash*)  

What is the outcome now? It's 'a. Which would mean that Lisp is pass-by-reference. 
So what's happening? Unless the changes to the hash bound in the function were echoed to the original, the hash was passed by reference.

The problem is actually rather simple. Parameter passing in Common Lisp *is* pass-by-value,
 but some of those values are references. It's a little bit like C:

  
  int x = 2;  
    
  int double(int i, int *pi)  
  {  
     *pi = i * 2;  
     return i * 2;  
  }  
    
  double(x, &x);  

Not only does double return 4, x is now set to 4.
 In "double(x, &x);" x is first passed to double by value, then by reference.
 Or rather, a reference (C pointer) to x is pass by value to double. There is no mystery to this; in fact it is a rather handy feature that lets you specify which parameters to a function should be mutable and which should not. It's efficient, low-level, and works very well in C.

In Common Lisp however, there are no pointers. References are not some explicit data type that can be set or derived from a variable. 
There is no "(ref x)". Instead reverences are treated mostly implicitly. "(list 1 2 3)" returns an object joined together with references and 
one can exploit this progamatically, but there is no way to explicitly pass a reference to an object.
 That is, there is no way to force a pass-by-reference on a Lisp object in the same way that "&x" works in C.
 
While many complex data structures, such as hashes and arrays can be treated implicitly as pass-by-reference,
 atomic types, such as numbers and characters cannot. Lists are a special case where one can modify the entire structure so long as 
they don't attempt to rebind the initial cons. So:


    (defun add-item-a (item list)  
      (push item list))  
      
    (defun add-item-b (item list)  
      (nconc list (cons item nil)))  


The first definition will no work as expected, but the latter will, so long as it is not passed an empty list. 
Objects and structures are similar in that one can rebind slots, but not replace the entire object.
 One can modify variables from a function which has been passed a reference to it, but one cannot modify the references themselves.

So what does one do when one wants to do something like this?:


    (let ((count 0))  
      (defun add-next-count (list)  
        (push (incf count) list)))  
      
    (defvar *counter-list* nil)  
      
    (add-next-count *counter-list*)  

Well, one option would be to rewrite the code to something a little more idiomatic. Another would be to use a macro.
 However, sometimes the clearest way to write the code is to pass a value to be modified, and macros add a lot of unneeded complexity 
if one can find another way.

For example passing the variable symbol can emulate a reference of sort:


    (defvar *list* nil)  
      
    (defun add-to-list (item list)  
      (push item (symbol-value list)))  
      
    (add-to-list '*list*)  

But this only works with dynamically bound, special variables. In order to rebind a lexical variable, 
one needs access to the scope in which it is bound, and this is lost in a function call.
 There is no way to pass a lexical variable by reference in Common Lisp. However, we can fake it.

Take this macro for example:


    (defmacro add-to-list (item list)  
      `(push ,item ,list))  


This works, and is idiomatic in Lisp. It works because it expands to code within the same scope in which it is being called.
 We can get the call-by-reference effect in Lisp simply by rewriting any functions that want to modify their parameters as macros. 
Unfortunately, this is not a perfect solution as macros are potential source of bugs which functions are not.
 If we wanted to make a very complex function into a macro it may be very difficult to debug or lead to complexity 
that could otherwise have been avoided (not long ago I ran into the very problem myself.) 
In a way, there is no way to avoid this, if we want the ability to consistently modify any parameters we need to use a macro.
 However, there might be a way to encapsulate the code that handles the rebinding of variables. Take this function for example:



    (defvar *temp-list*)  
      
    (defun add-to-list (item)  
      (push item *temp-list*))  

This function always operates on "*temp-list*". We can use "*temp-list*" as a means of passing values to and from "add-to-list". For example:



    (defvar *list* nil)  
      
    (setf *temp-list* *list*)  
    (add-to-list 'item)  
    (setf *list* *temp-list*)  
      
    *list*  

This code returns what we expect. With that in mind, we can certainly wrap this in a macro:



    (with-bindings ((&rest bindings) &body body)  
      `(let ,bindings  
         ,@body  
         ,@(mapcar (lambda (binding)  
                     `(setf ,@(reverse binding)))  
                   bindings)))  

such that:


    (with-bindings (*temp-list* *list*)  
      (add-to-list 'item))  


Does what the previous sample did. This gives me an idea. 
If I define a function and then define a macro around it that sets any variables that I want set.
 Furthermore I can write a macro that abstracts the whole process and allows me to define a function with several
 parameters passed in as "references". Here is the code:


  
  (defun maptree (function tree)  
    (mapcar (lambda (branch)  
       (if (atom branch)  
    (funcall function branch)  
    (maptree function branch)))  
     tree))  
    
  (defmacro define-function-with-references (name (&rest references) (&rest parameters) &body body)  
    (let ((_references nil)  
   (fun-name (gensym)))  
      (dotimes (count (list-length references))  
        (push (gensym) _references))  
      `(progn ,@(mapcar (lambda (_reference)  
     `(defvar ,_reference))  
          _references)  
       (defun ,fun-name ,parameters  
         ,@(maptree (lambda (atom)  
        (loop for reference in references  
           for _reference in _references  
           when (equal atom reference)  
           return _reference  
           finally (return atom)))  
      body))  
       (defmacro ,name ,(append references parameters)  
         `(let ,(mapcar (lambda (_reference reference)  
            `(,_reference ,reference))  
          ',_references (list ,@references))  
     (funcall ,#',fun-name ,,@parameters)  
     ,@(mapcar (lambda (reference _reference)  
          `(setf ,reference ,_reference))  
          (list ,@references) ',_references))))))  

What this macro "defines" a function with reference parameters. Actually, it defines a function that operates on global 
variables and wraps that function in a macro which sets those global variables to the value of passed in variables and
 sets the passed in variables to the value of the the globals upon completion of the function, effectively allowing the
 function to modify the value of parameters passed to it. The globals are named with generated symbols to prevent namespace collisions.
 So, code like this finally works:


    (let ((count 0))  
      (define-function-with-references add-next-count (list) ()  
        (push (incf count) list)))  
      
    (defvar *counter-list* nil)  
      
    (add-next-count *counter-list*)  
    (add-next-count *counter-list*)  
    (add-next-count *counter-list*)  
      
    *counter-list*  


This returns "(3 2 1)" as expected. Of course, it's unusual that this kind of idiom is the correct way of doing things, and
 it's likely that my macro could be done better, but it works.  
 
