Single Step Debugging in SBCL
===============================
+ Some how never works in slime
+ Only works in command prompt so use shell cmd prompt
 
    (defun fact(x)  
      (declare '(optimize (debug 3)))  
       (if (= x 0)  
            1  
            (* x (fact (1- x))))  
      
    (step (fact 5))  

When we execute above code in command prompt code goes to single stepper it will ask we can type following inputs,
+ step
+ next
+ out
+ stop etc..


  
    (proclaim (optimize (debug 3)))  
      
    (defun fact(x)  
       (break)  
        (if (= x 0)  
            1  
            (* x (fact  (1- x))))  
    
  (step (fact 5))  

The above code breaks and use START to start debugger then we can use step,next,out use STOP for stopping single stepping 
 
