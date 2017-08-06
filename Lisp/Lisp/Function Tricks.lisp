Function Tricks
===============

Apply for passing args from one fn to another
---------------------------------------------
If you want to pass arguments of one function to another function we can use apply

  (defun fun(&rest args)
  	(apply #'anotherfn args))