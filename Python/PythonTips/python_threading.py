""" Important Things to remember

1.  Python threads can't run bytecode in parallel on multiple CPU
    cores because of GIL (Global Interpreter lock)                                                                                                                 
    
    so calculation is not done parallely. Can't use threads for calc
    purposes
    Only One python threads runs at a time 
    
    Python interpreter enforces fairness between all of the threads that are
    executing to ensure they get a roughly equal amount of processing time


2.  Python threads are still useful despite the GIL because they provide
	an easy way to do multiple things at seemingly the same time

3. Use Python threads to make multiple system calls in parallel. This 
	allows you to do blocking I/O at the same time as computation.

	Blocking IO , System calls use threads.

4.                                                                                                                         
                                                                                                                                  
