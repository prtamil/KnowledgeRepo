Reloading Dynamic Libraries from saved SBCL Cores
==================================================

When writing applications based on SBCL for work or leisure I often find myself needing the final product to be an executable lisp core. 
SBCL has built-in support for this with save-lisp-and-die and external support through Buildapp and they work well for most cases.

The issue that I continuously ran head-first into was the behavior of the lisp image with regard to opening any shared
 objects at runtime after a restart. save-lisp-and-die has the option to disable or enable the reloading of these objects, 
but when it is enabled the object it will attempt to load will be searched for by an absolute path. 
So an application built with a library at /opt/local/lib/foo.so will fail on a system where the library exists at /usr/local/lib/foo.so even 
if the dynamic linker is configured to find the library foo.so at both paths.

To solve this problem we must first find the code responsible for reloading the objects in the first place and it is: SB-ALIEN::TRY-REOPEN-SHARED-OBJECT

As a parameter it takes an object that it should attempt to reload.
 If we’re reloading a core on a machine it was not compiled on the paths of these objects will be 
absolute and absolutely incorrect. What can be done is the function can be intercepted and the path stripped
 from the absolute name of the object to just the last component. This will signal the dynamic linker to search 
for the named library in accordance with its configuration. Obviously, since this happens at run time, 
if the object can be found at the exact path it should be favored over whatever would be found if left up to the dynamic linker.

The code to accomplish something like this would look almost exactly like this:

Code
-----
  (with-unlocked-packages (:sb-alien)  
    (let ((function (symbol-function 'sb-alien::try-reopen-shared-object)))  
      (setf (symbol-function 'sb-alien::try-reopen-shared-object)  
            #'(lambda (obj)  
                (declare (type sb-alien::shared-object obj))  
                (let ((path (sb-alien::shared-object-pathname obj)))  
                  (when (pathname-directory path)  
                    (unless (probe-file path)  
                      (let ((sub-path (make-pathname :name (pathname-name path)  
                                                     :type (pathname-type path))))  
                        (setf (sb-alien::shared-object-pathname obj)  
                              sub-path  
                              
                              (sb-alien::shared-object-namestring obj)  
                              (namestring sub-path))))))  
                (funcall function obj)))))  

This code can sit in a helper outside of the dependency chain configured by ASDF and be loaded into the image 
only when producing a core either programmatically or with an explicit —load or —eval during the build step. 
As long as the patch ends up in the resulting image that specific class of headaches should go away.  
