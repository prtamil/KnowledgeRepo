Best Case
=========


In comp.lang.lisp Chris Riesbeck offered this as a workaround for a similar question a few years ago:

(remprop 'count 'iter::synonym)

From then you need to use COUNTING as the iterate clause. CL:COUNT then should refer to the Common Lisp function. You would need to recompile the code.


Worst Case
==========
(iter (for i in '(1 2 3 4))
(for c = (count i (list 234 234 2)))
(collect c))

fails because CL:COUNT is shadowed by iterate and so i cant call the
function from that scope.

A workaround is:

(iter (for i in '(1 2 3 4))
(for c = (funcall #'cl:count i (list 234 234 2)))
(collect c))

But it is not nice to use this work-around.
