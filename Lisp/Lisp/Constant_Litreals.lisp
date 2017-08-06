Litreal Object such as '(0 0 0) is ref instead of value ?
---------------------------------------------------------
for ex why following code work like that.
(defun foo()
 (let ((some-list '(0 0 0)))
  (incf (car some-list))
  some-list))
 > (foo)
   (1 0 0)
 > (foo)
   (2 0 0)
 > (foo)
    (3 0 0)

Answer:
--------
  '(0 0 0) is a literal object. which is assumed to be constant (may not have protection from modification).
   So when refering it . its always the same object not a different object.

we should use (list 0 0 0) instead of '(0 0 0) so that everytime its different object.

  So use literal list like '(0 0 0) as constants always it might be good idea.


