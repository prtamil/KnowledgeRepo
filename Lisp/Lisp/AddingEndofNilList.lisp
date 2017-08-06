;;;;(setf (cdr (last lst)) (cons x nil)) = (nconc lst (cons x nil)) = (nconc lst (list x)) 

(defmacro pushend( item place)
	   `(setf ,place (nconc ,place (cons ,item nil)))) 

(defparameter l nil)
(pushend 10 l)
(pushend 20 l)
(pushend 30 l)
(pushend 40 l)

> (10 20 30 40)
