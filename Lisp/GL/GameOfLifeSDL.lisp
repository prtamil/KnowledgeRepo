(defmacro restartable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))


(defun make-brain(w h)
  (make-array (list h w) :initial-element :off))


(defun make-conway-array-brain(w h)
  (let ((dst (make-brain w h)))
    (loop for i below h
          do (loop for j below w
                   do (let ((cellval (random 2)))
                        (if (= 0 cellval)
                             (setf (aref dst j i) :off)
                            (setf (aref dst j i) :on)))))
    dst))



;;Conway Game of life
(defun alive-cells-p(state)
  (if (equal state :on)
      t
      nil))
(defun 2-or-3-alive-p(state neibhours)
   (if (and (alive-cells-p state)
            (or (= 2 (count :on neibhours))
                (= 3 (count :on neibhours))))
       t
       nil))
(defun more-than-3-neibhours-p(state neibhours)
  (if (and (alive-cells-p state)
           (> 3 (count :on neibhours)))
      t
      nil))

(defun fewer-than-2-live-cells-p(state neibhours)
  (if (and (alive-cells-p state)
           (< 2 (count :on neibhours)))
      t
      nil))
(defun  exactly-3-live-cells-p(state neibhours)
  (if (and (not (alive-cells-p state))
           (= 3 (count :on neibhours)))
      t
      nil))

(defun conway-rule(state neibhours)
  (cond ((2-or-3-alive-p state neibhours) :on)
        ((more-than-3-neibhours-p state neibhours) :off)
        ((fewer-than-2-live-cells-p state neibhours) :off)
        ((exactly-3-live-cells-p state neibhours) :on)
        (t :dying)))
  
  
    

  
(defun neibhours(cells x y)
  (let* ((mx (1- (array-dimension cells 1)))
         (my (1- (array-dimension cells 0)))
         (l (if (zerop x) mx (1- x)))
         (r (if (= x mx) 0 (1+ x)))
         (u (if (zerop y) my (1- y)))
         (d (if (= y my) 0 (1+ y))))
    (mapcar (lambda (x y)
              (aref cells y x))
            (list l x r l r l x r)
            (list u u u y y d d d))))
         
 (defun evolve(src)
   (let* ((w (array-dimension src 1))
          (h (array-dimension src 0))
          (dst (make-brain w h)))

     (loop for j below h
           do (loop for i below w
                    do (setf (aref dst j i)
                             (funcall 'conway-rule (aref src j i) (neibhours src i j)))))
     dst))

;;Graphical

(defclass bb()
  ((cells :accessor cells-of :initarg :cells)))

(defvar *width* 640)
(defvar *height* 480)
(defvar *oo* (make-instance 'bb :cells (make-conway-array-brain 50 50)))
;(cffi:defcallback display :void ()
;; (defun display ()
;;   (gl:clear :color-buffer-bit :depth-buffer-bit)
;;   (draw-box)
;;   ;;glut:swap-buffers
;;   (gl:flush)
;;   (sdl:update-display)
;; )

(defun init () 
 (gl:viewport 0 0 *width* *height*)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -50 250 -50 250 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))


(defun cube-main-sdl ()
  (sdl:with-init ()
    (sdl:window 320 240 :flags sdl:sdl-opengl)
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    (init)
    (display)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
	     (restartable (iidle))))))



;; (defun display-window ()
;;   (gl:clear-color 0 0 0 0)
;;   (gl:shade-model :flat))



(defun render-cell(x y cell)

  (flet ((draw-cell (x y)
           (gl:with-pushed-matrix
             (gl:translate x y 0)
             (gl:with-primitive :polygon
               (gl:vertex 0.1 0.1 0)
               (gl:vertex 0.9 0.1 0)
               (gl:vertex 0.9 0.9 0)
               (gl:vertex 0.1 0.9 0)))))

    (case cell
      (:on (gl:color 1 1 1)
         (draw-cell x y))
      (:off (gl:color 0.5 0.5 0.5)
         (draw-cell x y)))))
     ; (:off (gl:color 1.0 0.0 0.0)
      ;   (draw-cell x y)))))

(defun  display()
  (gl:clear :color-buffer)
  
  (let* ((cells (cells-of *oo*))
         (w (array-dimension cells 1))
         (h (array-dimension cells 0)))

    (loop for j below w
          do (loop for i below h
                   do (render-cell i j (aref cells j i)))))

  (sdl:update-display))


(defun idle ()
  (setf (cells-of *oo*) (evolve (cells-of *oo*)))
  (gl:flush)
  (sdl:update-display))

(defun iidle()
  (gl:clear-color 0 0 0 0)
  (idle)
  (display))

    
