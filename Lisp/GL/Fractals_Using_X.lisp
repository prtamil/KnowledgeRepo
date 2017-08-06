Here are a few fractal hacks I translated out of the BASIC code in
CHAOS AND FRACTALS by Peitgen, Jurgens and Saupe.

I've run them under CMUCL and Linux ACL 5 (with CLX).

To run them, you run

(fr:sierp) and (fr:s-sierp) for sierp.lisp
(fr:mrcm-s), (fr:mrcm-tt), (fr:mrcm-drag), (fr:mrcm-fern),
(fr:mrcm-maze), (fr:mrcm-twig) and (fr:mrcm-crystal) for mcrm.lisp,
and (fr:koch) for koch.lisp.


First, the `skeleton':

;;;;;;;;;;;;;;;;cut here, save as skel.lisp;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (optimize (speed 3) (safety 1) (debug 3) (space 0)))

(eval-when (compile load eval)
  (defpackage "FRACTALS"
    (:use "COMMON-LISP" "XLIB")
    (:nicknames "FR")))
;;;    (:export "DO-ALL-FRACTALS" "FRACTALS" "PETAL-DEMO"
;;;             "RECURRENCE-DEMO" "MIRA-DEMO" "ATTRACTOR-DEMO")))

(in-package "FRACTALS")

(defvar *display* nil)
(defvar *screen* nil)
(defvar *window* nil)
(defvar *root* nil)
(defvar *gc* nil)
(defvar *default-colormap* nil)
(declaim (type (unsigned-byte 32)
               *fg-pixel* *bg-pixel* *red-pixel* *blue-pixel* *green-pixel*))
(defvar *fg-pixel*)
(defvar *bg-pixel*)
(defvar *white-pixel*)
(defvar *red-pixel*)
(defvar *blue-pixel*)
(defvar *green-pixel*)



;;; Macro to define a fractal program.  Creates a defun that does
;;; setup and shutdown.  Stolen from cmucl demo program.

(defmacro deffractal (fun-name demo-name args x y width height doc &rest forms)
  `(progn
     (defun ,fun-name ,args
       ,doc
       (unless *display*
         #+cmu
         (multiple-value-setq (*display* *screen*) (ext:open-clx-display))
         #-cmu
         (progn
           ;; Portable method
           (setf *display* (xlib:open-display
                            #+allegro (short-site-name)
                            #-allegro (machine-instance)))
           (setf *screen* (xlib:display-default-screen *display*)))
         (setf *root* (screen-root *screen*))
         (setf *fg-pixel* (screen-black-pixel *screen*))
         (setf *bg-pixel* (screen-white-pixel *screen*))
         (setf *default-colormap* (screen-default-colormap *screen*))
         (setf *white-pixel* (alloc-color *default-colormap* "white"))
         (setf *red-pixel* (alloc-color *default-colormap* "red"))
         (setf *blue-pixel* (alloc-color *default-colormap* "blue"))
         (setf *green-pixel* (alloc-color *default-colormap* "green")))
       (let ((*window* (create-window :parent *root*
                                      :x ,x :y ,y
                                      :event-mask '(:visibility-change)
                                      :width ,width :height ,height
                                      ;; :override-redirect :on
                                      :background *bg-pixel*
                                      :border *fg-pixel*
                                      :border-width 2)))
        
         (setf *gc* (xlib:create-gcontext :drawable *window*
                                          :background *bg-pixel*
                                          :foreground *fg-pixel*))
         (set-wm-properties *window*
                            :name ,demo-name
                            :icon-name ,demo-name
                            :resource-name ,demo-name
                            :x ,x :y ,y :width ,width :height ,height
                            :user-specified-position-p t
                            :user-specified-size-p t
                            :min-width ,width :min-height ,height
                            :width-inc nil :height-inc nil)
         (map-window *window*)
         ;; Wait until we get mapped before doing anything.
         (wait-for-mapping *display* *window*)
         (unwind-protect
             (progn ,@forms)
           (unmap-window *window*)
           (wait-for-unmapping *display* *window*))))
     ;;; Don't really need the following...
     (setf (get ',fun-name 'demo-name) ',demo-name)
     (setf (get ',fun-name 'demo-doc) ',doc)
     (export ',fun-name)
     ',fun-name))


(defvar *name-to-function* (make-hash-table :test #'eq))
(defvar *keyword-package* (find-package "KEYWORD"))
(defvar *demo-names* nil)



;;;; Utilities.

(defun full-window-state (w)
  (with-state (w)
    (values (drawable-width w) (drawable-height w)
            (drawable-x w) (drawable-y w)
            (window-map-state w))))


(defun wait-for-mapping (display win)
  (display-finish-output display)
  (multiple-value-bind (width height x y mapped) (full-window-state win)
    (declare (ignore width height x y))
    (if (eq mapped :viewable)
        t
      (wait-for-mapping display win))))


(defun wait-for-unmapping (display win)
  (display-finish-output display)
  (multiple-value-bind (width height x y mapped) (full-window-state win)
    (declare (ignore width height x y))
    (if (eq mapped :unmapped)
        t
      (wait-for-unmapping display win))))


(defun fractal-force-output ()
  (display-force-output *display*))

(defun fractal-clear-window ()
  (clear-area *window*))




;;; Drawing utilities.  We save the X and Y values from the previous
;;; draw operation so we can draw lines where those values are the
;;; starting points for the new line.  This can be overridden by
;;; supplying the start X and Y coordinates.

;; Save X and Y values for relative plotting.
(declaim (integer *current-x* *current-y*))
(defvar *current-x* 0)
(defvar *current-y* 0)

(defun pset (x y &optional (foreground *blue-pixel*))
  (let ((trunc-x (truncate x))
        (trunc-y (truncate y)))
    (declare (integer trunc-x trunc-y))
    (with-gcontext (*gc* :foreground foreground)
      (xlib:draw-point *window* *gc* trunc-x trunc-y)
      (setf *current-x* trunc-x
            *current-y* trunc-y))))

(defun line (end-x end-y &optional (gc *gc*) (start-x *current-x*) (start-y *current-y*))
  (declare (type (or fixnum single-float) end-x end-y start-x start-y))
  (let ((x1 (truncate end-x))
        (y1 (truncate end-y))
        (x2 (truncate start-x))
        (y2 (truncate start-y)))
    (declare (fixnum x1 y1 x2 y2))
    (xlib:draw-line *window* gc x1 y1 x2 y2)
    (setf *current-x* x1
          *current-y* y1)))


;;;;;;;;;;;;;;;;end of skel.lisp;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;cut here; save as sierp.lisp;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Sierpinski gasket --- ``shortest possible'' programs.
;;;

(eval-when (compile load eval)
  (load "skel"))

(in-package "FRACTALS")

(defvar *w* 700)


(defun skewed ()
  (dotimes (y 600)
    (dotimes (x 600)
      (when (zerop (logand x y))
        (pset (+ x 30) (+ y 30)))))
  (fractal-force-output)
  (sleep 3))


(deffractal s-sierp "Skewed Sierpinski Gasket" ()
  10 10 700 700
  "Displays Skewed Sierpinski Gasket."
  (skewed))

(defun u-sierp ()
  (dotimes (y 512)
    (dotimes (x 512)
      (when (zerop (logand x (- y x)))
        (pset (- (+ x (+ (/ 512 2) 30)) (* y .5)) (+ y 30)))))
  (fractal-force-output)
  (sleep 3))

(deffractal sierp "Skewed Sierpinski Gasket" ()
  10 10 600 600
  "Displays Sierpinski Gasket."
  (u-sierp))

;;;;;;;;;;;;;;end of sierp.lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;cut here; save as mrcm.lisp ;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Multiple reduction copying machine
;;;

(declaim (optimize (speed 3) (safety 0) (debug 3)))

(eval-when (compile load eval)
  (load "skel"))

(in-package "FRACTALS")

#+cmu
(declaim (ext:start-block))

(declaim (single-float *w* *h* *left* *wl*))
(defparameter *w* 600.0)
(defparameter *h* 600.0)

(defvar *left* 60.0)
(defvar *wl* (+ *left* *w*))


(declaim (fixnum *level*))
(defparameter *level* 11)

(defstruct xform
  (transformations 0 :type fixnum)
  a
  b
  c
  d
  e
  f
  )


(defvar sierp
  (make-xform :transformations 3
              :a (make-array 3
                             :initial-contents '(0.5 0.5 0.5)
                             :element-type 'single-float)
              :b (make-array 3
                             :initial-contents '(0.0 0.0 0.0)
                             :element-type 'single-float)
              :c (make-array 3
                             :initial-contents '(0.0 0.0 0.0)
                             :element-type 'single-float)
              :d (make-array 3
                             :initial-contents '(0.5 0.5 0.5)
                             :element-type 'single-float)
              :e (make-array 3
                             :initial-contents '(0.0 300.0 150.0)
                             :element-type 'single-float)
              :f (make-array 3
                             :initial-contents '(0.0 0.0 300.0)
                             :element-type 'single-float)))

(defparameter twin-tree
  (make-xform :transformations 3
              :a (make-array 3
                             :initial-contents '(0.0 0.0 0.5)
                             :element-type 'single-float)
              :b (make-array 3
                             :initial-contents '(-0.5 0.5 0.0)
                             :element-type 'single-float)
              :c (make-array 3
                             :initial-contents '(0.5 -0.5 0.0)
                             :element-type 'single-float)
              :d (make-array 3
                             :initial-contents '(0.0 0.0 0.5)
                             :element-type 'single-float)
              :e (make-array 3
                             :initial-contents '(300.0 300.0 150.0)
                             :element-type 'single-float)
              :f (make-array 3
                             :initial-contents '(0.0 300.0 300.0)
                             :element-type 'single-float)))

(defparameter dragon
  (make-xform :transformations 3
              :a (make-array 3
                             :initial-contents '(0.0 0.0 0.0)
                             :element-type 'single-float)
              :b (make-array 3
                             :initial-contents '(0.577 0.577 0.577)
                             :element-type 'single-float)
              :c (make-array 3
                             :initial-contents '(-0.577 -0.577 -0.577)
                             :element-type 'single-float)
              :d (make-array 3
                             :initial-contents '(0.0 0.0 0.0)
                             :element-type 'single-float)
              :e (make-array 3
                             :initial-contents '(57.06 264.78 57.12)
                             :element-type 'single-float)
              :f (make-array 3
                             :initial-contents '(353.58 473.58 593.58)
                             :element-type 'single-float)))

(defvar maze
  (make-xform :transformations 3
              :a (make-array 3
                             :initial-contents '(.333 0.0 0.0)
                             :element-type 'single-float)
              :b (make-array 3
                             :initial-contents '(0.0 .333 -.333)
                             :element-type 'single-float)
              :c (make-array 3
                             :initial-contents '(0.0 1.0 1.0)
                             :element-type 'single-float)
              :d (make-array 3
                             :initial-contents '(.333 0.0 0.0)
                             :element-type 'single-float)
              :e (make-array 3
                             :initial-contents '(200.0 400.0 200.0)
                             :element-type 'single-float)
              :f (make-array 3
                             :initial-contents '(400.0 0.0 0.0)
                             :element-type 'single-float)))

(defvar twig
  (make-xform :transformations 3
              :a (make-array 3
                             :initial-contents '(0.387 0.441 -0.468)
                             :element-type 'single-float)
              :b (make-array 3
                             :initial-contents '(0.430 -0.091 0.020)
                             :element-type 'single-float)
              :c (make-array 3
                             :initial-contents '(0.430 -0.009 -0.113)
                             :element-type 'single-float)
              :d (make-array 3
                             :initial-contents '(-0.387 -0.322 0.015)
                             :element-type 'single-float)
              :e (make-array 3
                             :initial-contents '(153.6 253.14 240.0)
                             :element-type 'single-float)
              :f (make-array 3
                             :initial-contents '(313.2 303.54 240.0)
                             :element-type 'single-float)))

(defvar crystal
  (make-xform :transformations 4
              :a (make-array 4
                             :initial-contents '(0.255 0.255 0.255 0.370)
                             :element-type 'single-float)
              :b (make-array 4
                             :initial-contents '(0.0 0.0 0.0 -0.642)
                             :element-type 'single-float)
              :c (make-array 4
                             :initial-contents '(0.0 0.0 0.0 0.642)
                             :element-type 'single-float)
              :d (make-array 4
                             :initial-contents '(0.255 0.255 0.255 0.370)
                             :element-type 'single-float)
              :e (make-array 4
                             :initial-contents '(223.56 68.76 378.36 381.36)
                             :element-type 'single-float)
              :f (make-array 4
                             :initial-contents '(402.84 133.92 133.92 -3.66)
                             :element-type 'single-float)))

#|
(defvar crystal5
  (make-xform :transformations 5
              :a (make-array 5 :initial-contents '(0.5 0.5 0.5))
              :b (make-array 5 :initial-contents '(0.0 0.0 0.0))
              :c (make-array 5 :initial-contents '(0.0 0.0 0.0))
              :d (make-array 5 :initial-contents '(0.5 0.5 0.5))
              :e (make-array 5 :initial-contents '(0.0 300.0 150.0))
              :f (make-array 5 :initial-contents '(0.0 0.0 300.0))))

(defvar tree
  (make-xform :transformations 5
              :a (make-array 5 :initial-contents '(0.5 0.5 0.5))
              :b (make-array 5 :initial-contents '(0.0 0.0 0.0))
              :c (make-array 5 :initial-contents '(0.0 0.0 0.0))
              :d (make-array 5 :initial-contents '(0.5 0.5 0.5))
              :e (make-array 5 :initial-contents '(0.0 300.0 150.0))
              :f (make-array 5 :initial-contents '(0.0 0.0 300.0))))
|#


(defvar fern
  (make-xform :transformations 4
              :a (make-array 4
                             :initial-contents '(0.849 0.197 -0.15 0.0)
                             :element-type 'single-float)
              :b (make-array 4
                             :initial-contents '(0.037 -0.226 0.283 0.0)
                             :element-type 'single-float)
              :c (make-array 4
                             :initial-contents '(-0.037 0.226 0.260 0.0)
                             :element-type 'single-float)
              :d (make-array 4
                             :initial-contents '(0.849 0.197 0.237 0.160)
                             :element-type 'single-float)
              :e (make-array 4
                             :initial-contents '(45.0 240.0 345.0 300.0)
                             :element-type 'single-float)
              :f (make-array 4
                             :initial-contents '(109.8 29.4 -50.4 0.0)
                             :element-type 'single-float)))

(defun mrcm-top (figure)
  (dotimes (i *level*)
    (fractal-clear-window)
    (mrcm-1 i figure 0.0 *w* (* *h* .5) 0.0 0.0 *h*)
    (fractal-force-output)
    (sleep 1))
  (sleep 2))

(declaim (function mrcm-1 (fixnum
                           xform
                           single-float
                           single-float
                           single-float
                           single-float
                           single-float
                           single-float) t))

(defun mrcm-1 (level figure xleft xright xtop yleft yright ytop)
  (declare (fixnum level)
           (type xform figure)
           (single-float xleft xright xtop yleft yright ytop))
  (flet ((triangle (xa ya xb yb xc yc)
                   (declare (single-float xa ya xb yb xc yc))
                   (line xb yb *gc* xa ya)
                   (line xc yc)
                   (line xa ya)
                   (values)))
    (if (> level 1)
        (let ((transformations (xform-transformations figure))
              (a (xform-a figure))
              (b (xform-b figure))
              (c (xform-c figure))
              (d (xform-d figure))
              (e (xform-e figure))
              (f (xform-f figure)))
          (declare (fixnum transformations)
                   (type (simple-array (single-float) *) a b c d e f))
          (dotimes (i transformations)
            (let ((xl (+ (* (the single-float (aref a i)) xleft)
                         (* (the single-float (aref b i)) yleft)
                         (the single-float (aref e i))))
                  (yl (+ (* (the single-float (aref c i)) xleft)
                         (* (the single-float (aref d i)) yleft)
                         (the single-float (aref f i))))
                  (xr (+ (* (the single-float (aref a i)) xright)
                         (* (the single-float (aref b i)) yright)
                         (the single-float (aref e i))))
                  (yr (+ (* (the single-float (aref c i)) xright)
                         (* (the single-float (aref d i)) yright)
                         (the single-float (aref f i))))
                  (xt (+ (* (the single-float (aref a i)) xtop)
                         (* (the single-float (aref b i)) ytop)
                         (the single-float (aref e i))))
                  (yt (+ (* (the single-float (aref c i)) xtop)
                         (* (the single-float (aref d i)) ytop)
                         (the single-float (aref f i)))))
              (declare (single-float xl yl xr yr xt yt))
              (mrcm-1 (1- level) figure xl xr xt yl yr yt))))
      (triangle (the single-float (+ *left* xleft))
                (the single-float (- *wl* yleft))
                (the single-float (+ *left* xright))
                (the single-float (- *wl* yright))
                (the single-float (+ *left* xtop))
                (the single-float (- *wl* ytop))))
    (values)))


(deffractal mrcm-s "Multiple Reduction Copying Machine" ()
  10 10 700 700
  "Display a MRCM-generated Sierpinski gasket."
  (mrcm-top sierp))

(deffractal mrcm-tt "Multiple Reduction Copying Machine" ()
  10 10 700 700
  "Display a MRCM-generated `twin-tree'."
  (mrcm-top twin-tree))

(deffractal mrcm-drag "Multiple Reduction Copying Machine" ()
  10 10 700 700
  "Display a MRCM-generated `dragon'."
  (mrcm-top dragon))

(deffractal mrcm-fern "Multiple Reduction Copying Machine" ()
  10 10 700 700
  "Display a MRCM-generated `fern'."
  (mrcm-top fern))

(deffractal mrcm-maze "Multiple Reduction Copying Machine" ()
  10 10 700 700
  "Display a MRCM-generated `maze'."
  (mrcm-top maze))

(deffractal mrcm-twig "Multiple Reduction Copying Machine" ()
  10 10 700 700
  "Display a MRCM-generated `twig'."
  (mrcm-top twig))

(deffractal mrcm-crystal "Multiple Reduction Copying Machine" ()
  10 10 700 700
  "Display a MRCM-generated `crystal'."
  (mrcm-top crystal))

#+cmu
(declaim (ext:end-block))

;;;;;;;;;;;;;;end of mrcm.lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;cut here; save as koch.lisp;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Koch curve.
;;;

(eval-when (compile load eval)
  (load "skel"))

(in-package "FRACTALS")

(defparameter *w* 700)
(defparameter *h* 400)

(defparameter level 10)
(defparameter r 1/3)

(defun koch-c ()
  (dotimes (i level)
    (koch-draw-ppict i 30 (- *w* 30) (- *h* 30) (- *h* 30))
    (fractal-force-output)
    (sleep 2)
    (fractal-clear-window))
  (sleep 3))


(defun koch-draw-ppict (level xleft xright yleft yright)
  (if (> level 1)
      (let ((l (1- level)))

        ;; Left branch.
        (let ((xr (+ (* 1/3 xright) (* 2/3 xleft)))
              (yr (+ (* 1/3 yright) (* 2/3 yleft))))
          (koch-draw-ppict l xleft xr yleft yr)

          ;; Middle left branch.
          (let ((xl xr)
                (yl yr)
                (xr (- (+ (* 1/2 xright) (* 1/2 xleft))
                       (* r (- yleft yright))))
                (yr (+ (+ (* 1/2 yright) (* 1/2 yleft))
                       (* r (- xleft xright)))))
            (koch-draw-ppict l xl xr yl yr)

            ;; Middle right branch.
            (let ((xl xr)
                  (yl yr)
                  (xr (+ (* 2/3 xright) (* 1/3 xleft)))
                  (yr (+ (* 2/3 yright) (* 1/3 yleft))))
              (koch-draw-ppict l xl xr yl yr)
              
              ;; Right branch.
              (koch-draw-ppict l xr xright yr yright)))))

   
      (line xleft yleft *gc* xright yright)))

(deffractal koch "Koch curve" ()
  10 10 *w* *h*
  "Display the Koch curve."
  (koch-c))

;;;;;;;;;;;;;;;;end of koch.lisp;;;;;;;;;;;;;;;;;;;;;;;;


-- 
Fred Gilham                                      gil...@csl.sri.com
I have over the years been viewed as a man of the left and a man of
the right, and the truth is that I've never put much stake in such
labels. But this I have learned: the left patrols its borders and
checks membership credentials ever so much more scrupulously, even
ruthlessly, than does the right.            -- Richard John Neuhaus
