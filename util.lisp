; Math and other helper defines

(defparameter 2pi (* 2 pi))

(defun approxl (l1 l2 precision)
    (reduce (lambda (x y) (and x y)) (mapcar (lambda (x y) (< (abs (- x y)) precision)) l1 l2)))

(defun approx (a1 a2 precision)
    (< (abs (- a1 a2)) precision))

(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))

(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))

(defun transpose-2d (list-2d)
    (apply #'mapcar #'list list-2d))

(defun naverage (&rest args)
    (when args
        (/ (reduce #'+ args) (length args))))

(defun laverage (args)
    (when args
        (/ (reduce #'+ args) (length args))))

(defun diff-sq (x y)
    (expt (- x y) 2))

(defun inv (x)
    (/ 1 x))

(defun argx (fun tuples)
	(cadr (find (reduce fun tuples :key #'car) tuples :key #'car)))

; Takes in a list of tuples (2 element nested lists) in the following
; format:
;	(value, argument)
; and returns the argument from the minimum value
(defun argmin (tuples)
	(argx #'min tuples))

(defun argmax (tuples)
	(argx #'max tuples))


(defun rand-range (minval maxval)
    (+ minval (random (- maxval minval))))

(defun generate-k-random (k minval maxval)
    (format t "Generating ~d random values from between ~d and ~d~C" k minval maxval #\linefeed)
    (loop for i from 1 upto k collect (rand-range minval maxval)))

(defun generate-k-random-vals (k vals)
    ; (format t "Generating ~d random values from between ~d and ~d~C" k minval maxval #\linefeed)
    (format t "Selecting ~d values from list at random~C" k #\linefeed)
    (loop for i from 1 upto k collect
        (let ((inx (rand-range 1 (length vals))))
            (nth inx vals))))
    
(defun squared-differences (l1 l2)
    (mapcar (lambda(x y) (float (expt (- x y) 2))) l1 l2))

(defun sum-squared-differences (l1 l2)
    (reduce #'+ (squared-differences l1 l2)))

