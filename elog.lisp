;
; Functions used to manage numbers in log space.  These functiosn
; define arithmetic operations, and safe conversions to and from
; standard numbers.
;

; Some non-used value to represent logzero
(defparameter logzero (sqrt -1))

(defun either-zero (x y)
	(or (= x logzero) (= y logzero)))

(defun choose-nonzero (x y)
	(if (= x logzero)
     	y
     	x))

(defun eexp (x)
    (if (or (= x logzero) (will-cause-over-under-flow x))
        0
        (exp x)))

(defun elog (x)
    (if (= x 0)
        logzero
        (log x)))


(defun elogsum (x y)
	(if (either-zero x y)
		(choose-nonzero x y)
        (log-add x y)))

(defun elogdiff (x y)
    (if (= x y)
        logzero
		(if (either-zero x y)
			(choose-nonzero x y)
            (log-sub x y))))

(defun elogdivide (x y)
    (if (either-zero x y)
        logzero
        (- x y)))

(defun elogproduct (x y)
	(if (either-zero x y)
        logzero
        (+ x y)))

(defun elogpow (x e)
    (if (= x logzero)
        0
        (* x e)))

(defun will-cause-over-under-flow (e)
    ; min double exponent
    (or (> e 88) (< e -87)))

(defun log-add (x1 x2)
	(let ((xa (max x1 x2))
		  (xb (min x1 x2)))
		(let ((xc (- xb xa)))
			(assert (<= xc 0))
            (if (will-cause-over-under-flow xc)
                (identity xa)
                ; note this + is really a multiplication
                (+ xa (elog (+ 1 (eexp xc)))))
		)))

(defun log-sub (x1 x2)
	(let ((xa (max x1 x2))
		  (xb (min x1 x2)))
		(let ((xc (- xb xa)))
			(assert (<= xc 0))
            (if (will-cause-over-under-flow xc)
                (identity xa)
                (elogsum xa (elog (- 1 (eexp xc)))))
		)))
