(ql:quickload "split-sequence")

(load "util.lisp")
(load "kmeans.lisp")
(load "em.lisp")

(defun run-part-2-test (k epsilon maxattempts maxiter)
    (load-source-small)
    (defparameter image-list (2d-array-to-list (imago:image-pixels source)))
    (defparameter image-flat (flatten image-list))
    (defparameter image-flat-fixed (fix-image image-flat))

	; Collect all the run results in output
	(defparameter output 
		(loop for i from 1 to maxattempts
			collect (run-em i image-flat-fixed k epsilon maxiter)))
	
    ; Redefine output as tuples of likelihood,parameters 
	(defparameter output (mapcar (lambda (x) (list (em-likelihood x) x)) output))

	(defparameter emparams (select-params output))
    (loop for x below (imago:image-width source)
        do (loop for y below (imago:image-height source)
            do (replace-pixel source x y emparams)))

    (imago:write-png source (format nil "output-em-small-~d.png" k))
	(with-open-file (stream (format nil "output-em-small-~d.bic" k) :direction :output :if-exists :supersede :if-does-not-exist :create)
	    (format stream "~d" (bic (em-likelihood emparams) k (length image-flat)))))

    ; while line do (format t "~a~%" line))
(defun faithful ()
    (defparameter in (open "faithful.txt"))

    (defparameter data
        (when in
            (let ((stuff (loop for line = (read-line in nil)
                while line collect (mapcar #'read-from-string (split-sequence:split-sequence #\Space line)))))
            (close in)
            (identity stuff))))

    (defparameter erupt (mapcar #'car data))
    (defparameter output (expectation-maximization erupt 2 1e-6))
    
    )
    ; (defparameter image-list (2d-array-to-list (imago:image-pixels source)))
    ; (defparameter image-flat (flatten image-list))
    ; (defparameter image-flat-fixed (fix-image image-flat))
    ; ;(defparameter centers (k-means-clustering image-flat 8 1e-6))
    ; (defparameter output (expectation-maximization image-flat-fixed 3 1e-6))
    ; (defparameter centers (unfix-image (car output)))
    ; 
    ; (loop for x below (imago:image-width source)
    ;     do (loop for y below (imago:image-height source)
    ;         do (setf (imago:image-pixel source x y) (round (assign-center centers (imago:image-pixel source x y))))))
    ; (imago:write-png source "output-em.png"))
    ; 
    
; TEST CODE

; WORKS
(defun setuptest0 ()
    (defparameter logmus    (list (log 1) (log 2) (log 3)))
    (defparameter mus       (mapcar #'exp logmus))
    (defparameter logsigmas (list (log 1) (log 1) (log 1)))
    (defparameter sigmas    (mapcar #'exp logsigmas))
    (defparameter logws     (list (log 1/3) (log 1/3) (log 1/3)))
    (defparameter ws        (mapcar #'exp logws))
    (defparameter xs        '(1.5 1.7 2.7 2.4 5.3 6.2)))

(defun setuptest1 ()
    (defparameter logmus    (list (log 1) (log 2) (log 3)))
    (defparameter mus       (mapcar #'exp logmus))
    (defparameter logsigmas (list (log 1) (log 1) (log 1)))
    (defparameter sigmas    (mapcar #'exp logsigmas))
    (defparameter logws     (list (log 1/3) (log 1/3) (log 1/3)))
    (defparameter ws        (mapcar #'exp logws))
    (defparameter xs        '(1.1 1.2 1.5 1.7 2.7 2.4 5.3 3.6 6.2)))

(defun steptest ()
    ; Save the old values, for inspection 
    (defparameter oldmus    mus)
    (defparameter oldsigmas sigmas)
    (defparameter oldws     ws)

    (defparameter gammas (e-step mus sigmas ws xs))
    (defparameter nks    (m-step-nk gammas))
    (defparameter mus    (m-step-mu gammas nks xs))
    (defparameter sigmas (m-step-sigma gammas nks xs mus))
    (defparameter ws     (m-step-w nks)))

(defun log-steptest ()
    ; Save the old values, for inspection 
    (defparameter oldmus    logmus)
    (defparameter oldsigmas logsigmas)
    (defparameter oldws     logws)

    (defparameter loggammas (log-e-step logmus logsigmas logws xs))
    (defparameter lognks    (log-m-step-nk loggammas))
    (defparameter logmus    (log-m-step-mu loggammas lognks xs))
    (defparameter logsigmas (log-m-step-sigma loggammas lognks xs logmus))
    (defparameter logws     (log-m-step-w lognks)))
    
(defun steptest-b ()
    ; Save the old values, for inspection 
    (defparameter oldmus    mus)
    (defparameter oldsigmas sigmas)
    (defparameter oldws     ws)

    (defparameter gammas (super-log-e-step mus sigmas ws xs))
    (defparameter nks    (m-step-nk gammas))
    (defparameter mus    (m-step-mu gammas nks xs))
    (defparameter sigmas (m-step-sigma gammas nks xs mus))
    (defparameter ws     (m-step-w nks)))

(defun setuptest2 ()
    (defparameter logmus    '(1.8602991 1.2015285 1.9332358))
    (defparameter mus       (mapcar #'exp logmus))
    (defparameter logsigmas (list (log 1) (log 1) (log 1)))
    (defparameter sigmas    (mapcar #'exp logsigmas))
    (defparameter logws     (list (log 1/3) (log 1/3) (log 1/3)))
    (defparameter ws        (mapcar #'exp logws))
    (defparameter xs        '(1.1 1.2 1.5 1.7 2.7 2.4 5.3 3.6 6.2)))

(defun setuptest3 ()
    (defparameter logmus    (list (log 1.3931) (log 5.75) (log 2.8495)))
    (defparameter logsigmas (list (log 0.0493) (log 0.2025) (log 0.3134)))
    (defparameter logws     (list (log 0.430752) (log 0.222209) (log 0.347039)))
    (defparameter mus       (mapcar #'exp logmus))
    (defparameter sigmas    (mapcar #'exp logsigmas))
    (defparameter ws        (mapcar #'exp logws))

    (defparameter gammas (transpose-2d (mapcar (lambda(x) (mapcar #'elog x))
     (list '(0.9942    0.0000    0.0058)
           '(0.9925    0.0000    0.0075)
           '(0.9807    0.0000    0.0193)
           '(0.9083    0.0000    0.0917)
           '(0.0000    0.0000    1.0000)
           '(0.0001    0.0000    0.9999)
           '(0.0000    0.9999    0.0001)
           '(0.0000    0.0000    1.0000)
           '(0.0000    1.0000    0.0000)))))

    )

(defun teardown ()
    
    (defparameter oldmus    nil)
    (defparameter oldsigmas nil)
    (defparameter oldws     nil)

    (defparameter gammas    nil)
    (defparameter nks       nil)
    (defparameter mus       nil)
    (defparameter sigmas    nil)
    (defparameter ws        nil)
    
    (defparameter oldmus    nil)
    (defparameter oldsigmas nil)
    (defparameter oldws     nil)

    (defparameter loggammas nil)
    (defparameter lognks    nil)
    (defparameter logmus    nil)
    (defparameter logsigmas nil)
    (defparameter logws     nil))

(defun unit-tests ()
    (setuptest1)
    (steptest)
    (log-steptest)

    (assert (approx mus    (mapcar #'exp logmus) 1e-5))
    (assert (approx sigmas (mapcar #'exp logsigmas) 1e-5))

    (setuptest2)
    (steptest)
    (log-steptest)

    (assert (approx mus    (mapcar #'exp logmus) 1e-5))
    (assert (approx sigmas (mapcar #'exp logsigmas) 1e-5))

    (teardown)
    (print "All tests pass"))

(defun log-marginal-tests ()
    (defparameter logmus    '(1.7414839 0.74082375 1.7628961))
    (defparameter logsigmas '(-1.240931 -0.06719756 -1.4923488))
    (defparameter logws     '(-2.072121 -0.23226827 -2.509016))
    
    ; in this particular case, the max is the only "likely" value...all others
    ; are so small that log-add should disregard them
    (defparameter expected (reduce #'max (mapcar (lambda(mu sigma w) (+ w (log-normal-pdf mu sigma (car xs)))) (mapcar #'exp logmus) (mapcar #'exp logsigmas) logws)))
    (defparameter actual   (reduce #'log-add (mapcar (lambda(mu sigma w) (+ w (log-normal-pdf mu sigma (car xs)))) (mapcar #'exp logmus) (mapcar #'exp logsigmas) logws)))
    
    (assert (= expected actual)))

;(unit-tests)
;(expectation-maximization-test xs 3 1e-6)
; DOESNT @#!%@# WORK
; (defparameter logmus    (list (log 64434) (log 62344) (log 65325)))
; (defparameter mus    (mapcar #'exp logmus))
; (defparameter logsigmas (list (log 1) (log 1) (log 1)))
; (defparameter sigmas    (mapcar #'exp logsigmas))
; (defparameter logws     (list (log 1/3) (log 1/3) (log 1/3)))
; (defparameter ws    (mapcar #'exp logws))
; (defparameter xs '(65523 62352 65245 61234 64324 63245))
