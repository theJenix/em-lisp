; Require imago to run
(ql:quickload "imago")

(load "util.lisp")
(load "kmeans.lisp")
(load "em.lisp")

(defun load-source ()
    ; Load the image into a parameter called source
    (defparameter source (imago:read-image "party_spock.png")))

(defun load-source-small ()
    ; Load the image into a parameter called source
    (defparameter source (imago:read-image "party_spock_small.png")))

(defun fix-pixel (x)
	(+  1 (+ (- x 65536) 256)))

(defun fix-image (image-flat)
    (mapcar (lambda (x) (fix-pixel x)) image-flat))

(defun unfix-pixel (x)
    (+ -1 (+ (- x 256) 65536)))

(defun unfix-image (image-flat)
    (mapcar (lambda (x) (unfix-pixel x)) image-flat))

(defun replace-pixel (image x y emparams)
	(let ((repl (em-assign emparams (fix-pixel (imago:image-pixel source x y)))))
	(setf (imago:image-pixel source x y) (unfix-pixel (round repl)))))
					 	
(defun run-em (run data k epsilon maxiter)
	(format t "Attempt ~d~C" run #\linefeed)
    (expectation-maximization
		data
		(generate-k-random-vals k data)
		epsilon
		maxiter))

(defun run-em-kmeans (run data k epsilon maxiter)
	(format t "Attempt ~d~C" run #\linefeed)
	
    (expectation-maximization
		data
		(k-means-clustering data k 1e-6) ; Generate starting mu by using k-means clustering
		epsilon
		maxiter))

(defun select-params (output)
	(let ((emparams (argmax output)))
		(format t "Selecting ~d params~C" emparams #\linefeed)
		(identity emparams)))

; Implementation of the Bayesian Information Criterion
; Source: http://arxiv.org/PS_cache/astro-ph/pdf/0701/0701113v2.pdf
(defun bic (llh k N)
	(+ (* -2 llh) (* k (log N))))

(defun run-part-2 (k epsilon maxattempts maxiter)
    (load-source)
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

    (imago:write-png source (format nil "output-em-~d.png" k))
	(with-open-file (stream (format nil "output-em-~d.bic" k) :direction :output :if-exists :supersede)
	    (format stream "~d" (bic (em-likelihood emparams) k (length image-flat)))))

(defun run-part-3-rand ()
    (load-source)
    (defparameter image-list (2d-array-to-list (imago:image-pixels source)))
    (defparameter image-flat (flatten image-list))
    (defparameter image-flat-fixed (fix-image image-flat))

	; Collect all the run results in output
	(defparameter output 
		(loop for i from 1 to 20
			collect (run-em i image-flat-fixed 3 1e-6 20)))

    ; Write out the results: each row is a run, with random initialization in the first
	; column and k-means initialization in the second column 
	(with-open-file (stream "output-part-3-rand.txt" :direction :output :if-exists :supersede)
		(mapcar (lambda (o)
				(format stream "~d~C" (em-likelihood o) #\linefeed))
			 output)))

 (defun run-part-3-km ()
     (load-source)
     (defparameter image-list (2d-array-to-list (imago:image-pixels source)))
     (defparameter image-flat (flatten image-list))
     (defparameter image-flat-fixed (fix-image image-flat))

 	; Collect all the run results in output
 	(defparameter output-km 
 		(loop for i from 1 to 20
 			collect (run-em-kmeans i image-flat-fixed 3 1e-6 20)))

     ; Write out the results: each row is a run, with random initialization in the first
 	; column and k-means initialization in the second column 
 	(with-open-file (stream "output-part-3-km.txt" :direction :output :if-exists :supersede)
 		(mapcar (lambda (o)
 				(format stream "~d~C" (em-likelihood o) #\linefeed))
 			 output-km)))


(defun hw2-main()
	(run-part-2 3 1e-6 10 20)
	(run-part-2 5 1e-6  7 20)
	(run-part-2 8 1e-6  5 20)
	
	(run-part-3-rand)
	(run-part-3-km)

	; For part 5, we want to compute the BIC and the image for
	; ks 1, 2, 4, 6, and 7.  Just use the part 2 code/routine
	; with different k values and max attempts.
	(run-part-2 1 1e-6 10 20)
	(run-part-2 2 1e-6 10 20)
	(run-part-2 4 1e-6  8 20)
	(run-part-2 6 1e-6  7 20)
	(run-part-2 7 1e-6  6 20))