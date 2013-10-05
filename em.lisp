; EM for estimating mixtures of gaussians using log probabilities

; Need the elog functions
(load "elog.lisp")

; The normal distribution, given mu and sigma and x.
(defun normal-pdf (mu sigma x)
    (*  (inv (* sigma (sqrt 2pi)))
        (exp (- (/ (diff-sq x mu) (* 2 (expt sigma 2)))))))

; This computes the log of the normal pdf, used for managing underflow
; It takes in the log of mu, sigma, and the datapoint.
(defun log-normal-pdf (logmu logsigma logpt)
    (let ((mu    (eexp logmu))
    	  (sigma (eexp logsigma))
          (pt    (eexp logpt))) 
        (-  (* -0.5 (elog (* 2 pi (expt sigma 2))))
            (/ (diff-sq pt mu) (* 2 (expt sigma 2))))))


; Define a set of operators that will be used in the EM equations
; These operator definitions allow us to use the same functions
; when working with either log or standard numbers

; Standard functions
; (setf (symbol-function 'sum) #'+)
; (setf (symbol-function 'diff) #'-)
; (setf (symbol-function 'div) #'/)
; (setf (symbol-function 'mult) #'*)
; (setf (symbol-function 'pdf) #'normal-pdf)
; (setf (symbol-function 'pow) #'expt)
; (setf (symbol-function 'll-ln) #'log)

; Log functions
(setf (symbol-function 'sum)  #'elogsum)
(setf (symbol-function 'diff) #'elogdiff)
(setf (symbol-function 'div)  #'elogdivide)
(setf (symbol-function 'mult) #'elogproduct)
(setf (symbol-function 'pdf)  #'log-normal-pdf)
(setf (symbol-function 'pow)  #'elogpow)
(setf (symbol-function 'll-ln) #'identity)

; Compute the marginal distribution f(x) as mixture
; of gaussians
;   mus: The centers of n gaussians to use
;   sigmas: The standard deviations of n gaussians to use
;   ws: The weights to apply to each gaussian in the mixture 
(defun marginal (mus sigmas ws x)
    (reduce #'sum (mapcar (lambda(mu sigma w) (mult w (pdf mu sigma x))) mus sigmas ws)))

(defun normalize (vec)
    (let ((sum (reduce #'sum vec)))
        (mapcar (lambda (e) (div e sum)) vec)))

; mus = K x 1 list of possible mu values
; sigmas = K x 1 list of possible sigmas
; ws = N x 1 list of weights
; xs = N x 1 list of data points
; Return a N x K list of gamma, which is P(z_k|X) for each x_n
(defun e-step (mus sigmas ws xs)
    (mapcar (lambda (mu sigma w)
	    (mapcar (lambda (x)
                (div (mult w (pdf mu sigma x))
                     (marginal mus sigmas ws x)))
            xs))
        mus sigmas ws))

(defun m-step-nk (gammas)
    (mapcar (lambda (gammak) (reduce #'sum gammak)) gammas))

    ; (reduce #'sum (mapcar (lambda (g x) (mult g (elog x))) gammak xs)))
(defun m-step-mu (gammas nks xs)
; (print "Entering m-step-mu")
	(mapcar #'div
		(mapcar (lambda (gammak)
            (reduce #'sum (mapcar (lambda (g x) (mult g x)) gammak xs)))
			 	gammas)
		nks))

        ; (reduce #'sum (mapcar #'mult gammak (mapcar (lambda(x) (pow (diff (elog x) muk) 2)) xs))))
(defun m-step-sigma (gammas nks xs newmus)
; (print "Entering m-step-sigma")
	(mapcar (lambda (x) (pow x 0.5))
	    (mapcar #'div
	        (mapcar (lambda (gammak muk)
			; NOTE: this doesnt work for some reason...need to investigate later
            ; (let ((diffsq (mapcar (lambda(x) (pow (diff x muk) 2))) xs)))
	            (let ((diffsq (mapcar (lambda(x) (elog (expt (- (eexp x) (exp muk)) 2))) xs)))
	                (reduce #'sum (mapcar #'mult gammak diffsq))))
	            gammas newmus)
	        nks)))

(defun m-step-w (nks n)
    (mapcar (lambda (e) (div e n)) nks))
	
(defun log-likelihood (mus sigmas ws xs)
; (print "Entering log-likelihood")
	(reduce #'+
		(mapcar (lambda (x)
	    	(ll-ln (reduce #'sum
				(mapcar (lambda (mu sigma w)
                         ; (format t "~d ~d ~d ~d: ~d~C" mu sigma w x (mult w (pdf mu sigma x)) #\linefeed)
		        		 (mult w (pdf mu sigma x)))
					mus sigmas ws))))
		    xs)))

(defun em-likelihood (emparams)
	(fourth emparams))

; A lambda that when applied to lists of mus, sigmas and ws
; will make tuples that can be used to determine the most
; likely component that holds lv 
(defun make-tuple-fn (lv)
	(lambda (mu sigma w)
		(list (elogproduct w (log-normal-pdf mu sigma lv)) mu)))

(defun em-assign (emparams value)
	(let ((mus    (first  emparams))
  		  (sigmas (second emparams))
		  (ws     (third  emparams))
		  (lv     (elog   value))
		  (tuples))
		 (setf tuples (mapcar (make-tuple-fn lv) mus sigmas ws))
		 ; (format t "Mapping ~d to ~d~C" value (eexp (argmax tuples)) #\Linefeed)
		 (eexp (argmax tuples))))

(defun expectation-maximization (data starts epsilon maxiter)
    (let ((logdata (mapcar #'log data)))

		(setf k       (length starts))
		(setf mus     (mapcar #'log starts))
        (setf sigmas  (loop for i below k collect (log 1)))
        (setf ws      (normalize (loop for i below k collect 1)))
        (setf oldllh  1)

        (loop for i from 1 to maxiter do
            (format t "Iteration ~d" i)
            (format t " E ")
            (setf gammas (e-step mus sigmas ws logdata))
            
            (format t " M nks")
            (setf nks    (m-step-nk gammas))
            (format t " M mus")
            (setf mus    (m-step-mu gammas nks logdata))
            (format t " M sigmas")
            (setf sigmas (m-step-sigma gammas nks logdata mus))
            (format t " M ws")
            (setf ws     (m-step-w nks (log (length data))))
            (format t " LL ")
            (setf llh    (log-likelihood mus sigmas ws logdata))
            (setf relerror (abs (- 1.0 (/ llh oldllh))))
            (setf oldllh llh)
            (format t "- ll: ~d, relative error: ~f~C" llh relerror #\linefeed)
        while (>= relerror epsilon))
        (format t "mus ~d, sigmas ~d, ws ~d~C" mus sigmas ws #\linefeed)
        (list mus sigmas ws oldllh)))