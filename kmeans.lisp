; K-means Clustering

; Return a list of distance,center tuples
(defun compute-distance (centers x)
    (loop for center in centers collect (list (diff-sq center x) center)))

(defun assign-center (centers x) 
    (argmin (compute-distance centers x)))

; Given a set of centers and a point, assign each point to the one closest center
; Ties are broken by assigning x to the first center encountered
(defun assign-centers (centers xs)
    (loop for x in xs collect (list x (assign-center centers x))))

(defun estimate-center (assigned)
	(laverage (mapcar #'car assigned)))

(defun estimate-centers (assigned centers)
	(loop for c in centers
		collect (estimate-center (remove-if (complement (lambda (x)  (= (cadr x) c))) assigned))))

(defun k-means-clustering (data k epsilon)
    (let ((centers (generate-k-random k (reduce #'min data) (reduce #'max data))))
        (loop for i from 1 do
            (format t "Iteration ~d" i)
            (setf last_centers centers)
            (setf assigned (assign-centers   centers data))
            (setf centers  (estimate-centers assigned centers))
            (setf sse  (sum-squared-differences centers last_centers))
            (format t " - Change of ~f (SSD)~C" sse #\linefeed)
        while (>= sse epsilon))
        (print (mapcar #'round centers))
        (identity centers)))

