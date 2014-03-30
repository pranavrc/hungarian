;;; Common Lisp implementation of the Kuhn-Munkres (Hungarian) algorithm.
;;; http://en.wikipedia.org/wiki/Hungarian_algorithm

(defun order-of-matrix (matrix)
  "Given a regular matrix, return its order as a cons (r c)."
  (cons (length matrix) (length (car matrix))))

(defun max-in-matrix (matrix)
  "Find the largest element in the matrix."
  (apply #'max (mapcar #'(lambda (list) (apply #'max list)) matrix)))

(defun pad-matrix (matrix)
  "Pad non-square matrices with zeros to make them square."
  (let* ((order (order-of-matrix matrix))
         (pad-num (- (car order) (cdr order)))
         (max-element (max-in-matrix matrix)))
    (cond ((= pad-num 0) matrix)
          ((> pad-num 0)
           (mapcar #'(lambda (list)
                       (append list (make-list pad-num :initial-element max-element))) matrix))
          ((< pad-num 0)
           (append matrix (make-list (abs pad-num) :initial-element
                                     (make-list (length (car matrix)) :initial-element max-element))))
          (t matrix))))

(defun reduce-rows (matrix)
  "Reduce all elements in each row by the lowest element in the row."
  (mapcar #'(lambda (each-row)
	      (let* ((min-of-row (apply #'min each-row)))
		(mapcar #'(lambda (each-element)
			    (- each-element min-of-row)) each-row))) matrix))

(defun reduce-columns (matrix)
  "Reduce all elements in each column by the lowest element in the column."
  (let* ((transpose (apply #'mapcar #'list matrix)))
    (apply #'mapcar #'list (reduce-rows transpose))))

(defun find-zeros (matrix)
  "Return positions of all zeros in matrix."
  (let* ((zeros '())
         (row-count 0)
         (total-zeros 0)
         (zero-count (make-array (list 2 (car (order-of-matrix matrix))) :initial-element 0)))
    (mapcar #'(lambda (each-row)
                (let* ((column-count 0))
                  (mapcar #'(lambda (each-element)
                              (cond
                                ((eql each-element 0)
                                 (incf total-zeros)
                                 (incf (aref zero-count 0 row-count))
                                 (incf (aref zero-count 1 column-count))
                                 (setq zeros (nconc zeros (list (cons row-count column-count))))
                                 (incf column-count))
                                (t (incf column-count))))
                          each-row)
                  (incf row-count)))
            matrix)
    (list :count zero-count :zeros zeros :total-zeros total-zeros)))
