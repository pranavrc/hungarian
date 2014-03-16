;;; Common Lisp implementation of the Kuhn-Munkres (Hungarian) algorithm.
;;; http://en.wikipedia.org/wiki/Hungarian_algorithm

(defun order-of-matrix (matrix)
  "Given a regular matrix, return its order as a cons (r c)."
  (list (length matrix) (length (car matrix))))

(defun pad-matrix (matrix)
  "Pad non-square matrices with zeros to make them square."
  (let* ((pad-num (- (first (order-of-matrix matrix))
                     (second (order-of-matrix matrix)))))
    (cond ((= pad-num 0) matrix)
          ((> pad-num 0)
           (mapcar #'(lambda (list)
                       (append list (make-list pad-num :initial-element 0))) matrix))
          ((< pad-num 0)
           (append matrix (make-list (abs pad-num) :initial-element
                                     (make-list (length (car matrix)) :initial-element 0))))
          (t matrix))))
