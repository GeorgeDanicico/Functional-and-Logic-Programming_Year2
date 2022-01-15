(defun flattenEven(l)
  (cond
   ((and (numberp l) (= 0 (mod l 2))) (list l))
   ((atom l) nil)
   (t (mapcan #'flattenEven l))
   )
)

(defun flattenOdd(l)
  (cond
   ((and (numberp l) (= 1 (mod l 2))) (list l))
   ((atom l) nil)
   (t (mapcan #'flattenOdd l))
   )
)

(defun mergeLists(l1 l2)
  (cond 
   ((null l1) nil)
   (t (cons (car l1) (cons (list (car l2)) (mergeLists (cdr l1) (cdr l2)))))
   )
)

(defun main(l)
  (mergeLists (flattenEven l) (flattenOdd l))
)

; problem 4
(defun replaceAll(l e)
  (cond
   ((numberp l) e)
   ((atom l) l)
   (t (mapcar #'(lambda (L) (replaceAll L e)) l))
)
)
