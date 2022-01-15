(defun flattenOddLevels(l level)
  (cond
   ((and (= 1 (mod level 2)) (numberp l)) (list l))
   ((atom l) nil)
   (t (mapcan #'(lambda (L) (flattenOddLevels L (+ 1 level))) l))
   )
)

(defun zigzag(l n f)
  (cond
   ((null (cdr l)) t)
   ((and (= f 0) (>= (car l) n)) (zigzag (cdr l) (car l) 1))
   ((and (= f 1) (<= (car l) n)) (zigzag (cdr l) (car l) 0))
   (t nil)
   )
)

(defun mainZigzag(l)
  (cond
   ((< (car l) (cadr l)) (zigzag (cdr l) (car l) 0))
   (t nil)
   )
)

(defun main(l)
  (mainZigzag (flattenOddLevels l 0))
)

; problem 4
; replaceAll (l, e, e1) = 
;      atom l and l = e, return e1
;      atom l, return l
; otherwise return U i=1, n replaceAll(li, e e1).

(defun replaceAll(l e e1)
  (cond
   ((numberp l) l)
   ((and (atom l)  (equal l e)) e1)
   ((atom l) l)
   (t (mapcar #'(lambda (L) (replaceAll L e e1)) l))
   )
)
