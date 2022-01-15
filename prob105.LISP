
(defun maxim(l m)
  (cond
   ((null l) m)
   ((and (numberp (car l)) (> (car l) m)) (maxim (cdr l) (car l)))
   (t (maxim (cdr l) m))
   )
)

(defun minim(l m)
  (cond
   ((null l) m)
   ((and (numberp (car l)) (< (car l) m)) (minim (cdr l) (car l)))
   (t (minim (cdr l) m))
   )
)

(defun compDif(l)
  (- (maxim l 0) (minim l 9999))
)

(defun replaceAll(l)
  (cond
   ((atom l) nil)
   (t (list (cons (compDif l) (apply #'append (mapcar #'replaceAll l)))))
   )
)

(defun mainReplace(l)
  (car (replaceAll l))
)

; replaceEven(L) = 
;       L + 1, L is number and L % 2 = 1
;       L, L is atom
;      U i=1, n replaceEven(L), otherwise
(defun replaceEven(l)
  (cond
   ((and (numberp l) (= 0 (mod l 2))) (+ 1 l))
   ((atom l) l)
   (t (mapcar #'replaceEven l))
   )
)