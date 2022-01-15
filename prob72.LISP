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

(defun getMaxMin(l level)
  (cond
   ((= 0 (mod level 2)) (maxim l 0))
   (t (minim l 9999))
   )
)

(defun createList(l level)
  (cond
   ((atom l) nil)
   (t (cons (getMaxMin l (+ 1 level)) (apply #'append (mapcar #'(lambda (L) (createList L (+ 1 level))) l))))
   )
) 



; problem 4

(defun removeNumbers(l)
  (cond
   ((null l) nil)
   ((numberp (car l)) (removeNumbers (cdr l)))
   (t (cons (car l) (removeNumbers (cdr l))))
   )
)

(defun insert(L N P)
  (cond
   ((null L) (list N P))
   ((< P (cadr L)) (cons N (cons P  L)))
  (t (cons (car L) (cons (cadr L) (insert (cddr L) N P))))
   )
)

(defun mainInsert(l r)
  (cond
   ((null l) r)
   (t (mainInsert (cddr l) (insert r (car l) (cadr l))))
   )
)

(defun evenLevels(l level)
  (cond
   ((and (atom l) (= 1 (mod level 2))) (list l level))
   ((atom l) nil)
   (t (mapcan #'(lambda (L) (evenLevels L (+ 1 level))) l))
   )
)

(defun main(l)
  (removeNumbers (mainInsert (evenLevels l 0) nil))
)