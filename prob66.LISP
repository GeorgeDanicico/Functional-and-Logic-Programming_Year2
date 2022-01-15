(defun listStartWithEven(l)
  (cond
   ((null l) nil)
   ((and (numberp (car l)) (= 0 (mod (car l) 2))) T)
   ((numberp (car l)) nil)
   (t (listStartWithEven (cdr l)))
   )
)

(defun countAtoms(l)
  (cond
   ((null l) 0)
   ((and (atom (car l)) (not(numberp (car l)))) (+ 1 (countAtoms (cdr l))))
   (t (countAtoms (cdr l)))
   )
)

(defun flatten(l)
  (cond
   ((atom l) (list l))
   (t (mapcan #'flatten l))
   )
)

(defun getListFromLevel(l k level)
  (cond
  ((and (atom l) (equal level k)) (list l))
   ((atom l) nil)
   (t (mapcan #'(lambda (L) (getListFromLevel L k (+ 1 level))) l))
   )
)

(defun mainGetList(l k)
  (getListFromLevel l k 0)
)

(defun check(l il level)
  (cond
   ((atom l) 0)
   ((listStartWithEven (mainGetList il (+ 1 level))) (apply #'+ (countAtoms (mainGetList il (+ 1 level))) (mapcar #'(lambda (L) (check L il (+ 1 level))) l)))
   (t (apply #'+ (mapcar #'(lambda (L) (check L il (+ 1 level))) l)))
   )
)

(defun checkMain(l)
  (check l l 0)
)