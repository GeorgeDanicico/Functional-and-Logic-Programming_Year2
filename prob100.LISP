(defun flatten(l)
  (cond
   ((atom l) (list l))
   (t (mapcan #'flatten l))
   )
)

(defun mygcd(a b)
  (cond
  ((= b 0) a)
  (t (mygcd b (mod a b)))
  )
)

(defun gcdlist(l d)
  (cond
  ((null l) d)
  (t (gcdlist (cdr l) (mygcd (car l) d)))
  )
)

(defun mylength(l)
  (cond
   ((null l) 0)
   (t (+ 1 (mylength (cdr l))))
   )
)

(defun createGcdList(l)
  (cond 
   ((< (mylength l) 3) nil)
   ((and (numberp (cadr l)) (not(numberp (car l))) (not(numberp (caddr l)))) (cons (cadr l) (createGcdList (cdr l))))
   (t (createGcdList (cdr l)))
   )
)

(defun main(l)
  (gcdlist (createGcdList (flatten l)) 0)
)

; replaceTree(L, level, e) = 
;           level%2=1, e
;           atom l, l
;           U i=1, n replaceTree(li, level+1, e) othjerwise
(defun replaceTree(l level e)
  (cond 
   ((and (atom l) (= (mod level 2) 1)) e)
   ((atom l) l)
   (t (mapcar #'(lambda (L) (replaceTree L (+ 1 level) e)) l))
   )
)

(defun mainTree(l e)
  (replaceTree l -1 e)
)