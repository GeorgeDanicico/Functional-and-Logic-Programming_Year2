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

(defun flattenList(l level)
  (cond 
   ((and (numberp l) (= 0 (mod level 2)) (= 1 (mod l 2))) (list l))
   ((atom l) nil)
   (t (mapcan #'(lambda (L) (flattenList L (+ 1 level))) l))
   )
)

(defun solve(l)
  (gcdlist (flattenList l 0) 0)
)


; problem 4
(defun maxheight(l height)
  (cond
   ((atom l) (list height))
   (t  (mapcan #'(lambda (L) (maxheight L (+ 1 height))) l))
   )
)

(defun getmaxheight(l)
  (apply #'max (maxheight l 0))
)

(defun getNodeHeight(l level h n)
  (cond
   ((and (atom l) (equal l n)) (list (- h level)))
   ((atom l) nil)
   (t (mapcan #'(lambda (L) (getNodeHeight L (+ 1 level) h n)) l))
   )
)

(defun main(l n)
  (setq N (car (getNodeHeight l 0 (getmaxheight l) n)))

  (cond
   ((null N) -1)
   (t N)
   )
)

