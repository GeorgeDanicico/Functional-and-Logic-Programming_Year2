;-------------------------------
;10.a)
(defun product(l)
       (cond
        ((null l) 1)
        ((numberp (car l)) (* (car l) (product (cdr l))))
        (t (* 1 (product (cdr l))))
        )
)
;--------------------------------
;10.b)
(defun replaceFO(l e o)
  (cond
   ((null l) nil)
   ((equal (car l) e) (cons o (cdr l)))
   (t (cons (car l) (replaceFO (cdr l) e o)))
   )
)
;--------------------------------
;10.c)
(defun reverseList(l rl)
	(cond
		((null l) rl)
		(t (reverseList (cdr l) (cons (car l) rl)))
	)
)

(defun compute(op e1 e2)
	
	(cond
		((equal op '+) (+ e1 e2))
		((equal op '-) (- e1 e2))
		((equal op '*) (* e1 e2))
		(t (/ e1 e2))
	)
)

(defun parseExp(exp stack)
	(cond
		((null exp) (car stack))
		((numberp (car exp)) (parseExp (cdr exp) (cons (car exp) stack)))
		(t (parseExp (cdr exp) (cons (compute (car exp) (car stack) (car (cdr stack))) (cdr (cdr stack))) ))
	)
)

(defun mainParse(exp)
	 (parseExp (reverseList exp nil) nil)
)
;------------------------------------
;10.d)
(defun countOcc(l e)
  (cond
   ((null l) 0)
   ((equal (car l) e) (+ 1 (countOcc (cdr l) e)))
   (t (countOcc (cdr l) e) )
   )
)

(defun deleteOcc(l e)
	(cond 
		((null l) nil)
		((equal (car l) e) (deleteOcc (cdr l) e))
		(t (cons (car l) (deleteOcc (cdr l) e)))
	)
)

(defun pairs(l)
	(cond
		((null l) nil)
		((atom (car l)) (cons (list (car l) (countOcc l (car l))) (pairs (deleteOcc l (car l)))))
		(t (pairs (cdr l)))
	)
)



