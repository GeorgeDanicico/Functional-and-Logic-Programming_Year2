; removeNumbers(l, level)
;               (l), level mod 2 = 0
;              nil, level mod 2 = 1 && number l && l mod 2 = 0
;              (l), l is atom
;              U i=1, n,  removeNumbers(ln, level) otherwise

(defun removeNumbers(l level)
  (cond
   ((and (numberp l) (= (mod level 2) 1) (= (mod l 2) 0)) nil)
   ((atom l) (list l))
   (t (list (apply #'append (mapcar #'(lambda (a) (removeNumbers a (+ 1 level))) l))))
   )
)

(defun mainRemove(l)
  (car (removeNumbers l 0))
)