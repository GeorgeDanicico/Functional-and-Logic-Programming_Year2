(defun F(G L)
  (cond
   ((null l) nil)
   (T (lambda (X)
        (cond
         ((> X 0) (cons X (F G (cdr L))))
         (t X)
         )
        ))
   )
)

; removeEvenLevels(l level) = {
;              number l, return l
;             atom l and level % 2 = 0, return null
;             atom l, return l
;             otherwise return U i=1 n, removeEvenLevels(li level+1)u

(defun removeEvenLevels(l level)
  (cond 
  ((numberp l) (list l))
  ((and (atom l) (= 0 (mod level 2))) nil)
  ((atom l) (list l))
  (t (list (apply #' append (mapcar  #'(lambda (L) (removeEvenLevels L (+ 1 level))) l))))
  )
)

(defun main(l)
  (car (removeEvenLevels l 0))
)