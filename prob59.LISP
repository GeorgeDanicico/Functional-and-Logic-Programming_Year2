(DEFUN F(N)
  (COND
   ((= N 1) 1)
  (T (- (F (- N 1)) 1))
)
)

(defun replaceAll(l level e)
  (cond
   ((and (atom l) (= 1 (mod level 2))) e)
   ((atom l) l)
   (t (mapcar #'(lambda (L) (replaceAll L (+ 1 level) e)) l))
   )
)