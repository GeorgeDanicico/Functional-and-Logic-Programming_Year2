(defun replaceAll(l level)
  (cond
   ((and (atom l) (= 0 (mod level 2))) 0)
   ((atom l) l)
   (t (mapcar #'(lambda (L) (replaceAll L (+ 1 level))) l))
   )
)