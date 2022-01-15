(defun replacePred(l k)
  (cond
   ((and (numberp l) (>= l k)) (- l 1))
   ((numberp l) l)
   ((atom l) l)
   (t (mapcar #'(lambda (a) (replacePred a k)) l))
   )
)

(defun triple(x)
  (cond
   ((numberp x) (* x 3))
   ((atom x) x)
   (t (mapcar #'triple x))
   )
)