(defun digitsSum(l)
  (cond
   ((= l 0) 0)
   (t (+ (mod l 10) (digitsSum (+ (floor l 10)))))
   )
)

(defun repl(L level)
  (cond
   ((and (numberp L) (= 1 (mod level 2)))  (digitsSum L))
   ((atom L) L)
   (t (mapcar #'(lambda (a) (repl a (+ 1 level))) L))
   )
)

(defun removeM3(l)
  (cond
   ((and (numberp l) (= 0 (mod l 3))) nil)
   ((atom l) (list l))
   (t (list (apply #'append (mapcar #'removeM3 l))))
   )
)

(defun removeMain(l)
  (car (removeM3 l))
)