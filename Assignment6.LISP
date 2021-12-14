;  leftSubtree(L1L2...Ln, v - vertices, c - edges counter) = 
;            - if n = 0 then return []
;            - if v = c + 1 then return [] -> it means we reached the bottom of the left subtree
;            - otherwise return L1 U L2 U leftSubtree(L3...Ln, v + 1, c + L2)

(defun leftSubtree(l v c)
  (cond 
   ((null l) nil)
   ((= v (+ c 1)) nil)
   (t (cons (car l) (cons (cadr l) (leftSubtree (cddr l) (+ v 1) (+ c (cadr l))))))
   )
)

;  left(L1L2L3...Ln) = { leftSubtree(L3...Ln); } -> we remove the root of the tree and its number of subtrees.

(defun left(l) 
  (leftSubtree (cddr l) 0 0)
)

;  rightSubtree(L1L2...Ln, v - vertices, c - edges counter) = 
;            - if n = 0 then return []
;            - if v = c + 1 then return L1L2...Ln -> it means we reached the bottom of the left subtree, and
;                                                                        we are already on the right subtree
;            - otherwise return rightSubtree(L3...Ln, v + 1, c + L2)

(defun rightSubtree (l v c)
  (cond
   ((null l) nil)
   ((= v (+ c 1)) l)
    (t  (rightSubtree (cddr l) (+ v 1) (+ c (cadr l))))
   )
)

;  right(L1L2L3...Ln) = { rightSubtree(L3...Ln) } -> we remove the root of the tree and its number of subtrees.

(defun right (l)
  (rightSubtree (cddr l) 0 0)
)

;  appendList(L1L2...Ln, V1V2...Vm) = 
;                       - if n = 0 then return V1V2...Vm
;                       - if m = 0 then return L1L2...Ln
;                       - otherwise return L1 U appendList(L2...Ln, V1V2...Vm)

(defun appendList (l1 l2)
  (cond 
   ((null l1) l2)
   ((null l2) l1)
   (t (cons (car l1) (appendList (cdr l1) l2)))
   )
)

;  removeNumbers(L1L2...Ln) = 
;                     - if n = 0 then return []
;                     - if number(L1) then return removeNumbers(L2...Ln)
;                     - otherwise return L1 U removeNumbers(L2...Ln)

(defun removeNumbers(l)
  (cond
   ((null l) nil)
   ((numberp (car l)) (removeNumbers (cdr l)))
   (t (cons (car l) (removeNumbers (cdr l))))
   )
)

; postOrder(L1L2...Ln) =
;                   - if n = 0 then return []
;                   - otherwise return appendList(left(L1L2...Ln), (appendList (right(L1L2...Ln), L1)))

(defun postOrder (l)
  (cond
   ((null l) nil)
   (t (appendList (left l) (appendList (right l) (list (car l)))))
   )
)

(defun main(l)
  (removeNumbers (postOrder l))
)
