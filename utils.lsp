(defun pretty-print (input) 
  (princ "--------------\n(\n")
  (foreach item input
    (princ (car item))
    (princ " => ")
    (princ (cdr item))
    (princ "\n"))
  (princ ")\n--------------\n"))


(defun getval (index aList / index)
  (cdr (assoc index aList)))


(defun replace (index value lst / newlist index value lst)
  (setq newlist '()
        count 0)
  (while (< count (length lst))
    (if (= count index)
      (setq newlist (append newlist (list value)))
      ;Else
      (setq newlist (append newlist (list (nth count lst)))))
    (setq count (1+ count)))
  newlist)


(defun sublist ( lst index len / result )
    (setq len (if len 
                (min len (- (length lst) index)) 
                (- (length lst) index))
          index (+  index len))
    (repeat len 
      (setq result 
             (cons (nth 
                     (setq index (1- index)) lst) result)))
    result)


(defun matrix-replace (matrix row col lst / index )
  (setq index 0)
  (foreach item lst
    (vlax-safearray-put-element matrix row (+ col index) item)
    (setq index (1+ index))))


(defun safearray-replace (array index lst / offset)
  (setq offset 0)
  (foreach item lst
    (vlax-safearray-put-element array (+ index offset) item)
    (setq offset (1+ offset))))