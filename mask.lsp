; Selon le standard, il serait judicieux d'implémenter un algorithme de choix de masque en calculant un score de pénalité pour chacun des masques avec un qr code donné...
(defun floor (x / n)
  (if (or (= (setq n (fix x)) x) (< 0 x))
    n
    (1- n)))

(defun mask0-formula (row col)
  (if (= (rem (+ row col) 2) 0)
    1
    0))
  
(defun mask1-formula (row)
  (if (= (rem row 2) 0)
    1
    0))

(defun mask2-formula (col)
  (if (= (rem col 3) 0)
    1
    0))

(defun mask3-formula (row col)
  (if (= (rem (+ row col) 3) 0)
    1
    0))

(defun mask4-formula (row col)
  (if (= (rem (+ (floor (/ row  2)) 
          (floor (/ col 3))) 
      2) 0)
    1
    0))

(defun mask5-formula (row col)
  (if (= (+ (rem (* row col) 2)
            (rem (* row col) 3)) 
      0)
    1
    0))

(defun mask6-formula (row col)
  (if (= (rem (+ (rem (* row col) 2)
                 (rem (* row col) 3)) 
        2)
      0)
    1
    0))


(defun mask7-formula (row col)
  (if (= (rem (+ (rem (+ row col) 2)
              (rem (* row col) 3)) 
        2)
      0)
    1
    0))



(defun mask-formula (maskIndex row col)
  (cond ((= maskIndex 0) 
          (mask0-formula row col))
        ((= maskIndex 1) 
          (mask1-formula row col))
        ((= maskIndex 2) 
          (mask2-formula row col))
        ((= maskIndex 3) 
          (mask3-formula row col))
        ((= maskIndex 4) 
          (mask4-formula row col))
        ((= maskIndex 5) 
          (mask5-formula row col))
        ((= maskIndex 6) 
          (mask6-formula row col))
        ((= maskIndex 7) 
          (mask7-formula row col))))