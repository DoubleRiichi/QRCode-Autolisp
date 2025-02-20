; Défini un bloc représentant notre QR-CODE
(defun draw-Qrcode (qrCode)
  (entmake (list
          '(0 . "BLOCK")
          '(2 . "QR-CODE-NOMDATE")
          '(8 . "0")
          '(70 . 0)
          '(10 0.0 0.0 0.0)))
  (setq y 0)
  (foreach row qrCode
    (setq x 0)
    (foreach col row
      (if (= col 1)
        (progn
          (entmake (list 
                   (cons 0 "SOLID")
                   (cons 10 (list x y))
                   (cons 11 (list (1+ x) y))
                   (cons 12 (list x (1- y)))
                   (cons 13 (list (1+ x) (1- y)))
                   (cons 62 0)))))
      (setq x (1+ x)))
  (setq y (1+ y)))
  (setq bl_a (entmake '((0 . "ENDBLK"))))
  T)

