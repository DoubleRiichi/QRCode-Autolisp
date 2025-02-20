(defun get-Qrsize (QrVersion)
  (+ (* QrVersion 4) 17))


; Crée une matrice de taille qrsize*qrsize (en l'occurence pour version 3, 29*29)
; Un safearray est utilisé car modifier un élément à posion [Y][X] est plus intuitif, et évite d'avoir à copier ou reconstruire la liste.
(defun get-QrMatrix (QrVersion / len matrix)
    (setq len    (get-Qrsize QrVersion)
          matrix (vlax-make-safearray vlax-vbInteger (cons 0 (1- len)) (cons 0 (1- len))))
    matrix)

; 
; Insère *bit* dans matrix, aux positions row col 
(defun fill-area (matrix row col width height bit / lst fillRow rowIndex colIndex)
  (setq fillRow (repeat width (setq lst (append lst (list bit))))
        rowIndex   row)

  (while (< rowIndex (+ row height))
    (setq colIndex col)

    (repeat width
          (vlax-safearray-put-element matrix rowIndex colIndex bit)
          (setq colIndex (1+ colIndex)))
    (setq rowIndex (1+ rowIndex))))

; Si le support est limité à la version 3, on peut pré-construire la matrice pour optimiser les performances.
(defun get-module-seq (QrVersion)
  (setq len    (get-Qrsize QrVersion)
        matrix (get-QrMatrix QrVersion))

  ; Finder patterns + diviseurs
  (fill-area matrix 0 0 9 9 1)
  (fill-area matrix 0 (- len 8) 8 9 1)
  (fill-area matrix (- len 8) 0 9 8 1)

  ; Alignement pattern
  (fill-area matrix (- len 9) (- len 9) 5 5 1)
  
  ; Timing pattern
  (fill-area matrix 6 9 (* QrVersion 4) 1 1)
  (fill-area matrix 9 6 1 (* QrVersion 4) 1)
  ; Dark module
  (vlax-safearray-put-element matrix (- len 8) 8 1)
  
  ; à partir de la matrice, on construite une list de coordonnées (row col) dans qui nous donnera les positions et l'ordre d'insertion des modules 
  (setq rowStep -1
        row (1- len)
        col (1- len)
        sequence '()
        index 0)
  
  (while (>= col 0)
    (if  (= (vlax-safearray-get-element matrix row col) 0)
      (setq sequence (append sequence (list (list row col)))))

    (if (= (boole 1 index 1) 1)
      (progn 
        (setq row (+ row rowStep))
          (if (or (= row -1) (= row len))
            (progn
              (setq rowStep (- rowStep)
                    row (+ row rowStep)
                    col (- col (if (= col 7) 2 1)))) ; if col == 7: 2 else 1
            ;Else 
            (setq col (1+ col))))
        ;Else
        (setq col (1- col)))
    (setq index (1+ index))
  sequence))

(defun debug-print (matrix)
  (foreach lst matrix
    (foreach item lst
      (princ item)
      (princ " "))
    (princ "\n"))
  (princ))


(defun finder-patterns (matrix size)
  (fill-area matrix 0 0 7 7 1)
  (fill-area matrix 1 1 5 5 0)
  (fill-area matrix 2 2 3 3 1)

  (fill-area matrix (- size 7) 0 7 7 1)
  (fill-area matrix (- size 6) 1 5 5 0)
  (fill-area matrix (- size 5) 2 3 3 1)
  
  (fill-area matrix 0 (- size 7) 7 7 1)
  (fill-area matrix 1 (- size 6) 5 5 0)
  (fill-area matrix 2 (- size 5) 3 3 1))


(defun separator-patterns (matrix size)
  (fill-area matrix 7 0 8 1 0)
  (fill-area matrix 0 7 1 7 0)
  (fill-area matrix (- size 8) 0 8 1 0)
  (fill-area matrix 0 (- size 8) 1 7 0)
  (fill-area matrix 7 (- size 8) 8 1 0)
  (fill-area matrix (- size 7) 7 1 7 0))


(defun alignement-patterns (matrix size)
  (fill-area matrix (- size 9) (- size 9) 5 5 1)
  (fill-area matrix (- size 8) (- size 8) 3 3 0)
  (vlax-safearray-put-element matrix (- size 7) (- size 7) 1))


(defun timing-patterns (matrix size / pos)
  (setq pos 8)
  (while (< pos (+ (* version 4) 8))
      (vlax-safearray-put-element matrix 6   pos 1)
      (vlax-safearray-put-element matrix 6   (1+ pos) 0)
      (vlax-safearray-put-element matrix pos 6 1)
      (vlax-safearray-put-element matrix (1+ pos) 6 0)
      (setq pos (+ pos 2)))
  (vlax-safearray-put-element matrix 6 (- size 7) 1)
  (vlax-safearray-put-element matrix (- size 7) 6 1)
  (vlax-safearray-put-element matrix (- size 8) 8 1))

(defun dark-module (qrCode size)
  (vlax-safearray-put-element qrCode (- size 8) 8 1))


(defun add-fixed-patterns (matrix size)
  (finder-patterns matrix size)
  (separator-patterns matrix size)
  (alignement-patterns matrix size)
  (timing-patterns matrix size)
  (dark-module matrix size))


(defun get-masked-matrix (version codewords maskIndex / sequence matrix index bit row col)
  (setq sequence (get-module-seq version)
        matrix   (get-QrMatrix version)
        index 0)
  (foreach bit codewords
    (setq row (car (nth index sequence))
          col (car (cdr (nth index sequence))))
    (vlax-safearray-put-element matrix row col 
      ;Xor of current bit and given mask
      (boole 6 bit (mask-formula maskIndex row col)))
    (setq index (1+ index)))
  matrix)


(defun add-format-modules (matrix formatMasked / ubound index bit)
  (setq ubound (vlax-safearray-get-u-bound matrix 2)
        index  0) ; aura peut être besoin d'être incrémenté
  (matrix-replace matrix 8 0 (sublist formatMasked 0 6))
  (matrix-replace matrix 8 7 (sublist formatMasked 6 2))
  (matrix-replace matrix 8 (- ubound 8) (sublist formatMasked 7 nil))
  (vlax-safearray-put-element matrix 7 8 (nth 8 formatMasked))
    
  (foreach bit (sublist formatMasked 0 7)  
    (vlax-safearray-put-element matrix (- ubound index) 8 bit)
    (setq index (1+ index)))
  
  (setq index 0)
  
  (foreach bit (sublist formatMasked 9 nil)
    (vlax-safearray-put-element matrix (- 5 index) 8 bit)
    (setq index (1+ index))))