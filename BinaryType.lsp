; Fichier permettant de simuler un type binaire, conversion d'integer en listes de 0 et de 1, et inversement. 
; Opérations xor, and et left shift sur ces listes.
; Beaucoup de fonctions inutiles, maintenant que je sais qu'autolisp possède (lsh) et (boole)
(defun pad-bytes (bitList targetBitCount / bitCount missingZeros)
   (setq bitCount (length bitList))
  
  (if (< bitCount targetBitCount)
    (progn 
      (setq missingZeros (- targetBitCount bitCount))
      (repeat missingZeros
        (setq bitList (cons 0 bitList)))))
  bitList)


(defun dec->bits (number / remainder bitList)
  (setq remainder number)
  (setq bitList ())

  (while (/= number 0)
    (setq remainder (rem number 2))
    (setq bitList 
           (append bitList (list remainder)))
    (setq number (/ number 2)))
  
  (reverse bitList))


(defun dec->byte (number / bitList)
  (setq bitList (dec->bits number)
        bitList (pad-bytes bitList 8))
  bitList)

(defun decs->bytes (numberList / bitList)
  (foreach number numberList 
    (setq bitList (append bitList (dec->byte number))))
  bitList)

(defun clz32 (number / bitList)
  (setq bitList (dec->bits number)
        bitList (pad-bytes bitList 32)))


(defun bin (number)
  (dec->bits number))


(defun bits->dec (bits)
  (setq x 0
        len (1- (length bits))
        total 0)
  
  (while (<= x len)
  
    (setq bit (nth x bits)
          total (+ (* total 2) bit)
          x (1+ x)))
  total)


(defun bytes->dec (input-bits / result index len temp)
  (setq result '()
        index 0
        temp '())
  
  (while (< index (length input-bits))
    
    (setq temp (append temp (list (nth index input-bits))))
    
    (if (= (rem (1+ index) 8) 0)
      (setq result (append result (list (bits->dec temp)))
            temp  '()))

    (setq index (1+ index)))
  result)



(defun print-bytes (bits)
  (setq counter 0)
  (repeat (length bits)    
    (if (= (rem counter 8) 0)
      (princ " "))
    (if (= (rem counter 40) 0)
      (princ "\n"))
    
    (princ (itoa (nth counter bits)))
    (setq counter (1+ counter))))


(defun count-bytes (bits)
  (setq counter 0)
  (repeat (length bits)
    (setq counter (1+ counter)))
  (/ counter 8))


(defun NAND (bit1 bit2)
  (cond ((and (= bit1 1) (= bit2 1)) 0)
        (t 1)))


(defun _xor (bit1 bit2)
  (cond ((and (= bit1 0) (= bit2 0)) 0)
        ((and (= bit1 1) (= bit2 1)) 0)
        (t 1)))

; left shift
(defun << (bits insert / bits)
  (repeat insert 
    (setq bits (cdr bits)
          bits (append bits '(0)))))


; performe un xor pour deux listes de bits données 
(defun xor (bits1 bits2 / count result)
  ; Nécessaire si le nombre de bit diffère    
  (if (> (length bits1) (length bits2))
    (setq count (length bits1)
          bits2 (pad-bytes bits2 count))
    (setq count (length bits2)
          bits1 (pad-bytes bits1 count)))
  
  (setq counter 0
        result ())

  (while (< counter count)
    (setq result (append result ;(boole 6 ; 6 = XOR
                        (list (_xor (nth counter bits1) (nth counter bits2)))))
    (setq counter (1+ counter)))
  
  result)
  
; xor integers
(defun ixor (int1 int2 / bit1 bit2)
  (setq bit1 (dec->bits int1)
        bit2 (dec->bits int2))
  (bits->dec (xor bit1 bit2)))