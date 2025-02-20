(defun log2 (num)
  (/ (log num) (log 2)))

(setq GALOIS:LOG '())
(setq GALOIS:EXP '())
(setq GALOIS:value 1)
(setq GALOIS:exponent 1)

(while (< GALOIS:exponent 256)

  (setq GALOIS:value 
         (if (> GALOIS:value 127)
          (ixor (* GALOIS:value 2) 285)
          ;ELSE
          (* GALOIS:value 2)))

  (setq GALOIS:LOG (append GALOIS:LOG (list (cons GALOIS:value (rem GALOIS:exponent 255))))
        GALOIS:EXP (append GALOIS:EXP (list (cons (rem GALOIS:exponent 255) GALOIS:value))))
  (setq GALOIS:exponent (1+ GALOIS:exponent)))

; a && b ? EXP[(LOG[a] + LOG[b]) % 255 : 0]
; Implémente une multiplication dans le corps fini GF(256)
; (getval) défini dans utils.lsp
(defun GALOIS:mul (a b)
  (defun _get (a b)
    (rem (+ (getval a GALOIS:LOG) (getval b GALOIS:LOG)) 255))
  
  (if (and (/= a 0) (/= b 0))
    (getval (_get a b) GALOIS:EXP)
    ;Else
    (getval 0 GALOIS:EXP)))


(defun GALOIS:div (a b)
  (defun _get (a b)
    (rem (+ 
           (getval a GALOIS:LOG) 
           (* (getval b GALOIS:LOG) 254))
          255))
  
  (getval (_get a b) GALOIS:EXP))


; On prend tous les termes de poly1 et on utilise notre GALOIS:mul pour les multiplier au premier terme de poly2
; rebelote pour le second terme de poly2 et ainsi de suite...
; On obtien notre coefficient en effectuant un xor du coefficient précédent et du résultat de GALOIS:Mul jusqu'à ce que p1Index > index
(defun polynomial-mul (poly1 poly2 / index)
  (setq coefficents '()
        coeffsLength (1- (+ (length poly1) (length poly2))) 
        index 0)
  
  (while (< index coeffsLength)
    (setq coeff 0
          p1Index 0)
    (while (<= p1Index index)
      (setq p2Index (- index p1Index))
      (if (and (nth p1Index poly1) (nth p2Index poly2)) ; Dans certains cas ces appels à (nth) peuvent être au dessus de la longeur des polynômes, on vérifie
        (setq coeff (boole 6 coeff ; 6 = XOR
                       (GALOIS:mul (nth p1Index poly1)
                                   (nth p2Index poly2)))))

      (setq p1Index (1+ p1Index)))
    (setq coefficents (append coefficents (list coeff)))
    (setq index (1+ index)))
  coefficents)


(defun poly-edc-rest (data divisor / b data result lst coeff index factor )
  ; Crée une liste result de taille (length divisor) remplis de 0
  ; Pourrait utiliser un vlax-safearray plutôt qu'une liste... 
  (setq result (repeat (length divisor)
                          (setq lst (append lst '(0)))))
  (foreach b data
    (setq factor (boole 6 b (car result)) 
          result (cdr result)
          result (append result '(0))
          index  0)
    (foreach coeff divisor
      ; on remplace result[i] par result[i] xor (coeff * factor)  
      ; Voir fonction "replace" dans utils.lsp
      (setq result (replace index 
                      (boole 6 (nth index result)
                            (GALOIS:MUL coeff factor)) 
                      result)
            index (1+ index))))
  result)



(defun poly-rest (dividend divisor / quotientLen rest factor subtr index rest item map-rest)
  (setq quotientLen (1+ (- (length dividend) (length divisor)))
        rest        dividend)

  (repeat quotientLen
    (if (/= (car rest) 0)
      (progn
        (setq factor (GALOIS:DIV (car rest) (car divisor))
              subtr  (vlax-make-safearray vlax-vbInteger (cons 0 (1- (length rest)))))
        
        (safearray-replace subtr 0 (polynomial-mul divisor (list factor)))
        (setq index  0
              rest   (foreach item rest
                        (setq map-rest 
                              (append map-rest
                                      (list (boole 6 item (vlax-safearray-get-element subtr index))))
                              index (1+ index)))
              rest    (cdr rest)))
      ;Else
      (setq rest (cdr rest))))
  rest)



; degree correspond au nombre de modules à encoder, dépend de la version du QRcode
; Lent, sachant que l'on souhaite se limiter à une version 3 avec une qualité de correction d'erreur M, le polynome sera précalculé et accessible dans une variable.
(defun polynomial-generator (degree / index)
  (setq index 0
        lastPoly '(1))
  (while (< index degree)
    (setq lastPoly (polynomial-mul lastPoly (list 1 (getval index GALOIS:EXP))))
    (setq index    (1+ index)))
  (cdr lastPoly))

