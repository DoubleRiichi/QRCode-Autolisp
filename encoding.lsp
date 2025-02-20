
; Dans notre cas, nous n'avons besoin que d'encoder en mode Byte (0100) ce qui correspond à des caractères ISO/CEI 8859-1
; Nous supposons deux niveaux de correction d'erreur dans un soucis de simplicité, "M" et "L" selon la taille de la string en entrée. 55 caractères est la limite
; ce qui nous donne 55 'codewords' (ou byte, correspondant à 55 caractères) pour un Qr Code de version 3 et de niveau de correction L.
; Nous souhaitons encoder le nom du .dwg + un séparateur + la date et heure d'enregistrement.
; https://www.thonky.com/qr-code-tutorial/character-capacities
; https://www.thonky.com/qr-code-tutorial/error-correction-table

(setq byte-encoding '(0 1 0 0) ; ISO/CEI 8859-1
      ; Le nombre de bits utilisés pour donner la longueur de la string à encoder varie selon le type d'encodage et la version du qr code. 
      length-bits 8 ; version 3 + error correction M/L = 8. 
      termination-block '(0)
      ; Ne me demandez pas pourquoi ce n'est pas dans l'ordre
      EC-level (list  '(L . 1)
                      '(M . 0)
                      '(Q . 3)
                      '(H . 2)))


; Voir ISO-8859-1.lsp
; Encode une string en listes de bits avec un padding de 8
(defun encode-string (string / stringList stringBits char isoChar)
  (setq stringList (vl-string->list string)
        stringBits ())
  (foreach char stringList
    (setq isoChar (ISO:get (chr char))
          stringBits (append stringBits 
                             (pad-bytes (bin isoChar) 8))))
  stringBits)


(defun free-codewords (bits dataCapacity)
  (- dataCapacity (count-bytes bits)))


(defun free-bits (bits bitsCapacity)
  (- bitsCapacity (length bits)))


; Ajoute 236 puis 17 jusqu'à ce que le nombre de codewords est égale à la capacité maximale
; @param codewords    - liste de codewords sous forme de bits
; @param dataCapacity - la capacité maximale de la version du QrCode et niveau d'EC choisi (voir tables.lsp)
; @return nouvelle liste de codewords sous forme de bits
(defun fill-remaining (codewords dataCapacity / repeatCount counter)
  (setq repeatCount (free-codewords codewords dataCapacity)
        counter 0)
  (while (< counter repeatCount)
    (if (= (rem counter 2) 0)
      (setq codewords (append codewords (dec->byte 236)))
      ;else
      (setq codewords (append codewords (dec->byte 17))))
   (setq counter (1+ counter)))
  codewords)

; Récupère les modules de correction d'erreurs en divisant nos codewords par un polynome générateur
; voir math.lsp et tables.lsp
; @param Data: notre liste de codewords (encoding_mode + length-bits + encoded_string + terminator 0s + ) au format décimal 
; @param GeneratorPoly: polynome générateur
; @return Liste de modules de corrections d'erreurs au format décimal
(defun get-EDC (data generatorPoly / degree )
  (poly-edc-rest data generatorPoly))


(defun padding (len / result)
  (setq result '())
  (repeat len
    (setq result (append result '(0))))
  result)


; Ajoute des 0 tant que le nombre de bits libres n'est pas un multiple de 8
; @param len - la taille de la liste de codewords (binaire)
; @return le nombre de 0 nécessaires
(defun fill-0 (len / result)
  (setq result '())

    (while (/= (rem len 8) 0)
      (setq result (append result '(0))
            len (1+ len)))
  result)


; Crée la liste de données encodée : encoding_type + length-bits + encoded_string + terminating 0s + padding
; @param string       - la string à représenter sous forme de QrCode
; @param dataCapacity - la capacité de codewords disponible pour la string (voir table.lsp)
; @return Liste binaire 
(defun concat-data (string dataCapacity / qr-data remainingCount)
  (setq qr-data '()
        qr-data (append qr-data 
                        byte-encoding 
                        (pad-bytes (bin (strlen string)) length-bits)
                        (encode-string string))
        remainingCount (free-bits qr-data (* dataCapacity 8))) ; dataCapacity * 8 = bitsCapacity
  
  (cond ((>= remainingCount 4) ;4 bits libres 
        (setq qr-data (append qr-data '(0 0 0 0))))
        (t (setq qr-data (append qr-data (padding remainingCount)))))
  (setq qr-data (append qr-data (fill-0 (length qr-data)))) ; ajoute des 0 tants que le nombre de bits n'est pas un multiple de 8
  (setq qr-data (fill-remaining qr-data dataCapacity))
  qr-data)


; retourne les modules d'informations sur le masque et la correction d'erreurs utilisés 
; @param ECLevel : 'L, 'M, 'H, 'Q
; @param maskIndex [0,7]
; @return liste binaire de modules d'information
(defun calc-format-bits (ECLevelIndex maskIndex / data reminder bits)
  (setq ECvalue  (getval ECLevelIndex EC-level)
        data     (boole 7 (lsh ECvalue 3) maskIndex)
        reminder data)
  
  (repeat 10       ; (reminder << 1) xor ((reminder >> 9) * 0x537) 
    (setq reminder (boole 6 (lsh reminder 1) (* (lsh reminder -9) 1335))))
  
        ; (data << 10 | rem) ^ 0x5412
  (setq bits (boole 6 (boole 7 (lsh data 10) reminder) 
                      21522))
  (dec->bits bits))


(defun make-QrCode (string version errorLevel 
                   / generatorPoly dataCapacity size codewords qrCode moduleSequence dataCodewords edcCodewords index bit)
  
  (setq dataCapacity   (car (get-QrCapacity version errorLevel))
        generatorPoly  (get-ECGenerator version errorLevel)
        dataCodewords  (concat-data string dataCapacity)
        edcCodewords   (get-EDC (bytes->dec dataCodewords) generatorPoly)
        size           (get-Qrsize version)
        codewords      (append codewords dataCodewords (decs->bytes edcCodewords))
        qrCode         (get-masked-matrix version codewords 0)
        formatModules  (calc-format-bits errorLevel 0))
  
  (add-format-modules qrCode formatModules)
  (add-fixed-patterns qrCode size)
  
  (vlax-safearray->list qrCode))