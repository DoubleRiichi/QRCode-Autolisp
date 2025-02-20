
; Liste correspondant aux versions de QR Code et de leur capacité de modules de données et d'EC (error correction) selon le niveau de correction d'erreur choisi.
; (version . (niveau . (données . EC)))
(setq QR-CAPACITY 
  (list '((L . '(19 . 7))
          (M . '(16 . 10))
          (Q . '(13 . 13))
          (H . '(9 .  17)))
    
        '((L . '(34 . 10))
          (M . '(28 . 16))
          (Q . '(22 . 22))
          (H . '(16 . 28)))
    
        '((L . '(55 . 15))
          (M . '(44 . 26))
          (Q . '(34 . 18))
          (H . '(28 . 22)))
      
        '((L . '(80 . 20))
          (M . '(64 . 18))
          (Q . '(48 . 26))
          (H . '(36 . 16)))
       
        '((L . '(108 . 26))
          (M . '(86  . 24))
          (Q . '(62  . 18))
          (H . '(46  . 22)))))

(defun get-QrCapacity (version EClevel)
  (car (cdr (cdr (assoc EClevel (nth (1- version) QR-CAPACITY))))))


(setq EC-GENERATOR 
  (list '((L . '(127 122 154 164 11 68 117))
          (M . '(216 194 159 111 199 94 95 113 157 193))
          (Q . '(137 73 227 17 177 17 52 13 46 43 83 132 120))
          (H . '(119 66 83 120 119 22 197 83 249 41 143 134 85 53 125 99 79)))
    
        '((L . '(216 194 159 111 199 94 95 113 157 193))
          (M . '(59 13 104 189 68 209 30 8 163 65 41 229 98 50 36 59))
          (Q . '(89 179 131 176 182 244 19 189 69 40 28 137 29 123 67 253 86 218 230 26 145 245))
          (H . '(252 9 28 13 18 251 208 150 103 174 100 41 167 12 247 56 117 119 233 127 181 100 121 147 176 74 58 197)))
    
        '((L . '(29 196 111 163 112 74 10 105 105 139 132 151 32 134 26))
          (M . '(246 51 183 4 136 98 199 152 77 56 206 24 145 40 209 117 233 42 135 68 70 144 146 77 43 94))
          (Q . '(239 251 183 113 149 175 199 215 240 220 73 82 173 75 32 67 217 146))
          (H . '(89 179 131 176 182 244 19 189 69 40 28 137 29 123 67 253 86 218 230 26 145 245)))
      
        '((L . '(152 185 240 5 111 99 6 220 112 150 69 36 187 22 228 198 121 121 165 174))
          (M . '(239 251 183 113 149 175 199 215 240 220 73 82 173 75 32 67 217 146))
          (Q . '(246 51 183 4 136 98 199 152 77 56 206 24 145 40 209 117 233 42 135 68 70 144 146 77 43 94))
          (H . '(59 13 104 189 68 209 30 8 163 65 41 229 98 50 36 59)))
       
        '((L . '(246 51 183 4 136 98 199 152 77 56 206 24 145 40 209 117 233 42 135 68 70 144 146 77 43 94))
          (M . '(122 118 169 70 178 237 216 102 115 150 229 73 130 72 61 43 206 1 237 247 127 217 144 117))
          (Q . '(239 251 183 113 149 175 199 215 240 220 73 82 173 75 32 67 217 146))
          (H . '(89 179 131 176 182 244 19 189 69 40 28 137 29 123 67 253 86 218 230 26 145 245)))))



(defun get-ECGenerator (version EClevel)
  (car (cdr (cdr (assoc EClevel (nth (1- version) EC-GENERATOR))))))


(defun find-version (str ECLevel / version len versionLst lst result ECCapacity winner ECLevel)
  (setq len (strlen str)
        winner 108 ; plus grande capacité supportée
        ECCapacity 26
        ;ECLevel 'L
        version 5
        counter 1)
  
  (foreach versionLst QR-CAPACITY
    (setq lst versionLst)
    (while lst 
      (if (= (caar lst) ECLevel)
        (progn 
          (setq capacity (car (caddar lst))
                result   (- capacity len))

          (if (and (> result 2) (< result winner))
            (progn
              (setq winner result
                    version counter
                    dataCapacity (car (caddar lst)) ; +2 pour accomoder le type d'encodage et la taille de la chaine
                    ECCapacity   (cdr (caddar lst))
                    ECLevel      (caar lst))))))
      
      (setq lst (cdr lst)))
    (setq counter (1+ counter)))
  (list version
        ECLevel 
        dataCapacity
        ECCapacity))
