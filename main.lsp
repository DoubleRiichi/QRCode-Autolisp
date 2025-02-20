;; Expose les commandes à utiliser pour insérer le QR Code une première fois, et le mettre à jour lors d'une sauvegarde.
(vl-load-com)

(vlr-command-reactor "SaveReactor" '((:vlr-commandwillstart . main:react)))


(defun get-block (name / ent)
  (setq ss (ssget "X" (list '(0 . "INSERT") (cons 2 name))))
  (if ss
    (entget (ssname ss 0))
    ss))


(defun c:insert-qr ()
    ;(command "_REGENALL")
    (setq stringToEncode (strcat (getvar "DWGNAME") "@" (qr-datetime))
          errorLevel     (if (>= (strlen stringToEncode) 42)
                            'L
                            'M)
          stringToEncode (vl-string-subst  "" ".dwg" stringToEncode))
    
    (print stringToEncode)

    (setq insertPoint (getpoint "Choisissez un point d'insertion du Qr Code :"))

    (setq qr (make-QrCode stringToEncode 3 errorLevel))
    ;(debug-print qr)
    (draw-Qrcode qr)

    (command "_INSERT" "QR-CODE-NOMDATE" "_S" 1 "_R" -90 insertPoint)
    (princ))




(defun qr-generate ( / qr stringToEncode errorLevel oldQr oldInsertPoint)
  (setq stringToEncode (strcat (getvar "DWGNAME") "@" (qr-datetime))
        errorLevel     (if (>= (strlen stringToEncode) 42)
                        'L
                        'M)
        stringToEncode (vl-string-subst  "" ".dwg" stringToEncode))
  
  (setq qr (make-QrCode stringToEncode 3 errorLevel))
  
  (draw-Qrcode qr)
  
  (if (setq oldQr (get-block "QR-CODE-NOMDATE"))
    (progn
        
      (entdel (handent (cdr (assoc 5 oldQr))))
      (setq oldInsertPoint (cdr (assoc 10 oldQr)))
      (entmake  (list
            '(0 . "INSERT")            
            '(2 . "QR-CODE-NOMDATE")
            (assoc 41 oldQr)
            (assoc 42 oldQr)
            (assoc 50 oldQr)
            (cons  10 oldInsertPoint)))
    )
    ;Else 
    ; utilisation de commande pour laisser à l'utilisateur le choix du point de base une première fois
    ))


(defun qr-generate-svg (input path / qr)
  (setq qr (make-QrCode input))
  (save-svg qr path))




(defun main:react (reactor commandNameList / commandName)
 (cond
   ((and (wcmatch (setq commandName (car commandNameList)) "CLOSE") (get-block "QR-CODE-NOMDATE"))
      (qr-generate))
   ((and (wcmatch commandName "*SAVE*") (get-block "QR-CODE-NOMDATE"))
      (qr-generate)))
 (princ))