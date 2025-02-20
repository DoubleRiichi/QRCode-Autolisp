; Créer un svg représentant un Qr Code, à fin de débogages
(defun to-svg-str (qrcode / len svgStr pathStr row y col x)
  (setq len    (rtos (sqrt (length qrcode))) ; get the length of one side
        svgStr (strcat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" viewBox=\"0 0 " len " " len "\" stroke=\"none\">
	<rect width=\"100%\" height=\"100%\" fill=\"#FFFFFF\"/>
	<path d=\"" )
        pathStr "")
  
  (setq y 0)
  (foreach row qrcode
    (setq x 0)
    (foreach col qrcode
      (if (= col 1)
        (setq pathStr (strcat pathStr "M" (itoa x) ",M" (itoa y) "h1v1h-1z ")))
    (setq x (1+ x)))
  (setq y (1+ y)))
  
  (strcat svgStr pathStr "\" fill=\"#000000\"/></svg>"))


(defun save-svg (qrcode path / fp)
  (setq fp (open path "w"))
  (princ (to-svg-str qrcode) fp)
  (close fp))