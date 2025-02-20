(defun qr-datetime ( / cdate_val YYYY M D HH MM SS)
  ; Horodatage
  (setq cdate_val (rtos (getvar "CDATE") 2 6))

  ; Sépare la string en différentes unités de date et heure.
  (setq YYYY (substr cdate_val 1 4)
        M    (substr cdate_val 5 2)
        D    (substr cdate_val 7 2)
        HH   (substr cdate_val 10 2)
        MM   (substr cdate_val 12 2)
        SS   (substr cdate_val 14 2)
  )

  ; Output the current date and time to the Command
  ; prompt or return the formatted output as a string
  (strcat D M YYYY HH MM SS))


