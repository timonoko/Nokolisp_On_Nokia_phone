
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (20 / 5 - 2003) (12 : 47 : 14 64))
(defq *package* PUHGPS)

(defun alku ()
 (load '\sys\tsr5.lsp t)
 (compile-all 'muunnos)
 (setq temp (read-from-file 'dataa.txt))
 (print temp)
 (print '===>)
 (print (muunnos (car temp) (cadr temp)))
 (cr)
 (quit))

(defun face () (setq temp3 (command-line)))

(defq hima (muunnos '(60 11.667) '(24 56.387)))

(defun muunnos
 (lat long)
 (list (yksi-luku lat (car kehysarvot)) (yksi-luku long (cadr kehysarvot))))

(defun degmin->deg
 (x)
 (float (+ (car x) (/ (cadr x) 60))))

(defun yksi-luku
 (asteet kehys)
 (float
  (list
   (setq temp2
    (integer
     (setq temp
      (+
       (*
        (/
         (- (degmin->deg asteet) (degmin->deg (cdr (car kehys))))
         (-
          (degmin->deg (cdr (cadr kehys)))
          (degmin->deg (cdr (car kehys)))))
        (- (car (cadr kehys)) (caar kehys)))
       (caar kehys)))))
   (integer (* 50 (- temp temp2))))))

(defq kehysarvot
 (((72 60 8.58) (84 60 15.12))
  ((80 24 50.06) (100 25 11.56))))

(defq PUHGPS (alku face hima muunnos degmin->deg yksi-luku kehysarvot PUHGPS))
