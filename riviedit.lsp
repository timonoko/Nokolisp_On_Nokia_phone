
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (22 / 5 - 2003) (7 : 8 : 32 87))
(defq *package* RIVIEDIT)

(defq make-exe
 (progn
  (re-kaanna)
  (setq @t nil)
  (setq *LINE-NUMB* 0)
  (list (date) (continue-in 'nokia.exe) (eval (command-line)))))

(defun re-kaanna ()
 (mapc
  '(edit3 einsert insert-line delete-line eprint)
  (function (lambda (x) (uncompile x) (set x nil))))
 (setq save save2)
 (setq spawn spawn2)
 (compile-all 'save)
 (compile-all 'riviedit2)
 (compile-all 'dir)
 (compile-all 'command-line)
 (setq edit3 riviedit2))

(defun save2
 (y x)
 (hex)
 (if
  (or (atom x) (not (identp y)))
  (progn
   (prints
    '*package*
    'name
    'assumed
    'to
    'be
    *package*)
   (cr)
   (setq x (eval *package*))
   (setq y
    (compress (nconc (explode *package*) (cons 46 (explode 'LSP)))))
   (prints 'filename 'assumed 'to 'be y)))
 (let
  ((back
    (compress
     (reverse
      (append (explode 'KAB) (member 46 (reverse (explode y))))))))
  (cr)
  (prints 'old y '=> back)
  (unlink back)
  (rename-file y back))
 (setq y (create y))
 (out y)
 (cr)
 (print ''(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%))
 (cr)
 (print
  (list 'quote (list 'MIKKO-3 (date) (time))))
 (cr)
 (print (list 'defq '*package* *package*))
 (cr)
 (mapc x
  (quote
   (lambda
    (x)
    (out 0)
    (cr)
    (print x)
    (out y)
    (cr)
    (if
     (eq *package* 'BOOT)
     (pprint (list 'defq x (definition-of x)))
     (ppr-def x (definition-of x)))
    (cr))))
 (out 0)
 (close y))

(defun spawn2
 (com-file args ima-file)
 (unless com-file (setq com-file 'B:\NOKIA\COMMAND.COM))
 (if
  (fil-exists com-file)
  (progn
   (unless args (setq args (compress '(32 32))))
   (unless ima-file (setq ima-file 'A:\nokolisp.ima))
   (while
    (fil-exists ima-file)
    (print 'NOKOLISP-SUB-PROCESS?)
    (cr)
    (setq ima-file (explode ima-file))
    (rplaca (last ima-file) (add1 (car (last ima-file))))
    (setq ima-file (compress ima-file)))
   (prog1
    (list 'Welcome 'back (date) (time))
    (nspawn (if (eq ima-file 'DONT%SAVE%IMA) nil ima-file) com-file args)
    (unlink ima-file)))
  (list 'There 'is 'no com-file)))

(defun rprinte
 (x curs)
 (erase_page)
 (if (< 21 curs) (print '*MORE*) (printc 40))
 (let
  ((alku 0) (loppu 0))
  (while
   (< 21 (- curs alku))
   (setq alku (+ 21 alku)))
  (setq loppu (+ alku 21))
  (for
   (p alku loppu)
   (cr)
   (when (= curs p) (print '==>))
   (when (nthcdr p x) (tab 3) (eprint2 (nth p x))))
  (cr)
  (if (nthcdr loppu x) (print '*MORE*) (printc 41))))

(defun riviedit2
 (x curs)
 (setq x (nreverse (reverse x)))
 (unless curs (setq curs 0))
 (repeat
  (if edit-bye t
   (progn
    (unless (any-key) (rprinte x curs))
    (setq THIS (nth curs x))
    (case
     (compress
      (list
       (let ((c (readcc))) (if (zerop c) (readcc) c))))
     (- (setq edit-bye t) t)
     (K t)
     (H (setq curs (1- curs)) nil)
     (P (setq curs (1+ curs)) nil)
     (M
      (if
       (atom (nth curs x))
       (rplaca (nthcdr curs x) (edit-line (nth curs x)))
       (rplaca (nthcdr curs x) (riviedit2 (nth curs x))))
      nil)
     (p (cr) (pprint x) (readcc) nil)
     (y
      (push (nth curs x) *RE-DELETED*)
      (setq x (nthdel curs x))
      nil)
     (n
      (cr)
      (print '?:)
      (let ((y (read-with-edit))) (setq x (nthpush curs y x)))
      nil)
     (e
      (cr)
      (print 'EVAL:)
      (let
       ((y (read-with-edit)))
       (setq x (nthpush curs (eval y) x)))
      nil)
     (+ (setq x (nthpush curs (pop *RE-DELETED*) x)) nil)
     (v
      (let
       ((y (nth (1+ curs) x)))
       (setq x (nthpush curs y (nthdel (1+ curs) x))))
      nil)
     (b
      (setq x (nthpush curs (copy (nth curs x)) x))
      nil)
     (r
      (let
       ((y (nth curs x)))
       (unless
        (atom y)
        (setq y (reverse y))
        (setq x (nthdel curs x))
        (while y (setq x (nthpush curs (pop y) x)))))
      nil)
     (w
      (let
       ((y (list (nth curs x) (nth (1+ curs) x))))
       (setq x (nthpush curs y (nthdel curs (nthdel curs x)))))
      nil)
     (a
      (let
       ((y (list (nth curs x))))
       (setq x (nthpush curs y (nthdel curs x))))
      nil)
     (c
      (let
       ((y
         (append (nth curs x) (list (nth (1+ curs) x)))))
       (setq x (nthpush curs y (nthdel curs (nthdel curs x)))))
      nil)
     (q
      (let
       ((y (nth curs x)))
       (setq x
        (nthpush curs (list 'quote y) (nthdel curs x))))
      nil)
     (k (setq x (copy x)) nil)
     (z (eval (list 'edit THIS)) nil)
     (s
      (print 'SUBST:)
      (setq x
       (subst (read-with-edit) (progn (print 'WITH:) (read-with-edit)) x))
      nil)
     (h (setq *HEX* (not *HEX*)) nil)))))
 x)

(defq RIVIEDIT (make-exe re-kaanna save2 spawn2 rprinte riviedit2 RIVIEDIT))
