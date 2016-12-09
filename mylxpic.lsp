
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (23 / 5 - 2003) (12 : 14 : 34 56))
(defq *package* MYLXPIC)

(defun zoominus ()
 (setq X (- X (* movement 3)))
 (setq Y (- Y movement))
 (setq movement (* movement 2))
 (setq Z (1- Z)))

(defun zoomplus ()
 (setq movement (/ movement 2))
 (setq X (+ X (* movement 3)))
 (setq Y (+ Y movement))
 (setq Z (1+ Z)))

(defun palauta-int
 (i)
 (setq i (* 4 i))
 (pokew 0 i (car *INTJEMMA*))
 (pokew 0 (+ 2 i) (cadr *INTJEMMA*)))

(defun raiskaa-int
 (i)
 (setq i (* 4 i))
 (setq paskaa '(184 6 80 207))
 (setq a 0)
 (while paskaa
  (poke -26624 a (pop paskaa))
  (setq a (1+ a)))
 (pokew 0 i 0)
 (pokew 0 (+ 2 i) -26624))

(defun jemmaa-int
 (i)
 (setq i (* 4 i))
 (setq *INTJEMMA*
  (list (peekw 0 i) (peekw 0 (+ i 2)))))

(defq main (nlambda (f) (compile-all 'main2) (main2 f)))

(defun main2
 (f)
 (setq KAIKKI (dir f))
 (when
  (and (not f) (fil-exists '\LASTPICT.LSP))
  (setq \LASTPICT.LSP (read-from-file '\LASTPICT.LSP))
  (if
   (fil-exists (car \LASTPICT.LSP))
   (push (pop \LASTPICT.LSP) KAIKKI)
   (setq \LASTPICT.LSP nil)))
 (setq ch 0)
 (setq INVERT t)
 (setq Z 0)
 (setq movement 100)
 (while
  (and KAIKKI (not (= ch (char q))))
  (setq f (pop KAIKKI))
  (if \LASTPICT.LSP
   (progn
    (setq X (pop \LASTPICT.LSP))
    (setq Y (pop \LASTPICT.LSP))
    (setq Z (pop \LASTPICT.LSP))
    (setq movement (pop \LASTPICT.LSP))
    (setq INVERT (pop \LASTPICT.LSP)))
   (progn (setq X 0) (setq Y 0)))
  (repeat
   (print f)
   (progn
    (jemmaa-int 16)
    (raiskaa-int 16)
    (lxpic f X Y Z)
    (palauta-int 16))
   (unless
    (any-key)
    (siirto)
    (if FILEINFO (print (list f X Y Z))))
   (case
    (setq ch (readcc))
    (0
     (case
      (readcc)
      (72 (setq Y (- Y movement)) nil)
      (77 (setq X (+ X (* 3 movement))) nil)
      (80 (setq Y (+ Y movement)) nil)
      (75 (setq X (- X (* 3 movement))) nil)
      (59 (zoomplus) nil)
      (60 (zoominus) nil)
      (61 (setq ch 13) t)
      (62
       (out (create 'A:\sys\c.bat))
       (cr)
       (print 'cd)
       (sp)
       (print (cd))
       (cr)
       (print 'pic)
       (cr)
       (close (out))
       (out 0)
       t
       (setq ch (char q))
       t)))
    (43 (zoomplus) nil)
    (45 (zoominus) nil)
    (105 (setq INVERT (not INVERT)) nil)
    (120 (setq FILEINFO (not FILEINFO)) nil)
    (113 t)
    (13 t)
    (t nil))))
 (print-to-file '\LASTPICT.LSP (list f X Y Z movement INVERT)))

(defun siirto ()
 (setq c -2)
 (for
  (y 0 99)
  (for
   (page 0 1)
   (setq iso-y (+ (* y 80) (* page 8192)))
   (for
    (x 0 78 2)
    (setq iso-x (+ iso-y x))
    (pokew -26592 (setq c (+ 2 c)) (peekw -18432 iso-x))
    (if
     (zerop (remainder c 4))
     (pokew -18432
      (/ c 2)
      (logxor -1 (peekw -26592 (/ c 2))))))))
 (for
  (c 8000 16000 2)
  (pokew -18432 c (logxor -1 (peekw -26592 c)))))

(defun lxpic
 (kuva x0 y0 zoom)
 (poke 64 80 1)
 (pokew 64 81
  (+ 5 (if INVERT 0 2) (if FILEINFO 1024 0)))
 (poke 64 83 zoom)
 (pokew 64 84 0)
 (pokew 64 86 0)
 (pokew 64 88 x0)
 (pokew 64 90 y0)
 (pokew 64 92 640)
 (pokew 64 94 200)
 (fast-spawn 'a:\sys\lxpic.com kuva))

(defun huimaa () () (x))

(defq MYLXPIC (zoominus zoomplus palauta-int raiskaa-int jemmaa-int main main2 siirto lxpic huimaa MYLXPIC))
