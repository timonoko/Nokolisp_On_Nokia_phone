
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (7 / 12 - 2004) (6 : 33 : 27 45))
(defq *package* SEEVFN)

(defun Valitse-kirja ()
 (setq paskaa (sort (dir '????????.TXT)))
 (edit paskaa)
 (setq Book-filename (car paskaa)))

(defun super-slow ()
 (slow-clock)
 (output-byte 34 129)
 (output-byte 35 60)
 (output-byte 34 129)
 (input-byte 35))

(defq paskaa (SOTU09.TXT SOTU08.TXT SOTU07.TXT SOTU06.TXT SOTU05.TXT SOTU04.TXT SOTU03.TXT SOTU02.TXT SOTU01.TXT))

(defun Save-State ()
 (out (create 'SEE.BAT))
 (print 'SEEVFN)
 (sp)
 (print Book-filename)
 (sp)
 (print (car (LSEEK (in) 1)))
 (sp)
 (print (cdr (LSEEK (in) 1)))
 (cr)
 (close (out))
 (out 0))

(defun Kirjanmerkki ()
 (set_cursor 25 20)
 (print (time))
 (set_cursor 25 40)
 (print Book-filename)
 (set_cursor 25 54)
 (hex t)
 (setq Tamap (LSEEK (in) 1))
 (print (LSEEK (in) 2))
 (LSEEK (in) 0 Tamap)
 (print (LSEEK (in) 1))
 (hex))

(defun avaa-tiedosto
 (Book-filename)
 (repeat-times 30 (cr))
 (set_cursor 22 80)
 (setq KOLUMNI 0)
 (setq RIVI 1)
 (setq SANA nil)
 (setq TEXTIN-RIVI 1)
 (in (open Book-filename)))

(defun fast-clock
 (x)
 (output-byte 34 128)
 (output-byte 35 4)
 (output-byte 34 128)
 (input-byte 35))

(defun hyper-speed () (force-clock 1))

(defun backlight
 (x)
 (output-byte 34 168)
 (output-byte 35
  (if x (logor 1 (input-byte 35)) (logand 254 (input-byte 35)))))

(defun lid-on? ()
 (output-byte 34 114)
 (zerop (logand 16 (input-byte 35))))

(defun force-clock
 (x)
 (output-byte 34 64)
 (output-byte 35 x)
 (output-byte 34 128)
 (input-byte 35))

(defun slow-clock ()
 (force-clock 2)
 (output-byte 34 128)
 (output-byte 35 24)
 (output-byte 34 128)
 (input-byte 35))

(defun close-lid-readcc2
 (kerta)
 (unless
  (any-key)
  (slow-clock)
  (repeat
   (if
    (lid-on?)
    (progn
     (when (zerop kerta) (Save-State))
     (backlight)
     (setq kerta (+ 1 kerta))
     (if (< 100 kerta) (quit))
     (force-clock 5))
    (if BEE (backlight BEE)))
   (any-key)))
 (fast-clock)
 (readcc))

(defq alku
 (nlambda
  (Book-filename P1 P2)
  (fast-clock)
  (setq PAIKKA (cons P1 P2))
  (compile-all 'load-fnt)
  (compile-all 'see-file)
  (load-fnt)
  (if
   (and Book-filename (fil-exists Book-filename) (numberp P1))
   (see-file Book-filename PAIKKA)
   (progn (Valitse-kirja) (see-file Book-filename (cons 0 0))))
  (slow-clock)
  (quit)))

(defun lue-sana
 (ch)
 (setq SANA nil)
 (while (< 32 (nxtch)) (push (readc) SANA))
 (setq SANA (nreverse SANA)))

(defun rullaa ()
 (for
  (x 0 199)
  (for
   (y 2 40)
   (pokew -18432
    (+ (* 80 x) (- 80 (* 2 y) -2))
    (peekw -18432
     (+ (* 80 x) (- 80 (* 2 y))))))))

(defun see-file
 (Book-filename PAIKKA)
 (avaa-tiedosto Book-filename)
 (LSEEK (in) 0 PAIKKA)
 (catch
  'LOPPU
  (repeat
   (cond
    (SANA (setq KIRJAIN (pop SANA)))
    ((< 32 (nxtch))
     (lue-sana)
     (cond
      ((< (- 200 KOLUMNI) (* 10 (length SANA)))
       (setq KIRJAIN 11))
      (t (setq KIRJAIN (pop SANA))))
     t)
    ((= 26 (nxtch)) (setq KIRJAIN 10))
    (t (setq KIRJAIN (readc))))
   (seevfn2 KIRJAIN)
   nil))
 (Save-State)
 (close (in))
 (in 0))

(defun reverse-byte
 (x)
 (let
  ((y 0) (a 128) (b 1))
  (for
   (i 0 7)
   (unless (zerop (logand a x)) (setq y (+ y b)))
   (setq a (/ a 2))
   (setq b (* b 2)))
  y))

(defun seevfn2
 (x line)
 (cond
  ((< 31 x)
   (if
    (and (= 32 x) (< 1 KOLUMNI))
    (setq KOLUMNI (+ 4 KOLUMNI)))
   (setq line (- 80 (* RIVI 2)))
   (setq x (- x 32))
   (setq x (nth x FONTTI))
   (setq LEVEYS (car x))
   (setq x (cdr x))
   (while x
    (poke -18432 (+ (* 80 KOLUMNI) line) (car (car x)))
    (poke -18432
     (+ (* 80 KOLUMNI) line -1)
     (cadr (car x)))
    (setq KOLUMNI (+ KOLUMNI 1))
    (pop x))
   (if (< 1 KOLUMNI) (setq KOLUMNI (+ KOLUMNI 2)))
   (when
    (< 190 KOLUMNI)
    (setq RIVI (1+ RIVI))
    (setq KOLUMNI 0)))
  ((member x '(10 11))
   (if (= x 10) (setq TEXTIN-RIVI (1+ TEXTIN-RIVI)))
   (setq RIVI (1+ RIVI))
   (setq KOLUMNI 0)))
 (when
  (< 38 RIVI)
  (case
   (close-lid-readcc2 0)
   (98 (setq BEE (not BEE)) (backlight BEE))
   (102
    (close (in))
    (in 0)
    (Valitse-kirja)
    (in (open Book-filename))
    (repeat-times 30 (cr))
    (Kirjanmerkki)
    (setq KOLUMNI 0)
    (setq RIVI 1)
    (setq SANA nil))
   (0
    (case
     (readcc)
     ((75 59 80 68)
      (repeat-times 30 (cr))
      (Kirjanmerkki)
      (setq KOLUMNI 0)
      (setq RIVI 1))
     ((77 60 71)
      (setq SANA nil)
      (setq Tamap (LSEEK (in) 1))
      (setq Uusip
       (-
        (logor (* 256 (car Tamap)) (high-byte (cdr Tamap)))
        6))
      (if (< Uusip 0) (setq Uusip 0))
      (close (in))
      (in (open Book-filename))
      (LSEEK
       (in)
       0
       (cons (high-byte Uusip) (* 256 (low-byte Uusip))))
      (repeat-times 30 (cr))
      (Kirjanmerkki)
      (setq KOLUMNI 0)
      (setq RIVI 1))
     (t (rullaa) (Kirjanmerkki) (setq RIVI (- RIVI 1)))))
   ((113 81) (throw 'LOPPU))
   (101
    (cr)
    (in
     (prog1
      (in)
      (in 0)
      (readc)
      (print 'EVAL:)
      (pprint (eval (read-with-edit)))
      (readcc)))
    (repeat-times 30 (cr))
    (Kirjanmerkki)
    (setq KOLUMNI 0)
    (setq RIVI 1))
   (t (rullaa) (Kirjanmerkki) (setq RIVI (- RIVI 1)))))
 LEVEYS)

(defun load-fnt
 (Book-filename)
 (setq Book-filename 'a:\sys\SPXBT116.VFN)
 (in (open Book-filename))
 (repeat-times 10 (readc-bin))
 (setq FONTTI nil)
 (for
  (merkit 33 159)
  (setq yksi)
  (for
   (x 1 13)
   (push (list (reverse-byte (readc-bin)) (reverse-byte (readc-bin))) yksi))
  (while (equal (list 0 0) (car yksi)) (pop yksi))
  (push (nreverse yksi) FONTTI))
 (setq FONTTI (nreverse FONTTI))
 (close (in))
 (in 0))

(defq SEEVFN
 (Valitse-kirja super-slow paskaa Save-State Kirjanmerkki avaa-tiedosto
  fast-clock hyper-speed backlight lid-on? force-clock slow-clock
  close-lid-readcc2 alku lue-sana rullaa see-file reverse-byte
  seevfn2 load-fnt SEEVFN))
