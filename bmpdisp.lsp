
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (15 / 5 - 2003) (0 : 12 : 45 82))
(defq *package* BMPDISP)

(defun IN8-filename
 (f)
 (compress
  (nreverse
   (nconc (explode '8NI) (cdddr (reverse (explode f)))))))

(defun bmploop
 (f)
 (unless X-loop (setq X-loop 100) (setq Y-loop 100))
 (if (fil-exists 'B:\NOKIA\COMMAND.COM) (setq vehje 'nokia))
 (repeat
  (unless (any-key) (bmpdisp f X-loop Y-loop vehje))
  (case
   (progn (setq ch (readcc)) (if (zerop ch) (readcc) ch))
   (72 (setq Y-loop (- Y-loop 20)) nil)
   (77 (setq X-loop (+ X-loop 20)) nil)
   (80 (setq Y-loop (+ Y-loop 20)) nil)
   (75 (setq X-loop (- X-loop 20)) nil)
   (13 t))))

(defun my-lseek
 (file eka toka offset)
 (AX-reg eka)
 (BX-reg toka)
 (mach-code 247 235)
 (CX-reg (DX-reg))
 (DX-reg (+ offset (AX-reg)))
 (BX-reg file)
 (AX-reg (plus 16896 0))
 (if (INT- 33) (cons (DX-reg) (AX-reg))))

(defun my-mul
 (x y)
 (print (list x y))
 (AX-reg x)
 (BX-reg y)
 (mach-code 247 235)
 (list (AX-reg) (BX-reg) (CX-reg) (DX-reg)))

(defun bmpdisp
 (f X-orig Y-orig vehje)
 (let
  ((X-size)
   (Y-size)
   (c-count)
   (toisinpain (case vehje (nokia 255) (hp 255) (t 0)))
   (loimitus (case vehje (nokia nil) (t t))))
  (in (open f))
  (repeat-times 18 (readc-bin))
  (progn
   (setq X-size (+ (readc-bin) (* 256 (readc-bin))))
   (readc-bin)
   (readc-bin)
   (setq Y-size (+ (readc-bin) (* 256 (readc-bin))))
   (if
    (< (- X-size 640) X-orig)
    (setq X-orig (- X-size 640)))
   (if
    (< (- Y-size 200) Y-orig)
    (setq Y-orig (- Y-size 200)))
   (if (< X-orig 0) (setq X-orig 0))
   (if (< Y-orig 0) (setq Y-orig 0)))
  (setq Y-orig (- Y-size Y-orig 200))
  (my-lseek (in) Y-orig (+ 3 (/ X-size 8)) 62)
  (if (eq vehje 'nokia) (display-mode 99) (display-mode 6))
  (catch
   'paskaa
   (if loimitus
    (for
     (rivi 0 199)
     (my-lseek
      (in)
      (+ Y-orig rivi)
      (+ 3 (/ X-size 8))
      (+ 62 (/ X-orig 8)))
     (setq c-count
      (- 16112
       (+
        (* (/ rivi 2) 80)
        (* (remainder rivi 2) 8192))))
     (if (any-key) (throw 'paskaa))
     (for
      (char 0 79)
      (poke -18432 (+ char c-count) (logxor toisinpain (readc-bin)))))
    (for
     (rivi 0 199)
     (my-lseek
      (in)
      (+ Y-orig rivi)
      (+ 3 (/ X-size 8))
      (+ 62 (/ X-orig 8)))
     (setq c-count (- 15920 (* rivi 80)))
     (if (any-key) (throw 'paskaa))
     (for
      (char 0 79)
      (poke -18432 (+ char c-count) (logxor toisinpain (readc-bin)))))))
  (close (in))
  (in 0)))

(defq BMPDISP (IN8-filename bmploop my-lseek my-mul bmpdisp BMPDISP))
