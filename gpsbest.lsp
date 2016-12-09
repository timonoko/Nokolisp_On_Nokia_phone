
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (17 / 5 - 2003) (9 : 19 : 33 30))
(defq *package* GPSBEST)

(defun g ()
 (compile-all 'bmploop)
 (bmploop (car (dir '*.bmp)) 'nokia)
 (quit))

(defun set-cursor
 (x y)
 (AX-reg 512)
 (BX-reg 0)
 (DX-reg (+ x (* 256 y)))
 (INT- 16))

(defun nokia-exe ()
 (compile-all 'bmploop)
 (continue-in 'gpsbest.exe)
 (setq *DSEG* nil)
 (bmploop (car (dir '*.bmp)) 'nokia)
 (quit))

(defun kursori
 (x y)
 (color 129)
 (point (- x 20) y)
 (draw (+ x 20) y)
 (point x (- y 20))
 (draw x (+ y 20)))

(defun print-loc
 (x y)
 (let
  ((h (pixpoint->latlong 320 100)))
  (set-cursor x y)
  (print-deg (car h))
  (set-cursor x (1+ y))
  (print-deg (cdr h))))

(defun print-deg
 (x)
 (print (integer x))
 (printc 39)
 (format 2 (float (* 60 (- x (integer x))))))

(defun fib
 (x)
 (cond
  ((< x 2) x)
  ((< x 24)
   (+ (fib (1- x)) (fib (- x 2))))
  (t
   (float (+ (fib (1- x)) (fib (- x 2)))))))

(defun pixpoint->latlong
 (x-off y-off)
 (float
  (cons
   (+
    (car *VasenAlaReuna*)
    (*
     (- *bmp-Y-size* (+ Y-loop y-off))
     (/ (- (car *OikeaYlaReuna*) (car *VasenAlaReuna*)) *bmp-Y-size*)))
   (+
    (cdr *VasenAlaReuna*)
    (*
     (+ X-loop x-off)
     (/ (- (cdr *OikeaYlaReuna*) (cdr *VasenAlaReuna*)) *bmp-X-size*))))))

(defun bmploop
 (f vehje)
 (when
  (fil-exists 'LASTLOCA.GPS)
  (setq temp (read-from-file 'LASTLOCA.GPS))
  (setq f (car temp))
  (setq X-loop (cadr temp))
  (setq Y-loop (caddr temp)))
 (when (fil-exists (IN8-filename f)) (read-IN8 (IN8-filename f)) (bmp-kuvan-koko f))
 (unless X-loop (setq X-loop 100) (setq Y-loop 100))
 (repeat
  (print-loc 5 2)
  (unless (any-key) (bmpdisp f X-loop Y-loop vehje) (kursori 320 100))
  (set-cursor 5 2)
  (print-loc 5 2)
  (case
   (progn (setq ch (readcc)) (if (zerop ch) (readcc) ch))
   (72 (setq Y-loop (- Y-loop 20)) nil)
   (77 (setq X-loop (+ X-loop 20)) nil)
   (80 (setq Y-loop (+ Y-loop 20)) nil)
   (75 (setq X-loop (- X-loop 20)) nil)
   (13 t)))
 (print-to-file 'LASTLOCA.GPS (list f X-loop Y-loop)))

(defun IN8-filename
 (f)
 (compress
  (nreverse
   (nconc (explode '8NI) (cdddr (reverse (explode f)))))))

(defun long-kartall
 (long)
 (float
  (integer
   (/
    (- long (cdr *VasenAlaReuna*))
    (/ (- (cdr *OikeaYlaReuna*) (cdr *VasenAlaReuna*)) *bmp-X-size*)))))

(defun lat-kartall
 (lat)
 (float
  (- *bmp-Y-size*
   (integer
    (/
     (- lat (car *VasenAlaReuna*))
     (/ (- (car *OikeaYlaReuna*) (car *VasenAlaReuna*)) *bmp-Y-size*))))))

(defun bmp-kuvan-koko
 (f)
 (in (open f))
 (repeat-times 18 (readc-bin))
 (progn
  (setq *bmp-X-size* (+ (readc-bin) (* 256 (readc-bin))))
  (readc-bin)
  (readc-bin)
  (setq *bmp-Y-size* (+ (readc-bin) (* 256 (readc-bin)))))
 (close (in))
 (in 0)
 (list *bmp-X-size* *bmp-Y-size*))

(defun lue-lukupari ()
 (float
  (let ((iik (lue-asteluku))) (plm-item) (cons (lue-asteluku) iik))))

(defun lue-asteluku
 (hui hui2)
 (float
  (progn
   (setq hui
    (/
     (+ (plm-item) (progn (plm-item) (/ (plm-item) 100)))
     100))
   (+
    (setq hui2 (integer hui))
    (* 100 (/ (- hui hui2) 60))))))

(defun read-IN8
 (f)
 (in (open f))
 (repeat (eq (plm-item) 'NMEARECT))
 (plm-item)
 (setq *VasenAlaReuna* (lue-lukupari))
 (plm-item)
 (setq *OikeaYlaReuna* (lue-lukupari))
 (close (in))
 (in 0)
 (list *VasenAlaReuna* *OikeaYlaReuna*))

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

(defq GPSBEST
 (g set-cursor nokia-exe kursori print-loc print-deg fib
  pixpoint->latlong bmploop IN8-filename long-kartall lat-kartall
  bmp-kuvan-koko lue-lukupari lue-asteluku read-IN8 my-lseek
  my-mul bmpdisp GPSBEST))
