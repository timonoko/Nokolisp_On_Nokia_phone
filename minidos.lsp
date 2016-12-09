
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (23 / 11 - 2004) (8 : 16 : 45 89))
(defq *package* MINIDOS)

(defun dossi () (slow-clock) (nquit 1))

(defun suspend () (force-clock 5))

(defun salanumerot
 (s)
 (n 'DATA)
 (PKUNZIP.EXE '-cms 'SALANUME.ZIP))

(defun hyper-speed () (force-clock 1))

(defun backlight
 (x)
 (output-byte 34 168)
 (output-byte 35
  (if x (logor 1 (input-byte 35)) (logand 254 (input-byte 35)))))

(defun close-lid-readcc ()
 (unless
  (any-key)
  (slow-clock)
  (repeat (when (lid-on?) (backlight) (force-clock 5)) (any-key)))
 (hyper-speed)
 (readcc))

(defun force-clock
 (x)
 (output-byte 34 64)
 (output-byte 35 x)
 (output-byte 34 128)
 (input-byte 35))

(defun lid-on? ()
 (output-byte 34 114)
 (zerop (logand 16 (input-byte 35))))

(defun phone-on? ()
 (output-byte 34 167)
 (not (zerop (logand 16 (input-byte 35)))))

(defun dos-eval
 (x)
 (out (create 'DOTHIS.BAT))
 (while x (sp) (print (pop x)))
 (cr)
 (print 'exit)
 (cr)
 (close (out))
 (out 0)
 (fast-spawn)
 (delete-file 'DOTHIS.BAT))

(defun super-slow ()
 (slow-clock)
 (output-byte 34 129)
 (output-byte 35 60)
 (output-byte 34 129)
 (input-byte 35))

(defun slow-clock ()
 (force-clock 2)
 (output-byte 34 128)
 (output-byte 35 24)
 (output-byte 34 128)
 (input-byte 35))

(defun fast-clock
 (x)
 (output-byte 34 128)
 (output-byte 35 4)
 (output-byte 34 128)
 (input-byte 35))

(defun spawn
 (com-file args ima-file)
 (unless com-file
  (setq com-file 'a:\sys\command.com)
  (setq args (compress (strapp2 '(/k A:\SYS\AUTOEXEC.BAT)))))
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

(defun backup-B: ()
 (PKZIP.EXE '-rpu '-xGEOS.INI 'A:\NOKIA\DOCUMENT\MMC\BACKUP2\PAKUP 'B:\*.*))

(defun save-fast ()
 (fast-clock)
 (setq *PLURP* (cd))
 (cd 'B:\NOKIA)
 (save)
 (XCOPY.EXE '/Y '*.LSP *PLURP*)
 (delete-file '*.LSP)
 (cd *PLURP*)
 (slow-clock)
 (dir))

(defun n (x) (N.EXE x) (sort (dir)))

(defun piuha (x) (if x (send-file x) (receive-file t)) nil)

(defun main () (compile-all 'minidos) (minidos f))

(defun receive-file
 (fast)
 (TRANSFER.EXE
  '/COM1
  '/R
  (if fast '/B38400)
  'XXXTEMP.ZIP)
 (PKUNZIP.EXE '/O 'XXXTEMP.ZIP)
 (delete-file 'XXXTEMP.ZIP))

(defun send-file
 (x fast)
 (delete-file 'XXXTEMP.ZIP)
 (PKZIP.EXE 'XXXTEMP.ZIP x)
 (TRANSFER.EXE
  '/COM1
  '/S
  (if fast '/B38400)
  'XXXTEMP.ZIP)
 (delete-file 'XXXTEMP.ZIP))

(defun wordstar
 (x)
 (slow-clock)
 (WORDSTAR.COM x)
 (setq temp (cd))
 (cd 'a:\sys\)
 (RESTODIS.EXE)
 (cd temp))

(defun pic
 (k)
 (unless MYLXPIC (load 'A:\NOKIA\DOCUMENT\MMC\LSP\MYLXPIC.LSP))
 (eval (list 'main k)))

(defun gps ()
 (hyper-speed)
 (cd 'A:\NOKIA\DOCUMENT\MMC\GPS)
 (load 'GPSBMPNO.LSP)
 (eval '(main))
 (quit))

(defun strapp2
 (x)
 (if x
  (nconc
   (explode
    (if (atom (car x)) (car x) (eval (car x))))
   (nconc (list 32) (strapp2 (cdr x))))))

(defun minidos ()
 (cd
  (prog1
   (cd)
   (cd '\)
   (delete-file '\NOKOLISP.IMA)
   (delete-file '\NOKOLISP.IMB)))
 (backlight t)
 (mapc
  (dir 'a:\sys\????????.???)
  (function
   (lambda
    (x)
    (when
     (member (car (last (explode x))) '(77 69))
     (set x
      (list
       'lambda
       'jama
       (list
        'fast-spawn
        (list quote (compress (nconc (explode 'A:\SYS\) (explode x))))
        '(compress (strapp2 jama)))))))))
 (backlight)
 (repeat
  (cr)
  (progn
   (cr)
   (cr)
   (cr)
   (cr)
   (cr)
   (tab 75)
   (print 'GPS))
  (progn
   (cr)
   (cr)
   (cr)
   (cr)
   (cr)
   (tab 75)
   (print 'BOOK))
  (progn
   (cr)
   (cr)
   (cr)
   (cr)
   (cr)
   (tab 75)
   (print 'DOS))
  (progn
   (cr)
   (cr)
   (cr)
   (cr)
   (cr)
   (tab 75)
   (print 'QUIT))
  (cr)
  (cr)
  (cr)
  (tab 14)
  (print '(GPS PIC -- PIUHA /// N WORDSTAR BOOK DOS))
  (cr)
  (case
   (readcc)
   (0
    (case
     (readcc)
     ((63 59) (gps))
     (64 (pic) nil)
     (65 t)
     (66 (piuha) nil)
     (67 (n) (print (dir)) nil)
     (68 (wordstar) nil)
     ((71 60)
      (load 'a:\sys\seevfn.lsp)
      (cd '\NOKIA\DOCUMENT\MMC\BOOKS)
      (in (open 'see.bat))
      (read)
      (eval
       (list
        'alku
        (read)
        (read)
        (read)
        (progn (close (in)) (in 0)))))
     ((79 61) (dossi))
     (62 (quit))
     (t t)))
   (t t))))

(defq MINIDOS
 (dossi suspend salanumerot hyper-speed backlight close-lid-readcc
  force-clock lid-on? phone-on? dos-eval super-slow slow-clock
  fast-clock spawn backup-B: save-fast n piuha main receive-file
  send-file wordstar pic gps strapp2 minidos MINIDOS))
