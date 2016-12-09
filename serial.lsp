
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (1 / 5 - 2003) (5 : 8 : 22 25))
(defq *package* SERIAL)

(defun taajuus ()
 (initialize-serial-port 'COM1 2400 8 'none 1)
 (repeat-times 100
  (while (zerop (logand 1 (input-byte line-status-reg))) nil)
  (sp)
  (setq iik (input-byte base-address))
  (setq aak 1)
  (repeat-times 8
   (print
    (if (zerop (logand aak iik)) '_ '=))
   (setq aak (* 2 aak)))))

(defun standard-init () (initialize-serial-port 'COM1 9600 8 'none 1))

(defun send-byte
 (x)
 (if
  (zerop (logand 32 (input-byte line-status-reg)))
  nil
  (progn (output-byte base-address x) x)))

(defun byte-received? ()
 (if (zerop (logand 1 (input-byte line-status-reg))) nil (input-byte base-address)))

(defun initialize-serial-port
 (port baud-rate word-length parity stop-bits)
 (setq base-address
  (peekw 64
   (case port
    (COM1 0)
    (COM2 2)
    (t
     (error-reset (list 'NO 'SUCH 'PORT port))))))
 (setq line-control-reg (+ base-address 3))
 (setq line-status-reg (+ base-address 5))
 (setq interrupt-enable-register (- line-control-reg 2))
 (output-byte line-control-reg 128)
 (setq baud-rate-divisor
  (/ 1152 (compress (nreverse (cddr (nreverse (explode baud-rate)))))))
 (output-byte (- line-control-reg 2) (high-byte baud-rate-divisor))
 (output-byte (- line-control-reg 3) (low-byte baud-rate-divisor))
 (output-byte line-control-reg
  (+
   (case word-length
    (5 0)
    (6 1)
    (7 2)
    (8 3)
    (t (error-reset (list 'word-length word-length '?))))
   (case stop-bits
    (1 0)
    (2 4)
    (t (error-reset (list 'stop-bits stop-bits '?))))
   (case parity
    (none 0)
    (odd 8)
    (even 24)
    (t (error-reset (list 'parity parity '?)))))))

(defq SERIAL (taajuus standard-init send-byte byte-received? initialize-serial-port SERIAL))
