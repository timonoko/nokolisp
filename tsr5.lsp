
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (15 / 5 - 2003) (0 : 29 : 8 44))
(defq *package* TSR5)

(defun format
 (n x)
 (float
  (progn
   (when (< x 0) (printc 45) (setq x (abs x)))
   (print (integer x))
   (setq x (- x (integer x)))
   (printc 46)
   (while
    (< 0 n)
    (setq x (* 10 x))
    (setq n (1- n))
    (if (zerop (integer x)) (print 0)))
   (unless (zerop (integer x)) (print (integer x))))))

(defun sqrt (x) (tsr5 'q x))

(defmacro tan
 (x)
 (list
  '/
  (list 'sin x)
  (list 'cos x)))

(defmacro cos
 (x)
 (list
  'sin
  (list '- '(/ pi 2) x)))

(defq *FLOAT-OPET*
 ((plus + 2 0)
  (difference - 2 0)
  (times * 2 0)
  (quotient / 2 0)
  (lessp l 2 0)
  (greaterp g 2 0)
  (eqn e 2 0)
  (sub1 - 2 1)
  (1- - 2 1)
  (add1 + 2 1)
  (1+ + 2 1)
  (sin s 1)
  (arctan r 1)
  (abs a 1)
  (sqrt q 1)
  (integer i 1)))

(defq pi 3.1415926536E+00)

(defun arctan (x) (tsr5 'r x))

(defun sin (x) (tsr5 's x))

(defun integer (x) (tsr5 'i x))

(defun make-float-expr
 (x)
 (if
  (atom x)
  (if
   (and
    (identp x)
    (let
     ((x2 (car (explode x))))
     (or
      (< 47 x2 58)
      (member x2 '(43 45 46)))))
   (list 'quote x)
   x)
  (if
   (and (atom (car x)) (assoc (car x) *FLOAT-OPET*))
   (let
    ((x2
      (cons
       'tsr5
       (cons
        (list 'quote (cadr (assoc (car x) *FLOAT-OPET*)))
        (cons
         (make-float-expr (cadr x))
         (if
          (= 2 (caddr (assoc (car x) *FLOAT-OPET*)))
          (list
           (if
            (caddr x)
            (make-float-expr (caddr x))
            (cadddr (assoc (car x) *FLOAT-OPET*))))))))))
    (if
     (member (car x) '(lessp greaterp eqn))
     (list 'not (list 'zerop x2))
     x2))
   (cons (make-float-expr (car x)) (make-float-expr (cdr x))))))

(defq TSR5
 (format sqrt tan cos *FLOAT-OPET* pi arctan sin integer make-float-expr
  TSR5 float fib tsr5-from-string tsr5-to-string tsr5))

(defmacro float (x) (make-float-expr (macroexpand x)))

(defun fib
 (x)
 (cond
  ((< x 2) x)
  ((< x 24)
   (+ (fib (1- x)) (fib (- x 2))))
  (t
   (float (+ (fib (1- x)) (fib (- x 2)))))))

(defun tsr5-from-string
 (adr c s)
 (setq c (peek *DSEG* adr))
 (repeat-times c (push (peek *DSEG* (setq adr (1+ adr))) s))
 (compress (nreverse s)))

(defun tsr5-to-string
 (x adr p)
 (setq p (explode x))
 (poke *DSEG* adr (length p))
 (while p (poke *DSEG* (setq adr (1+ adr)) (pop p))))

(defun tsr5
 (op x y)
 (unless *DSEG*
  (let
   ((c (cdr (command-line))))
   (setq *DSEG* (pop c))
   (setq *OPE-ADDR* (pop c))
   (setq *P1-ADDR* (pop c))
   (setq *P2-ADDR* (pop c))
   (setq *P3-ADDR* (pop c))))
 (poke *DSEG* *OPE-ADDR* (car (explode op)))
 (tsr5-to-string x *P1-ADDR*)
 (tsr5-to-string y *P2-ADDR*)
 (INT- 250)
 (tsr5-from-string *P3-ADDR*))
