
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (30 / 11 - 1986) (19 : 25 : 48))
(defq *package* DOKUMIZE)

(defun ##pr-rivi
 (x tabs)
 (if (greaterp (tab) 60) (##rivi (add1 tabs)))
 (cond
  ((atom x) (print x))
  ((eq (car x) quote)
   (printc 39)
   (##pr-rivi (cadr x) tabs))
  (t
   (printc 40)
   (while x
    (cond
     ((atom x) (printc 46) (sp) (print x))
     ((atom (car x)) (##pr-rivi (car x) tabs))
     (t (##pr-rivi (car x) (add1 tabs))))
    (pop x)
    (if x (sp)))
   (printc 41))))

(defun dokumize
 (##PACKAGE file)
 (when file (out (setq file (create file))) (echo t))
 (setq ##NAME (list 'package ##PACKAGE))
 (setq *RIVI* 1)
 (setq *SIVU* 1)
 (let
  ((##pack (eval ##PACKAGE)))
  (##rivi)
  (##viiva)
  (##viiva)
  (##rivi)
  (##comment)
  (tab 20)
  (pr-many 'CONTENTS 'OF 'PACKAGE ##PACKAGE)
  (##viiva)
  (##viiva)
  (##rivi)
  (##rivi)
  (##rivi)
  (##rivi)
  (tab 8)
  (printc 40)
  (print 'defq)
  (sp)
  (print ##PACKAGE)
  (printc 40)
  (mapc ##pack
   (function (lambda (##name) (##rivi) (tab 20) (print ##name))))
  (printc 41)
  (printc 41)
  (mapc ##pack
   (function
    (lambda
     (##NAME)
     (unless
      (eq ##NAME ##PACKAGE)
      (##sivu)
      (##rivi 8)
      (uncompile ##NAME)
      (##doku-print
       (if
        (eq (car (eval ##NAME)) 'lambda)
        (cons 'defun (cons ##NAME (cdr (eval ##NAME))))
        (if
         (eq (car (eval ##NAME)) 'mlambda)
         (cons 'defmacro (cons ##NAME (cdr (eval ##NAME))))
         (if
          (eq (car (eval ##NAME)) 'defstruct)
          (eval ##NAME)
          (list 'defq ##NAME (eval ##NAME)))))
       9))))))
 (when file (close file) (echo nil) (out 0)))

(defun ##atomize
 (x)
 (if (atom x) x (##atomize (car x))))

(defun ##pr-list
 (x tabs)
 (if
  (atom x)
  (print x)
  (progn
   (printc 40)
   (while x
    (##pr-list (pop x) (add1 tabs))
    (if x (if (greaterp (tab) 60) (##rivi tabs) (sp))))
   (printc 41))))

(defq ##viiva
 (progn (##rivi) (##comment) (repeat-times 75 (print '=))))

(defq ##comment (progn (print ';) (print ';)))

(defq ##sivu
 (progn
  (setq *RIVI* 0)
  (cr)
  (cr)
  (##viiva)
  (when ##EKA (cr) (##comment) (tab 60) (print 'continued..))
  (printc 12)
  (##viiva)
  (cr)
  (##comment)
  (sp)
  (pr-many ##PACKAGE '/)
  (when ##NAME (print 'Definition-of) (sp) (print ##NAME))
  (when ##EKA
   (sp)
   (print '/)
   (sp)
   (print (##atomize ##EKA))
   (sp)
   (print 'continued..))
  (tab 68)
  (print 'sivu:)
  (sp)
  (print *SIVU*)
  (##viiva)
  (setq *SIVU* (add1 *SIVU*))
  (repeat-times 3 (cr))))

(defun ##rivi
 (tabs)
 (when (> *RIVI* 55) (##sivu))
 (setq *RIVI* (add1 *RIVI*))
 (cr)
 (tab tabs))

(defun ##doku-print
 (x tabs)
 (cond
  ((atom x) (print x))
  ((eq (car x) 'comment)
   (##rivi tabs)
   (##pr-list x tabs))
  ((eq (car x) quote)
   (printc 39)
   (##doku-print (cadr x) (add1 tabs)))
  ((depthl 12 x) (##pr-rivi x tabs))
  (t
   (unless
    (or (lessp *RIVI* 40) (depthl 30 x))
    (##sivu)
    (tab (sub1 tabs)))
   (let
    ((##EKA (car x)) (tupla (not (depthl 80 x))))
    (printc 40)
    (while x
     (##doku-print (pop x) (plus tabs 1))
     (if
      (and x (atom (car x)) (lessp (tab) 60))
      (sp)
      (progn (if x (##rivi tabs)) (when tupla (##rivi tabs)))))
    (printc 41)
    (when tupla
     (tab 50)
     (##comment)
     (print 'end-of-)
     (print
      (if (atom ##EKA) ##EKA (list (##atomize ##EKA) '&&))))))))

(defq DOKUMIZE (##pr-rivi dokumize ##atomize ##pr-list ##viiva ##comment ##sivu ##rivi ##doku-print DOKUMIZE))
(progn (in 0) ())
