
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (2 / 10 - 2006) (9 : 9 : 37 26))
(defq *package* BOOT)

(defq any-key
 (lambda () (msdos 11) (= (low-byte (AX-reg)) 255)))

(defq select-disk
 (lambda
  (x)
  (DX-reg (1- (logand (car (explode x)) 15)))
  (msdos 14)
  (get-cur-disk)))

(defq error-hook
 (mlambda
  (x)
  (` car
   (or
    (err-set , x)
    (if
     (not (= 0 (in) (out)))
     (error-reset 'CRASH))
    (progn
     (cr)
     (print 'CRASH:y/n/e:)
     (case
      (compress (list (readcc)))
      (y (error-reset 'CRASH))
      (e (cr) (print 'EVAL:) (list (eval (read-with-edit))))))))))

(defq with-definition-of
 (mlambda
  (name . rest)
  (` let
   ((comp&uncomp))
   (when
    (symbolp , name)
    (while
     (assoc , name *COMPILED-FUNCTIONS*)
     (push (caar *COMPILED-FUNCTIONS*) comp&uncomp)
     (uncompile (caar *COMPILED-FUNCTIONS*)))
    ,@ rest
    (mapc comp&uncomp compile)
    , name))))

(defq %tracep%
 (mlambda
  (t%name t%arg t%resu)
  (` let
   ((t%out (out)) (t%resu) (%trace-tab% (add1 %trace-tab%)))
   (out 0)
   (cr)
   (tab %trace-tab%)
   (print ', t%name)
   (print '<=)
   (mapc , (cons 'list t%arg) eprint2)
   (setq t%resu (progn ,@ t%resu))
   (cr)
   (tab %trace-tab%)
   (print ', t%name)
   (print '=>)
   (eprint2 t%resu)
   (cr)
   (out t%out)
   t%resu)))

(defq * (macro (x) (*+-/ x 'times)))

(defq *+-/
 (lambda
  ((x y . z) ope)
  (if z
   (list ope (list ope x y) (*+-/ z ope))
   (if y (list ope x y) x))))

(defq *main-loop*
 (repeat
  (unless (atom *main-loop*) (error-reset '(recompile)))
  (hex nil)
  (if
   (zerop (in))
   (progn
    (cr)
    (prin1 *LINE-NUMB*)
    (prin1 '>)
    (setq @n (read-with-edit))
    (setq @n
     (case @n
      (+ '@)
      (* '@e)
      (e '(edit))
      (quit '(quit))
      (t
       (cond
        ((assoc @n @t) (edit-line (cdr (assoc @n @t))))
        ((eq *UP/DO* 'UP)
         (let
          ((ln (sub1 *LINE-NUMB*)))
          (sp)
          (repeat
           (setq @n (edit-line (cdr (assoc ln @t))))
           (if *UP/DO*
            (progn
             (repeat-times 40 (sp))
             (repeat-times 40 (printc 8))
             (setq ln
              (if (eq *UP/DO* 'UP) (sub1 ln) (add1 ln)))
             nil)
            t))
          @n))
        (t @n)))))
    (unless
     (member @n '(@ @n @t @e ()))
     (setq @ @n)
     (when @n
      (push (cons *LINE-NUMB* @n) @t)
      (setq *LINE-NUMB* (if *LINE-NUMB* (add1 *LINE-NUMB*) 0))
      (rplacd (nthcdr 20 @t) nil)))
    (setq @e (error-hook (eval @n)))
    (cr)
    (hex *HEX*)
    (pprint @e))
   (progn
    (cr)
    (print 'file)
    (print '>)
    (setq @n (read))
    (if
     (eq @n 'END-OF-FILE)
     (progn (close (in)) (in 0))
     (pprint (eval @n)))))
  nil))

(defq *mapgen*
 (lambda
  (f rest output input)
  (let
   ((fun (gensym))
    (vars (map rest '(lambda (x) (gensym)))))
   (` flet
    ((, fun , vars
      (if ,
       (car vars)
       (, output
        (, f ,@
         (map vars
          '(lambda (x) (if input (list input x) x))))
        (, fun ,@
         (map vars '(lambda (x) (list 'cdr x))))))))
    (, fun ,@ rest)))))

(defq *setf-method-1* ((color . color) (get . put) (aref . aset) (peek . poke) (peekw . pokew)))

(defq *struct*
 (nslambda (x) (print-struct x (cddr (eval (car x))))))

(defq + (macro (x) (*+-/ x 'plus)))

(defq -
 (macro
  (x)
  (list
   'difference
   (car x)
   (cons '+ (cdr x)))))

(defq / (macro (x) (*+-/ x 'quotient)))

(defq :
 (mlambda x
  (flet
   ((:3
     (arg x)
     (member (compress (nconc (explode ':) (explode arg))) x))
    (:2
     (args x)
     (if args
      (if
       (atom args)
       (cdr (:3 args x))
       (cons
        (if
         (atom (car args))
         (cadr (:3 (car args) x))
         (:2 (car args) x))
        (:2 (cdr args) x))))))
   (cons
    (car x)
    (:2 (cadr (definition-of (car x))) (cdr x))))))

(defq < (mlambda x (<=> x 'lessp)))

(defq <=>
 (lambda
  (x ope)
  (if
   (cddr x)
   (` and
    (, ope ,
     (pop x)
     ,
     (if
      (atom (car x))
      (progn (setq %temp (car x)) (pop x))
      (progn
       (setq %temp '%temp)
       (` setq %temp , (pop x)))))
    ,
    (<=> (cons %temp x) ope))
   (cons ope x))))

(defq = (mlambda x (<=> x 'eqn)))

(defq > (mlambda x (<=> x 'greaterp)))

(defq BOOT
 (any-key select-disk error-hook with-definition-of %tracep%
  * *+-/ *main-loop* *mapgen* *setf-method-1* *struct* +
  - / : < <=> = > BOOT END-OF-FILE IBM-trky LSEEK Mapc
  ` `2 append append-to-file autost break caaaar caaadr caaar
  caadar caaddr caadr cadaar cadadr cadar caddar case cd
  cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr
  cddar cdddar cddddr char chdir command-line comment compile
  compile-all copy date dd definition-of defmacro defstruct
  defun del-member delete delete-file delete-line dir
  dispose divide do dolist dos-eval dotimes edit edit-line edit3
  einsert eprint eprint2 erase_page fast-spawn fib fil-exists
  file-size filter flat flet for gensym get get-cur-dir get-cur-disk
  home insert-line intersection let let* load locate macroexpand
  make-exe map mapc mapcan mapcar mapcon maplist mergesort mkdir
  msdos nmerge nmerge1 nth nthdel nthpush nthswap orderp
  plm-item pop ppr-def pr-many princ print-struct print-to-file
  print-to-list prints push push-once put quit read-from-file read-from-list
  read-with-edit readcc-- rmdir save set_cursor setf
  setf-method sort sp spawn split string-append subst synon-for-prims
  time trace uncompile unless unlink untrace unwind-protect when
  where-is-ref))

(defq END-OF-FILE END-OF-FILE)

(defq IBM-trky
 (progn
  (setq tulos nil)
  (repeat
   (setq ch (readcc))
   (cond
    ((eq ch 27) t)
    ((zerop ch) (push (cons (readcc) (readcc)) tulos) nil)))))

(defq LSEEK
 (lambda
  (file method pos)
  (BX-reg file)
  (CX-reg (if pos (car pos) 0))
  (DX-reg (if pos (cdr pos) 0))
  (AX-reg (plus 16896 method))
  (if (INT- 33) (cons (DX-reg) (AX-reg)))))

(defq Mapc
 (mlambda (f . rest) (*mapgen* f rest 'progn 'car)))

(defq ` (macro (x) (`2 x)))

(defq `2
 (lambda
  (x)
  (cond
   ((atom x) x)
   ((eq (car x) ',)
    (list 'cons (cadr x) (`2 (cddr x))))
   ((eq (car x) ',@)
    (list 'append (cadr x) (`2 (cddr x))))
   ((atom (car x))
    (list
     'cons
     (list quote (car x))
     (`2 (cdr x))))
   ((equal (car x) '',)
    (list
     'cons
     (list 'list quote (cadr x))
     (`2 (cddr x))))
   (t
    (list 'cons (`2 (car x)) (`2 (cdr x)))))))

(defq append
 (lambda
  (x y)
  (if x (cons (car x) (append (cdr x) y)) y)))

(defq append-to-file
 (lambda
  (file x y)
  (setq y (out))
  (out (setq file (open file)))
  (LSEEK file 2)
  (pprint x)
  (cr)
  (close file)
  (out y)
  x))

(defq autost
 (progn
  (out 0)
  (uncompile)
  (cr)
  (hex t)
  (print (list (reclaim) 'nodes 'free))
  (hex nil)
  (echo nil)
  (comp-debug nil)
  (synon-for-prims)
  (if (null @t) (setq @t (list nil)))
  (compile 'macroexpand 'compile)
  (compile
   'readcc--
   'mapc
   'map
   'gensym
   'uncompile
   '`2
   'sp
   'append
   'nthpush
   'nthswap
   'nthdel
   'read-from-list
   'print-to-list
   'edit-line
   'read-with-edit
   '*main-loop*
   'compile-all)
  (cr)
  (print 'ok>)
  (readcc)
  (display-mode 3)
  (*main-loop*)))

(defq break
 (lambda
  (x&x y&y)
  (cr)
  (print 'BREAK:)
  (print x&x)
  (repeat
   (cr)
   (prints 'EVAL '(cont) ':)
   (setq y&y (read-with-edit))
   (if
    (eq y&y 'cont)
    t
    (progn (pprint (eval y&y)) (cr) nil)))))

(defq caaaar (lambda (x) (car (caaar x))))

(defq caaadr (lambda (x) (car (caadr x))))

(defq caaar (lambda (x) (car (caar x))))

(defq caadar (lambda (x) (car (cadar x))))

(defq caaddr (lambda (x) (car (caddr x))))

(defq caadr (lambda (x) (car (cadr x))))

(defq cadaar (lambda (x) (car (cdaar x))))

(defq cadadr (lambda (x) (car (cdadr x))))

(defq cadar (lambda (x) (car (cdar x))))

(defq caddar (lambda (x) (car (cddar x))))

(defq case
 (mlambda
  (x . y)
  (let
   ((x2 x)
    (y2
     (progn
      (unless (atom x) (setq x (gensym)))
      (cons
       'cond
       (map y
        (function
         (lambda
          (y)
          (cond
           ((eq (car y) 't) y)
           ((identp (car y))
            (`
             (eq , x ', (car y))
             ,@
             (cdr y)))
           ((atom (car y))
            (`
             (equal , x ', (car y))
             ,@
             (cdr y)))
           (t
            (`
             (member , x ', (car y))
             ,@
             (cdr y)))))))))))
   (if
    (eq x2 x)
    y2
    (` let ((, x , x2)) , y2)))))

(defq cd
 (lambda
  (x)
  (when x
   (when (member (char :) (explode x)) (select-disk x))
   (chdir x))
  (compress
   (nconc
    (explode (get-cur-disk))
    (nconc (explode ':\) (explode (get-cur-dir)))))))

(defq cdaaar (lambda (x) (cdr (caaar x))))

(defq cdaadr (lambda (x) (cdr (caadr x))))

(defq cdaar (lambda (x) (cdr (caar x))))

(defq cdadar (lambda (x) (cdr (cadar x))))

(defq cdaddr (lambda (x) (cdr (caddr x))))

(defq cdadr (lambda (x) (cdr (cadr x))))

(defq cdar (lambda (x) (cdr (car x))))

(defq cddaar (lambda (x) (cdr (cdaar x))))

(defq cddadr (lambda (x) (cdr (cdadr x))))

(defq cddar (lambda (x) (cdr (cdar x))))

(defq cdddar (lambda (x) (cdr (cddar x))))

(defq cddddr (lambda (x) (cdr (cdddr x))))

(defq char
 (mlambda
  (x)
  (cond
   ((or (identp x) (numberp x)) (car (explode x)))
   (t 116))))

(defq chdir
 (lambda (x) (DX-reg (ASCIIZ 0 x)) (msdos 59)))

(defq command-line
 (lambda ()
  (msdos 98)
  (let
   ((psp (BX-reg)) (line (list 40)))
   (for
    (p 0 (sub1 (peek psp 128)))
    (push (peek psp (plus 130 p)) line))
   (read-from-list (reverse line)))))

(defq comment (mlambda () ()))

(defq compile
 (lambda %x$
  (let
   ((defq
     (quote
      (mlambda
       (%x$ y)
       (list 'setq %x$ (list 'function y))))))
   (map %x$
    (quote
     (lambda
      (%x$)
      (if
       (or
        (not (identp %x$))
        (assoc %x$ *COMPILED-FUNCTIONS*)
        (atom (eval %x$))
        (numberp (car (eval %x$))))
       ()
       (let
        ((%exp% (macroexpand (eval %x$))) (%mac% nil) (%margs% nil))
        (when
         (member (car %exp%) '(mlambda macro nslambda nlambda))
         (setq %mac% (car %exp%))
         (setq %margs% (cadr %exp%))
         (let
          ((new (flat %margs%)))
          (setq %exp%
           (cons
            (list
             'function
             (cons 'lambda (cons new (cddr %exp%))))
            new))))
        (push (list %x$ (eval %x$)) *COMPILED-FUNCTIONS*)
        (progv (list %x$) (set %x$ %exp%) (setq %exp% (ncompile %exp%)))
        (set %x$ (if %mac% (list %mac% %margs% %exp%) %exp%))
        %x$))))))))

(defq compile-all
 (lambda
  (name callers)
  (let
   ((body (eval name)))
   (when
    (member (car body) '(lambda progn))
    (setq body (macroexpand body))
    (flet
     ((no-eval
       (body)
       (cond
        ((eq (car body) 'quote) nil)
        ((identp body)
         (cond
          ((eq body name) nil)
          ((member body callers) nil)
          ((member (car (eval body)) '(lambda progn))
           (compile-all body (cons name callers)))))
        ((atom body) nil)
        (t (append (no-eval (car body)) (no-eval (cdr body)))))))
     (append (no-eval body) (compile name)))))))

(defq copy
 (lambda
  (x)
  (if
   (atom x)
   x
   (cons (copy (car x)) (copy (cdr x))))))

(defq date
 (progn
  (msdos 42)
  (list
   (low-byte (DX-reg))
   '/
   (high-byte (DX-reg))
   '-
   (CX-reg))))

(defq dd
 (let
  ((all (dir '*.* 16)) (tav (dir)))
  (filter all '(lambda (x) (not (member x tav))))))

(defq definition-of
 (lambda
  (x)
  (let
   ((y (assoc x *COMPILED-FUNCTIONS*)))
   (if y (cadr y) (eval x)))))

(defq defmacro
 (mlambda
  (x . y)
  (` prog1
   (if , x
    (list ', x 'redefined)
    ',
    x)
   (defq , x (mlambda ,@ y)))))

(defq defstruct
 (nlambda
  (name . slots)
  (push-once ': *include*)
  (push-once name *last-load*)
  (set name (cons 'defstruct (cons name slots)))
  (let
   ((ope '(list 'cddr x))
    (mname (compress (nconc (explode 'make-) (explode name))))
    (pname (compress (nconc (explode 'print-) (explode name)))))
   (set mname
    (` mlambda , slots
     (list
      'list
      ''*struct*
      '(, quote , name)
      ,@ slots)))
   (set pname
    (` mlambda
     (x)
     (list
      'print-struct
      (list 'cdr x)
      '(, quote , slots))))
   (push-once mname *include*)
   (push-once pname *include*)
   (mapc slots
    (function
     (lambda
      (slot)
      (let*
       ((gname
         (compress
          (nconc (explode name) (nconc (explode '-) (explode slot)))))
        (pname (compress (nconc (explode 'set-) (explode gname)))))
       (set gname (` mlambda (x) (list 'car , ope)))
       (set pname
        (` mlambda
         (x y)
         (list 'setf (list 'car , ope) y)))
       (push-once (cons gname pname) *setf-method*)
       (push-once gname *include*)
       (push-once pname *include*)
       (setq ope (` list 'cdr , ope)))))))
  name))

(defq defun
 (mlambda (x . y) (` defq , x (lambda ,@ y))))

(defq del-member
 (lambda
  (x y)
  (if y
   (if
    (equal x (car y))
    (cdr y)
    (rplacd y (del-member x (cdr y)))))))

(defq delete
 (lambda
  (x y)
  (cond
   ((atom y) y)
   ((null y) y)
   ((equal (car y) x) (delete x (cdr y)))
   (t
    (cons (delete x (car y)) (delete x (cdr y)))))))

(defq delete-file (lambda (x) (map (dir x) unlink)))

(defq delete-line
 (lambda
  (x)
  (AX-reg (plus 1536 1))
  (CX-reg (times 256 (sub1 x)))
  (DX-reg 6223)
  (BX-reg 1792)
  (INT- 16)))

(defq dir
 (lambda
  (x attr)
  (unless x (setq x '*.*))
  (unless attr (setq attr 0))
  (let
   ((tulos (list (find-first x attr))) (one))
   (if tulos (while (setq one (find-next attr)) (push one tulos)))
   tulos)))

(defq dispose
 (lambda
  (x)
  (if
   (not (atom x))
   (progn (rplaca x 'roska) (rplacd x 'roska)))))

(defq divide
 (lambda (x y) (cons (quotient x y) (remainder x y))))

(defq do
 (mlambda
  (vars (test retu) . rest)
  (` let , vars
   (repeat-until , test ,@ rest ,@
    (map vars
     (quote
      (lambda
       (x)
       (if
        (cddr x)
        (list 'setq (car x) (caddr x)))))))
   , retu)))

(defq dolist
 (mlambda
  ((v l) . rest)
  (let
   ((s (gensym)))
   (` let
    ((, s , l) (, v nil))
    (while , s (setq , v (pop , s)) ,@ rest)))))

(defq dos-eval
 (lambda
  (x)
  (let
   ((c (explode '/C)))
   (dolist
    (y x)
    (nconc c (list 32))
    (nconc c (explode y)))
   (fast-spawn () (compress c)))))

(defq dotimes
 (mlambda
  ((var up) . rest)
  (` for (, var 0 , (sub1 up)) ,@ rest)))

(defq edit
 (nlambda
  (name)
  (display-mode 2)
  (unless
   (atom edit3)
   (print 'WAIT)
   (compile
    'erase_page
    'eprint2
    'eprint
    'prints
    'delete-line
    'insert-line
    'locate
    'einsert
    'copy
    'subst
    'edit3))
  (hex)
  (if name (setq *LAST-EDIT* nil) (setq name (pop *LAST-EDIT*)))
  (with-definition-of name
   (untrace name)
   (unless
    (member name (eval *package*))
    (cr)
    (prints name
     'is
     'not
     'member
     'of
     'this
     ':)
    (setq *package* (edit-line *package*))
    (if
     (null (eval *package*))
     (set *package* (list name *package*))
     (if
      (not (member name (eval *package*)))
      (set *package* (cons name (eval *package*))))))
   (setq edit-bye ())
   (set name (edit3 (eval name) 0 *LAST-EDIT*))
   (push name *LAST-EDIT*)
   (set_cursor 22 1)
   (erase_page))
  name))

(defq edit-line
 (lambda
  (line pos ch pc long-line)
  (flet
   ((p1
     (x)
     (while x (printc (pop x)))
     (printc 32)
     (printc 8))
    (cuf () (printc (nth pos line)))
    (pr-rest
     (x)
     (p1 x)
     (sp)
     (repeat-times (add1 (length x)) (printc 8)))
    (push-ch
     (ch)
     (printc ch)
     (setq line (nthpush pos ch line))
     (setq pos (add1 pos))
     (pr-rest (nthcdr pos line)))
    (refresh ()
     (repeat-times pos (printc 8))
     (p1 line)
     (repeat-times (- (length line) pos) (printc 8)))
    (l-line
     (z)
     (while
      (greaterp (length line) 70)
      (push (pop line) long-line)
      (setq pos (sub1 pos))
      (printc 8)
      (setq z t))
     (while
      (and long-line (lessp (length line) 70))
      (push (pop long-line) line)
      (cuf)
      (setq pos (add1 pos))
      (setq z t))
     (if z (refresh))))
   (setq *UP/DO* nil)
   (setq line
    (if
     (eq line '####)
     (setq line (list 32))
     (print-to-list line)))
   (while (nthcdr 70 line) (push (pop line) long-line))
   (setq pos (length line))
   (p1 line)
   (repeat
    (l-line)
    (setq pc ch)
    (setq ch
     (readcc--
      (quote
       ((82 . 32)
        (83 . 4)
        (80 . 14)
        (77 . 6)
        (72 . 16)
        (71 . 1)
        (79 . 5)
        (75 . 2)))))
    (cond
     ((and
       (greaterp pos 0)
       (or (eqn ch 8) (eqn ch 127)))
      (setq line (nthdel (sub1 pos) line))
      (setq pos (sub1 pos))
      (printc 8)
      (pr-rest (nthcdr pos line))
      nil)
     ((greaterp ch 31) (push-ch ch) nil)
     ((null line) nil)
     ((eqn ch 1)
      (repeat-times pos (printc 8))
      (setq pos 0)
      nil)
     ((eqn ch 18) (refresh) nil)
     ((eqn ch 2)
      (when (greaterp pos 0) (printc 8) (setq pos (sub1 pos)))
      nil)
     ((eqn ch 6)
      (when
       (lessp pos (length line))
       (cuf)
       (setq pos (add1 pos)))
      nil)
     ((eqn ch 16) (setq *UP/DO* 'UP))
     ((eqn ch 14) (setq *UP/DO* 'DOWN))
     ((eqn ch 4)
      (setq line (nthdel pos line))
      (pr-rest (nthcdr pos line))
      nil)
     ((eqn ch 5)
      (repeat-times (difference (length line) pos) (cuf))
      (setq pos (length line))
      nil)
     ((eqn ch 13) t)
     ((eqn ch 9)
      (when
       (eqn pos (length line))
       (setq apr nil)
       (setq line (reverse line))
       (repeat-until
        (or
         (null line)
         (member (car line) '(40 41 39 32)))
        (printc 8)
        (printc 32)
        (printc 8)
        (push (pop line) apr))
       (setq line (reverse line))
       (when apr
        (unless
         (equal pc 9)
         (setq apr (str-compress apr))
         (setq *PAPRL* (apropos apr)))
        (if *PAPRL*
         (setq apr (explode (pop *PAPRL*)))
         (setq apr (list 63)))
        (p1 apr)
        (setq line (append line apr)))
       (setq pos (length line)))
      nil)
     ((eqn ch 3) (error-reset ^C))
     (t
      (mapc
       (nconc (explode '^) (list (plus ch 64)))
       push-ch)
      nil)))
   (if *UP/DO* (repeat-times pos (printc 8)) (cr))
   (read-from-list (append (reverse long-line) line)))))

(defq edit3
 (lambda
  (v dept goto p ch *MORE*)
  (setq v (append v))
  (setq p (if goto (pop goto) 0))
  (if goto
   (progn (cr) (print goto))
   (progn
    (eprint v)
    (set_cursor 1 40)
    (prints *package* name dept p)
    (cr)))
  (repeat
   (when
    (greaterp p 22)
    (while
     (greaterp p 22)
     (setq p (sub1 p))
     (push (pop v) *MORE*))
    (unless goto (eprint v)))
   (setq THIS (nth p v))
   (if goto
    (setq ch 6)
    (progn
     (set_cursor (plus 1 p) 1)
     (cr)
     (repeat
      (greaterp
       (setq ch
        (readcc--
         (quote
          ((62 . 114)
           (61 . 99)
           (60 . 119)
           (59 . 97)
           (83 . 121)
           (82 . 110)
           (73 . 57)
           (72 . 56)
           (71 . 55)
           (77 . 54)
           (75 . 52)
           (81 . 51)
           (80 . 50)
           (79 . 49)))))
       32))
     (setq ch (compress (list ch)))))
   (cond
    ((member ch '(1 2 3))
     (let
      ((cou 21))
      (repeat
       (if
        (lessp p (length v))
        (if
         (greaterp p 21)
         (progn
          (set_cursor 1 1)
          (print '*MORE*)
          (push (pop v) *MORE*)
          (delete-line 2)
          (insert-line 24)
          (set_cursor 24 4)
          (eprint2 (nth 22 v)))
         (setq p (add1 p))))
       (or
        (eqn ch 2)
        (unless (nthcdr p v) (eprint v) t)
        (and (eqn ch 3) (zerop (setq cou (sub1 cou))))))))
    ((member ch '(7 8 9))
     (let
      ((cou 22))
      (repeat
       (if
        (lessp p 1)
        (when *MORE*
         (push (pop *MORE*) v)
         (delete-line 24)
         (insert-line 2)
         (set_cursor 2 4)
         (eprint2 (car v)))
        (setq p (sub1 p)))
       (or
        (eqn ch 8)
        (unless *MORE* (eprint v) t)
        (and (eqn ch 9) (zerop (setq cou (sub1 cou))))))))
    ((equal ch 6)
     (if
      (atom THIS)
      (unless goto
       (repeat-times 5 (sp))
       (rplaca (nthcdr p v) (edit-line THIS))
       (eprint v))
      (progn
       (rplaca (cdxr p v) (edit3 THIS (add1 dept) goto))
       (if edit-bye (push (plus (length *MORE*) p) *LAST-EDIT*) (eprint v))))
     (setq goto nil))
    ((eq ch 'l)
     (set_cursor 25 1)
     (print 'locate:)
     (if
      (setq goto (locate (read-with-edit) v))
      (setq p (pop goto))
      (eprint v)))
    ((eq ch '-)
     (print 'BYE)
     (setq edit-bye t)
     (setq *LAST-EDIT* (list (plus (length *MORE*) p))))
    ((eq ch 'v)
     (setq ch (nth p v))
     (rplaca (cdxr p v) (car (cdxr (add1 p) v)))
     (rplaca (cdxr (add1 p) v) ch)
     (setq p (add1 p))
     (eprint v))
    ((eq ch 'b)
     (setq v (einsert (copy (car (cdxr p v))) p v)))
    ((eq ch 's)
     (set_cursor 24 1)
     (print 'SUBST:)
     (eprint
      (setq v
       (subst (read-with-edit) (progn (print 'WITH:) (read-with-edit)) v))))
    ((eq ch 'y)
     (delete-line (plus p 2))
     (push (nth p v) *JEMMA*)
     (setq v (nthdel p v))
     (if (greaterp (length v) 20) (eprint v)))
    ((eq ch 'p)
     (home)
     (erase_page)
     (cr)
     (hex *HEX*)
     (pprint (append (reverse *MORE*) v))
     (hex nil)
     (cr)
     (reclaim)
     (readcc)
     (eprint v))
    ((eq ch '+)
     (setq v (nthpush p (car *JEMMA*) v))
     (nconc *JEMMA* (list (car *JEMMA*)))
     (pop *JEMMA*)
     (eprint v))
    ((eq ch 'n)
     (insert-line (plus p 2))
     (set_cursor (plus p 2) 5)
     (setq v (nthpush p (read-with-edit) v))
     (setq p (add1 p)))
    ((and (eq ch 'r) (not (atom THIS)))
     (push nil v)
     (rplacd
      (cdxr p v)
      (nconc
       (car (cdxr (add1 p) v))
       (cdxr (plus p 2) v)))
     (pop v)
     (eprint v))
    ((eq ch 'a)
     (rplaca (cdxr p v) (list (car (cdxr p v))))
     (eprint v))
    ((and (eq ch 'c) (not (atom THIS)))
     (nconc THIS (list (car (cdxr (add1 p) v))))
     (rplacd (cdxr p v) (cdxr (plus 2 p) v))
     (eprint v))
    ((eq ch 'w)
     (push nil v)
     (rplacd
      (cdxr p v)
      (cons
       (list
        (car (cdxr (add1 p) v))
        (car (cdxr (plus p 2) v)))
       (cdxr (plus 3 p) v)))
     (pop v)
     (eprint v))
    ((eq ch 'q)
     (rplaca
      (cdxr p v)
      (list 'quote (car (cdxr p v))))
     (eprint v))
    ((equal ch 4))
    ((eq ch 'e)
     (set_cursor 25 1)
     (print 'EVAL:)
     (let
      ((kama (err-set (eval (read-with-edit)))))
      (if kama (setq v (einsert (car kama) p v)) (readcc)))
     (eprint v))
    ((and (eq ch 'z) (identp THIS))
     (let
      ((*package* *package*) (*LAST-EDIT* nil) (edit-bye edit-bye))
      (eval (list 'edit THIS)))
     (eprint v)
     ())
    ((eq ch 'k)
     (cr)
     (set_cursor 25 10)
     (print '(RAKENTEEN TAYDELLINEN KOPIO - ODOTA !!))
     (cr)
     (setq v (copy v))
     (eprint v))
    ((eq ch 'f) (eprint v))
    ((eq ch 'h)
     (setq *HEX* (not *HEX*))
     (eprint v))
    ((eq ch '?)
     (home)
     (erase_page)
     (cr)
     (pprint
      (quote
       ((numerot = ylos alas sisaan ulos etc)
        (l S = etsi S)
        (n S = tunge tahan S)
        (e S = tunge tahan (eval S))
        (y = poista tama)
        (p = nayta kunnolla)
        (r = poista sulut)
        (a = lisaa sulut)
        (w = yhdista kaksi)
        (s = korvaa tama talla)
        (b = tama tublana)
        (v = vaihda 2 keskenaan)
        (z = editoi tunnuksen arvoa)
        (c = jatka tata listaa sita seuraavalla)
        (f = virkista naytto)
        (k = fl kopio tasta)
        (q = tahan kojootti)
        (- = ulos kaikesta)
        (h = hexa-tulostus JUU/EI))))
     (cr)
     (readcc)
     (eprint v))
    (t
     (cr)
     (set_cursor 22 10)
     (printc 7)
     (print '(mantti! jos haluat apua paina " ? "))
     (cr)))
   (or edit-bye (equal ch 4)))
  (if *MORE* (nconc (reverse *MORE*) v) v)))

(defq einsert
 (lambda
  (x p y)
  (push nil y)
  (rplacd (cdxr p y) (cons x (cdxr (add1 p) y)))
  (insert-line (plus p 2))
  (set_cursor (plus p 2) 4)
  (eprint2 x)
  (cdr y)))

(defq eprint
 (lambda
  (x)
  (home)
  (erase_page)
  (if *MORE* (print '*MORE*) (printc 40))
  (cr)
  (for
   (p 0 22)
   (when
    (nthcdr p x)
    (tab 3)
    (eprint2 (nth p x))
    (cr)))
  (if (nthcdr 23 x) (print '*MORE*) (printc 41))))

(defq eprint2
 (lambda
  (x)
  (hex *HEX*)
  (sp)
  (sp)
  (if
   (atom x)
   (print x)
   (let
    ((dec 10))
    (printc 40)
    (while
     (and x (lessp (tab) 60))
     (cond
      ((atom x)
       (printc 46)
       (sp)
       (print x)
       (setq x nil))
      ((lessp dec 0) (print '&))
      ((atom (car x))
       (print (car x))
       (if (cdr x) (sp)))
      ((depthl dec (car x)) (print (car x)))
      (t (print '&) (sp)))
     (setq dec (plus dec -3))
     (pop x))
    (printc 41)))
  (hex nil)))

(defq erase_page (progn (repeat-times 49 (cr)) (home)))

(defq fast-spawn (lambda (x y) (spawn x y 'DONT%SAVE%IMA)))

(defq fib
 (lambda
  (x)
  (if
   (< x 2)
   x
   (+ (fib (1- x)) (fib (- x 2))))))

(defq fil-exists (lambda (x) (find-first x 0)))

(defq file-size
 (lambda
  (file)
  (setq file (open file))
  (prog1 (LSEEK file 2) (close file))))

(defq filter
 (lambda
  (m%x m%f)
  (if m%x
   (if
    (m%f (car m%x))
    (cons (car m%x) (filter (cdr m%x) m%f))
    (filter (cdr m%x) m%f)))))

(defq flat
 (lambda
  (x)
  (if
   (atom x)
   (list x)
   (if
    (cdr x)
    (nconc (flat (car x)) (flat (cdr x)))
    (flat (car x))))))

(defq flet
 (mlambda
  (defi . body)
  (` let-functions ,
   (map defi
    '(lambda ((name . body)) (` , name (lambda ,@ body))))
   ,@ body)))

(defq for
 (mlambda
  ((var alku loppu steppi) . body)
  (if (null steppi) (setq steppi 1))
  (` let
   ((, var , alku))
   (repeat-times ,
    (if
     (and (numberp alku) (numberp loppu) (equal steppi 1))
     (difference (add1 loppu) alku)
     (` quotient (difference (plus , loppu , steppi) , alku) , steppi))
    ,@ body
    (setq , var ,
     (if
      (equal steppi 1)
      (` add1 , var)
      (` plus , var , steppi)))))))

(defq gensym
 (lambda
  (x)
  (unless x (setq x '..))
  (compress
   (nconc
    (explode x)
    (explode (setq %gensym-seed% (if (null %gensym-seed%) 1 (add1 %gensym-seed%))))))))

(defq get
 (lambda (name pname) (cadr (assoc pname (assoc name properties)))))

(defq get-cur-dir
 (progn
  (SI-reg (ASCIIZ 0 '*))
  (DX-reg 0)
  (msdos 71)
  (UNASCIIZ (SI-reg))))

(defq get-cur-disk
 (progn
  (msdos 25)
  (compress (list (plus 65 (low-byte (AX-reg)))))))

(defq home (mlambda () '(set_cursor 1 1)))

(defq insert-line
 (lambda
  (x)
  (AX-reg (plus 1792 1))
  (CX-reg (times 256 (sub1 x)))
  (DX-reg 6223)
  (BX-reg 1792)
  (INT- 16)))

(defq intersection
 (lambda
  (x y)
  (if x
   (if
    (member (car x) y)
    (cons (car x) (intersection (cdr x) y))
    (intersection (cdr x) y)))))

(defq let
 (mlambda
  (vars . rest)
  (`
   (function
    (lambda , (if (caar vars) (map vars car) vars) ,@ rest))
   ,@
   (if (caar vars) (map vars cadr)))))

(defq let*
 (mlambda
  (vars . body)
  (let
   ((let2
     (function
      (lambda
       (vars body)
       (if vars
        (`
         (let (, (car vars)) ,@ (let2 (cdr vars) body)))
        body)))))
   (car (let2 vars body)))))

(defq load
 (lambda
  (load-file silent)
  (if
   (fil-exists load-file)
   (in
    (prog1
     (in)
     (in (open load-file))
     (repeat
      (setq @n (read))
      (cond
       ((eq @n 'END-OF-FILE) (close (in)) t)
       (t
        (setq @e (eval @n))
        (unless silent
         (cr)
         (print 'file)
         (print '>)
         (print @e))
        (zerop (in)))))))
   (list 'There 'is 'no load-file))))

(defq locate
 (lambda
  (x y)
  (if
   (atom y)
   nil
   (if
    (member x y)
    (list (sub1 (length (member x (reverse y)))))
    (let
     ((z 0) (z2))
     (while
      (and y (not z2))
      (if
       (not (setq z2 (locate x (pop y))))
       (setq z (add1 z))))
     (if z2 (cons z z2)))))))

(defq macroexpand
 (lambda
  (%x$ hantaa-vaan)
  (cond
   ((atom %x$) %x$)
   ((eq (car %x$) quote) %x$)
   ((eq (car %x$) 'lambda)
    (cons (car %x$) (cons (cadr %x$) (macroexpand (cddr %x$)))))
   ((and
     (eq (car %x$) 'if)
     (member (car (cadr %x$)) '(null not)))
    (macroexpand
     (list
      'if
      (cadr (cadr %x$))
      (cadddr %x$)
      (caddr %x$))))
   ((and
     (not hantaa-vaan)
     (identp (car %x$))
     (member (car (eval (car %x$))) '(macro mlambda)))
    (macroexpand
     (eval
      (cons
       (list quote
        (cons
         (if
          (eq (car (eval (car %x$))) 'mlambda)
          'nlambda
          'nslambda)
         (cdr (eval (car %x$)))))
       (cdr %x$)))))
   (t (cons (macroexpand (car %x$)) (macroexpand (cdr %x$) t))))))

(defq make-exe
 (progn
  (edit fib)
  (setq @t nil)
  (setq *LINE-NUMB* 0)
  (compile 'dir 'command-line)
  (list (date) (continue-in 'noko.exe) (eval (command-line)))))

(defq map
 (lambda
  (m%x m%f)
  (cond
   (m%x (cons (m%f (car m%x)) (map (cdr m%x) m%f))))))

(defq mapc
 (lambda (m%x m%f) (while m%x (m%f (pop m%x)))))

(defq mapcan
 (mlambda (f . rest) (*mapgen* f rest 'nconc 'car)))

(defq mapcar
 (mlambda (f . rest) (*mapgen* f rest 'cons 'car)))

(defq mapcon (mlambda (f . rest) (*mapgen* f rest 'nconc)))

(defq maplist (mlambda (f . rest) (*mapgen* f rest 'cons)))

(defq mergesort
 (lambda
  (a)
  (cond
   ((null (cdr a)) a)
   ((null (cddr a))
    (if
     (orderp (car a) (cadr a))
     a
     (let
      ((tmp (cadr a)))
      (rplaca (cdr a) (car a))
      (rplaca a tmp))))
   (t
    (let
     ((b (split a)))
     (nmerge (mergesort a) (mergesort b)))))))

(defq mkdir
 (lambda (x) (DX-reg (ASCIIZ 0 x)) (msdos 57)))

(defq msdos
 (mlambda
  (x)
  (` progn
   (AX-reg ,
    (if (numberp x) (times 256 x) (` times 256 , x)))
   (INT- 33))))

(defq nmerge
 (lambda
  (a b)
  (cond
   ((null a) b)
   ((null b) a)
   ((orderp (car a) (car b)) (nmerge1 a b))
   (t (nmerge1 b a)))))

(defq nmerge1
 (lambda
  (a b)
  (let
   ((res a) (tmp a))
   (flet
    ((ali
      (x y z)
      (while
       (and x (orderp (car x) (car y)))
       (setq tmp x)
       (pop x))
      (rplacd tmp y)
      (setq tmp y)
      (pop y)
      (setq a (if z x y))
      (setq b (if z y x))))
    (pop a)
    (while b
     (ali a b t)
     (if a (ali b a) (setq b nil)))
    res))))

(defq nth
 (mlambda (x y) (` car (nthcdr , x , y))))

(defq nthdel
 (lambda
  (x y)
  (if y
   (if
    (zerop x)
    (cdr y)
    (rplacd y (nthdel (sub1 x) (cdr y)))))))

(defq nthpush
 (lambda
  (x y z)
  (if
   (zerop x)
   (cons y z)
   (rplacd z (nthpush (sub1 x) y (cdr z))))))

(defq nthswap
 (lambda
  (x y z)
  (if
   (zerop x)
   (rplaca z y)
   (nthswap (sub1 x) y (cdr z)))))

(defq orderp
 (lambda
  (x y)
  (cond
   ((equal x y) t)
   ((null x) t)
   ((null y) nil)
   ((atom x)
    (if
     (atom y)
     (cond
      ((numberp x)
       (if (numberp y) (not (greaterp x y)) t))
      ((identp x)
       (if
        (identp y)
        (let
         ((n 1))
         (while
          (eqn (getchar x n) (getchar y n))
          (setq n (add1 n)))
         (not (greaterp (getchar x n) (getchar y n))))
        nil))
      (t t))
     t))
   ((atom y) nil)
   ((equal (car x) (car y))
    (orderp (cdr x) (cdr y)))
   (t (orderp (car x) (car y))))))

(defq plm-item
 (lambda
  (ch)
  (flet
   ((upcase
     (x)
     (if
      (and (greaterp x 96) (lessp x 123))
      (plus x -32)
      x))
    (lex-char
     (x)
     (or
      (and (greaterp x 64) (lessp x 91))
      (and (greaterp x 96) (lessp x 123))
      (and (greaterp x 47) (lessp x 58))
      (eq x 36)
      (eq x 95))))
   (repeat
    (or (greaterp (setq ch (readc)) 32) (eqn ch 26)))
   (cond
    ((and (eqn ch 47) (eqn (nxtch) 42))
     (repeat (and (eqn (readc) 42) (eqn (nxtch) 47)))
     (readc)
     (plm-item))
    ((eq ch 26) 'END-OF-FILE)
    ((lex-char ch)
     (let
      ((resu (list (upcase ch))))
      (repeat-until (not (lex-char (nxtch))) (push (upcase (readc)) resu))
      (compress (nreverse resu))))
    (t (compress (list ch)))))))

(defq pop
 (mlambda
  (x)
  (` prog1 (car , x) (setf , x (cdr , x)))))

(defq ppr-def
 (lambda
  (x body)
  (pprint
   (if
    (eq (car body) 'lambda)
    (cons 'defun (cons x (cdr body)))
    (if
     (eq (car body) 'mlambda)
     (cons 'defmacro (cons x (cdr body)))
     (if
      (eq (car body) 'defstruct)
      body
      (list 'defq x body)))))))

(defq pr-many
 (lambda x (while x (print (pop x)) (sp))))

(defq princ (lambda (x) (print x)))

(defq print-struct
 (lambda
  (x sdef)
  (let
   ((y (add1 (tab))))
   (printc 40)
   (prin1 (pop x))
   (while sdef
    (cr)
    (tab y)
    (prin1 (pop sdef))
    (printc 58)
    (tab (plus y 10))
    (sp)
    (if
     (eq (caar x) '*struct*)
     (eval (pop x))
     (prin1 (pop x))))
   (printc 41))
  x))

(defq print-to-file
 (lambda
  (file x y)
  (setq y (out))
  (out (setq file (create file)))
  (pprint x)
  (cr)
  (close file)
  (out y)
  x))

(defq print-to-list
 (lambda
  (x)
  (let
   ((line (list nil)))
   (flet
    ((printc (x) (nconc line (list x)))
     (print-in-lisp
      (x)
      (cond
       ((null x) (printc 40) (printc 41))
       ((eq x t) (printc 116))
       ((eq x quote)
        (mapc '(113 117 111 116 101) printc))
       ((atom x) (mapc (explode x) printc))
       ((eq (car x) quote) (printc 39) (print-in-lisp (cadr x)))
       (t
        (printc 40)
        (let
         ((x x))
         (while x
          (print-in-lisp (pop x))
          (if x
           (if
            (atom x)
            (progn
             (printc 32)
             (printc 46)
             (printc 32)
             (print-in-lisp x)
             (setq x ()))
            (printc 32)))))
        (printc 41)))
      x))
    (print-in-lisp x))
   (cdr line))))

(defq prints
 (lambda x (while x (print (pop x)) (sp))))

(defq push
 (mlambda
  (x y)
  (` setf , y (cons , x , y))))

(defq push-once
 (mlambda
  (x y)
  (if
   (atom x)
   (` if
    (not (member , x , y))
    (push , x , y))
   (` let ((%once% , x)) (push-once %once% , y)))))

(defq put
 (lambda
  (name pname prop)
  (prog
   (f1 f2)
   (cond
    ((null properties)
     (setq properties (list (list name (list pname prop)))))
    (t
     (setq f1 (assoc name properties))
     (cond
      (f1
       (setq f2 (assoc pname (cdr f1)))
       (cond
        (f2 (rplacd f2 (list prop)))
        (t (nconc f1 (list (list pname prop))))))
      (t (setq properties (cons (list name (list pname prop)) properties)))))))))

(defq quit
 (lambda (x) (unless x (setq x 0)) (nquit 0)))

(defq read-from-file
 (lambda
  (file x y)
  (setq y (in))
  (in (setq file (open file)))
  (setq x (read))
  (close file)
  (in y)
  x))

(defq read-from-list
 (lambda
  (line fratom)
  (flet
   ((readc () (or (pop line) 41))
    (nxtch () (or (car line) 41))
    (read-in-lisp ()
     (let
      ((ch 32))
      (setq fratom t)
      (flet
       ((readident
         (ch)
         (let
          ((str (list ch)))
          (while
           (and
            (not (member (nxtch) '(40 41)))
            (greaterp (nxtch) 32))
           (nconc str (list (readc))))
          (compress str)))
        (readlist ()
         (let
          ((i (read-in-lisp)) (c2 nil))
          (if
           (and fratom (eq i nil))
           (setq fratom nil)
           (let
            ((temp i) (c1 nil))
            (while (lessp (nxtch) 33) (setq c1 (readc)))
            (setq c2 (nxtch))
            (setq i (readlist))
            (if
             (eqn c2 46)
             (cons temp (cadr i))
             (cons temp i)))))))
       (repeat (setq ch (readc)) (greaterp ch 32))
       (cond
        ((eqn ch 40) (readlist))
        ((eqn ch 41) nil)
        ((eqn ch 39) (list quote (read-in-lisp)))
        (t (readident ch)))))))
   (read-in-lisp))))

(defq read-with-edit (edit-line '####))

(defq readcc--
 (lambda
  (y x)
  (setq x (readcc))
  (if
   (zerop x)
   (or
    (cdr (assoc (setq x (readcc)) y))
    (+ x 128))
   x)))

(defq rmdir
 (lambda (x) (DX-reg (ASCIIZ 0 x)) (msdos 58)))

(defq save
 (lambda
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
  (progn
   (out (create 'c:\bat\nc.bat))
   (pr-many 'noko 'load (list 'quote y))
   (close (out))
   (out 0))
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
  (close y)))

(defq set_cursor
 (lambda
  (y x)
  (AX-reg 512)
  (BX-reg 0)
  (DX-reg (+ (1- x) (* 256 (1- y))))
  (INT- 16)))

(defq setf (mlambda (to from) (setf-method to from)))

(defq setf-method
 (lambda
  (to from)
  (let
   ((simp
     (cdr
      (or (assoc (car to) *setf-method*) (assoc (car to) *setf-method-1*)))))
   (cond
    (simp (nconc (cons simp (cdr to)) (list from)))
    ((identp to) (` setq , to , from))
    ((atom to) ())
    ((eq (car to) 'nth)
     (` car (rplaca (nthcdr ,@ (cdr to)) , from)))
    ((eq (car to) 'nthcdr)
     (` cdr
      (rplacd
       (nthcdr (sub1 , (cadr to)) , (caddr to))
       , from)))
    (t
     (let
      ((ope (explode (car to))))
      (cond
       ((and
         (eqn (car ope) 99)
         (eqn (car (last ope)) 114))
        (pop ope)
        (list
         (if (eqn (car ope) 97) 'car 'cdr)
         (list
          (if (eqn (car ope) 97) 'rplaca 'rplacd)
          (if
           (cddr ope)
           (list (compress (cons 99 (cdr ope))) (cadr to))
           (cadr to))
          from)))
       (t (` print '(? setf , to , from))))))))))

(defq sort
 (lambda
  (x no-copy)
  (unless
   (atom mergesort)
   (compile
    'orderp
    'nmerge1
    'nmerge
    'split
    'mergesort))
  (mergesort (if no-copy x (nreverse (reverse x))))))

(defq sp (printc 32))

(defq spawn
 (lambda
  (com-file args ima-file)
  (unless com-file
   (setq com-file 'Z:\command.com)
   (unless (fil-exists com-file) (setq com-file 'C:\command.com)))
  (if
   (fil-exists com-file)
   (progn
    (unless args (setq args (compress '(32 32))))
    (unless ima-file (setq ima-file 'C:\nokolisp.ima))
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
   (list 'There 'is 'no com-file))))

(defq split
 (lambda
  (l)
  (and
   (cdr l)
   (setq l (nthcdr (1- (quotient (length l) 2)) l))
   (prog1 (cdr l) (rplacd l nil)))))

(defq string-append
 (lambda x
  (compress
   (mapcan
    (function (lambda (x) (if x (explode x) (list 32))))
    x))))

(defq subst
 (lambda
  (old new tree)
  (cond
   ((null tree) tree)
   ((equal old tree) new)
   ((atom tree) tree)
   (t
    (cons
     (subst old new (car tree))
     (subst old new (cdr tree)))))))

(defq synon-for-prims
 (progn
  (setq prin1 print)
  (setq selectq case)
  (setq nth
   (quote
    (mlambda (x y) (` car (nthcdr , x , y)))))
  (setq nthcdr cdxr)
  (setq terpri cr)
  (setq 1+ add1)
  (setq 1- sub1)
  (setq peek-char nxtch)
  (setq read-char readc)
  (setq read-char-no-hang readcc)
  (setq symbolp identp)
  (setq listp
   (quote
    (lambda (x) (or (null x) (not (atom x))))))))

(defq time
 (progn
  (msdos 44)
  (list
   (high-byte (CX-reg))
   ':
   (low-byte (CX-reg))
   ':
   (high-byte (DX-reg))
   (low-byte (DX-reg)))))

(defq trace
 (lambda
  (name)
  (setq %trace-tab% 0)
  (with-definition-of name
   (let
    ((def (definition-of name)))
    (if
     (eq (car def) 'lambda)
     (unless
      (eq (car (caddr def)) '%tracep%)
      (setf
       (cddr def)
       (` (%tracep% , name , (cadr def) , (cddr def))))))))))

(defq uncompile
 (lambda x
  (if x
   (map
    (reverse x)
    (function
     (lambda
      (x)
      (let
       ((comp (assoc x *COMPILED-FUNCTIONS*)))
       (if comp
        (progn
         (if (eq comp (car *COMPILED-FUNCTIONS*)) (no-compiled-code (eval x)))
         (setq *COMPILED-FUNCTIONS* (del-member comp *COMPILED-FUNCTIONS*))
         (set x (cadr comp))
         (car comp)))))))
   (let
    ((name nil))
    (mapc *COMPILED-FUNCTIONS*
     (function
      (lambda
       (x)
       (push (car x) name)
       (set (car x) (cadr x)))))
    (setq *COMPILED-FUNCTIONS* nil)
    (unless (atom *main-loop*) (no-compiled-code))
    name))))

(defq unless
 (mlambda
  (x . y)
  (list
   'if
   (list 'not x)
   (cons 'progn y))))

(defq unlink
 (lambda
  (x)
  (when x (DX-reg (ASCIIZ 0 x)) (if (msdos 65) x))))

(defq untrace
 (lambda
  (x)
  (with-definition-of x
   (when
    (eq (car (caddr (eval x))) '%tracep%)
    (rplacd (cdr (eval x)) (cadddr (caddr (eval x))))))))

(defq unwind-protect
 (mlambda (x . y) (` progn (err-set , x) ,@ y)))

(defq when
 (mlambda
  (x . y)
  (list 'if x (cons 'progn y))))

(defq where-is-ref
 (lambda
  (what pack)
  (flet
   ((in-fun
     (fu what)
     (cond
      ((equal fu what))
      ((atom fu) nil)
      (t
       (or (in-fun (car fu) what) (in-fun (cdr fu) what))))))
   (mapcan
    (function
     (lambda (fu) (if (in-fun (definition-of fu) what) (list fu))))
    pack))))

(in 0)
