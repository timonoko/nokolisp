
'(%Z%%M% %R%.%L%.%B%.%S% %E% %Y%)
'(MIKKO-3 (1 / 1 - 1980) (23 : 28 : 25 50))
(defq *package* MOUSE)

(defun WaitForButton ()
 (repeat (AX-reg 3) (INT- 51) (not (zerop (BX-reg)))))

(defun demo3 ()
 (display-mode 6)
 (SetMouseCursorPosition 300 100)
 (point 300 100)
 (ShowCursor)
 (repeat
  (WaitForButton)
  (let
   ((x (GetMousePosition&ButtonStatus)))
   (case
    (cadr x)
    (1 (HideCursor) (draw (caddr x) (cadddr x)) (ShowCursor) nil)
    (2 t)
    (t nil))))
 (HideCursor)
 (GetMousePosition&ButtonStatus)
 (display-mode 2))

(defq mask2
 (**************** **************** **************** ****************
  **************** **************** **************** ****************
  **************** **************** **************** ****************
  **************** **************** **************** ****************
  ****************))

(defun rivi-numeroks
 (x n)
 (setq n 0)
 (setq x (explode x))
 (for
  (z 0 15)
  (setq n (times 2 n))
  (if
   (nthcdr z x)
   (if (eqn (nth z x) 42) (setq n (add1 n)))))
 n)

(defun splat
 (seg off screen cursor)
 (pokew seg off (rivi-numeroks screen))
 (pokew seg (plus off 32) (rivi-numeroks cursor)))

(defun my-cursor
 (n screen cursor)
 (let
  ((seg (lisp-DS)) (off (plus 2000 (times 64 n))))
  (for
   (x 0 15)
   (splat seg
    (plus off (times 2 x))
    (if (nthcdr x screen) (nth x screen) '*****************)
    (if (nthcdr x cursor) (nth x cursor) '----------------)))
  off))

(defq mask1 (*--------------- ***------------- *****----------- *******--------- **-**----------- ----**---------- -----**--------- ------**--------))

(defun demo2 ()
 (display-mode 6)
 (if
  (zerop (car (MouseInitialization)))
  'No-Mouse!
  (progn
   (SetMouseCursorPosition 300 100)
   (point 300 100)
   (ShowCursor)
   (repeat
    (let
     ((x (GetMousePosition&ButtonStatus)))
     (case
      (cadr x)
      (1 (HideCursor) (draw (caddr x) (cadddr x)) (ShowCursor) nil)
      (2 t)
      (t nil))))
   (HideCursor)
   (GetMousePosition&ButtonStatus)))
 (display-mode 2))

(defun demo ()
 (if
  (zerop (car (MouseInitialization)))
  'No-Mouse!
  (progn
   (SetMouseCursorPosition 300 100)
   (ShowCursor)
   (repeat
    (let
     ((x (cadr (GetMousePosition&ButtonStatus))))
     (case x
      (1
       (HideCursor)
       (printc 7)
       (print 'Wrong-button)
       (cr)
       (ShowCursor)
       nil)
      (2 t)
      (t nil))))
   (HideCursor)
   (GetMousePosition&ButtonStatus))))

(defq MouseInitialization (call-mouse 0))

(defq ShowCursor (call-mouse 1))

(defq HideCursor (call-mouse 2))

(defq GetMousePosition&ButtonStatus (call-mouse 3))

(defun SetMouseCursorPosition (x y) (call-mouse 4 () x y))

(defun GetButtonPressInformation (x) (call-mouse 5 x))

(defun GetButtonReleaseInformation (x) (call-mouse 6 x))

(defun SetMinimum&MaximumXPosition (x y) (call-mouse 7 () x y))

(defun SetMinimum&MaximumYPosition (x y) (call-mouse 8 () x y))

(defun DefineGraphicsCursorBlock (x y pointer) (call-mouse 9 x y pointer))

(defq DefineTextCursor (call-mouse 10))

(defq ReadMouseMotionCounters (call-mouse 11))

(defq DefineEventHandler (call-mouse 12))

(defq LightPenEmulationModeOn (call-mouse 13))

(defq LightPenEmulationModeOff (call-mouse 14))

(defun SetMouseMotion/PixelRatio (hor ver) (call-mouse 15 () hor ver))

(defun ConditionOff
 (left top right bottom)
 (SI-reg right)
 (DI-reg bottom)
 (let
  ((resu (call-mouse 16 () left top)))
  (list (caddr resu) (cadddr resu) (SI-reg) (DI-reg))))

(defq SetDoubleSpeedThreshold (call-mouse 19))

(defmacro call-mouse
 (ax bx cx dx)
 (` progn
  (AX-reg , ax)
  ,
  (if bx (` BX-reg , bx))
  ,
  (if cx (` CX-reg , cx))
  ,
  (if dx (` DX-reg , dx))
  (ES-reg (lisp-DS))
  (INT- 51)
  (list (AX-reg) (BX-reg) (CX-reg) (DX-reg))))

(defq MOUSE
 (WaitForButton demo3 mask2 rivi-numeroks splat my-cursor mask1
  demo2 demo MouseInitialization ShowCursor HideCursor GetMousePosition&ButtonStatus
  SetMouseCursorPosition GetButtonPressInformation GetButtonReleaseInformation
  SetMinimum&MaximumXPosition SetMinimum&MaximumYPosition DefineGraphicsCursorBlock
  DefineTextCursor ReadMouseMotionCounters DefineEventHandler LightPenEmulationModeOn
  LightPenEmulationModeOff SetMouseMotion/PixelRatio ConditionOff
  SetDoubleSpeedThreshold call-mouse MOUSE))
(progn (in 0) ())
