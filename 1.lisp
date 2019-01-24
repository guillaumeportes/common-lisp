(defun make-even (x)
  (if (evenp x)
      x
      (+ 1 x)))

(defun further (x)
  (if (zerop x)
      x
      (if (< x 0)
	  (- x 1)
	  (+ x 1))))

(further 1)
(further -2)
(further 0)

(defun my-not (x)
  (if x nil t))

(my-not t)
(my-not nil)
(my-not 'x)
(my-not 3)

(defun ordered (x y)
  (if (< x y)
      (list x y)
      (list y x)))

(ordered 1 3)
(ordered 5 3)

(defun compare (x y)
  (cond ((< x y) '(x is smaller than y))
	((= x y) '(x and y are equal))
	(t '(y is smaller than x))))

(defun my-abs (x)
  (cond ((< x 0) (- 0 x))
	(t x)))

(defun emphasize3 (x)
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
	((equal (first x) 'bad) (cons 'awful (rest x)))
	(t (cons 'very x))))

(defun make-odd (x)
  (cond ((not (oddp x)) (+ x 1))
	(t x)))

(defun constrain (x min max)
  (cond ((< x min) min)
	((> x max) max)
	(t x)))

(defun constrain2 (x min max)
  (if (< x min)
      min
      (if (> x max)
	  max
	  x)))

(defun firstzero (l)
  (cond ((= 0 (first l)) 'first)
	((= 0 (second l)) 'second)
	((= 0 (third l)) 'third)
	(t 'none)))

(defun cycle (n)
  (cond ((< n 99) (+ 1 n))
	(t 1)))

(defun how-compute (a b c)
  (cond ((= c (+ a b)) 'sum-of)
	((= c (* a b)) 'product-of)
	(t 'beats-me)))

(defun geq (x y)
  (and (>= x y) t))

(defun f4.16 (x)
  (cond ((and (> x 0) (oddp x)) (* x x))
	((and (< x 0) (oddp x)) (* x 2))
	(t (/ x 2))))

(defun f4.17p (x y)
  (and (or (and (or (equal x 'boy) (equal x 'girl)) (equal y 'child))
	   (and (or (equal x 'man) (equal x 'woman)) (equal y 'adult)))
       t))

(defun rock-paper-scissors (a b)
  (cond ((equal a b) 'tie)
	((or (and (equal a 'scissors)
		  (equal b 'paper))
	     (and (equal a 'paper)
		  (equal b 'rock))
	     (and (equal a 'rock)
		  (equal a 'scissors)))
	 'first-wins)
	(t 'second-wins)))

(defun f4.19.1 (x y z w)
  (cond ((not x) nil)
	((not y) nil)
	((not z) nil)
	(t w)))

(defun f4.19.2 (x y z w)
  (if x
      (if y
	  (if z
	      w))))

(defun f4.20.1 (x y)
  (if (> x y)
      'first-is-bigger
      (if (< x y)
	  'first-is-smaller
	  'numbers-are-equal)))

(defun f4.20.2 (x y)
  (or
   (and
    (< x y)
    'first-is-smaller)
   (and
    (> x y)
    'first-is-bigger)
   'numbers-are-equal))

(defun f4.21.1 (x y)
  (if (> x y)
      t
      (if (zerop x)
	  t
	  (zerop y))))

(defun f4.21.2 (x y)
  (cond ((> x y) t)
	((zerop x) t)
	(t (zerop y))))

(defun boilingp (temp scale)
  (cond ((equal scale 'fahrenheit) (>= temp 212))
	((equal scale 'celsius) (>= temp 100))))

(defun my-abs (x)
  (if (< x 0) (- x) x))

(defun logical-and (x y)
  (if x
      (if y
	  t
	  nil)))

(defun logical-and2 (x y)
  (cond ((not x) nil)
	((not y) nil)
	(t t)))

(defun logical-or (x y)
  (and (or x y) t))

(defun nand (x y)
  (not (and x y)))

(defun f4.37.2 (x y)
  (nand (nand x x) (nand y y)))

(defun f4.37.1 (x y)
  (nand
   (nand (nand (nand x x) (nand x x)) (nand (nand y y) (nand y y)))
   (nand (nand (nand x x) (nand x x)) (nand (nand y y) (nand y y)))))

(defun nor (x y)
  (not (or x y)))

(defun f4.38.1 (x)
  (nor x x))

(defun f4.38.2 (x y)
  (nor (nor x x) (nor y y)))

(defun good-style (p)
  (let ((p-plus-5 (+ p 5)))
    (list 'result 'is p-plus-5)))

(defun throw-die ()
  "returns a number between 1 and 5"
  (+ 1 (random 6)))

(defun throw-dice ()
  "throws two dice"
  (list (throw-die) (throw-die)))

(defun snake-eyes-p (x)
  "is this a snake eyes throw"
  (equal '(1 1) x))

(defun boxcars-p (x)
  "is this a boxcar throw"
  (equal '(6 6) x))

(defun instant-win-p (x)
  "is this a 7 or a 11"
  (let ((s (+ (first x) (second x))))
    (member s '(7 11))))

(defun instant-loss-p (x)
  "is this a 2, a 3 or a 12"
  (let ((s (+ (first x) (second x))))
    (member s '(2 3 12))))

(defun say-throw (x)
  (cond ((snake-eyes-p x) 'snake-eyes)
	((boxcars-p x) 'boxcars)
	(t (+ (first x) (second x)))))

(defun craps ()
  (let ((tt (throw-dice)))
    (append
     (list
      'throw (first tt)
      'and (second tt)
      '-- (say-throw tt) '--
     (cond ((instant-win-p tt) '(you win))
	   ((instant-loss-p tt) '(you lose))
	   (t (append '(your point is) (list (say-throw tt))))))))
