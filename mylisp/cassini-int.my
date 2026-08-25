;; Cassini oval generator — pure my-lisp, no trigonometry
;;
;; Implicit equation: (x^2 + y^2)^2 = a^2(x^2 - y^2) + b^4
;; Solved for y at each x:
;;   y^4 + (2x^2 + a^2)y^2 - (b^4 + a^2*x^2 - x^4) = 0
;;   Substituting z=y^2:  z^2 + A*z - B = 0
;;   where A = 2x^2 + a^2, B = b^4 + a^2*x^2 - x^4
;;   z = (-A + sqrt(A^2 + 4B)) / 2
;;   y = sqrt(z)
;;
;; Advantages over polar form: no trig needed, pure arithmetic.
;; All operations use exact rational arithmetic via core.my sqrt.
;;
;; Parameters: a=1 controls lobe separation; b=1.3 ensures single loop
;; (b > a means one connected region instead of two).

(def cassini-a 10000)     ; fixed-point scale 1e4 => a=1.0
(def cassini-b4 (* 13000 13000))  ; b=1.3 => b^4 on integer scale

;; Compute positive y for Cassini oval at given x (integer grid)
;; Returns 0 if no solution exists (outside the oval)
(def cassini-y-at-x
  (lambda (x)
    (let ((x2 (* x x))
          (a2 cassini-a))
      (let* ((A (+ (* 2 x2) a2))
             (B (+ cassini-b4 (* a2 x2) (- 0 (* x2 x2))))
             (disc (+ (* A A) (* 4 B))))
        (cond ((< disc 0) 0)
              (t (let ((z (quotient (+ (isqrt disc) (- 0 A)) 2)))
                   (cond ((< z 0) 0)
                         (t (isqrt z))))))))))

;; Generate full upper profile as (x y) pairs
(def cassini-profile-upper
  (lambda (x-min x-max n-steps)
    (let ((dx (quotient (- x-max x-min) n-steps)))
      (map (lambda (i)
             (let ((x (+ x-min (* i dx))))
               (list x (cassini-y-at-x x))))
           (numbers-up-to (+ n-steps 1))))))

;; Full profile: upper + mirrored lower (for closed contour)
(def cassini-full-profile
  (lambda (x-min x-max n-steps)
    (let ((upper (cassini-profile-upper x-min x-max n-steps))
          (lower (reverse
                   (map (lambda (p)
                          (list (car p) (- 0 (cadr p))))
                        (cassini-profile-upper x-min x-max n-steps)))))
      (append upper lower))))

;; Normalize to unit max-radius for comparison pipeline
;; (uses normalize from lib.my conceptually -- here standalone
;;  because we want pure integers)
(def cassini-normalized
  (lambda (n-steps half-width)
    (let* ((raw (cassini-profile-upper (- 0 half-width) half-width n-steps))
           ;; find max |y| for scaling
           (max-y (fold-max-y raw 0)))
      (map (lambda (p)
             (list (car p)
                   (quotient (* 10000 (cadr p)) max-y)))
           raw))))

(def fold-max-y
  (lambda (pts acc)
    (cond ((atom pts) acc)
          (t (let ((y (abs-val (cadr (car pts)))))
               (fold-max-y (cdr pts) (cond ((> y acc) y) (t acc))))))))

(def abs-val (lambda (x) (cond ((< x 0) (- 0 x)) (t x))))

;; ── Full pipeline: Cassini vs real egg contour ──
;; Compare Cassini profile against egg contour using chamfer distance

(def cassini-vs-egg
  (lambda (egg-points)
    (let* ((cassini (cassini-profile-upper (- 0 13000) 13000 32))
           ;; normalize both to same scale
           (cn (normalize-int cassini))
           (en (normalize-int egg-points))
           ;; subsample to same count
           (cs (stride-sub cn 64))
           (es (stride-sub en 64))
           ;; chamfer on integer grid
           (d1 (mean-min-dist cs es))
           (d2 (mean-min-dist es cs)))
      (quotient (+ d1 d2) 2))))

(def normalize-int
  (lambda (pts)
    ;; centre + scale so max span = 10000
    (let* ((xs (map (lambda (p) (car p)) pts))
           (ys (map (lambda (p) (cadr p)) pts))
           (minx (fold-min xs 999999999))
           (maxx (fold-max xs 0))
           (miny (fold-min ys 999999999))
           (maxy (fold-max ys 0))
           (cx (quotient (+ minx maxx) 2))
           (cy (quotient (+ miny maxy) 2))
           (spread (max (list (- maxx minx) (- maxy miny)))))
      (map (lambda (p)
             (list (quotient (* 10000 (- (car p) cx)) spread)
                   (quotient (* 10000 (- (cadr p) cy)) spread)))
           pts))))

(def fold-min
  (lambda (lst acc)
    (cond ((atom lst) acc)
          (t (let ((v (car lst)))
               (fold-min (cdr lst) (cond ((< v acc) v) (t acc))))))))

(def fold-max
  (lambda (lst acc)
    (cond ((atom lst) acc)
          (t (let ((v (car lst)))
               (fold-max (cdr lst) (cond ((> v acc) v) (t acc))))))))

(def stride-sub
  (lambda (pts n)
    (cond ((<= (length pts) n) pts)
          (t (let ((step (quotient (length pts) n)))
               (stride-walk pts step n 0 (quote ())))))))

(def stride-walk
  (lambda (pts step n i acc)
    (cond ((or (>= i n) (atom pts)) (reverse acc))
          (t (stride-walk (cdr pts) step n (+ i 1)
                          (cons (car pts) acc))))))

(def mean-min-dist
  (lambda (a b)
    (quotient
      (reduce (lambda (acc p) (+ acc (min-dist p b))) 0 a)
      (length a))))

(def min-dist
  (lambda (p pts)
    (min-dist-iter p pts 999999999)))

(def min-dist-iter
  (lambda (p pts best)
    (cond ((atom pts) best)
          (t (let ((d (dist2 p (car pts))))
               (min-dist-iter (cdr pts) (cond ((< d best) d) (t best))))))))

(def dist2
  (lambda (p q)
    (let ((dx (- (car p) (car q)))
          (dy (- (cadr p) (cadr q))))
      (+ (* dx dx) (* dy dy)))))
