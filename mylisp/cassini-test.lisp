;; brahmanda geometry library for my-lisp
;; Written against my-lisp 0.28.x core.my conventions:
;;   cond with (t ...) else · nth is INDEX-FIRST · no `if`
;; All shape math runs on [x y] pair lists; sqrt from core.my.

(def sq
  (lambda (x) (* x x)))

(def dist2
  (lambda (p q)
    (+ (sq (- (car p) (car q)))
       (sq (- (cadr p) (cadr q))))))

(def centroid-x
  (lambda (pts)
    (/ (reduce (lambda (a b) (+ a b)) 0.0 (map (lambda (p) (car p)) pts)) (length pts))))

(def centroid-y
  (lambda (pts)
    (/ (reduce (lambda (a b) (+ a b)) 0.0 (map (lambda (p) (cadr p)) pts)) (length pts))))

;; normalize: centre at origin, scale so max axis span = 1
(def normalize
  (lambda (pts)
    (let* ((xs (map (lambda (p) (car p)) pts))
           (ys (map (lambda (p) (cadr p)) pts))
           (minx (min xs))
           (maxx (max xs))
           (miny (min ys))
           (maxy (max ys))
           (cx (/ (+ minx maxx) 2.0))
           (cy (/ (+ miny maxy) 2.0))
           (spread (max (list (- maxx minx) (- maxy miny))))
           (s (/ 1.0 spread)))
      (map (lambda (p)
             (list (* (- (car p) cx) s)
                   (* (- (cadr p) cy) s)))
           pts))))

;; stride-subsample to at most n points (M0 stand-in for arc-length
;; resampling; documented limitation, see PLAN.md phase 2)
(def subsample
  (lambda (pts n)
    (let ((cnt (length pts)))
      (cond ((< cnt n) pts)
          (t (let ((step (quotient cnt n)))
              (map (lambda (i) (nth (* i step) pts))
                   (numbers-up-to n))))))))

(def numbers-up-to
  (lambda (n)
    (let ((build (lambda (self i acc)
                   (cond ((< i 0) acc)
                         (t (self self (- i 1) (cons i acc)))))))
      (build build (- n 1) (quote ())))))

(def nearest-dist2
  (lambda (p pts)
    ;; min of squared distances (sqrt applied once per list later)
    (reduce (lambda (best q)
              (let ((d (dist2 p q)))
                (cond ((< d best) d)
                      (t best))))
            1e18
            pts)))

(def mean-sqrt-list
  (lambda (lst)
    (/ (reduce (lambda (a b) (+ a b)) 0.0 (map sqrt lst))
       (length lst))))

;; symmetric chamfer distance (lower = closer shapes)
(def chamfer
  (lambda (a b)
    (* 0.5
       (+ (mean-sqrt-list (map (lambda (p) (nearest-dist2 p b)) a))
          (mean-sqrt-list (map (lambda (p) (nearest-dist2 p a)) b))))))

;; compare two point sets end-to-end
(def compare-shapes
  (lambda (a b cap)
    (chamfer (subsample (normalize a) cap)
             (subsample (normalize b) cap))))

;; quantize normalized coords to an integer grid (x1000): after this
;; every downstream operation is small-integer arithmetic -- the exact
;; rational engine stops paying its bignum tax
(def quantize
  (lambda (pts)
    (map (lambda (p)
           (list (quotient (* 1000 (car p)) 1)
                 (quotient (* 1000 (cadr p)) 1)))
         pts)))

;; squared-distance chamfer: no sqrt anywhere, integer domain
(def chamfer2
  (lambda (a b)
    (/ (+ (reduce (lambda (x y) (+ x y)) 0 (map (lambda (p) (nearest-dist2 p b)) a))
          (reduce (lambda (x y) (+ x y)) 0 (map (lambda (p) (nearest-dist2 p a)) b)))
       (+ (length a) (length b)))))

(def compare-shapes-int
  (lambda (a b cap)
    (chamfer2 (quantize (subsample (normalize a) cap))
              (quantize (subsample (normalize b) cap)))))

;; rational -> decimal string via long division.
;; core.my's number->string handles INTEGERS only: for 1/3 it calls
;; digit->string on 1/3, which does (nth 1/3 <digit-table>) -- the
;; fractional index descends cdr forever and dies on the empty list.
;; This helper prints any positive rational with `k` fraction digits.
(def rat-frac-digits
  (lambda (rem k)
    (cond ((= k 0) "")
          ((= rem 0) "")
          (t (let ((d (quotient (* 10 rem) 1)))
               (string-append
                 (number->string d)
                 (rat-frac-digits (- (* 10 rem) d) (- k 1))))))))

(def rat->string-d
  (lambda (x k)
    (let* ((neg (< x 0))
          (v (cond ((< x 0) (- 0 x)) (t x)))
          (ip (quotient v 1))
          (rem (- v ip)))
      (cond ((= rem 0) (number->string ip))
            (t (string-append
                 (cond (neg "-") (t ""))
                 (string-append (number->string ip)
                                (string-append "." (rat-frac-digits rem k)))))))))

(def rat->string
  (lambda (x) (rat->string-d x 6)))
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

(def cassini-b4-test (* 13000 13000))
(def profile (cassini-profile-upper (- 0 13000) 13000 20))
(print (length profile))
(print (nth 5 profile))
(print (fold-max-y profile 0))
(print "CASSINI-OK")
