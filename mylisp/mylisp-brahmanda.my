;; brahmanda curve generator — pure my-lisp
;; The curve: r(t) = 1 + asin(sin t); point = (r cos t, r sin t).
;; asin(sin t) is the triangle wave folding t into [-pi/2, pi/2],
;; so no inverse trig is needed — exact rational pi and comparisons.
;; sin/cos come from a 10-term Taylor series in floats (documented
;; choice: trig is transcendental; exactness here is not load-bearing
;; for M0 shape comparison).

(def pi-approx
  (/ 355 113))

(def two-pi
  (* 2.0 pi-approx))

;; fold any angle into [-pi, pi]
(def fold-angle
  (lambda (t)
    (let ((shifted (- t (* two-pi (floor-quotient (+ t pi-approx) two-pi)))))
      shifted)))

(def floor-quotient
  (lambda (a b)
    (cond ((< a 0) (- 0 (quotient (- 0 a) b)))
          (t (quotient a b)))))

;; Taylor helpers: sin = x * P(x^2), cos = Q(x^2)
(def p-term
  (lambda (u k n)
    (cond ((> k 5) 1.0)
          (t (- 1.0 (/ (* u (p-term u (+ k 1) n))
                       (* (* 2 k) (+ (* 2 k) 1))))))))

(def q-term
  (lambda (u k n)
    (cond ((> k 5) 1.0)
          (t (- 1.0 (/ (* u (q-term u (+ k 1) n))
                       (* (+ (* 2 k) -1) (* 2 k))))))))

(def my-sin
  (lambda (x)
    (let ((f (fold-angle x)))
      (* f (p-term (* f f) 1 10)))))

(def my-cos
  (lambda (x)
    (let ((f (fold-angle x)))
      (q-term (* f f) 1 10))))

;; triangle wave: asin(sin t) for t in [-pi, pi]
(def triangle-wave
  (lambda (t)
    (cond ((> t (/ pi-approx 2.0)) (- pi-approx t))
          ((< t (- 0.0 (/ pi-approx 2.0))) (- (- 0.0 pi-approx) t))
          (t t))))

(def brahmanda-point
  (lambda (t)
    (let ((r (+ 1.0 (triangle-wave (fold-angle t)))))
      (list (* r (my-cos t))
            (* r (my-sin t))))))

(def generate-brahmanda
  (lambda (n)
    (map (lambda (i)
           (brahmanda-point (- (* 2.0 pi-approx (/ i n)) pi-approx)))
         (numbers-up-to n))))
