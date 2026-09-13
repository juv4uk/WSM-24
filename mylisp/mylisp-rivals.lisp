;; Rival curves for the brahmanda trial: Cassini oval & Lissajous 3:2.
;; Integer fixed-point arithmetic (scale 1e4) + exact isqrt -- no
;; bignum explosion, no NIL hazards. Coordinates are integers on a
;; 1e4 grid; normalize/quantize treat them like any other points.

(def fixed
  (lambda (v)
    (quotient (* 10000 v) 1)))

(def pi-fixed
  (quotient (* 10000 (/ 355 113)) 1))

(def fold-fixed
  (lambda (t)
    (- t (* 62832 (floor-quotient (+ t 15708) 62832)))))

(def sin-fixed
  (lambda (x)
    ;; x is an integer on the 1e4 grid; Taylor in fixed-point,
    ;; depth 4 (error < 1e-6 rad for |x| <= pi)
    (let ((xx (quotient (* x x) 10000)))
      (* x (p-fixed xx 1 4)))))

(def p-fixed
  (lambda (u k n)
    (cond ((> k n) 10000)
          (t (let ((inner (p-fixed u (+ k 1) n))
                   (den (* (* 2 k) (+ (* 2 k) 1))))
               (- 10000 (quotient (* u inner) den)))))))

(def cos-fixed
  (lambda (x)
    (let ((xx (quotient (* x x) 10000)))
      (q-fixed xx 1 4))))

(def q-fixed
  (lambda (u k n)
    (cond ((> k n) 10000)
          (t (let ((inner (q-fixed u (+ k 1) n))
                   (den (* (+ (* 2 k) -1) (* 2 k))))
               (- 10000 (quotient (* u inner) den)))))))

;; ── Cassini oval (a=1, b=1.3): single loop, slightly squarish oval ──
;; r^2 = a^2 cos2t + sqrt(b^4 - a^4 sin^2 2t); all in fixed-point.
(def cassini-point
  (lambda (t)
    (let* ((c2 (cos-fixed (* 20000 t)))
           (s2 (sin-fixed (* 20000 t)))
           ;; b^4 = 28561e4 ; a^4 = 10000e4 (fixed^4 => scale 1e16)
           (raw (- 2856100000000 (* 100000000 (* s2 s2))))
           (inner (cond ((< raw 1856100000000) 1856100000000) (t raw)))
           ;; sqrt(inner): inner on 1e8 scale -> result on 1e4 scale
           (sq (isqrt inner))
           (r2 (+ (* 10000 c2) (* 10000 sq)))
           (r (isqrt r2)))
      (list (* r (cos-fixed t))
            (* r (sin-fixed t))))))

(def generate-cassini
  (lambda (n)
    (map (lambda (i)
           (cassini-point (- (* 62832 (/ i n)) 15708)))
         (numbers-up-to n))))

;; ── Lissajous 3:2 (the kinship-table champion) ──
(def lissajous-point
  (lambda (t)
    (list (* 8000 (sin-fixed (* 30000 t)))
          (* 7000 (sin-fixed (* 20000 t))))))

(def generate-lissajous
  (lambda (n)
    (map (lambda (i)
           (lissajous-point (- (* 62832 (/ i n)) 15708)))
         (numbers-up-to n))))
