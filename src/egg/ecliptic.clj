(ns egg.ecliptic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Трансцендентна "яєчна" крива
;; Побудована через asin(sin(t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def resolution
  8760)

(def radius
  1.0)

(defn egg-point
  [t]

  (let [u (Math/asin (Math/sin t))

        ;; радіус змінюється згідно asin(sin(t))
        r (+ radius u)

        x (* r (Math/cos t))

        y (* r (Math/sin t))]

    [x y]))

(defn generate

  ([]

   (generate resolution))

  ([n]

   (mapv

    (fn [i]

      (let [t (- (* 2.0
                    Math/PI
                    (/ i n))
                 Math/PI)]

        (egg-point t)))

    (range n))))
