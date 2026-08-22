(ns brahmanda.projection)

;; ==========================================
;; XYZ -> RA/DEC
;; ==========================================

(defn xyz->radec
  [{:keys [x y z]}]

  {:ra  (Math/atan2 y x)
   :dec (Math/asin z)})

;; ==========================================
;; Полярна рівнопроміжна
;; ==========================================

(defn south-polar
  [{:keys [ra dec]}]

  (let [r (+ (/ Math/PI 2.0) dec)]

    {:x (* r (Math/cos ra))
     :y (* r (Math/sin ra))}))

;; ==========================================
;; Ортографічна
;; ==========================================

(defn orthographic
  [{:keys [ra dec]}]

  {:x (* (Math/cos dec)
         (Math/cos ra))

   :y (* (Math/cos dec)
         (Math/sin ra))})

;; ==========================================
;; Стереографічна
;; ==========================================

(defn stereographic
  [{:keys [ra dec]}]

  (let [k (/ 1.0
             (- 1.0 (Math/sin dec)))]

    {:x (* k
           (Math/cos dec)
           (Math/cos ra))

     :y (* k
           (Math/cos dec)
           (Math/sin ra))}))
