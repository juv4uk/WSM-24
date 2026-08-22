(ns brahmanda.vector)

(defn vec3
  [x y z]
  {:x x :y y :z z})

(defn length
  [{:keys [x y z]}]
  (Math/sqrt
   (+ (* x x)
      (* y y)
      (* z z))))

(defn normalize
  [v]
  (let [l (length v)]
    (vec3
      (/ (:x v) l)
      (/ (:y v) l)
      (/ (:z v) l))))
