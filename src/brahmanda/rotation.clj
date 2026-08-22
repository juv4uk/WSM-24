(ns brahmanda.rotation)

(defn rotate-x
  [{:keys [x y z]} angle]

  (let [c (Math/cos angle)
        s (Math/sin angle)]

    {:x x

     :y (- (* y c)
           (* z s))

     :z (+ (* y s)
           (* z c))}))
