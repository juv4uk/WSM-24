(ns brahmanda.sample
  (:require

   [brahmanda.ecliptic :as e]
   [brahmanda.coordinates :as c]
   [brahmanda.projection :as p]))

(defn sample

  [n]

  (for [i (range n)]

    (let [t (* (/ (* 2 Math/PI) n) i)]

      (->

       (e/point t)

       c/xyz->radec

       p/south-polar-equidistant))))
