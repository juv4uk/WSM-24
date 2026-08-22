(ns brahmanda.coordinates)

(defn xyz->radec

  [{:keys [x y z]}]

  (let [ra (Math/atan2 y x)

        dec (Math/asin z)]

    {:ra ra
     :dec dec}))
