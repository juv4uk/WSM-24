(ns brahmanda.sphere
  (:require [brahmanda.math :as m]))

(def ε
  (m/deg->rad 23.439281))

(defn ecliptic
  [t]

  {:x (Math/cos t)

   :y (* (Math/sin t)
         (Math/cos ε))

   :z (* (Math/sin t)
         (Math/sin ε))})
