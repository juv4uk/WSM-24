(ns brahmanda.ecliptic
  (:require
   [brahmanda.math :as m]
   [brahmanda.vector :as v]))

;; Нахил екліптики

(def epsilon
  (m/deg->rad 23.439281))

(def cos-e
  (Math/cos epsilon))

(def sin-e
  (Math/sin epsilon))

(defn point
  "Точка екліптики на одиничній сфері"

  [t]

  (v/vec3

   (Math/cos t)

   (* (Math/sin t) cos-e)

   (* (Math/sin t) sin-e)))
