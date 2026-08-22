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

;; Restored 2026-08-22: brahmanda.core references sphere/equator but the
;; last chat iteration of sphere.clj only kept ecliptic. Celestial equator
;; = great circle with declination 0 (y-z components of ecliptic without
;; the axial tilt rotation).
(defn equator
  [t]

  {:x (Math/cos t)

   :y (Math/sin t)

   :z 0.0})
