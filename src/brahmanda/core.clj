(ns brahmanda.core
  (:gen-class)
  (:require
   [brahmanda.sphere :as sphere]
   [brahmanda.projection :as proj]
   [brahmanda.draw :as draw]))

;; ==========================================
;; Кількість точок
;; ==========================================

(def samples 720)

;; ==========================================
;; Генерація кривої
;; ==========================================

(defn sample-curve
  [curve-fn]

  (for [i (range samples)]

    (let [t (* (/ (* 2.0 Math/PI) samples)
               i)]

      (->

       (curve-fn t)

       proj/xyz->radec

       proj/south-polar))))

;; ==========================================
;; Дані
;; ==========================================

(def ecliptic

  (sample-curve sphere/ecliptic))

(def equator

  (sample-curve sphere/equator))

;; ==========================================
;; Головна функція
;; ==========================================

(defn -main
  [& _]

  (println "Generating SVG...")

  (draw/save-svg

   "out/ecliptic.svg"

   ;; координатні осі
   (draw/line
    0
    draw/cy
    draw/width
    draw/cy
    "#aaaaaa")

   (draw/line
    draw/cx
    0
    draw/cx
    draw/height
    "#aaaaaa")

   ;; контрольне коло
   (draw/circle
    (/ Math/PI 2.0)
    "#888888")

   ;; екватор
   (draw/polyline
    equator
    "#0066ff")

   ;; екліптика
   (draw/polyline
    ecliptic
    "#ff0000"))

  (println "Done."))
