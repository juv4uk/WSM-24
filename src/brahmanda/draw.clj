(ns brahmanda.draw
  (:require
    [clojure.string :as str]))

;; ==========================================
;; Розмір полотна
;; ==========================================

(def width 900)
(def height 900)

(def cx (/ width 2.0))
(def cy (/ height 2.0))

(def scale 220.0)

;; ==========================================
;; Перетворення координат
;; ==========================================

(defn screen
  "Перетворює математичні координати у координати SVG."
  [{:keys [x y]}]

  [(+ cx (* scale x))
   (- cy (* scale y))])

;; ==========================================
;; SVG примітиви
;; ==========================================

(defn circle
  [r color]

  (str
    "<circle "
    "cx=\\"" cx "\\" "
    "cy=\\"" cy "\\" "
    "r=\\"" (* scale r) "\\" "
    "fill=\\"none\\" "
    "stroke=\\"" color "\\" "
    "stroke-width=\\"1\\"/>"))

(defn line
  [x1 y1 x2 y2 color]

  (str
    "<line "
    "x1=\\"" x1 "\\" "
    "y1=\\"" y1 "\\" "
    "x2=\\"" x2 "\\" "
    "y2=\\"" y2 "\\" "
    "stroke=\\"" color "\\" "
    "stroke-width=\\"1\\"/>"))

(defn polyline
  [points color]

  (let [pts
        (->> points
             (map screen)
             (map (fn [[x y]]
                    (str x "," y)))
             (str/join " "))]

    (str
      "<polyline "
      "fill=\\"none\\" "
      "stroke=\\"" color "\\" "
      "stroke-width=\\"2\\" "
      "points=\\"" pts "\\"/>")))

;; ==========================================
;; Документ
;; ==========================================

(defn svg
  [& body]

  (str
    "<?xml version=\\"1.0\\" encoding=\\"UTF-8\\"?>\\n"

    "<svg "
    "xmlns=\\"http://www.w3.org/2000/svg\\" "
    "width=\\"" width "\\" "
    "height=\\"" height "\\" "
    "viewBox=\\"0 0 " width " " height "\\">\\n"

    (apply str body)

    "\\n</svg>"))

(defn save-svg
  [filename & body]

  (spit filename
        (apply svg body)))
