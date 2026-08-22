(ns egg.draw

  (:import
   [javax.swing JFrame JPanel]
   [java.awt Graphics Graphics2D Color BasicStroke RenderingHints]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Налаштування
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def margin 40)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Межі контуру
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn bounds [points]

  (let [xs (map first points)
        ys (map second points)]

    {:xmin (apply min xs)
     :xmax (apply max xs)
     :ymin (apply min ys)
     :ymax (apply max ys)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Масштабування
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn transform

  [points width height]

  (let [{:keys [xmin xmax ymin ymax]}
        (bounds points)

        dx (- xmax xmin)
        dy (- ymax ymin)

        scale
        (min

          (/ (- width (* 2 margin)) dx)

          (/ (- height (* 2 margin)) dy))

        cx (/ (+ xmin xmax) 2.0)
        cy (/ (+ ymin ymax) 2.0)

        ox (/ width 2.0)
        oy (/ height 2.0)]

    (mapv

      (fn [[x y]]

        [(int (+ ox
                 (* scale
                    (- x cx))))

         (int (- oy
                 (* scale
                    (- y cy))))])

      points)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Малювання
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn show-contour

  [points]

  (let [width 900
        height 900

        screen
        (transform points width height)

        panel

        (proxy [JPanel] []

          (paintComponent [g]

            (proxy-super paintComponent g)

            (.setRenderingHint
              ^Graphics2D g
              RenderingHints/KEY_ANTIALIASING
              RenderingHints/VALUE_ANTIALIAS_ON)

            (.setStroke
              ^Graphics2D g
              (BasicStroke. 2))

            (.setColor
              ^Graphics2D g
              Color/BLACK)

            (doseq [[[x1 y1] [x2 y2]]

                    (partition 2 1 screen)]

              (.drawLine

                ^Graphics g

                x1 y1

                x2 y2))

            ;; замкнути контур

            (let [[x1 y1] (first screen)
                  [x2 y2] (last screen)]

              (.drawLine

                ^Graphics g

                x1 y1

                x2 y2))))

        frame (JFrame. "Egg Research")]

    (.setSize frame width height)

    (.setContentPane frame panel)

    (.setDefaultCloseOperation
      frame
      JFrame/EXIT_ON_CLOSE)

    (.setVisible frame true)))
