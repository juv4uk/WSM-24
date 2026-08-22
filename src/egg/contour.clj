(ns egg.contour
  (:require
   [clojure.string :as str]))

(defn parse-line [line]

  (let [[x y] (str/split line #",")]

    [(Double/parseDouble x)
     (Double/parseDouble y)]))

(defn load-contour [filename]

  (->> (slurp filename)
       str/split-lines
       (remove str/blank?)
       (mapv parse-line)))
