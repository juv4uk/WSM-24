(ns egg.core
  (:require
   [clojure.java.io :as io]
   [clojure.data.csv :as csv]
   [egg.compare :as compare]
   [egg.contour :as contour]))

(defn contour-files [dir]

  (->> (file-seq (io/file dir))
       (filter #(.isFile %))
       (filter #(.endsWith (.getName %) ".bmp.csv"))))

(defn analyze-all-eggs [dir]

  (sort-by
   :score

   (for [file (contour-files dir)]

     (let [points (contour/load-contour (.getPath file))

           score
           (compare/compare-contour points)]

       {:file (.getName file)
        :score score}))))

(defn save-results [results filename]

  (with-open [w (io/writer filename)]

    (csv/write-csv
     w

     (cons
      ["File" "Score"]

      (map
       (fn [{:keys [file score]}]

         [file score])

       results)))))

(defn -main [& _]

  (println "Comparing contours...")

  (let [results

        (analyze-all-eggs "resources/eggs")

        out

        "resources/results.csv"]

    (save-results results out)

    (println "Done.")

    (println "Best match:")

    (println (first results))))
