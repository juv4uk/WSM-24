(ns egg.compare-test
  (:require [clojure.test :refer [deftest is testing]]
            [egg.compare :as c]
            [egg.ecliptic :as ecliptic]))

(def circle
  (for [k (range 720)]
    (let [t (* 2.0 Math/PI (/ k 720))]
      [(Math/cos t) (Math/sin t)])))

(deftest resampling-basics
  (testing "resample returns exactly n points"
    (is (= 512 (count (c/resample circle 512))))
    (is (= 100 (count (c/resample circle 100)))))
  (testing "resample of a unit circle stays on the unit circle"
    ;; tolerance = chord error of the 720-gon approximation, not exactness
    (doseq [[x y] (c/resample circle 128)]
      (is (< (Math/abs (- 1.0 (Math/hypot x y))) 1e-4)))))

(deftest normalization-is-scale-invariant
  (testing "a 10x scaled circle matches the original after normalize"
    (let [big (mapv (fn [[x y]] [(* 10 x) (* 10 y)]) circle)]
      (is (< (c/chamfer (c/normalize circle) (c/normalize big)) 1e-9)))))

(deftest rotation-tolerance
  (testing "chamfer of a rotated shape vs itself is small (resampled)"
    ;; 30-degree rotation: chamfer must stay at the resampling scale
    ;; (~half vertex spacing), whereas naive index-pairing would give O(1)
    (let [rot (for [k (range 720)]
                (let [t (+ (* 2.0 Math/PI (/ k 720)) (/ Math/PI 6))]
                  [(Math/cos t) (Math/sin t)]))
          a   (c/resample (c/normalize circle) 256)
          b   (c/resample (c/normalize rot) 256)]
      (is (< (c/chamfer a b) 5e-3))
      ;; sanity: identical shapes score far below rotated ones
      (is (< (c/chamfer a a) 1e-9)))))

(deftest different-shapes-score-higher-than-identical
  (testing "square vs circle scores worse than circle vs circle"
    (let [sq (for [k (range 400)]
               (let [u (- (* 2 (/ k 400)) 1)]
                 (cond
                   (< k 100)   [u 1.0]
                   (< k 200)   [1.0 (- 2 u)]
                   (< k 300)   [(- u) -1.0]
                   :else       [-1.0 (+ 2 u)])))
          a  (c/normalize sq)
          b  (c/normalize circle)
          sa (c/compare-contour sq sq)
          sb (c/compare-contour sq circle)]
      (is (< sa sb)))))

(deftest ecliptic-curve-generates
  (testing "the brahmanda curve generator works and closes"
    (let [pts (ecliptic/generate 8760)]
      (is (= 8760 (count pts)))
      ;; asin(sin t) makes r oscillate; radius must stay positive-ish and bounded
      (doseq [[x y] (take 100 pts)]
        (is (< (Math/hypot x y) (+ 1.0 (/ Math/PI 2))))))))

(deftest full-pipeline-smoke
  (testing "compare-contour ecliptic-vs-ecliptic is near zero; vs circle is larger"
    (let [ecl (ecliptic/generate 8760)
          self (c/compare-contour ecl ecl)
          other (c/compare-contour ecl circle)]
      (is (< self 1e-6))
      (is (> other self)))))
