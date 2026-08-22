(ns egg.compare
  "Shape comparison: real contours vs the brahmanda (ecliptic-on-south-
  polar-projection) curve. M0 rewrite fixing the five defects recorded
  in docs/provenance.md:
  - works on REAL contour points, not a parametric model;
  - single point format: [x y] vectors everywhere;
  - normalize computes bounds once (was O(n^2));
  - resampling to a common point count + symmetric chamfer distance
    (index-pair comparison was rotation-fragile);
  - no calc-r: the parametric egg model is gone.")

;; ---------------------------------------------------------------- normalize

(defn bounds
  [points]
  (reduce (fn [[minx miny maxx maxy] [x y]]
            [(min minx x) (min miny y) (max maxx x) (max maxy y)])
          [Double/POSITIVE_INFINITY Double/POSITIVE_INFINITY
           Double/NEGATIVE_INFINITY Double/NEGATIVE_INFINITY]
          points))

(defn normalize
  "Translate to centre, scale so the max axis span becomes 1.
   Bounds are computed once, not per point."
  [points]
  (let [[minx miny maxx maxy] (bounds points)
        cx     (/ (+ minx maxx) 2.0)
        cy     (/ (+ miny maxy) 2.0)
        spread (max (- maxx minx) (- maxy miny))
        scale  (if (pos? spread) (/ 1.0 spread) 1.0)]
    (mapv (fn [[x y]]
            [(* (- x cx) scale) (* (- y cy) scale)])
          points)))

;; ---------------------------------------------------------------- resample

(defn cumulative-lengths
  "cum[i] = polygon arc length up to vertex i; cum has (n+1) entries,
  cum[0] = 0.0, cum[n] = perimeter (closing edge included)."
  [points]
  (let [cnt (count points)]
    (reductions (fn [acc i]
                  (let [[x1 y1] (nth points (mod (dec i) cnt))
                        [x2 y2] (nth points (mod i cnt))]
                    (+ acc (Math/hypot (- x2 x1) (- y2 y1)))))
                0.0 (range 1 (inc cnt)))))

(defn resample
  "Resample a closed polygon to n points at equal arc length."
  ([points] (resample points 512))
  ([points n]
   {:pre [(> (count points) 1) (pos? n)]}
   (let [cnt (count points)
        cum (cumulative-lengths points)
        per (last cum)
        step (/ per n)]
    (when (pos? per)
      (mapv (fn [k]
              (let [target (* k step)
                    j (loop [j 1]
                        (if (>= (nth cum j) target)
                          j
                          (recur (inc j))))
                    t (/ (- target (nth cum (dec j)))
                         (- (nth cum j) (nth cum (dec j))))
                    [x1 y1] (nth points (mod (dec j) cnt))
                    [x2 y2] (nth points (mod j cnt))]
                [(+ x1 (* t (- x2 x1)))
                 (+ y1 (* t (- y2 y1)))]))
            (range n))))))

;; ---------------------------------------------------------------- distance

(defn nearest-dist
  "Distance from point p to the nearest point of pts (linear scan;
  M0 sizes are 512 x 512)."
  [p pts]
  (let [[px py] p]
    (apply min (map (fn [[x y]] (Math/hypot (- x px) (- y py))) pts))))

(defn chamfer
  "Symmetric chamfer distance: mean nearest-neighbour distance A->B plus
  B->A, halved. Rotation-tolerant compared to naive index pairing.
  Lower = closer shape."
  [a b]
  (let [mean-a→b (/ (reduce + (map #(nearest-dist % b) a)) (count a))
        mean-b→a (/ (reduce + (map #(nearest-dist % a) b)) (count b))]
    (* 0.5 (+ mean-a→b mean-b→a))))

;; ---------------------------------------------------------------- public

(defn compare-contour
  "Full pipeline: normalize both shapes, resample to n points, return
  the chamfer score (lower = better match)."
  ([contour-points other-points] (compare-contour contour-points other-points 512))
  ([contour-points other-points n]
   (let [a (-> contour-points normalize (resample n))
         b (-> other-points normalize (resample n))]
     (when (and a b)
       (chamfer a b)))))
