(ns core
  (:require
   [clojure.string :as str]))

(defn read-plot-map [input]
  (into {}
        (for [[y line] (map-indexed vector (str/split-lines input))
              [x char] (map-indexed vector line)]
          [[x y] {:crop char}])))

(def L [-1 0])
(def R [1 0])
(def U [0 -1])
(def D [0 1])

(def neighbour-positions [L R U D])

(defn crawl-region [grid-map [position {:keys [crop]}]]
  (let [region-id (random-uuid)]
    (loop [p position region {}]
      (let [neighbours (->> (select-keys grid-map (map #(map + p %) neighbour-positions))
                            (reduce-kv #(cond-> %1 (= (:crop %3) crop) (assoc %2 %3)) {}))
            region (-> (reduce-kv #(cond-> %1 (not (contains? %1 %2)) (assoc %2 %3)) region neighbours)
                       (assoc-in [p :crop] crop)
                       (assoc-in [p :region] region-id)
                       (assoc-in [p :perimeter] (- 4 (count neighbours))))]
        (if-let [p* (ffirst (remove (fn [[_ v]] (:region v)) region))]
          (recur p* region)
          {region-id region})))))

(defn new-region? [regions position]
  (not-any? (fn [[_ v]] (contains? v position)) regions))

(defn part-1 []
  (let [grid-map (read-plot-map (slurp "12/input.txt"))]
    (->> (reduce #(merge %1 (when (new-region? %1 (first %2)) (crawl-region grid-map %2))) {} grid-map)
         (reduce-kv (fn [price _ region] (+ price (* (count region) (apply + (map :perimeter (vals region)))))) 0))))

(defn fence-neighbours
  "Given a position, return all neighbours in the 'move-direction' who share a fence in the 'check-direction',
   for examle: if left of position is another region (so needs a fence), check if neightbour above also needs a fence,
   and so on"
  [region position check-direction move-direction]
  (let [{:keys [crop]} (get region position)
        square-to-check (get region (map + position check-direction))]
    (when (or (nil? square-to-check) (not= (:crop square-to-check) crop))
      (lazy-seq (cons position (let [next-position (map + position move-direction)]
                                 (when (= (get-in region [next-position :crop]) crop)
                                   (fence-neighbours region next-position check-direction move-direction))))))))

(defn combined-perimeter
  "For each position and for each direction, find all neighbours who share a fence. Then update the length of fence required
   for this position, but set fence to all examined neighbours to empty, so each length of fence counts as one. The count all
   positions which have a fence. Since we move in both direction, the starting square is counted twice each time, so make sure
   to not consider it when collating the whoe fence. Also, make sure that we don't reset the fence to empty for current position"
  [region]
  (->> (reduce-kv
        (fn [region position {:keys [fence]}]
          (let [p (cond-> {}
                    (nil? (:fence-l fence)) (assoc :fence-l (into (set (fence-neighbours region position L U)) (fence-neighbours region position L D)))
                    (nil? (:fence-r fence)) (assoc :fence-r (into (set (fence-neighbours region position R U)) (fence-neighbours region position R D)))
                    (nil? (:fence-d fence)) (assoc :fence-d (into (set (fence-neighbours region position D L)) (fence-neighbours region position D R)))
                    (nil? (:fence-u fence)) (assoc :fence-u (into (set (fence-neighbours region position U L)) (fence-neighbours region position U R))))]
            (as-> (update region position update :p merge p) $
              (reduce #(update %1 %2 assoc-in [:p :fence-l] #{}) $ (disj (:fence-l p) position))
              (reduce #(update %1 %2 assoc-in [:p :fence-r] #{}) $ (disj (:fence-r p) position))
              (reduce #(update %1 %2 assoc-in [:p :fence-u] #{}) $ (disj (:fence-u p) position))
              (reduce #(update %1 %2 assoc-in [:p :fence-d] #{}) $ (disj (:fence-d p) position)))))
        region region)
       (mapcat (comp (juxt :fence-l :fence-r :fence-u :fence-d) :p val))
       (filter seq)
       (count)))

(defn part-2 []
  (let [grid-map (read-plot-map (slurp "12/input.txt"))]
    (->> (reduce #(merge %1 (when (new-region? %1 (first %2)) (crawl-region grid-map %2))) {} grid-map)
         (reduce-kv (fn [price _ region] (+ price (* (count region) (combined-perimeter region)))) 0))))
