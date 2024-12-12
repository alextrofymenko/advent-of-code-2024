(ns core
  (:require
   [clojure.string :as str]))

(defn read-plot-map [input]
  (into {}
        (for [[y line] (map-indexed vector (str/split-lines input))
              [x char] (map-indexed vector line)]
          [[x y] {:crop char}])))

(def neighbour-positions [[0 1] [1 0] [0 -1] [-1 0]])

(defn crawl-region [grid-map [position {:keys [crop]}]]
  (let [region-id (random-uuid)]
    (loop [p position region {}]
      (let [neighbours (->> (select-keys grid-map (map #(map + p %) neighbour-positions))
                            (reduce-kv #(cond-> %1 (= (:crop %3) crop) (assoc %2 %3)) {}))
            region (-> (reduce-kv #(cond-> %1 (not (contains? %1 %2)) (assoc %2 %3)) region neighbours)
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
