(ns core
  (:require
   [clojure.string :as str]))

(defn map-dimensions [input]
  (let [rows (str/split-lines input)]
    {:max-y (count rows)
     :max-x (count (first rows))}))

(defn find-antennas [input]
  (->> (for [[x row] (map-indexed vector (str/split-lines input))
             [y col] (map-indexed vector row)
             :when (not= \. col)]
         [col [x y]])
       (reduce #(update %1 (first %2) conj (last %2)) {})))

(defn unique-pairs [coords]
  (set
   (for [c1 coords c2 coords
         :when (not= c1 c2)]
     (sort-by (juxt first second) [c1 c2]))))

(defn find-antinodes [{:keys [max-x max-y]} antennas]
  (for [[_antenna coords] antennas
        [c1 c2] (unique-pairs coords)
        :let [dist (map - c1 c2)]
        antinode [(map + c1 dist) (map - c2 dist)]
        :when (and (< -1 (first antinode) max-x)
                   (< -1 (last antinode) max-y))]
    antinode))

(defn part-1 []
  (let [input (slurp "08/input.txt")]
    (->> (find-antennas input)
         (find-antinodes (map-dimensions input))
         (set)
         (count))))
