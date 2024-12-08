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

(defn antinode [f coord dist] (lazy-seq (as-> (map f coord dist) $ (cons $ (antinode f $ dist)))))
(def pos-antinode (partial antinode +))
(def neg-antinode (partial antinode -))

(defn find-antinodes
  ([dimensions antennas]
   (find-antinodes dimensions {} antennas))
  ([{:keys [max-x max-y]} {:keys [with-harmonics?]} antennas]
   (let [within-bounds? #(and (< -1 (first %) max-x)
                              (< -1 (last %) max-y))]
     (for [[_antenna coords] antennas
           [c1 c2] (unique-pairs coords)
           :let [dist (map - c1 c2)]
           ;; Not neg/pos fns are swapped becase part 2 description states -
           ;; "an antinode occurs at any grid position", meaning we no longer
           ;; need to respect the "one of the antennas is twice as far away" condition
           ;; i.e. instead of antinodes going "outwards"
           ;;      <-   ->
           ;; ....#..A..A..#....
           ;; such that antennas do not get ovewritten by themselves, instead they can go "inwards"
           ;;        -><-
           ;; ....#..A..A..#....
           ;; effectively "replacing" each other on the resulting map. I got here by accident,
           ;; which was very lucky, I should pay more attention to the descriptions in the future
           antinode (if with-harmonics?
                      (concat
                       (take-while within-bounds? (neg-antinode c1 dist))
                       (take-while within-bounds? (pos-antinode c2 dist)))
                      (concat
                       (take 1 (take-while within-bounds? (pos-antinode c1 dist)))
                       (take 1 (take-while within-bounds? (neg-antinode c2 dist)))))]
       antinode))))

(defn part-1 []
  (let [input (slurp "08/input.txt")]
    (->> (find-antennas input)
         (find-antinodes (map-dimensions input))
         (set)
         (count))))

(defn part-2 []
  (let [input (slurp "08/input.txt")]
    (->> (find-antennas input)
         (find-antinodes (map-dimensions input) {:with-harmonics? true})
         (set)
         (count))))
