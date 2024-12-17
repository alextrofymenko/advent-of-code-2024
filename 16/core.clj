(ns core
  (:require
   [clojure.string :as str]))

(def directions
  {:east  [1 0]
   :west  [-1 0]
   :north [0 -1]
   :south [0 1]})

(defn grab-a-map [input]
  (let [reindeer (atom nil)
        maze (for [[y line] (map-indexed vector (str/split-lines input))
                   [x char] (map-indexed vector line)
                   :let [_find-the-reindeer
                         (when (= \S char)
                           (reset! reindeer {:loc [x y] :dir :east}))]]
               [[x y] char])]
    {:maze     (into {} maze)
     :reindeer @reindeer}))

(defn neighbours [grid-map reindeer]
  (for [direction directions
        :let  [neighbour {:loc (map + (:loc reindeer) (val direction)) :dir (key direction)}]
        :when (contains? #{\. \E} (get grid-map (:loc neighbour)))]
    neighbour))

(defn dijkstra
  "I thought I would mostly copy my solution from day 18, which I managed to do first,
   but the solution there isn't really Dijkstra, it's probably a variant that happens
   to work with path there distances are uniform and there are no loops. This is a proper
   implementation, but still mostly based on my work in day 18 (and thanks to Tijmen to
   pointing me in the right direction)"
  [grid-map start]
  (loop [paths   [{:frontier start :cost 0}]
         visited #{}]
    (let [{:keys [frontier cost]} (first paths)]
      (if (= \E (get grid-map (:loc frontier)))
        cost
        (let [new-paths (for [n (neighbours grid-map frontier)
                              :when (not (contains? visited (:loc n)))]
                          {:frontier n
                           :cost     (cond-> (+ 1 cost)
                                       (not= (:dir frontier) (:dir n)) (+ 1000))})]
          (recur (into [] (sort-by :cost (concat (rest paths) new-paths)))
                 (conj visited (:loc frontier))))))))

(defn part-1 []
  (let [{:keys [maze reindeer]} (grab-a-map (slurp "16/input.txt"))]
    (dijkstra maze reindeer)))
