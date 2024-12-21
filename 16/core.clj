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

(defn end? [grid-map frontier]
  (= \E (get grid-map (:loc frontier))))

(defn dijkstra
  "I thought I would mostly copy my solution from day 18, which I managed to do first,
   but the solution there isn't really Dijkstra, it's probably a variant that happens
   to work with path there distances are uniform and there are no loops. This is a proper
   implementation, but still mostly based on my work in day 18 (and thanks to Tijmen to
   pointing me in the right direction)

   For part 2 the trick I found was to consider a location 'visited' only if a previous path
   had a poorer score, otherwise it's fair game again.
   This made it much slower after part part 2, but I spent so much time on this already that those
   extra 30s is just about good enough :')"
  [grid-map start]
  (loop [paths     [{:frontier start :cost 0 :route #{}}]
         visited   {}
         completed #{}]
    (let [{:keys [frontier cost route]} (first paths)]
      (if (or (empty? paths)
              (and (seq completed)
                   (> cost (:cost (first completed)))))
        completed
        (let [frontiers* (when-not (end? grid-map frontier)
                           (for [n (neighbours grid-map frontier)
                                 :let [cost* (cond-> (+ 1 cost)
                                               (not= (:dir frontier) (:dir n)) (+ 1000))]
                                  ;; Only consider path if previous path had a higher cost
                                 :when (<= cost* (get visited (:loc n) Integer/MAX_VALUE))]
                             {:frontier n
                              :cost     cost*
                              :route    (conj route (:loc n))}))]
          (recur (into [] (sort-by :cost (concat (rest paths) frontiers*)))
                 (->> (filter #(<= (count (neighbours grid-map (:frontier %))) 2) frontiers*) ;; Do not consider junctions
                      (reduce #(update %1 (get-in %2 [:frontier :loc]) (fnil min Integer/MAX_VALUE) (:cost %2)) visited))
                 (cond-> completed (end? grid-map frontier) (conj (first paths)))))))))

(defn part-1 []
  (let [{:keys [maze reindeer]} (grab-a-map (slurp "16/input.txt"))]
    (:cost (first (dijkstra maze reindeer)))))

(defn past-2 []
  (let [{:keys [maze reindeer]} (grab-a-map (slurp "16/input.txt"))]
    (inc (count (into #{} (mapcat :route) (dijkstra maze reindeer))))))
