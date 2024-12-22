(ns core
  (:require
   [clojure.string :as str]))

(defn grab-a-map [input]
  (let [start     (atom nil)
        end       (atom nil)
        racetrack (for [[y line] (map-indexed vector (str/split-lines input))
                        [x char] (map-indexed vector line)
                        :let     [_find-the-start (when (= \S char) (reset! start [x y]))
                                  _find-the-end   (when (= \E char) (reset! end [x y]))]]
                    [[x y] char])]
    {:racetrack (into {} racetrack)
     :start     @start
     :end       @end}))

(defn neighbours [grid-map robot]
  (for [direction [[1 0] [-1 0] [0 1] [0 -1]]
        :let  [neighbour (map + robot direction)]
        :when (contains? #{\. \E} (get grid-map neighbour))]
    neighbour))

(defn neighbours++ [grid-map robot]
  (for [direction [[1 0] [-1 0] [0 1] [0 -1]]
        :let  [neighbour (map + robot direction)]
        :when (= \# (get grid-map neighbour))
        :let  [neighbour++ (map + neighbour direction)]
        :when (contains? #{\. \E} (get grid-map neighbour++))]
    neighbour++))

(defn cheating-dijkstra
  "I misundertood the descrtiption originally, and spent a couple of days
   trying to find how to find shortcuts in an existing path. I wasn't getting
   quite the right answers, so looked on Reddit and found an excellent GIF that
   explained a really cool approach -
   https://www.reddit.com/r/adventofcode/comments/1hikcs7/2024_day_20_part_1_the_race_is_done_in_a

   Not sure what's going on with my scores, adding +1 here and there seems to fix it though :D"
  [grid-map start end cheats]
  (loop [paths     [{:frontier start :cost 1}]
         visited   #{}
         shortcuts []]
    (let [{:keys [frontier] :as p} (first paths)]
      (if (= frontier end)
        (cons p shortcuts)
        (let [frontiers* (for [n (neighbours grid-map frontier)
                               :when (not (contains? visited n))]
                           (-> (assoc p :frontier n)
                               (update :cost inc)))
              shortcuts* (when (> cheats 0)
                           (for [n++ (neighbours++ grid-map frontier)
                                 :when (not (contains? visited n++))]
                             (let [p* (first (cheating-dijkstra grid-map n++ end (dec cheats)))]
                               {:from   frontier
                                :to     n++
                                :offset (:cost p)
                                :cost   (+ 1 (:cost p) (:cost p*))})))]
          (recur (into [] (sort-by :cost (concat (rest paths) frontiers*)))
                 (conj visited frontier)
                 (into shortcuts shortcuts*)))))))

(defn part-1 []
  ;; It's slow, but at least it worked :')
  (let [{:keys [racetrack start end]} (grab-a-map (slurp "20/input.txt"))
        [original & cheats] (cheating-dijkstra racetrack start end 1)]
    (->> (map #(- (:cost original) (:cost %)) cheats)
         (reduce #(cond-> %1 (>= %2 100) inc) 0))))
