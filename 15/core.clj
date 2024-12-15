(ns core
  (:require
   [clojure.string :as str]))

(def directions
  {\^ [0 -1]
   \> [1 0]
   \v [0 1]
   \< [-1 0]})

(defn warehouse-coords [warehouse-map]
  (into {}
        (for [[y line] (map-indexed vector (str/split-lines warehouse-map))
              [x char] (map-indexed vector line)]
          [[x y] char])))

(defn movement-coords [robot-movements]
  (for [line (str/split-lines robot-movements)
        char line]
    (directions char)))

(defn init [input]
  (let [[warehouse-map robot-movements] (str/split input #"\n\n")]
    {:warehouse-map (warehouse-coords warehouse-map)
     :robot-movements (movement-coords robot-movements)}))

(defn next-empty-space [warehouse-map coords direction]
  (let [next-step (map + coords direction)]
    (case (get warehouse-map next-step)
      \# nil
      \. next-step
      \O (recur warehouse-map next-step direction))))

(defn process-step [warehouse-map direction]
  ;; Pretty slow, but that's expected. I just wanted to see what perforamnce would be like (hint: it's not great)
  (let [robot     (first (keep (fn [[coords tile]] (when (= \@ tile) coords)) warehouse-map))
        next-step (map + robot direction)
        next-tile (get warehouse-map next-step)]
    (cond
      (= \. next-tile)
      (assoc warehouse-map robot \. next-step \@)

      (= \O next-tile)
      (if-let [empty-space (next-empty-space warehouse-map next-step direction)]
        (assoc warehouse-map robot \. next-step \@ empty-space \O)
        warehouse-map)

      :else warehouse-map)))

(defn process-steps [{:keys [warehouse-map robot-movements]}]
  (reduce process-step warehouse-map robot-movements))

(defn sum-coordinates [warehouse-map]
  (reduce-kv
   (fn [sum [x y] tile]
     (cond-> sum
       (= \O tile) (+ x (* 100 y))))
   0 warehouse-map))

(defn part-1 []
  (-> (slurp "15/input.txt") init process-steps sum-coordinates))

(defn print-map
  "I got coordinates the wrong way around again, had to print to prove it ¬_¬"
  [warehouse-map]
  (let [x (apply max (map first (keys warehouse-map)))
        y (apply max (map second (keys warehouse-map)))]
    (doseq [y (range (inc y))]
      (prn (apply str (map #(get warehouse-map [% y]) (range (inc x))))))))

(comment
  (-> (init (slurp "15/input.txt"))
      #_(update  :robot-movements #(take 2 %))
      process-steps
      print-map))
