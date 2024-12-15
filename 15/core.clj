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

(defn wide-warehouse-coords [warehouse-map]
  (into {}
        (for [[y line] (map-indexed vector (str/split-lines warehouse-map))
              [x char] (map-indexed vector line)
              double-x [0 1]]
          [[(+ double-x (* x 2)) y]
           (cond
             (= \O char) (if (= double-x 0) \[ \])
             (= \@ char) (if (= double-x 0) \@ \.)
             :else char)])))

(defn movement-coords [robot-movements]
  (for [line (str/split-lines robot-movements)
        char line]
    (directions char)))

(defn init
  ([input]
   (init input {}))
  ([input {:keys [double-wide?]}]
   (let [[warehouse-map robot-movements] (str/split input #"\n\n")]
     {:warehouse-map (if double-wide?
                       (wide-warehouse-coords warehouse-map)
                       (warehouse-coords warehouse-map))
      :robot-movements (movement-coords robot-movements)})))

(defn next-empty-space [warehouse-map coords direction]
  (let [next-step (map + coords direction)]
    (case (get warehouse-map next-step)
      \#         nil
      \.         next-step
      (\O \[ \]) (recur warehouse-map next-step direction))))

(defn next-empty-vertical-space-for-double-box [warehouse-map coords direction]
  (let [tile    (get warehouse-map coords)
        empties (cond
                  (= tile \.) nil
                  (= tile \#) [:wall]
                  :else (let [[left-coords right-coords] (if (= tile \[)
                                                           [coords (map + coords [1 0])]
                                                           [(map + coords [-1 0]) coords])
                              left-next-step             (map + left-coords direction)
                              right-next-step            (map + right-coords direction)
                              next-empty-spaces          (cond
                                                           (and (= \. (get warehouse-map left-next-step))
                                                                (= \. (get warehouse-map right-next-step)))
                                                           [left-coords right-coords]

                                                           (or (= \# (get warehouse-map left-next-step))
                                                               (= \# (get warehouse-map right-next-step)))
                                                           [:wall]

                                                           :else
                                                           (into [left-coords right-coords]
                                                                 (mapcat #(next-empty-vertical-space-for-double-box warehouse-map % direction))
                                                                 [left-next-step right-next-step]))]
                          next-empty-spaces))]
    empties))

(defn move-double-boxes-horizontally [warehouse-map [from-x y] [to-x]]
  (let [distance (Math/abs (- from-x to-x))
        direction (if (> from-x to-x) [-1 0] [1 0])
        boxes-coords (map #(vector (+ from-x (* (first direction) %)) y) (range distance))
        boxes (->> (select-keys warehouse-map boxes-coords)
                   (sort-by (if (> from-x to-x)
                              (comp ffirst)
                              (comp - ffirst))))]
    (reduce
     (fn [warehouse-map [coords edge]]
       (assoc warehouse-map coords \. (map + coords direction) edge))
     warehouse-map boxes)))

(defn move-double-boxes-vertically [warehouse-map boxes-coords direction]
  (let [boxes (->> (select-keys warehouse-map boxes-coords)
                   (sort-by (if (neg? (second direction))
                              (comp second first)
                              (comp - second first))))]
    (reduce
     (fn [warehouse-map [coords edge]]
       (assoc warehouse-map coords \. (map + coords direction) edge))
     warehouse-map boxes)))

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

      (and (contains? #{\[ \]} next-tile) (= 0 (first direction)))
      (let [boxes-to-move (next-empty-vertical-space-for-double-box warehouse-map next-step direction)]
        (if (every? seq? boxes-to-move)
          (-> (move-double-boxes-vertically warehouse-map boxes-to-move direction)
              (assoc robot \. next-step \@))
          warehouse-map))

      (and (contains? #{\[ \]} next-tile) (= 0 (second direction)))
      (if-let [empty-space (next-empty-space warehouse-map next-step direction)]
        (-> (move-double-boxes-horizontally warehouse-map next-step empty-space)
            (assoc robot \. next-step \@))
        warehouse-map)

      :else warehouse-map)))

(defn process-steps [{:keys [warehouse-map robot-movements]}]
  (reduce process-step warehouse-map robot-movements))

(defn sum-coordinates
  "Description was ambiguous:
   > distances are measured from the edge of the map to the closest edge of the box in question

   It made me think that distance was calculated from the left side of the map to the left side of the box,
   if it was in the left half, or from the right side of the map to the right side of the box. This is not
   the case though, it's always to the left side. That wasted some time. "
  [warehouse-map]
  (reduce-kv
   (fn [sum [x y] tile]
     (cond-> sum
       (contains? #{\O \[} tile) (+ x (* 100 y))))
   0 warehouse-map))

(defn part-1 []
  (-> (slurp "15/input.txt") init process-steps sum-coordinates))

(defn part-2 []
  (-> (slurp "15/input.txt") (init {:double-wide? true}) process-steps sum-coordinates))

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
