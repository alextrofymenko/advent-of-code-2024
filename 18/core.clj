(ns core)

(defn scan-memory [input]
  (partition 2 (map parse-long (re-seq #"\d+" input))))

(defn predict-corrupted-locations [bytes total corrupted]
  (let [corrupted-bytes (into #{} (take corrupted) bytes)]
    (into {}
          (for [y (range total)
                x (range total)]
            [[x y] (if (contains? corrupted-bytes [x y]) \# \.)]))))

(defn neighbours [grid-map position]
  (for [dir   [[1 0] [-1 0] [0 1] [0 -1]]
        :let  [neighbour (map + dir position)]
        :when (= \. (get grid-map neighbour))]
    neighbour))

(defn dijkstra [grid-map start end]
  (loop [unvisited (dissoc grid-map start)
         paths     #{{:frontier start :cost 0}}]
    (let [new-paths (set
                     (for [p paths
                           n (neighbours unvisited (:frontier p))]
                       (-> (assoc p :frontier n) (update :cost inc))))
          completed (filter #(= end (:frontier %)) new-paths)]
      (if (or (empty? new-paths) (seq completed))
        (first completed)
        (recur (apply dissoc unvisited (map :frontier new-paths)) new-paths)))))

(defn part-1 []
  (-> (scan-memory (slurp "18/input.txt"))
      (predict-corrupted-locations 71 1024)
      (dijkstra [0 0] [70 70])
      (:cost)))

;; Divide and conquer (thanks, Tijmen)
(defn blocking-byte [bytes]
  (loop [low 1024 high (count bytes)]
    (let [corrupted (+ low (int (/ (- high low) 2)))
          path? (-> (predict-corrupted-locations bytes 71 corrupted)
                    (dijkstra [0 0] [70 70])
                    (some?))]
      (if (<= (- high low) 1)
        (cond-> corrupted path? inc)
        (recur
         (if path? corrupted low)
         (if path? high corrupted))))))

(defn part-2 []
  (let [bytes (scan-memory (slurp "18/input.txt"))]
    (nth bytes (dec (blocking-byte bytes)))))
