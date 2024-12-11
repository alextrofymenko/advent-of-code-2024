(ns core
  (:require
   [clojure.string :as str]))

(defn read-grid-map [input]
  (into {}
        (for [[y line] (map-indexed vector (str/split-lines input))
              [x char] (map-indexed vector line)]
          [[x y] (parse-long (str char))])))

(def directions [[0 1] [1 0] [0 -1] [-1 0]])

(defn next-step [grid-map position current-level]
  (lazy-seq
   (cons position (when (<= current-level 9)
                    (seq (keep #(when (= (get grid-map %) (inc current-level))
                                  (next-step grid-map % (inc current-level)))
                               (map #(map + position %) directions)))))))

;; This took me so much trail [sic] and error :')
(defn map-paths [[node & nodes]]
  (if (seq nodes)
    (for [n nodes
          :when (first n)
          p (map-paths n)]
      (cons node p))
    [[node]]))

(defn check-trails [grid-map position]
  (if (= (get grid-map position) 0)
    (let [trails (map-paths (next-step grid-map position 0))]
      (filter #(= (count %) 10) trails))
    nil))

(defn part-1 []
  (let [grid-map (read-grid-map (slurp "10/input.txt"))]
    (reduce-kv
     (fn [score trailhead _current-level]
       (+ score (count (into #{} (map last) (check-trails grid-map trailhead)))))
     0 grid-map)))
