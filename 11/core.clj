(ns core
  (:require
   [clojure.string :as str]))

(defn read-stones [input]
  (str/split input #" "))

(defn transform-stone [number]
  (let [num-digits (count number)]
    (cond
      (= number "0") ["1"]
      (even? num-digits) (let [[left right] (split-at (/ num-digits 2) number)]
                           [(apply str left) (-> (apply str right) parse-long str)])
      :else [(str (* 2024 (parse-long number)))])))

(defn blink [n stones]
  (if (= n 0)
    stones
    (recur (dec n) (mapcat transform-stone stones))))

(defn part-1 []
  (count (blink 25 (read-stones "70949 6183 4 3825336 613971 0 15 182"))))

(def blink-sum
  "Took me 4 days to think of doing it this way (completed on day 14) - keeping the sum
   in memory rather than the entire seq. It's both faster (almost instant thanks to memoize),
   and takes less memory (~80MB even with memoize). The original solution would have taken
   weeks, assuming it didn't run out of memory"
  (memoize
   (fn [n stone]
     (if (> n 0)
       (apply + (map #(blink-sum (dec n) %) (transform-stone stone)))
       1))))

(defn part-2 []
  (apply + (map #(blink-sum 75 %) (read-stones "70949 6183 4 3825336 613971 0 15 182"))))
