(ns core
  (:require
   [clojure.string :as str]))

(defn read-towels-and-patterns [input]
  (let [[towels patterns] (str/split input #"\n\n")]
    {:towels (str/split towels #", ")
     :patterns (str/split-lines patterns)}))

(defn match-pattern [towels pattern]
  (let [towels (filter #(str/includes? pattern %) towels)]
    (loop [possible-matches {pattern 0}]
      ;; Part 2 took me a while to get right. Keeping the matches patters in memory
      ;; proved far too expensive, so instead I had to keep track of the number
      ;; of splits for each subpattern, and later add them all together. Effectively,
      ;; for every new pattern that matches a sub-pattern, add a score of 1, then reduce
      ;; them back into a map of subpattern and counts, and repeat the process
      (let [new-matches (for [[p n] possible-matches
                              towel (filter #(str/starts-with? p %) towels)]
                          [(subs p (count towel)) (if (= n 0) 1 n)])]
        (if (seq new-matches)
          (recur (reduce
                  #(update %1 (first %2) (fnil + 0) (second %2))
                  {"" (get possible-matches "")} new-matches))
          (get possible-matches ""))))))

(defn part-1 []
  (let [{:keys [towels patterns]} (read-towels-and-patterns (slurp "19/input.txt"))]
    (count (keep #(match-pattern towels %) patterns))))

(defn part-2 []
  (let [{:keys [towels patterns]} (read-towels-and-patterns (slurp "19/input.txt"))]
    (->> (keep #(match-pattern towels %) patterns)
         (apply +))))
