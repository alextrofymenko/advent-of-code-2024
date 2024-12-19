(ns core
  (:require
   [clojure.string :as str]))

(defn read-towels-and-patterns [input]
  (let [[towels patterns] (str/split input #"\n\n")]
    {:towels (str/split towels #", ")
     :patterns (str/split-lines patterns)}))

(defn match-pattern [towels pattern]
  (let [towels (filter #(str/includes? pattern %) towels)]
    (loop [possible-matches {pattern []}]
      (or (get possible-matches "")
          (let [new-matches (for [[p ts] possible-matches
                                  t* (filter #(str/starts-with? p %) towels)]
                              [(subs p (count t*)) (conj ts t*)])]
            (when (seq new-matches)
              (recur (into {} new-matches))))))))

(defn part-1 []
  (let [{:keys [towels patterns]} (read-towels-and-patterns (slurp "19/input.txt"))]
    (count (keep #(match-pattern towels %) patterns))))
