(ns core
  (:require
   [clojure.string :as str]))

(def mix bit-xor)
(def prune #(mod % 16777216))

(defn next-secret-number [secret-number]
  (let [step-1 (-> (* secret-number 64) (mix secret-number) prune)
        step-2 (-> (/ step-1 32) int (mix step-1) prune)]
    (-> (* 2048 step-2) (mix step-2) prune)))

(defn lazy-secret-number [secret-number]
  (lazy-seq (cons secret-number (lazy-secret-number (next-secret-number secret-number)))))

(defn part-1 []
  (->> (str/split-lines (slurp "22/input.txt"))
       (map parse-long)
       (map #(nth (lazy-secret-number %) 2000))
       (apply +)))

(defn price-change-seqs [secret-numbers]
  (let [ones-vector (into [] (map #(mod % 10)) secret-numbers)]
    (->> (partition 2 1 ones-vector)
         (map #(- (second %) (first %1)))
         (partition 4 1)
         (map-indexed (fn [i change-seq] [change-seq (get ones-vector (+ i 4))]))
         (reduce #(cond-> %1 (not (contains? %1 (first %2))) (assoc (first %2) (second %2))) {}))))

(defn part-2 []
  (let [buyers-prices-idx (->> (str/split-lines (slurp "22/input.txt"))
                               (map (comp price-change-seqs #(take 2001 %) lazy-secret-number parse-long)))]
    (->> (into #{} (mapcat keys) buyers-prices-idx)
         (reduce (fn [bananas change-seq] (max bananas (apply + (keep #(get % change-seq) buyers-prices-idx)))) 0))))
