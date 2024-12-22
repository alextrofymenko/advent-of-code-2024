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
