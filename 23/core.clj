(ns core
  (:require
   [clojure.string :as str]))

(defn scan-computers [input]
  (->> (str/split-lines input)
       (map (comp set #(str/split % #"-")))))

(defn create-index [pairs]
  (reduce
   (fn [index pair]
     (-> index
         (update (first pair) (fnil conj #{}) (last pair))
         (update (last pair) (fnil conj #{}) (first pair))))
   {} pairs))

(defn find-triplets [index]
  (reduce-kv
   (fn [triplets pc-1 pcs]
     (into triplets
           (for [[pc-2 pcs-2] (select-keys index pcs)
                 pc-3 (filter #(contains? (get index %) pc-1) pcs-2)
                 :when (or (str/starts-with? pc-1 "t")
                           (str/starts-with? pc-2 "t")
                           (str/starts-with? pc-3 "t"))]
             #{pc-1 pc-2 pc-3})))
   #{} index))

(defn part-1 []
  (->> (scan-computers (slurp "23/input.txt"))
       (create-index)
       (find-triplets)
       (count)))
