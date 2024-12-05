(ns core
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(defn rules-idx [rules]
  (reduce
   (fn [rules rule]
     (let [[page-1 page-2] (str/split rule #"\|")]
       (-> rules
           (update page-1 update :before (fnil conj #{}) page-2)
           (update page-2 update :after (fnil conj #{}) page-1))))
   {} (str/split-lines rules)))

(defn read-rules-and-updates [input]
  (let [[rules updates] (str/split input #"\n\n")]
    {:rules   (rules-idx rules)
     :updates (map #(str/split % #",") (str/split-lines updates))}))

(defn page-right-order? [rules page next-pages]
  (every? #(let [page-rules (get rules page)]
             (and (or (empty? (:before page-rules))
                      (contains? (:before page-rules) %))
                  (or (empty? (:after page-rules))
                      (not (contains? (:after page-rules) %)))))
          next-pages))

(defn update-right-order? [rules pages]
  (loop [[page & next-pages] pages]
    (when (page-right-order? rules page next-pages)
      (or (empty? next-pages)
          (recur next-pages)))))

(defn find-middle-page [pages]
  (nth pages (int (/ (count pages) 2))))

(defn sum-middle-pages [updates]
  (apply + (map (comp parse-long find-middle-page) updates)))

(defn part-1 []
  (let [{:keys [rules updates]} (read-rules-and-updates (slurp "05/input.txt"))]
    (->> (filter #(update-right-order? rules %) updates)
         (sum-middle-pages))))

(defn re-order-pages [rules pages]
  (->> (map (juxt identity #(set/intersection (get-in rules [% :after]) (set pages))) pages)
       (sort-by (comp count second))
       (map first)))

(defn part-2 []
  (let [{:keys [rules updates]} (read-rules-and-updates (slurp "05/input.txt"))]
    (->> (remove #(update-right-order? rules %) updates)
         (map #(re-order-pages rules %))
         (sum-middle-pages))))
