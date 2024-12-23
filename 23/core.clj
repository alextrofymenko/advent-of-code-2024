(ns core
  (:require
   [clojure.set :as set]
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

(defn biggest-party-for-linked-pcs [index linked-pcs]
  (let [all-pcs-in-party (reduce-kv #(conj %1 (conj %3 %2)) #{} (select-keys index linked-pcs))]
    (reduce
     (fn [bigest-party pcs]
       (let [party (apply set/intersection (disj all-pcs-in-party pcs))]
         (if (> (count party) (count bigest-party))
           party
           bigest-party)))
     #{} all-pcs-in-party)))

(defn biggest-party-per-pc
  "There are at most 13 PCs in any one group because that's the most any one PC
   is linked to, as seen in the index. This isn't really relevant, but it gave me
   the idea that sets we're working with aren't really that large.
   The idea is that for every PC, we look at the PCs it's linked to, and then look
   at the PCs that they are linked to. Combined together, we should find a set that
   contains all the PCs, assuming the largest is actually 13, or if it isn't, then
   we still get the largest group possible for any one PC."
  [index]
  (reduce-kv
   (fn [biggest-party pc linked-pcs]
     (assoc biggest-party pc (biggest-party-for-linked-pcs index linked-pcs)))
   {} index))

(defn the-biggest-party [index]
  (->> (biggest-party-per-pc index) (sort-by (comp count val)) last last))

(defn get-password [group]
  (str/join #"," (sort group)))

(defn part-2 []
  (->> (scan-computers (slurp "23/input.txt"))
       (create-index)
       (the-biggest-party)
       (get-password)))
