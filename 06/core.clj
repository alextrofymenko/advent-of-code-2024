(ns core
  (:require
   [clojure.string :as str]))

(defn read-the-situation [input]
  (let [guard (atom nil)
        floor (for [[y line] (map-indexed vector (str/split-lines input))
                    [x char] (map-indexed vector line)
                    :let [_oh-no-a-guard!
                          (when (contains? #{\^ \> \v \<} char)
                            (reset! guard {:loc [x y] :dir char}))]]
                [[x y] char])]
    {:floor (into {} floor)
     :guard @guard}))

(def NEXT-STEP
  {\^ [0 -1]
   \> [1 0]
   \v [0 1]
   \< [-1 0]})

(def RIGHT-TURN
  {\^ \>
   \> \v
   \v \<
   \< \^})

(defn predict-next-guard-location [floor guard]
  (let [next-loc (map + (:loc guard) (get NEXT-STEP (:dir guard)))]
    (when-let [tile (get floor next-loc)]
      (if (= tile \#)
        (recur floor (update guard :dir RIGHT-TURN))
        (assoc guard :loc next-loc)))))

(defn predict-path [{:keys [floor guard]}]
  (loop [guard-path [guard]]
    (if-let [next-guard-location (predict-next-guard-location floor (peek guard-path))]
      (if (contains? (set guard-path) next-guard-location)
        ::loop
        (recur (conj guard-path next-guard-location)))
      guard-path)))

(defn part-1 []
  (->> (read-the-situation (slurp "06/input.txt"))
       (predict-path)
       (into #{} (map :loc))
       (count)))

(defn part-2 []
  ;; This is slow, there is probably a smarter solution :'(
  (let [situation (read-the-situation (slurp "06/input.txt"))]
    (->> (into #{} (map :loc) (rest (predict-path situation)))
         (pmap #(= ::loop (predict-path (update situation :floor assoc % \#))))
         (filter true?)
         (count))))
