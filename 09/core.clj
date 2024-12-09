(ns core)

(defn read-blocks [input]
  (int-array
   (into [] (comp (partition-all 2)
                  (map-indexed vector)
                  (mapcat
                   (fn [[id [file-spaces empty-spaces]]]
                     (concat (repeat file-spaces id) (some-> empty-spaces (repeat -1))))))
         (keep #(some-> % str parse-long) input))))

(def empty-space? neg?)

(defn swap-blocks! [blocks i j]
  (let [i' (aget blocks i)]
    (aset blocks i (aget blocks j))
    (aset blocks j i')))

(defn sort-blocks! [blocks]
  (loop [left-cursor  0
         right-cursor (dec (count blocks))]
    (when (> right-cursor left-cursor)
      (if (empty-space? (aget blocks left-cursor))
        (if (empty-space? (aget blocks right-cursor))
          (recur left-cursor (dec right-cursor))
          (do
            (swap-blocks! blocks left-cursor right-cursor)
            (recur (inc left-cursor) (dec right-cursor))))
        (recur (inc left-cursor) right-cursor)))))

(defn part-1 []
  (let [blocks (read-blocks (slurp "09/input.txt"))]
    (sort-blocks! blocks)
    (->> (take-while (complement empty-space?) blocks)
         (map-indexed vector)
         (reduce #(+ %1 (* (first %2) (second %2))) 0))))
