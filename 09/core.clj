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

(defn lookahead [blocks cursor]
  (let [ref-block (aget blocks cursor)]
    (loop [c cursor]
      (if (and (= ref-block (aget blocks c)) (< c (dec (count blocks))))
        (recur (inc c))
        [ref-block (- c cursor)]))))

(defn lookbehind [blocks cursor]
  (let [ref-block (aget blocks cursor)]
    (loop [c cursor]
      (if (and (= ref-block (aget blocks c)) (> c 0))
        (recur (dec c))
        (- cursor c)))))

(defn free-space [blocks right-cursor required-size]
  (loop [left-cursor 0]
    (when (< left-cursor right-cursor (count blocks))
      (when-let [[ref-block size] (lookahead blocks left-cursor)]
        (if (and (empty-space? ref-block) (>= size required-size))
          left-cursor
          (recur (+ left-cursor size)))))))

(defn sort-chunks! [blocks]
  (loop [right-cursor (dec (count blocks))]
    (when (> right-cursor 0)
      (if (empty-space? (aget blocks right-cursor))
        (recur (dec right-cursor))
        (let [size (lookbehind blocks right-cursor)]
          (when-let [left-cursor (free-space blocks right-cursor size)]
            (dotimes [n size] (swap-blocks! blocks (+ left-cursor n) (- right-cursor n))))
          (recur (- right-cursor size)))))))

(defn part-2 []
  (let [blocks (read-blocks (slurp "09/input.txt"))]
    (sort-chunks! blocks)
    (->> (map-indexed vector blocks)
         (reduce (fn [res [i v]] (+ res (* i (if (neg? v) 0 v)))) 0))))
