(ns core
  (:require
   [clojure.string :as str]))

(defn directional-offsets [word-length]
  {:up         (map #(vector 0 (- 0 %)) (range word-length))
   :up-right   (map #(vector (+ 0 %) (- 0 %)) (range word-length))
   :right      (map #(vector (+ 0 %) 0) (range word-length))
   :down-right (map #(vector (+ 0 %) (+ 0 %)) (range word-length))
   :down       (map #(vector 0 (+ 0 %)) (range word-length))
   :down-left  (map #(vector (- 0 %) (+ 0 %)) (range word-length))
   :left       (map #(vector (- 0 %) 0) (range word-length))
   :up-left    (map #(vector (- 0 %) (- 0 %)) (range word-length))})

(defn read-grid-map [input]
  (into {}
        (for [[y line] (map-indexed vector (str/split-lines input))
              [x char] (map-indexed vector line)]
          [[x y] char])))

(defn find-words [word-grid starting-pos offsets]
  (for [[_direction offsets] offsets]
    (let [coords (map #(map + starting-pos %) offsets)]
      (apply str (map word-grid coords)))))

(defn part-1 []
  (let [search-word "XMAS"
        word-grid (read-grid-map (slurp "04/input.txt"))
        offsets (directional-offsets (count search-word))]
    (reduce-kv (fn [total coord char]
                 (if (= char \X)
                   (let [words (find-words word-grid coord offsets)]
                     (+ total (count (filter #(= search-word %) words))))
                   total))
               0 word-grid)))

(defn combine-diagonals [{:keys [up-right down-right down-left up-left]}]
  {:forward-slash (concat (reverse (drop 1 down-left)) up-right)
   :backward-slash (concat (reverse (drop 1 down-right)) up-left)})

(defn part-2 []
  (let [search-word "MAS"
        search-words #{search-word (apply str (reverse search-word))}
        word-grid (read-grid-map (slurp "04/input.txt"))
        offsets (combine-diagonals (directional-offsets (dec (count search-word))))]
    (reduce-kv (fn [total coord char]
                 (if (= char \A)
                   (let [words (find-words word-grid coord offsets)]
                     (cond-> total
                       (= 2 (count (filter #(contains? search-words %) words))) (inc)))
                   total))
               0 word-grid)))
