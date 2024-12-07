(ns core
  (:require
   [clojure.string :as str]))

(defn equations [input]
  (for [l (str/split-lines input)]
    (let [[result nums] (str/split l #": ")]
      [(parse-long result) (map parse-long (str/split nums #" "))])))

(def BIT->OP {\0 * \1 +})

(defn operations
  "There are only 2 equations so we can make use of binary representation of each permutation,
   i.e. if there are 4 number [1 2 3 4] there are 3 places for operations (between them), which
   brings the total number of possible equations to 8 (2^3). So for each binary representation
   of numbers 0-7, replace a bit with an operation, e.g. 0 -> 000 -> ***, 3 -> 011 -> *++, etc"
  [n]
  (for [x (range (Math/pow 2 n))
        :let [binary-mask (format (str "%0" n "d") (biginteger (Integer/toString x 2)))]]
    (mapv BIT->OP binary-mask)))

(defn solvable?
  "For all possible permutations of operations, try all equations to see if they satisfy the result"
  [[result nums]]
  (some #(= result (second
                    (reduce (fn [[i1 n1] [i2 n2]]
                              [i2 ((get % i1) n1 n2)])
                            (map-indexed vector nums))))
        (operations (dec (count nums)))))

(defn part-1 []
  (apply + (keep #(when (solvable? %) (first %)) (equations (slurp "07/input.txt")))))
