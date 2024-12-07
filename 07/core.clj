(ns core
  (:require
   [clojure.string :as str]))

(defn equations [input]
  (for [l (str/split-lines input)]
    (let [[result nums] (str/split l #": ")]
      [(parse-long result) (map parse-long (str/split nums #" "))])))

(defn operation-permutations
  "Since there are 3 equations for part 2, binary representation is no longer suitable,
   however the description from part 1 (below) still largely applies, we just need to increase
   the base to the number of operations. The solution for part 2 will hold for up to base 36,
   which is the max supported for Integer/toString (10 digits and 26 letters).

   Description from part 1:
   There are only 2 equations so we can make use of binary representation of each permutation,
   i.e. if there are 4 number [1 2 3 4] there are 3 places for operations (between them), which
   brings the total number of possible equations to 8 (2^3). So for each binary representation
   of numbers 0-7, replace a bit with an operation, e.g. 0 -> 000 -> ***, 3 -> 011 -> *++, etc"
  [n ops-idx]
  (for [x (range (Math/pow (count ops-idx) n))
        :let [base-str (format (str "%0" n "d") (biginteger (Integer/toString x (count ops-idx))))]]
    (mapv ops-idx base-str)))

(defn solvable?
  "For all possible permutations of operations, try all equations to see if they satisfy the result"
  [[result nums] operations]
  (let [ops-idx (zipmap (apply str (range (count operations))) operations)]
    (some #(= result (second
                      (reduce (fn [[i1 n1] [i2 n2]]
                                [i2 ((get % i1) n1 n2)])
                              (map-indexed vector nums))))
          (operation-permutations (dec (count nums)) ops-idx))))

(defn part-1 []
  (->> (equations (slurp "07/input.txt"))
       (keep #(when (solvable? % [* +]) (first %)))
       (apply +)))

(defn part-2 []
  (let [|| #(parse-long (str %1 %2))]
    (->> (equations (slurp "07/input.txt"))
         (keep #(when (solvable? % [* + ||]) (first %)))
         (apply +))))
