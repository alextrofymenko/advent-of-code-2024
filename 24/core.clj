(ns core
  (:require
   [clojure.string :as str]))

(defn ->logic-gate [gate input-1 input-2]
  (case gate
    "AND" (= input-1 input-2 "1")
    "OR"  (or (= "1" input-1) (= "1" input-2))
    "XOR" (not= input-1 input-2)))

(defn ->fn [logic]
  (let [[_ input-wire-1 gate input-wire-2 output-wire] (re-find #"(.*?) (.*?) (.*?) -> (.*)" logic)]
    (fn [system-state]
      (let [input-1 (get system-state input-wire-1)
            input-2 (get system-state input-wire-2)]
        (when (and input-1 input-2)
          (let [output (if (->logic-gate gate input-1 input-2) "1" "0")]
            {output-wire output}))))))

(defn read-system [input]
  (let [[wires logic] (str/split input #"\n\n")]
    {:init  (->> (str/split-lines wires)
                 (mapcat #(str/split % #": "))
                 (apply hash-map))
     :logic (map ->fn (str/split-lines logic))}))

(defn run-system [{:keys [init logic]}]
  (loop [state init]
    (let [state' (reduce merge state (keep #(% state) logic))]
      (if (= state' state)
        state
        (recur state')))))

(defn read-output [state]
  (for [[k v] (sort-by key state)
        :when (str/starts-with? k "z")]
    v))

(defn ->number [bits]
  (apply + (map-indexed #(if (= %2 "1") (bit-shift-left 1 %1) 0) bits)))

(defn part-1 []
  (-> (slurp "24/input.txt") read-system run-system read-output ->number))

(defn part-2
  "I think this was probably the hardest puzzle yet. I have no idea why this works,
   I just followed the steps from the main post + comment from Reddit:
   https://www.reddit.com/r/adventofcode/comments/1hla5ql/comment/m3kws15/

   1. If the output of a gate is z, then the operation has to be XOR unless it is the last bit.
   2. If the output of a gate is not z and the inputs are not x, y then it has to be AND / OR, but not XOR.
   3. If you have a XOR gate with inputs x, y, there must be another XOR gate with this gate as an input.
      Search through all gates for an XOR-gate with this gate as an input; if it does not exist, your (original) XOR gate is faulty.
   4. Similarly, if you have an AND-gate, there must be an OR-gate with this gate as an input.
      If that gate doesn't exist, the original AND gate is faulty.

   (3 & 4 don't apply for x00 and y00)"
  []
  (let [[_ logic] (str/split (slurp "24/input.txt") #"\n\n")
        logic (str/split-lines logic)
        step-1 (filter #(re-find #"... [^X].*? ... -> z[^(45)]" %) logic)
        step-2 (filter #(re-find #"[^xy].. XOR [^xy].. -> [^z]" %) logic)
        step-3 (for [line logic
                     :when (re-find #"[xy](?!00).*? XOR [xy](?!00).*? " line)
                     :let [output (re-find #"(?<= -> ).*" line)]
                     :when (empty? (filter #(re-find (re-pattern (str output " XOR | XOR " output)) %) logic))]
                 line)
        step-4 (for [line logic
                     :when (and (re-find #" AND " line) (not (re-find #"x00|y00" line)))
                     :let [output (re-find #"(?<= -> ).*" line)]
                     :when (empty? (filter #(re-find (re-pattern (str output " OR | OR " output)) %) logic))]
                 line)]
    (->> (into #{} (concat step-1 step-2 step-3 step-4))
         (map #(re-find #"(?<= -> ).*" %))
         (sort)
         (str/join ","))))
