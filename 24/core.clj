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
