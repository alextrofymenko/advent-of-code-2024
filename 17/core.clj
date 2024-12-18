(ns core
  (:require
   [clojure.string :as str]))

(defn init-program [input]
  (let [[registers program] (str/split input #"\n\n")]
    {:registers (zipmap [:A :B :C] (map parse-long (re-seq #"\d+" registers)))
     :input     (mapv parse-long (re-seq #"\d+" program))
     :pointer   0
     :output    []}))

(defn combo-operand [registers operand]
  (case operand
    (0 1 2 3) operand
    4 (:A registers)
    5 (:B registers)
    6 (:C registers)
    7 (throw (Exception. "Reserved"))))

(defn xdv [{:keys [registers input pointer] :as program-state} result-register]
  (let [operand (get input (inc pointer))
        result  (/ (:A registers) (bit-shift-left 1 (combo-operand registers operand)))]
    (-> program-state
        (assoc-in [:registers result-register] (int result))
        (update :pointer + 2))))

(defn bxl [{:keys [registers input pointer] :as  program-state}]
  (let [operand (get input (inc pointer))
        result  (bit-xor (:B registers) operand)]
    (-> program-state
        (assoc-in [:registers :B] result)
        (update :pointer + 2))))

(defn bst [{:keys [registers input pointer] :as  program-state}]
  (let [operand (get input (inc pointer))
        result  (mod (combo-operand registers operand) 8)]
    (-> program-state
        (assoc-in [:registers :B] result)
        (update :pointer + 2))))

(defn jnz [{:keys [registers input pointer] :as  program-state}]
  (if (= 0 (:A registers))
    (update program-state :pointer + 2)
    (assoc program-state :pointer (get input (inc pointer)))))

(defn bxc [{:keys [registers] :as  program-state}]
  (-> program-state
      (assoc-in [:registers :B] (bit-xor (:B registers) (:C registers)))
      (update :pointer + 2)))

(defn out [{:keys [registers input pointer] :as  program-state}]
  (let [operand (get input (inc pointer))]
    (-> program-state
        (update :output conj (mod (combo-operand registers operand) 8))
        (update :pointer + 2))))

(def instructions
  {0 #(xdv % :A)
   1 bxl
   2 bst
   3 jnz
   4 bxc
   5 out
   6 #(xdv % :B)
   7 #(xdv % :C)})

(defn program-loop [input]
  (loop [{:keys [input pointer output] :as  program-state} (init-program input)]
    (if-let [instruction (instructions (get input pointer))]
      (recur (instruction program-state))
      (str/join "," output))))

(defn part-1 []
  (program-loop (slurp "17/input.txt")))
