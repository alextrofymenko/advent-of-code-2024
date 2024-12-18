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
        (assoc-in [:registers result-register] (long result))
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

(defn program-loop [initial-state]
  (loop [{:keys [input pointer output] :as  program-state} initial-state]
    (if-let [instruction (instructions (get input pointer))]
      (recur (instruction program-state))
      output)))

(defn part-1 []
  (let [state (init-program (slurp "17/input.txt"))]
    (str/join "," (program-loop state))))

(defn octal-set-bit [octal-v n bit]
  (concat (take n octal-v) [bit] (drop (inc n) octal-v)))

(defn octal->long [octal-v]
  (long (read-string (apply str "0" octal-v))))

(defn compatible-bit [{:keys [input] :as initial-state} octal-v n]
  (when (< n (count input))
    (let [n' (- (dec (count input)) n)]
      (for [bit   (range 8)
            :let  [register-a (octal->long (octal-set-bit octal-v n bit))
                   output (program-loop (assoc-in initial-state [:registers :A] register-a))]
            :when (= (get input n') (get output n'))]
        bit))))

(defn smallest-octal-v [initial-state octal-v n]
  (first
   (for [b (compatible-bit initial-state octal-v n)
         :let [bs (smallest-octal-v initial-state (octal-set-bit octal-v n b) (inc n))]
         :when (or (= n (dec (count (:input initial-state)))) (seq bs))]
     (cons b bs))))

(defn part-2
  "A hint from Reddit set me on the right path, but I hit a wall again with recursion,
   and the final result is just me throwing things at said wall until something stuck.
   I was hoping to get a list of numbers back, but I coudln't figure out how to unfold
   the nested list I got from from smallest-octal-v, and just by adding 'first' there
   got me the answer I was looking for. I though I understood lazy-seq, but I do not.

   The trick to the solution is realising that the program transforms 8-bit numbers in
   reverse order, i.e. the first bit in Register A in octal form becomes the last bit
   in the output string. For example, comparing octal input vs actual output:

   o3000000000000000 -> [5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 6]
   o5000000000000000 -> [5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 0] ;; Last digit correct
   o7000000000000000 -> [5 5 5 5 5 5 5 5 5 5 5 5 5 5 1 2]

   o5600000000000000 -> [5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 0] ;; Last 2 digits correct

   So the most-significant digit in our octal input number is 5. Then next one is 6,
   and so on. There can be more than one answer at any one stage, for example after
   [5 6] either 0 or 1 works, but examining the sequence fully may terminate some seqs
   sooner. That's what I had to use recursion for and got infinitely confused. Maybe
   one day I will be able to solve it quicker"
  []
  (let [{:keys [input] :as initial-state} (init-program (slurp "17/input.txt"))]
    (octal->long (smallest-octal-v initial-state (vec (repeat (count input) 0)) 0))))
