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
    ;; (prn (get input pointer) program-state)
    (if-let [instruction (instructions (get input pointer))]
      (recur (instruction program-state))
      (str/join "," output))))

(defn part-1 []
  (program-loop (slurp "17/input.txt")))

(comment
  (program-loop (slurp "/Users/alex/code/personal/advent-of-code-2024/17/input.txt"))
  (program-loop "0 0 9\n\n2 6")
  (program-loop "10 0 0\n\n5,0,5,1,5,4")
  (program-loop "2024 0 0\n\n0,1,5,4,3,0")
  (program-loop "0 29 0\n\n1,7")
  (program-loop "0 2024 43690\n\n4,0")


  (program-loop "117440 0 0\n\n0,3,5,4,3,0")

  (out {:registers {} :input [3 0] :pointer 0})

  (bst {:registers {:A 3} :input [2 4] :pointer 0})              ;; => {:B (mod A 8) = [0 - 7]}
  (bxl {:registers {:B 1} :input [1 1] :pointer 0})              ;; => {:B [1 1 3 3 5 5 7 7]}
  (xdv {:registers {:A 1 :B 1} :input [7 5] :pointer 0} :C)      ;; => {:C (/ A 2^)}
  (bxc {:registers {:B 1 :C 132123} :input [1 1] :pointer 0})    ;; => {:B xor C}
  (bxl {:registers {:B 1} :input [1 4] :pointer 0})              ;; => {:B 12314}
  (xdv {:registers {:A 1} :input [0 3] :pointer 0} :A)           ;; => {:A (/ A 8)}
  (out {:registers {:B 1} :input [5 5] :pointer 0})              ;; => {:out (mod B 8)}
  (jnz {:registers {:B 1} :input [3 0] :pointer 0})              ;; => {:out (mod B 8)}

  (map #(bit-xor % 4) [1357123])

  ;;                                      [x16   >15]
  ;; [[bst] [bxl] [cdv] [bxc] [bxl] [adv] [out] [jnz]]
  ;; [[2 4] [1 1] [7 5] [4 7] [1 4] [0 3] [5 5] [3 0]]

  (count (re-seq #"\d" (program-loop "13421772800N 0 0\n\n2,4,1,1,7,5,4,7,1,4,0,3,5,5,3,0")))



  ;; 16 (program-loop "5 0 0\n\n2,4,1,1,7,5,4,7,1,4,0,3,5,5,3,0")
  (bst {:registers {:A 5 :B 0 :C 0} :input [2 4] :pointer 0})
  (bxl {:registers {:A 5 :B 5 :C 0} :input [1 1] :pointer 0})
  (xdv {:registers {:A 5 :B 4 :C 0} :input [7 5] :pointer 0} :C)
  (bxc {:registers {:A 5 :B 4 :C 0} :input [1 1] :pointer 0})
  (bxl {:registers {:A 5 :B 4 :C 0} :input [1 4] :pointer 0})
  (xdv {:registers {:A 5 :B 0 :C 0} :input [0 3] :pointer 0} :A)
  (out {:registers {:A 0 :B 0 :C 0} :input [5 5] :pointer 0})
  (jnz {:registers {:A 0 :B 0 :C 0} :input [3 0] :pointer 0})


  ;; 5 => 0
  ;; 6 => 3 46 => 3,0
  ;; 0,1 => 5 368, 369 => 5,3,0


  (* 46 8)
  (program-loop "8564 0 0\n\n2,4,1,1,7,5,4,7,1,4,0,3,5,5,3,0")

  (let [outputs {"2" 7, "3" 6, "0" 5, "1" 4, "6" 3, "7" 2, "5" 0 "4,0" 43}]
    (loop [out ["0" "3" "5" "5" "3" "0,4"]
           a 0]
      (if-let [o (outputs (first out))]
        (recur (rest out) (+ (cond-> (* 8 a)) o))
        a)))

  (program-loop (str (+ (* 23558) ) " 0 0\n\n2,4,1,1,7,5,4,7,1,4,0,3,5,5,3,0"))

  (/ (- (/ (- (/ (- 8564 4) 8) 6) 8) 5) 8)

  (loop [a  5
         a' [6 0 0 6 8564]]
    (prn (first a') (program-loop (str a " 0 0\n\n2,4,1,1,7,5,4,7,1,4,0,3,5,5,3,0")))
    (if-let [a* (first a')]
      (recur (+ (* a 8) a*) (rest a'))
      a))



  (loop [a  2
         a' [0 5 6 4]]
    (prn a' (program-loop (str a " 0 0\n\n2,4,1,1,7,5,4,7,1,4,0,3,5,5,3,0")))
    (if-let [a* (first a')]
      (recur (+ (* a 8) a*) (rest a'))
      a))


  (for [a' (range 1000000)
        :let [out (program-loop (str a' " 0 0\n\n2,4,1,1,7,5,4,7,1,4,0,3,5,5,3,0"))]
        :when (re-find #"4,0,3,5,5,3,0" out)]
    [a' out])

  (into {}
        (sort-by
         (comp - second)
         (for [a (range 8)]
           [(program-loop (format "%s 0 0\n\n2,4,1,1,7,5,4,7,1,4,0,3,5,5,3,0" a)) a])))

  (for [a (range 80000)
        :when (= "0,4" (program-loop (format "%s 0 0\n\n2,4,1,1,7,5,4,7,1,4,0,3,5,5,3,0" a)))]
    a)

  (Math/pow 5 16)
  )
