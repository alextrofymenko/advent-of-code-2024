(ns core)

(defn valid-instructions [memory]
  (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" memory))

(defn part-1 []
  (reduce
   (fn [total [_ n1 n2]]
     (+ total (* (parse-long n1) (parse-long n2))))
   0 (valid-instructions (slurp "03/input.txt"))))
