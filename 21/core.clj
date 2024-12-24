(ns core
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def numpad
  {\7  [0 0] \8 [1 0] \9 [2 0]
   \4  [0 1] \5 [1 1] \6 [2 1]
   \1  [0 2] \2 [1 2] \3 [2 2]
   nil [0 3] \0 [1 3] \A [2 3]})

(def arrowpad
  {nil [0 0] \^ [1 0] \A [2 0]
   \<  [0 1] \v [1 1] \> [2 1]})

(def coords->arrow
  {[0 1]  \v
   [0 -1] \^
   [1 0]  \>
   [-1 0] \<})

(defn dxdy->coords [[dx dy]]
  (if (neg? dx)
    (concat
     (when-not (= 0 dx) (repeat (abs dx) [(/ dx (abs dx)) 0]))
     (when-not (= 0 dy) (repeat (abs dy) [0 (/ dy (abs dy))])))
    (concat
     (when-not (= 0 dy) (repeat (abs dy) [0 (/ dy (abs dy))]))
     (when-not (= 0 dx) (repeat (abs dx) [(/ dx (abs dx)) 0])))))

(defn path-by-coords [current-coord dxdy-coords]
  (reduce #(conj %1 (map + (peek %1) %2)) [current-coord] dxdy-coords))

(defn null-in-path? [pad current dxdy]
  (some (comp nil? (set/map-invert pad)) (path-by-coords current dxdy)))

(defn key-seq-from-x-to-y [pad current next]
  (let [current-coord (get pad current)
        next-coord    (get pad next)
        dxdy-coords   (dxdy->coords (map - next-coord current-coord))
        reverse?      (null-in-path? pad current-coord dxdy-coords)]
    (cond-> (mapv coords->arrow dxdy-coords)
      reverse? ((comp vec reverse))
      true (conj \A))))

(defn input->arrows [pad input]
  (->> input (cons \A) (partition 2 1) (mapcat #(key-seq-from-x-to-y pad (first %) (second %))) (apply str)))

(def nums->arrows (partial input->arrows numpad))
(def arrows->arrows (partial input->arrows arrowpad))

(def num-presses
  "Stolen straight from Day 11. I couldn't quite remember how to write this again,
   so I copy-pastad and tweaked until it worked. One gotcha was doing the regex split
   in order to get individual sequence of presses processed together, because '<<' is
   not the same as '<' and '<' separately"
  (memoize
   (fn
     [n stone]
     (if (> n 0)
       (apply + (map #(num-presses (dec n) (apply str (arrows->arrows %))) (re-seq #".*?A" stone)))
       (count stone)))))

(defn part-1 []
  (apply + (map complexity (str/split-lines (slurp "21/input.txt")))))

;; Debugging :')

(def coord->num (set/map-invert numpad))
(def coord->arrows (set/map-invert arrowpad))
(def dir->coord
  {\< [-1 0]
   \> [1 0]
   \^ [0 -1]
   \v [0 1]})

(defn decode-arrow
  [buttons start]
  (reduce #(map + %1 (dir->coord %2)) start buttons))

(defn decode-arrows [s]
  (apply str (reduce #(conj %1 (coord->arrows (decode-arrow %2 (arrowpad (or (peek %1) \A))))) [] (str/split s #"A"))))

(defn decode-nums [s]
  (apply str (reduce #(conj %1 (coord->num (decode-arrow %2 (numpad (or (peek %1) \A))))) [] (str/split s #"A"))))

(comment
  (-> (code->my-input "379A") decode-arrows decode-arrows decode-nums)
  )
