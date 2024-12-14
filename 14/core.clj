(ns core
  (:require
   [clojure.string :as str]))

(defn robot-specs [input]
  (for [line (str/split-lines input)
        :let [[_ px py vx vy] (re-find #"p=(\d+),(\d+) v=(-?\d+),(-?\d+)" line)]]
    {:px (parse-long px)
     :py (parse-long py)
     :vx (parse-long vx)
     :vy (parse-long vy)}))

(defn init-map [input]
  (let [robots (robot-specs (slurp input))]
    {:robots   robots
     :map-size (if (> (count robots) 100)
                 {:x 101 :y 103}
                 {:x 11 :y 7})}))

(defn project-location [robot seconds]
  {:px (+ (:px robot) (* seconds (:vx robot)))
   :py (+ (:py robot) (* seconds (:vy robot)))})

(defn correct-for-map-size [location map-size]
  (let [px (rem (:px location) (:x map-size))
        py (rem (:py location) (:y map-size))]
    {:px (cond-> px (neg? px) (+ (:x map-size)))
     :py (cond-> py (neg? py) (+ (:y map-size)))}))

(defn robots-by-quadrant [{:keys [robots map-size]} seconds]
  (reduce
   (fn [quadrants robot]
     (let [location (-> (project-location robot seconds)
                        (correct-for-map-size map-size))
           left?    (< (:px location) (int (/ (:x map-size) 2)))
           right?   (> (:px location) (int (/ (:x map-size) 2)))
           top?     (< (:py location) (int (/ (:y map-size) 2)))
           bottom?  (> (:py location) (int (/ (:y map-size) 2)))]
       (cond-> quadrants
         (and left? top?)     (update :q1 conj location)
         (and right? top?)    (update :q2 conj location)
         (and left? bottom?)  (update :q3 conj location)
         (and right? bottom?) (update :q4 conj location))))
   {} robots))

(defn safety-factor [by-quandrant]
  (apply * (map (comp count val) by-quandrant)))

(defn part-1 []
  (-> (init-map "14/input.txt")
      (robots-by-quadrant 100)
      (safety-factor)))

(defn draw-map [by-quadrant]
  (let [by-coords (reduce #(assoc %1 [(:px %2) (:py %2)] \X) {} (mapcat val by-quadrant))]
    (doseq [y (range 103)]
      (prn (apply str (map #(get by-coords [% y] \.) (range 101)))))))

(defn part-2
  "I had no idea what this meant, whether it was referring to a previous exercise or what.
   I saw that https://adventofcode.com/2015/ had a XMAS tree for example, but couldn't see
   how I could guess where in the image it would be. So I went to check Reddit and found this:

   > Like many other people have pointed out, the states cycle every 101*103=10403
     iterations which unfortunately is still a lot of states to manually check.
   > To speed things up, I used the heuristic that if the robots formed a random image,
     they'd be spread out evenly, so the safety factor would be high. Conversely if the
     robots formed a structured image we'd expect clustering in certain quadrants which
     would lower the overall safety score.

   > So I sorted the states by safety score and the 11th lowest safety score had a Christmas tree 🎄

   For me this was the lowest safety score - [6475 97078128]. I just printed the map and there it was!
   This is both brilliant and frustrating, I don't think I would have ever gotten there on my own..."
  []
  (let [map-and-robots (init-map "14/input.txt")]
    (->> (range 1 (inc (* 101 103)))
         (map #(vector % (safety-factor (robots-by-quadrant map-and-robots %))))
         (sort-by second)
         (take 50))))

(comment
  (draw-map (robots-by-quadrant (init-map "14/input.txt") 6475)))
