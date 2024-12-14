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
