(ns core
  (:require
   [clojure.string :as str]))

(def directions
  {\^ [0 -1]
   \> [1 0]
   \v [0 1]
   \< [-1 0]})

(defn warehouse-coords [warehouse-map]
  (into {}
        (for [[y line] (map-indexed vector (str/split-lines warehouse-map))
              [x char] (map-indexed vector line)]
          [[x y] char])))

(defn wide-warehouse-coords [warehouse-map]
  (into {}
        (for [[y line] (map-indexed vector (str/split-lines warehouse-map))
              [x char] (map-indexed vector line)
              double-x [0 1]]
          [[(+ double-x (* x 2)) y]
           (cond
             (= \O char) (if (= double-x 0) \[ \])
             (= \@ char) (if (= double-x 0) \@ \.)
             :else char)])))

(defn movement-coords [robot-movements]
  (for [line (str/split-lines robot-movements)
        char line]
    (directions char)))

(defn init
  ([input]
   (init input {}))
  ([input {:keys [double-wide?]}]
   (let [[warehouse-map robot-movements] (str/split input #"\n\n")]
     {:warehouse-map (if double-wide?
                       (wide-warehouse-coords warehouse-map)
                       (warehouse-coords warehouse-map))
      :robot-movements (movement-coords robot-movements)})))

(defn next-empty-space [warehouse-map coords direction]
  (let [next-step (map + coords direction)]
    (case (get warehouse-map next-step)
      \#         nil
      \.         next-step
      (\O \[ \]) (recur warehouse-map next-step direction))))

(defn next-empty-vertical-space-for-double-box [warehouse-map coords direction]
  (let [tile    (get warehouse-map coords)
        empties (cond
                  (= tile \.) nil
                  (= tile \#) [:wall]
                  :else (let [[left-coords right-coords] (if (= tile \[)
                                                           [coords (map + coords [1 0])]
                                                           [(map + coords [-1 0]) coords])
                              left-next-step             (map + left-coords direction)
                              right-next-step            (map + right-coords direction)
                              next-empty-spaces          (cond
                                                           (and (= \. (get warehouse-map left-next-step))
                                                                (= \. (get warehouse-map right-next-step)))
                                                           [left-coords right-coords]

                                                           (or (= \# (get warehouse-map left-next-step))
                                                               (= \# (get warehouse-map right-next-step)))
                                                           [:wall]

                                                           :else
                                                           (into [left-coords right-coords]
                                                                 (mapcat #(next-empty-vertical-space-for-double-box warehouse-map % direction))
                                                                 [left-next-step right-next-step]))]
                          next-empty-spaces))]
    empties))

(defn move-double-boxes-horizontally [warehouse-map [from-x y] [to-x]]
  (let [distance (Math/abs (- from-x to-x))
        direction (if (> from-x to-x) [-1 0] [1 0])
        boxes-coords (map #(vector (+ from-x (* (first direction) %)) y) (range distance))
        boxes (->> (select-keys warehouse-map boxes-coords)
                   (sort-by (if (> from-x to-x)
                              (comp ffirst)
                              (comp - ffirst))))]
    (reduce
     (fn [warehouse-map [coords edge]]
       (assoc warehouse-map coords \. (map + coords direction) edge))
     warehouse-map boxes)))

(defn move-double-boxes-vertically [warehouse-map boxes-coords direction]
  (let [boxes (->> (select-keys warehouse-map boxes-coords)
                   (sort-by (if (neg? (second direction))
                              (comp second first)
                              (comp - second first))))]
    (reduce
     (fn [warehouse-map [coords edge]]
       (assoc warehouse-map coords \. (map + coords direction) edge))
     warehouse-map boxes)))

(defn process-step [warehouse-map direction]
  ;; Pretty slow, but that's expected. I just wanted to see what perforamnce would be like (hint: it's not great)
  (let [robot     (first (keep (fn [[coords tile]] (when (= \@ tile) coords)) warehouse-map))
        next-step (map + robot direction)
        next-tile (get warehouse-map next-step)]
    (cond
      (= \. next-tile)
      (assoc warehouse-map robot \. next-step \@)

      (= \O next-tile)
      (if-let [empty-space (next-empty-space warehouse-map next-step direction)]
        (assoc warehouse-map robot \. next-step \@ empty-space \O)
        warehouse-map)

      (and (contains? #{\[ \]} next-tile) (= 0 (first direction)))
      (let [boxes-to-move (next-empty-vertical-space-for-double-box warehouse-map next-step direction)]
        (if (every? seq? boxes-to-move)
          (-> (move-double-boxes-vertically warehouse-map boxes-to-move direction)
              (assoc robot \. next-step \@))
          warehouse-map))

      (and (contains? #{\[ \]} next-tile) (= 0 (second direction)))
      (if-let [empty-space (next-empty-space warehouse-map next-step direction)]
        (-> (move-double-boxes-horizontally warehouse-map next-step empty-space)
            (assoc robot \. next-step \@))
        warehouse-map)

      :else warehouse-map)))

(defn process-steps [{:keys [warehouse-map robot-movements]}]
  (reduce process-step warehouse-map robot-movements))

(defn sum-coordinates
  "Description was ambiguous:
   > distances are measured from the edge of the map to the closest edge of the box in question

   It made me think that distance was calculated from the left side of the map to the left side of the box,
   if it was in the left half, or from the right side of the map to the right side of the box. This is not
   the case though, it's always to the left side. That wasted some time. "
  [warehouse-map]
  (reduce-kv
   (fn [sum [x y] tile]
     (cond-> sum
       (contains? #{\O \[} tile) (+ x (* 100 y))))
   0 warehouse-map))

(defn part-1 []
  (-> (slurp "15/input.txt") init process-steps sum-coordinates))

(defn part-2 []
  (-> (slurp "15/input.txt") (init {:double-wide? true}) process-steps sum-coordinates))

(defn print-map
  "I got coordinates the wrong way around again, had to print to prove it ¬_¬"
  [warehouse-map]
  (let [x (apply max (map first (keys warehouse-map)))
        y (apply max (map second (keys warehouse-map)))]
    (doseq [y (range (inc y))]
      (prn (apply str (map #(get warehouse-map [% y]) (range (inc x))))))))

(comment
  (-> (init (slurp "15/input.txt"))
      #_(update  :robot-movements #(take 2 %))
      process-steps
      print-map))



;; Game! (ish)
;;
;; At the time I only printed the map step by step, but I had planted the idea of making the
;; debugging process interactive, so after the event I decided to actually sit down and do it.
;;
;; Pretty hacky, but doesn't disrupt the normal bb REPL wokrflow with extra deps
;; clj -Sdeps '{:deps {clojure-lanterna/clojure-lanterna {:mvn/version "0.9.7"}}}' -M 15/core.clj game!

(def arrow->coord
  {:up    [0 -1]
   :down  [0 1]
   :left  [-1 0]
   :right [1 0]})

(def fg-colours
  {\@ :black
   \# :red
   \. :white
   \O :green
   \[ :green
   \] :green})

(def bg-colours
  {\@ :white})

(defn move-robot [warehouse-map key-pressed]
  (if (contains? #{:up :down :left :right} key-pressed)
    (process-step warehouse-map (arrow->coord key-pressed))
    warehouse-map))

(when (= *command-line-args* ["game!"])
  (let [term          ((requiring-resolve 'lanterna.terminal/get-terminal) :text)

        file          (atom "15/input.txt")
        double-wide?  (atom true)
        auto-mode?    (atom false)
        instructions  ["Welcome to the Warehouse Simulator 2024!"
                       ""
                       "You can use arrows to move! Also:"
                       " - R - resets to initial state"
                       " - A - auto mode!"
                       " - D - toggles double-wide mode"
                       " - [1-3] - use samples 1, 2 or 3"
                       " - I - use main input"
                       " - Q - quit!"]
        init-state    #(do
                         ((requiring-resolve 'lanterna.terminal/clear) term)
                         ((requiring-resolve 'lanterna.terminal/set-fg-color) term :default)
                         ((requiring-resolve 'lanterna.terminal/put-string) term (str/join "\n" instructions))
                         (init (slurp @file) {:double-wide? @double-wide?}))
        draw-map      (fn [term previous-state current-state]
                        (let [intro-offset (inc (count instructions))
                              tiles-to-draw (second
                                             ((requiring-resolve 'clojure.data/diff) (:warehouse-map previous-state)
                                                                                     (:warehouse-map current-state)))]
                          (doseq [[[x y] c] (sort-by (comp (juxt first second) key) tiles-to-draw)]
                            ((requiring-resolve 'lanterna.terminal/set-fg-color) term (get fg-colours c))
                            ((requiring-resolve 'lanterna.terminal/set-bg-color) term (get bg-colours c :default))
                            ((requiring-resolve 'lanterna.terminal/put-character) term c x (+ y intro-offset)))))]

    ((requiring-resolve 'lanterna.terminal/start) term)
    (.setCursorVisible term false)

    (loop [previous-state nil
           current-state (init-state)]
      (draw-map term previous-state current-state)
      (if (and @auto-mode? (first (:robot-movements current-state)))
        (let [interrupt? ((requiring-resolve 'lanterna.terminal/get-key-blocking) term {:timeout 10})]
          (when interrupt?
            (reset! auto-mode? false))
          (recur current-state
                 (-> current-state
                     (update :warehouse-map process-step (first (:robot-movements current-state)))
                     (update :robot-movements rest))))
        (let [k ((requiring-resolve 'lanterna.terminal/get-key-blocking) term)]
          (reset! auto-mode? false)
          (case k
            \q ::quit
            \r (recur nil (init-state))
            \a (do (reset! auto-mode? true) (recur nil (init-state)))
            \d (do (swap! double-wide? not) (recur nil (init-state)))
            \1 (do (reset! file "15/sample-1.txt") (recur nil (init-state)))
            \2 (do (reset! file "15/sample-2.txt") (recur nil (init-state)))
            \3 (do (reset! file "15/sample-3.txt") (recur nil (init-state)))
            \i (do (reset! file "15/input.txt") (recur nil (init-state)))
            (recur current-state (update current-state :warehouse-map move-robot k))))))

    ((requiring-resolve 'lanterna.terminal/stop) term)))
