(ns monkey-map.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn r [[row _]] row)
(defn c [[_ col]] col)

; need to create data structure which can query the whole row or whole column
; the way the sample (and my input) map are set up. There are no "gaps" between parts of the map

(defn parse-map-line [line]
  (mapv #(case % \space nil %) line))

(comment (parse-map-line "        ..#."))

(defn parse-map [input]
  (mapv parse-map-line input))

(comment
  (parse-map
    (list "         ..#"
          "         #.."
          "         ..."
          "         ..."
          "...#.......#"
          "........#..."
          "..#....#...."
          "..........#."
          "        ...#...."
          "        .....#.."
          "        .#......"
          "......#.")))

(def get-row
  (memoize
    (fn [m row]
      (let [map-row (get m row)
            col (count (take-while (partial = nil) map-row))]
        [[row col] (filterv (partial not= nil) map-row)]))))

(comment
  (get-row
    [[nil nil nil nil \. \. \# nil nil nil nil nil]
     [nil nil nil nil \# \. \. nil nil nil nil nil]
     [nil nil nil nil \. \. \. nil nil nil nil nil]
     [nil nil nil nil \. \. \. nil nil nil nil nil]
     [\. \. \. \# \. \. \. \. \. \. \. \#]
     [\. \. \. \. \. \. \. \. \# \. \. \.]
     [\. \. \# \. \. \. \. \# \. \. \. \.]
     [\. \. \. \. \. \. \. \. \. \. \# \.]
     [nil nil nil nil nil nil nil nil \. \. \. \# \. \. \. \.]
     [nil nil nil nil nil nil nil nil \. \. \. \. \. \# \. \.]
     [nil nil nil nil nil nil nil nil \. \# \. \. \. \. \. \.]
     [\. \. \. \. \. \. \# \.]]
    2))

(def get-col
  (memoize
    (fn [m col]
      (let [map-col (mapv #(get % col nil) m)
            row (count (take-while (partial = nil) map-col))]
        [[row col] (filterv (partial not= nil) map-col)]))))

(comment
  (get-col
    [[nil nil nil nil nil nil nil nil nil \. \. \#]
     [nil nil nil nil nil nil nil nil nil \# \. \.]
     [nil nil nil nil nil nil nil nil nil \. \. \.]
     [nil nil nil nil nil nil nil nil nil \. \. \.]
     [\. \. \. \# \. \. \. \. \. \. \. \#]
     [\. \. \. \. \. \. \. \. \# \. \. \.]
     [\. \. \# \. \. \. \. \# \. \. \. \.]
     [\. \. \. \. \. \. \. \. \. \. \# \.]
     [nil nil nil nil nil nil nil nil \. \. \. \# \. \. \. \.]
     [nil nil nil nil nil nil nil nil \. \. \. \. \. \# \. \.]
     [nil nil nil nil nil nil nil nil \. \# \. \. \. \. \. \.]
     [\. \. \. \. \. \. \# \.]]
    3))
(comment
  (get-col
    [[nil nil nil nil nil nil nil nil nil \. \. \#]
     [nil nil nil nil nil nil nil nil nil \# \. \.]
     [nil nil nil nil nil nil nil nil nil \. \. \.]
     [nil nil nil nil nil nil nil nil nil \. \. \.]
     [\. \. \. \# \. \. \. \. \. \. \. \#]
     [\. \. \. \. \. \. \. \. \# \. \. \.]
     [\. \. \# \. \. \. \. \# \. \. \. \.]
     [\. \. \. \. \. \. \. \. \. \. \# \.]
     [nil nil nil nil nil nil nil nil \. \. \. \# \. \. \. \.]
     [nil nil nil nil nil nil nil nil \. \. \. \. \. \# \. \.]
     [nil nil nil nil nil nil nil nil \. \# \. \. \. \. \. \.]
     [\. \. \. \. \. \. \# \.]]
    13))

(defn parse-movement [input]
  ; number letter pairs
  ; do something then alternate row/column movement
  (let [steps (map parse-long (re-seq #"\d+" input))
        rotations (re-seq #"[LR]" input)]
    (map vector steps rotations)))

(comment
  (parse-movement "10R5L5R10L4R5L5"))

(defn parse-input [input]
  (let [[input-map _ input-movement] (partition-by str/blank? input)
        m (parse-map input-map)
        moves (parse-movement (first input-movement))]
    [m moves]))

(comment
  (parse-input
    (list
      "        ...#"
      "        .#.."
      "        #..."
      "        ...."
      "...#.......#"
      "........#..."
      "..#....#...."
      "..........#."
      "       ...#...."
      "       .....#.."
      "       .#......"
      "       ......#."
      ""
      "10R5L5R10L4R5L5")))

(defn walk [line steps idx]
  (if (neg? steps)
    ; reverse line
    ; reverse idx (mod (- idx) (count line))
    ; take steps as if normal
    ; unreverse
    ; unreverse idx
    (let [reversed-line (reverse line)
          reversed-idx (mod (- (inc idx)) (count line))
          new-reversed-idx (walk reversed-line (abs steps) reversed-idx)
          new-idx (mod (- (inc new-reversed-idx)) (count line))]
      new-idx)
    (->> line
         (cycle)
         (drop idx)
         ; drop starting point
         (drop 1)
         ; first is now starting point
         (take steps)
         (take-while (partial not= \#))
         (count)
         ; return new idx
         ; will need to loop
         (#(mod (+ idx %) (count line))))))

(comment (walk (vector \. \# \. \.) 0 2))
(comment (walk (vector \. \# \. \.) 1 2))
(comment (walk (vector \. \# \. \.) 2 2))
(comment (walk (vector \. \# \. \.) 3 2))
(comment (walk (vector \. \# \. \.) 10 2))
; length 4: 0 -> 3 = -1, 1 -> 2 = -2
; length 3: 0 -> 2 = -1, 1 -> 1 = -2
(comment (walk (vector \. \# \. \.) -1 2))
(comment (walk (vector \. \# \. \.) -2 2))
(comment (walk (vector \. \# \. \.) -3 2))
(comment (walk (vector \. \# \. \.) -10 2))
(comment (walk (vector \. \. \. \.) 1 2))
(comment (walk (vector \. \. \. \.) 2 2))
(comment (walk (vector \. \. \. \.) 10 2))
(comment (walk (vector \. \. \. \.) -1 2))
(comment (walk (vector \. \. \. \.) -2 2))
(comment (walk (vector \. \. \. \.) -10 2))

(defn rotate [direction rotation]
  (case direction
    :right (case rotation "R" :down "L" :up)
    :down (case rotation "R" :left "L" :right)
    :left (case rotation "R" :up "L" :down)
    :up (case rotation "R" :right "L" :left)))

(defn rotations->directions [prev-direction rotations]
  (if (empty? rotations)
    nil
    (let [rotation (first rotations)
          direction (rotate prev-direction rotation)]
      (cons direction (lazy-seq (rotations->directions direction (rest rotations)))))))

(comment (rotations->directions :right ["R"]))
(comment (rotations->directions :right ["L"]))
(comment (rotations->directions :right ["R" "R" "L" "R"]))

(defn walk-turn
  "Each move is of the form [steps new-direction]
  new-direction is after taking steps.
  Returns lazy sequence of [new-direction [coords]]"
  [m direction [row col] moves]
  (if (empty? moves)
    nil
    (let [[steps new-direction] (first moves)
          [[offset-row offset-col] line] (case direction
                                           :right (get-row m row)
                                           :down (get-col m col)
                                           :left (get-row m row)
                                           :up (get-col m col))
          ; looped together just cuz
          [idx steps] (case direction
                        :right [(- col offset-col) steps]
                        :down [(- row offset-row) steps]
                        :left [(- col offset-col) (- steps)]
                        :up [(- row offset-row) (- steps)])
          new-idx (walk line steps idx)
          new-location (case direction
                         :right [row (+ new-idx offset-col)]
                         :down [(+ new-idx offset-row) col]
                         :left [row (+ new-idx offset-col)]
                         :up [(+ new-idx offset-row) col])]
      (cons [new-direction new-location] (lazy-seq (walk-turn m new-direction new-location (rest moves)))))))

(comment
  (walk-turn
    [[nil nil nil nil nil nil nil nil nil \. \. \#]
     [nil nil nil nil nil nil nil nil nil \# \. \.]
     [nil nil nil nil nil nil nil nil nil \. \. \.]
     [nil nil nil nil nil nil nil nil nil \. \. \.]
     [\. \. \. \# \. \. \. \. \. \. \. \#]
     [\. \. \. \. \. \. \. \. \# \. \. \.]
     [\. \. \# \. \. \. \. \# \. \. \. \.]
     [\. \. \. \. \. \. \. \. \. \. \# \.]
     [nil nil nil nil nil nil nil nil \. \. \. \# \. \. \. \.]
     [nil nil nil nil nil nil nil nil \. \. \. \. \. \# \. \.]
     [nil nil nil nil nil nil nil nil \. \# \. \. \. \. \. \.]
     [\. \. \. \. \. \. \# \.]]
    :right [0 9] (list [10 :down])))
(comment
  (walk-turn
    [[nil nil nil nil nil nil nil nil nil \. \. \#]
     [nil nil nil nil nil nil nil nil nil \# \. \.]
     [nil nil nil nil nil nil nil nil nil \. \. \.]
     [nil nil nil nil nil nil nil nil nil \. \. \.]
     [\. \. \. \# \. \. \. \. \. \. \. \#]
     [\. \. \. \. \. \. \. \. \# \. \. \.]
     [\. \. \# \. \. \. \. \# \. \. \. \.]
     [\. \. \. \. \. \. \. \. \. \. \# \.]
     [nil nil nil nil nil nil nil nil \. \. \. \# \. \. \. \.]
     [nil nil nil nil nil nil nil nil \. \. \. \. \. \# \. \.]
     [nil nil nil nil nil nil nil nil \. \# \. \. \. \. \. \.]
     [\. \. \. \. \. \. \# \.]]
    :right [0 9] (list [10 :down] [2 :left] [2 :up])))

(defn run-app [input]
  (let [[m moves] (parse-input input)
        directions (rotations->directions :right (map second moves))
        ; moves now with directions instead
        moves (map (fn [[steps _] direction] [steps direction]) moves directions)
        [initial-location _] (get-row m 0)
        [final-direction [final-row final-col]] (last (walk-turn m :right initial-location moves))]
    (+ (* 1000 (inc final-row)) (* 4 (inc final-col)) (case final-direction :right 0 :down 1 :left 2 :up 3))))

(comment
  (run-app
    (list
      "        ...#"
      "        .#.."
      "        #..."
      "        ...."
      "...#.......#"
      "........#..."
      "..#....#...."
      "..........#."
      "         ...#...."
      "         .....#.."
      "         .#......"
      "         ......#."
      ""
      "10R5L5R10L4R5L5")))

(defn -main []
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (run-app)
       (println)))
