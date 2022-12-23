(ns monkey-map.core
  (:require [clojure.string :as str])
  (:gen-class))

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

(defn parse-movement [input]
  ; number letter pairs
  ; do something then alternate row/column movement
  (let [steps (map parse-long (re-seq #"\d+" input))
        rotations (re-seq #"[LR]" input)]
    ; I messed up and the final movement is not considered
    (map vector (concat steps [0]) (concat rotations ["R" "L"]))))

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
      "        ...#...."
      "        .....#.."
      "        .#......"
      "        ......#."
      ""
      "10R5L5R10L4R5L5")))

(defn get-m-row [m row] (m row))
(defn get-m-col [m col] (map #(get % col) m))

(comment
  (get-m-col [[nil nil nil nil nil nil nil nil nil \. \. \#]
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
              [nil nil nil nil nil nil nil nil \. \. \. \. \. \. \# \.]]
             3))

(defn rotate [direction rotation]
  (case direction
    :right (case rotation "R" :down "L" :up)
    :down (case rotation "R" :left "L" :right)
    :left (case rotation "R" :up "L" :down)
    :up (case rotation "R" :right "L" :left)))

(defn step-right [m [row col :as old-location]]
  (let [new-location [row (inc col)]
        new-location (if (get-in m new-location) new-location
                       (->> (range (inc col))
                            (map vector (repeat row))
                            (drop-while #(nil? (get-in m %)))
                            (first)))]
    (case (get-in m new-location)
      \. [new-location :right]
      \# [old-location :right])))

(comment
  (step-right [[nil nil nil nil nil nil nil nil nil \. \. \#]
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
               [nil nil nil nil nil nil nil nil \. \. \. \. \. \. \# \.]]
             [0 9]))
(comment
  (step-right [[nil nil nil nil nil nil nil nil nil \. \. \#]
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
               [nil nil nil nil nil nil nil nil \. \. \. \. \. \. \# \.]]
             [1 11]))
(comment
  (step-right [[nil nil nil nil nil nil nil nil nil \. \. \#]
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
               [nil nil nil nil nil nil nil nil \. \. \. \. \. \. \# \.]]
             [2 11]))

(defn step-down [m [row col :as old-location]]
  (let [new-location [(inc row) col]
        new-location (if (get-in m new-location) new-location
                       (->> (range (inc row))
                            (#(map vector % (repeat col)))
                            (drop-while #(nil? (get-in m %)))
                            (first)))]
    (case (get-in m new-location)
      \. [new-location :down]
      \# [old-location :down])))

(defn step-left [m [row col :as old-location]]
  (let [new-location [row (dec col)]
        new-location (if (get-in m new-location) new-location
                       (->> (range (count (get-m-row m row)) (dec col) -1)
                            (map vector (repeat row))
                            (drop-while #(nil? (get-in m %)))
                            (first)))]
    (case (get-in m new-location)
      \. [new-location :left]
      \# [old-location :left])))

(defn step-up [m [row col :as old-location]]
  (let [new-location [(dec row) col]
        new-location (if (get-in m new-location) new-location
                       (->> (range (count (get-m-col m col)) (dec row) -1)
                            (#(map vector % (repeat col)))
                            (drop-while #(nil? (get-in m %)))
                            (first)))]
    (case (get-in m new-location)
      \. [new-location :up]
      \# [old-location :up])))

(defn walk
  "Returns a lazy sequence of location and direction.
  Walks until out of steps"
  [m location direction steps]
  (if (zero? steps) (cons [location direction] nil)
    (let [[new-location new-direction] ((case direction
                                          :right step-right
                                          :down step-down
                                          :left step-left
                                          :up step-up) m location)]
      (cons [new-location new-direction]
            ; try to move forward. If blocked return early
            (if (= location new-location) nil
              (lazy-seq (walk m new-location new-direction (dec steps))))))))

(defn walks
  "Moves must be [steps new-direction]
  Returns lazy seq of new location and new direction (not the one we were walking along)"
  [m location direction moves]
  (if (empty? moves) nil
    (let [[steps rotation] (first moves)
          [new-location new-direction] (last (walk m location direction steps))
          new-direction (rotate new-direction rotation)]
      (cons [new-location new-direction] (lazy-seq (walks m new-location new-direction (rest moves)))))))

(comment
  (walk [[nil nil nil nil nil nil nil nil nil \. \. \#]
         [nil nil nil nil nil nil nil nil nil \# \. \.]
         [nil nil nil nil nil nil nil nil nil \. \. \.]
         [nil nil nil nil nil nil nil nil nil \. \. \.]
         [\.  \.  \.  \#  \.  \.  \.  \.  \.  \. \. \#]
         [\.  \.  \.  \.  \.  \.  \.  \.  \#  \. \. \.]
         [\.  \.  \#  \.  \.  \.  \.  \#  \.  \. \. \.]
         [\.  \.  \.  \.  \.  \.  \.  \.  \.  \. \# \.]
         [nil nil nil nil nil nil nil nil \.  \. \. \# \. \. \. \.]
         [nil nil nil nil nil nil nil nil \.  \. \. \. \. \# \. \.]
         [nil nil nil nil nil nil nil nil \.  \# \. \. \. \. \. \.]
         [nil nil nil nil nil nil nil nil \.  \. \. \. \. \. \# \.]]
        [5 10] :right 10))

(defn run-app [input]
  (let [[m moves] (parse-input input)
        ; moves now with directions instead
        initial-location [0 (->> (get-m-row m 0) (take-while nil?) (count))]
        [[final-row final-col] final-direction] (last (walks m initial-location :right moves))]
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

; (defn -main []
;   (->> *in*
;        (java.io.BufferedReader.)
;        (line-seq)
;        (run-app)
;        (println)))

(defn step-right2 [m [row col :as old-location]]
  (let [new-location [row (inc col)]
        new-direction :right
        [new-location new-direction] (if (get-in m new-location)
                                       [new-location new-direction]
                                       ; this is hardcoded for the sample

                                       ; (let [offset (mod row 4)]
                                       ;   (case (quot row 4)
                                       ;     0 (let [row (- 11 offset)]
                                       ;         [(->> (range (count (get-m-row m row)) 0 -1)
                                       ;               (#(map vector (repeat row) %))
                                       ;               (drop-while #(nil? (get-in m %)))
                                       ;               (first)) :left])
                                       ;     1 (let [col (- 15 offset)]
                                       ;         [(->> (range (count (get-m-col m col)))
                                       ;               (#(map vector % (repeat col)))
                                       ;               (drop-while #(nil? (get-in m %)))
                                       ;               (first)) :down])
                                       ;     2 (let [row (- 3 offset)]
                                       ;         [(->> (range (count (get-m-row m row)) 0 -1)
                                       ;               (#(map vector (repeat row) %))
                                       ;               (drop-while #(nil? (get-in m %)))
                                       ;               (first)) :left])))

                                       (let [offset (mod row 50)]
                                         (case (quot row 50)
                                           0 (let [row (- 149 offset)]
                                               [(->> (range (count (get-m-row m row)) 0 -1)
                                                     (#(map vector (repeat row) %))
                                                     (drop-while #(nil? (get-in m %)))
                                                     (first)) :left])
                                           1 (let [col (+ 100 offset)]
                                               [(->> (range (count (get-m-col m col)) 0 -1)
                                                     (#(map vector % (repeat col)))
                                                     (drop-while #(nil? (get-in m %)))
                                                     (first)) :up])
                                           2 (let [row (- 49 offset)]
                                               [(->> (range (count (get-m-row m row)) 0 -1)
                                                     (#(map vector (repeat row) %))
                                                     (drop-while #(nil? (get-in m %)))
                                                     (first)) :left])
                                           3 (let [col (+ 50 offset)]
                                               [(->> (range (count (get-m-col m col)) 0 -1)
                                                     (#(map vector % (repeat col)))
                                                     (drop-while #(nil? (get-in m %)))
                                                     (first)) :up])))

                                       )]
    (case (get-in m new-location)
      \. [new-location new-direction]
      \# [old-location :right])))

(comment
  (step-right2 [[nil nil nil nil nil nil nil nil nil \. \. \#]
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
                [nil nil nil nil nil nil nil nil \. \. \. \. \. \. \# \.]]
               [5 11]))

(defn step-down2 [m [row col :as old-location]]
  (let [new-location [(inc row) col]
        new-direction :down
        [new-location new-direction] (if (get-in m new-location)
                                       [new-location new-direction]
                                       ; this is hardcoded for the sample

                                       ; (let [offset (mod col 4)]
                                       ;   (case (quot col 4)
                                       ;     0 (let [col (- 11 offset)]
                                       ;         [(->> (range (count (get-m-row m col)) 0 -1)
                                       ;               (#(map vector % (repeat col)))
                                       ;               (drop-while #(nil? (get-in m %)))
                                       ;               (first)) :up])
                                       ;     1 (let [row (- 11 offset)]
                                       ;         [(->> (range (count (get-m-col m row)))
                                       ;               (#(map vector (repeat row) %))
                                       ;               (drop-while #(nil? (get-in m %)))
                                       ;               (first)) :right])
                                       ;     2 (let [col (- 3 offset)]
                                       ;         [(->> (range (count (get-m-row m col)) 0 -1)
                                       ;               (#(map vector % (repeat col)))
                                       ;               (drop-while #(nil? (get-in m %)))
                                       ;               (first)) :up])
                                       ;     3 (let [row (- 7 offset)]
                                       ;         [(->> (range (count (get-m-row m row)))
                                       ;               (#(map vector (repeat row) %))
                                       ;               (drop-while #(nil? (get-in m %)))
                                       ;               (first)) :right])))

                                       (let [offset (mod col 50)]
                                         (case (quot col 50)
                                           0 (let [col (+ 100 offset)]
                                               [(->> (range (count (get-m-row m col)))
                                                     (#(map vector % (repeat col)))
                                                     (drop-while #(nil? (get-in m %)))
                                                     (first)) :down])
                                           1 (let [row (+ 150 offset)]
                                               [(->> (range (count (get-m-col m row)) 0 -1)
                                                     (#(map vector (repeat row) %))
                                                     (drop-while #(nil? (get-in m %)))
                                                     (first)) :left])
                                           2 (let [row (+ 50 offset)]
                                               [(->> (range (count (get-m-col m row)) 0 -1)
                                                     (#(map vector (repeat row) %))
                                                     (drop-while #(nil? (get-in m %)))
                                                     (first)) :left])))

                                       )]
    (case (get-in m new-location)
      \. [new-location new-direction]
      \# [old-location :down])))

(defn step-left2 [m [row col :as old-location]]
  (let [new-location [row (dec col)]
        new-direction :left
        [new-location new-direction] (if (get-in m new-location)
                                       [new-location new-direction]
                                       ; this is hardcoded for the sample

                                       ; (let [offset (mod row 4)]
                                       ;   (case (quot row 4)
                                       ;     0 (let [col (+ 4 offset)]
                                       ;         [(->> (range (count (get-m-col m col)))
                                       ;               (#(map vector % (repeat col)))
                                       ;               (drop-while #(nil? (get-in m %)))
                                       ;               (first)) :down])
                                       ;     1 (let [col (- 15 offset)]
                                       ;         [(->> (range (count (get-m-col m col)) 0 -1)
                                       ;               (#(map vector % (repeat col)))
                                       ;               (drop-while #(nil? (get-in m %)))
                                       ;               (first)) :up])
                                       ;     2 (let [col (- 7 offset)]
                                       ;         [(->> (range (count (get-m-col m col)) 0 -1)
                                       ;               (#(map vector % (repeat col)))
                                       ;               (drop-while #(nil? (get-in m %)))
                                       ;               (first)) :up])))

                                       (let [offset (mod row 50)]
                                         (case (quot row 50)
                                           0 (let [row (- 149 offset)]
                                               [(->> (range (count (get-m-row m row)))
                                                     (#(map vector (repeat row) %))
                                                     (drop-while #(nil? (get-in m %)))
                                                     (first)) :right])
                                           1 (let [col (+ 0 offset)]
                                               [(->> (range (count (get-m-col m col)))
                                                     (#(map vector % (repeat col)))
                                                     (drop-while #(nil? (get-in m %)))
                                                     (first)) :down])
                                           2 (let [row (- 49 offset)]
                                               [(->> (range (count (get-m-row m row)))
                                                     (#(map vector (repeat row) %))
                                                     (drop-while #(nil? (get-in m %)))
                                                     (first)) :right])
                                           3 (let [col (+ 50 offset)]
                                               [(->> (range (count (get-m-col m col)))
                                                     (#(map vector % (repeat col)))
                                                     (drop-while #(nil? (get-in m %)))
                                                     (first)) :down])))

                                       )]
    (case (get-in m new-location)
      \. [new-location new-direction]
      \# [old-location :left])))

(defn step-up2 [m [row col :as old-location]]
  (let [new-location [(dec row) col]
        new-direction :up
        [new-location new-direction] (if (get-in m new-location)
                                       [new-location new-direction]
                                       ; this is hardcoded for the sample

                                       ; (let [offset (mod col 4)]
                                       ;   (case (quot col 4)
                                       ;     0 (let [col (- 11 offset)]
                                       ;         [(->> (range (count (get-m-row m col)))
                                       ;               (#(map vector % (repeat col)))
                                       ;               (drop-while #(nil? (get-in m %)))
                                       ;               (first)) :down])
                                       ;     1 (let [row (+ 0 offset)]
                                       ;         [(->> (range (count (get-m-col m row)))
                                       ;               (#(map vector (repeat row) %))
                                       ;               (drop-while #(nil? (get-in m %)))
                                       ;               (first)) :right])
                                       ;     2 (let [col (- 3 offset)]
                                       ;         [(->> (range (count (get-m-row m col)))
                                       ;               (#(map vector % (repeat col)))
                                       ;               (drop-while #(nil? (get-in m %)))
                                       ;               (first)) :down])
                                       ;     3 (let [row (- 7 offset)]
                                       ;         [(->> (range (count (get-m-row m row)))
                                       ;               (#(map vector (repeat row) %))
                                       ;               (drop-while #(nil? (get-in m %)))
                                       ;               (first)) :left])))

                                       (let [offset (mod col 50)]
                                         (case (quot col 50)
                                           0 (let [row (+ 50 offset)]
                                               [(->> (range (count (get-m-col m row)))
                                                     (#(map vector (repeat row) %))
                                                     (drop-while #(nil? (get-in m %)))
                                                     (first)) :right])
                                           1 (let [row (+ 150 offset)]
                                               [(->> (range (count (get-m-col m row)))
                                                     (#(map vector (repeat row) %))
                                                     (drop-while #(nil? (get-in m %)))
                                                     (first)) :right])
                                           2 (let [col (+ 0 offset)]
                                               [(->> (range (count (get-m-col m col)) 0 -1)
                                                     (#(map vector % (repeat col)))
                                                     (drop-while #(nil? (get-in m %)))
                                                     (first)) :up])))

                                       )]
    (case (get-in m new-location)
      \. [new-location new-direction]
      \# [old-location :up])))

(defn walk2
  "Returns a lazy sequence of location and direction.
  Walks until out of steps or hits a wall"
  [m location direction steps]
  (if (zero? steps) (cons [location direction] nil)
    (let [[new-location new-direction]
          ((case direction
             :right step-right2
             :down step-down2
             :left step-left2
             :up step-up2) m location)]
      (cons [new-location new-direction]
            ; try to move forward. If blocked return early
            (if (= location new-location) nil
              (lazy-seq (walk2 m new-location new-direction (dec steps))))))))

(defn walks2
  "Moves must be [steps new-direction]
  Returns lazy seq of new location and new direction (not the one we were walking along)"
  [m location direction moves]
  (if (empty? moves) nil
    (let [[steps rotation] (first moves)
          [new-location new-direction] (last (walk2 m location direction steps))
          new-direction (rotate new-direction rotation)]
      (cons [new-location new-direction] (lazy-seq (walks2 m new-location new-direction (rest moves)))))))

(comment
  (walk2 [[nil nil nil nil nil nil nil nil nil \. \. \#]
          [nil nil nil nil nil nil nil nil nil \# \. \.]
          [nil nil nil nil nil nil nil nil nil \. \. \.]
          [nil nil nil nil nil nil nil nil nil \. \. \.]
          [\.  \.  \.  \#  \.  \.  \.  \.  \.  \. \. \#]
          [\.  \.  \.  \.  \.  \.  \.  \.  \#  \. \. \.]
          [\.  \.  \#  \.  \.  \.  \.  \#  \.  \. \. \.]
          [\.  \.  \.  \.  \.  \.  \.  \.  \.  \. \# \.]
          [nil nil nil nil nil nil nil nil \.  \. \. \# \. \. \. \.]
          [nil nil nil nil nil nil nil nil \.  \. \. \. \. \# \. \.]
          [nil nil nil nil nil nil nil nil \.  \# \. \. \. \. \. \.]
          [nil nil nil nil nil nil nil nil \.  \. \. \. \. \. \# \.]]
         [5 10] :right 10))

(defn run-app2 [input]
  (let [[m moves] (parse-input input)
        ; moves now with directions instead
        initial-location [0 (->> (get-m-row m 0) (take-while nil?) (count))]
        path (walks2 m initial-location :right moves)
        [[final-row final-col] final-direction] (last path)]
    (+ (* 1000 (inc final-row)) (* 4 (inc final-col)) (case final-direction :right 0 :down 1 :left 2 :up 3))))

(comment
  (run-app2
    (list
      "        ...#"
      "        .#.."
      "        #..."
      "        ...."
      "...#.......#"
      "........#..."
      "..#....#...."
      "..........#."
      "        ...#...."
      "        .....#.."
      "        .#......"
      "        ......#."
      ""
      "10R5L5R10L4R5L5")))

(defn -main []
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (run-app2)
       (println)))
