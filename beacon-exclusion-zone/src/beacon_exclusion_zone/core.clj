(ns beacon-exclusion-zone.core
  (:require [clojure.set :as set])
  (:gen-class))

(defn parse-line [line]
  (->> line
       (re-seq #"-?\d+")
       (map parse-long)
       ((fn [[x1 y1 x2 y2]]
          {:sensor [x1 y1]
           :beacon [x2 y2]}))))

(comment
  (parse-line "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"))

(defn distance [[x1 y1] [x2 y2]] (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn build-system [input]
  (->> input
       (map parse-line)
       (mapv (fn [pair]
              (let [{sensor :sensor beacon :beacon} pair]
                (assoc pair :radius (distance sensor beacon)))))))

; a naive solution which checks every sensor and radius
; returns true if covered by a sensor
(defn covered-by [system location]
  (some #(if (>= (:radius %) (distance (:sensor %) location)) % nil) system))

(comment
  (covered-by
    '({:sensor [2 18], :beacon [-2 15], :radius 7}
      {:sensor [9 16], :beacon [10 16], :radius 1}
      {:sensor [13 2], :beacon [15 3], :radius 3}
      {:sensor [12 14], :beacon [10 16], :radius 4}
      {:sensor [10 20], :beacon [10 16], :radius 4}
      {:sensor [14 17], :beacon [10 16], :radius 5}
      {:sensor [8 7], :beacon [2 10], :radius 9}
      {:sensor [2 0], :beacon [2 10], :radius 10}
      {:sensor [0 11], :beacon [2 10], :radius 3}
      {:sensor [20 14], :beacon [25 17], :radius 8}
      {:sensor [17 20], :beacon [21 22], :radius 6}
      {:sensor [16 7], :beacon [15 3], :radius 5}
      {:sensor [14 3], :beacon [15 3], :radius 1}
      {:sensor [20 1], :beacon [15 3], :radius 7})
    [-1 10]))

(defn run-app [y input]
  (let [system (build-system input)
        objects (->> system (map #(hash-set (:sensor %) (:beacon %))) (apply set/union))
        left (->> system (map #(- (first (:sensor %)) (:radius %))) (apply min))
        right (->> system (map #(+ (first (:sensor %)) (:radius %))) (apply max))
        up (->> system (map #(- (second (:sensor %)) (:radius %))) (apply min))
        down (->> system (map #(+ (second (:sensor %)) (:radius %))) (apply max))]
    (->> (range left right)
         (map #(vector % y))
         (filter (partial (complement contains?) objects))
         (filter (partial covered-by system))
         (count))))

(comment
  (run-app
    10
    (list "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
          "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
          "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
          "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
          "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
          "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
          "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
          "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
          "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
          "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
          "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
          "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
          "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
          "Sensor at x=20, y=1: closest beacon is at x=15, y=3")))

; (defn -main []
;   (->> *in*
;        (java.io.BufferedReader.)
;        (line-seq)
;        (run-app 2000000)
;        (println)))

; create a manhattan distance circle
(defn perimeter [[center-x center-y] radius]
  ; plan is to go clockwise from [0 radius]
  (let [top-right (map #(vector % (- radius %)) (range radius))
        bottom-right (map #(vector (- radius %) (- %)) (range radius))
        bottom-left (map #(vector (- %) (- % radius)) (range radius))
        top-left (map #(vector (- % radius) %) (range radius))]
    (->> (concat top-right bottom-right bottom-left top-left)
         (map (fn [[x y]] [(+ center-x x) (+ center-y y)])))))

(comment (perimeter [1 1] 4))

(defn run-app2 [left right up down input]
  (let [system (build-system input)
        objects (->> system (map #(hash-set (:sensor %) (:beacon %))) (apply set/union))]
    (->> system
         (map #(perimeter (:sensor %) (inc (:radius %))))
         (apply concat)
         (distinct)
         (filter (fn [[x y]] (and (<= left x right) (<= up y down))))
         (filter (partial (complement covered-by) system))
         (first)
         ((fn [[x y]] (+ y (* x 4000000)))))))

(comment
  (run-app2
    0 20 0 20
    (list "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
          "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
          "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
          "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
          "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
          "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
          "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
          "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
          "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
          "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
          "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
          "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
          "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
          "Sensor at x=20, y=1: closest beacon is at x=15, y=3")))

(defn -main []
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (run-app2 0 4000000 0 4000000)
       (println)))
