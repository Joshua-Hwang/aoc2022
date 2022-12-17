(ns regolith-reservoir.core
  (:require [clojure.string :as str])
  (:gen-class))

(first [1 2])
(second [1 2])

(let [x [1 2 3 4]]
  (map vector x (rest x)))

; rock-locations is a set
(defn add-line [rock-locations [[x1 y1] [x2 y2]]]
  (if (= x1 x2)
    ; y changes
    (let [x x1]
      (->> (range (min y1 y2) (inc (max y1 y2)))
           (map vector (repeat x))
           (into rock-locations)))
    ; x changes
    (let [y y1]
      (->> (range (min x1 x2) (inc (max x1 x2)))
           (#(map vector % (repeat y)))
           (into rock-locations)))))

(comment
  (add-line
    #{} [[498 4] [498 6]]))

(defn parse-line [line]
  (->> (str/split line #" -> ")
       (map #(str/split % #","))
       (map (partial mapv parse-long))
       (#(map vector % (rest %)))))

(comment
  (parse-line "503,4 -> 502,4 -> 502,9 -> 494,9"))

(defn construct-rock-locations [input]
  (->> input
       (map parse-line)
       (apply concat)
       (reduce add-line #{})))

(comment
  (construct-rock-locations
    ["498,4 -> 498,6 -> 496,6"
     "503,4 -> 502,4 -> 502,9 -> 494,9"]))

(defn simulate-sand-grain [rock-locations [x y]]
  (cond
    ; sand falls
    (not (contains? rock-locations [x (inc y)]))
    (cons [x y] (lazy-seq (simulate-sand-grain rock-locations [x (inc y)])))
    ; down left
    (not (contains? rock-locations [(dec x) (inc y)]))
    (cons [x y] (lazy-seq (simulate-sand-grain rock-locations [(dec x) (inc y)])))
    ; down right
    (not (contains? rock-locations [(inc x) (inc y)]))
    (cons [x y] (lazy-seq (simulate-sand-grain rock-locations [(inc x) (inc y)])))
    ; stop
    :else (list [x y])))

(comment
  (simulate-sand-grain
    #{[502 9]
      [494 9]
      [496 9]
      [503 4]
      [497 6]
      [498 5]
      [502 7]
      [495 9]
      [496 6]
      [500 9]
      [501 9]
      [502 8]
      [502 6]
      [498 9]
      [502 5]
      [499 9]
      [498 6]
      [497 9]
      [502 4]
      [498 4]}
    [500 0]))

(defn simulate-sand-grains [rock-locations sand-source]
  (let [sand-path (simulate-sand-grain rock-locations sand-source)]
    ; the let means lazy sequences are only evaluated once. Last sand-path still takes
    ; linear time but doesn't compute a second time
    (concat [sand-path]
            (lazy-seq (simulate-sand-grains
                        (conj rock-locations (last sand-path))
                        sand-source)))))

(comment
  (take 20
        (simulate-sand-grains
          #{[502 9]
            [494 9]
            [496 9]
            [503 4]
            [497 6]
            [498 5]
            [502 7]
            [495 9]
            [496 6]
            [500 9]
            [501 9]
            [502 8]
            [502 6]
            [498 9]
            [502 5]
            [499 9]
            [498 6]
            [497 9]
            [502 4]
            [498 4]}
          [500 0])))

(defn run-app [input]
  (let [rock-locations (construct-rock-locations input)
        lowest-y (->> rock-locations
                      (map second)
                      (apply max))]
    (->> (simulate-sand-grains rock-locations [500 0])
         (take-while (partial every? (fn [[_ y]] (>= lowest-y y))))
         (count))))

(comment
  (run-app
    ["498,4 -> 498,6 -> 496,6"
     "503,4 -> 502,4 -> 502,9 -> 494,9"]))

; (defn -main []
;   (->> *in*
;        (java.io.BufferedReader.)
;        (line-seq)
;        (run-app)
;        (println)))

(defn simulate-sand-grains2 [rock-locations lowest-y sand-source]
  (let [sand-path (take-while
                    (fn [[_ y]] (> lowest-y y))
                    (simulate-sand-grain rock-locations sand-source))]
    ; the let means lazy sequences are only evaluated once. Last sand-path still takes
    ; linear time but doesn't compute a second time
    (concat [sand-path]
            (lazy-seq (simulate-sand-grains2
                        (conj rock-locations (last sand-path))
                        lowest-y
                        sand-source)))))

(comment
  (take 25
        (simulate-sand-grains2
          #{[502 9]
            [494 9]
            [496 9]
            [503 4]
            [497 6]
            [498 5]
            [502 7]
            [495 9]
            [496 6]
            [500 9]
            [501 9]
            [502 8]
            [502 6]
            [498 9]
            [502 5]
            [499 9]
            [498 6]
            [497 9]
            [502 4]
            [498 4]}
          11
          [500 0])))

(defn run-app2 [input]
  (let [rock-locations (construct-rock-locations input)
        lowest-y (->> rock-locations
                      (map second)
                      (apply max)
                      (+ 2))]
    (->> (simulate-sand-grains2 rock-locations lowest-y [500 0])
         (take-while (fn [sand-path] (< 1 (count sand-path))))
         (count)
         (inc))))

(comment
  (run-app2
    ["498,4 -> 498,6 -> 496,6"
     "503,4 -> 502,4 -> 502,9 -> 494,9"]))

(defn render-board [rock-locations]
  (let [left (dec (->> rock-locations (map first) (apply min)))
        right (inc (->> rock-locations (map first) (apply max)))
        up (dec (->> rock-locations (map second) (apply min)))
        down (inc (->> rock-locations (map second) (apply max)))]
    (str/join
      \newline
      (map
        (fn [y]
          (str/join
            (map
              (fn [x] (if (contains? rock-locations [x y]) \# \.))
              (range left right))))
        (range up down)))))

(comment
  (render-board
    #{[502 9]
      [494 9]
      [496 9]
      [503 4]
      [497 6]
      [498 5]
      [502 7]
      [495 9]
      [496 6]
      [500 9]
      [501 9]
      [502 8]
      [502 6]
      [498 9]
      [502 5]
      [499 9]
      [498 6]
      [497 9]
      [502 4]
      [498 4]}))

; (defn -main []
;   (->> *in*
;        (java.io.BufferedReader.)
;        (line-seq)
;        (run-app2)
;        (println)))

(defn -main []
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (construct-rock-locations)
       (render-board)
       (println)))
