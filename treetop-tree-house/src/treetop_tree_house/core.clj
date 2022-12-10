(ns treetop-tree-house.core
  (:gen-class))

(defn spy [& args] (println "DEBUG:" args) args)

; All trees on outside are valid
; Each cell should contain the maximum up, down, left, right
; Where N is number of cells (N = W*H)
; This would take 4N operations and 4N memory
; Actually each of the checks don't need the information for the other.
; There isn't much value in skipping cells that are already visible because the calculation needs to happen for all trees

; trees is a sequence line of trees
; calculates if tree is visible from perspective of first
; first tree is visible
(defn calculate-visible [trees]
  (first (reduce (fn [[visible-trees max-height] tree]
             [(conj visible-trees (if (< max-height tree) true false)) (max max-height tree)])
           [[] -1]
           trees)))
(comment (calculate-visible [3 0 3 7 3]))

(comment (apply map vector [[1 2 3 4] [4 4 4 4] [9 8 7 6]]))
(comment (map vector [1 2 3 4] [4 4 4 4] [9 8 7 6]))

(comment (map list [[3 0 3 7 3] [2 5 5 1 2] [6 5 3 3 2] [3 3 5 4 9] [3 5 3 9 0]]))

; Will need to mark on a grid all trees which are visible
; input is a lazy seq which will become a real vector I can probably change how the sequence gets read for all four directions
; create grid for visible trees
(defn run-app [input]
  (let [grid (mapv (fn [line]
                     (mapv #(- (int %) (int \0)) line))
                   input)
        ; Could be more memory optimised but would have to manipulate coords
        west-first (->> grid (map calculate-visible) (mapv vec))
        east-first (->> grid (map reverse) (map calculate-visible) (map reverse) (mapv vec))
        north-first (->> grid (apply map list) (map calculate-visible) (apply map vector) (mapv vec))
        south-first (->> grid (apply map list) (map reverse) (map calculate-visible) (map reverse) (apply map vector) (mapv vec))]
    (->> [east-first west-first north-first south-first]
         (apply map (partial map #(some identity %&)))
         (flatten)
         (filter identity)
         (count))))
(comment (run-app ["30373"
                   "25512"
                   "65332"
                   "33549"
                   "35390"]))

; (defn -main []
;   (-> *in*
;       (java.io.BufferedReader.)
;       (line-seq)
;       (run-app)
;       (println)))

; looking towards first
; distance is either 1 or whatever the last one was +(some number of trees)
; because there are only 9 values we'll use a hashmap linking a height to the latest location
; distance is calculated
(defn calculate-scenic [trees]
  (first (reduce (fn [[visible-counts prev-heights idx] tree-height]
                   (let [dist (->> (range tree-height 10)
                                   (map prev-heights)
                                   (apply max)
                                   (- idx))]
                     [(conj visible-counts dist) (assoc prev-heights tree-height idx) (inc idx)]))
                 [[] (zipmap (range 10) (repeat 0)) 0]
                 trees)))
; should be [0 1 2 3 1]
(comment (calculate-scenic [3 0 3 7 3]))
; should be [0 1 1 1 2 5 1]
(comment (calculate-scenic [9 5 3 1 2 7 3]))

(defn run-app2 [input]
  (let [grid (mapv (fn [line]
                     (mapv #(- (int %) (int \0)) line))
                   input)
        ; Could be more memory optimised but would have to manipulate coords
        west-facing (->> grid (map calculate-scenic) (mapv vec))
        east-facing (->> grid (map reverse) (map calculate-scenic) (map reverse) (mapv vec))
        north-facing (->> grid (apply map list) (map calculate-scenic) (apply map vector) (mapv vec))
        south-facing (->> grid (apply map list) (map reverse) (map calculate-scenic) (map reverse) (apply map vector) (mapv vec))]
    (->> [east-facing west-facing north-facing south-facing]
         (apply map (partial map *))
         (flatten)
         (apply max))))
(comment (run-app2 ["30373"
                    "25512"
                    "65332"
                    "33549"
                    "35390"]))

(defn -main []
  (-> *in*
      (java.io.BufferedReader.)
      (line-seq)
      (run-app2)
      (println)))

