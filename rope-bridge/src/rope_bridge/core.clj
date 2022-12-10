(ns rope-bridge.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn spy [arg]
  (println "DEBUG:" arg)
  arg)

(defn parse-input [input]
  (reduce
    (fn [parsed-input line]
      (-> line
          (str/split #" " 2)
          ((fn [[direction digits]] [direction (Integer/parseInt digits)]))
          (#(conj parsed-input %))))
    []
    input))

(defn generate-coords [[x y] [direction number]]
  (case direction
    "U" (map #(vector x (+ y %)) (range 1 (inc number)))
    "D" (map #(vector x (- y %)) (range 1 (inc number)))
    "R" (map #(vector (+ x %) y) (range 1 (inc number)))
    "L" (map #(vector (- x %) y) (range 1 (inc number)))))
(comment (generate-coords [0 0] ["R" 4]))
(comment (generate-coords [0 0] ["U" 14]))

(defn shift-toward [diff val]
  ((cond (pos? diff) inc
         (neg? diff) dec
         :else identity)
   val))
(comment (shift-toward -2 3))
(comment (shift-toward 2 3))
(comment (shift-toward 0 3))

(defn move-tail [[head-x head-y] [tail-x tail-y]]
  (let [diff-x (- head-x tail-x)
        diff-y (- head-y tail-y)]
    (cond
      (and (< 1 (abs diff-x)) (< 0 (abs diff-y))) [(shift-toward diff-x tail-x) (shift-toward diff-y tail-y)]
      (and (< 0 (abs diff-x)) (< 1 (abs diff-y))) [(shift-toward diff-x tail-x) (shift-toward diff-y tail-y)]
      (< 1 (abs diff-x)) [(shift-toward diff-x tail-x) tail-y]
      (< 1 (abs diff-y)) [tail-x (shift-toward diff-y tail-y)]
      :else [tail-x tail-y])))

(defn run-app [input]
  (->> input
       (parse-input)
       ; function assumes prev-head and prev-tail are in equilibrium
       (reduce (fn [[prev-coords prev-head-coord prev-tail-coord] movement]
                 (reduce
                   ; prev-coords contains tail-coord
                   ; prev-head-coord just to keep parameters in line with outer reduce
                   (fn [[prev-coords prev-head-coord prev-tail-coord] head-coord]
                     (let [tail-coord (move-tail head-coord prev-tail-coord)]
                       [(conj prev-coords tail-coord) head-coord tail-coord]))
                   [prev-coords prev-head-coord prev-tail-coord]
                   (generate-coords prev-head-coord movement)))
               [#{[0 0]} [0 0] [0 0]])
       (first)
       (count)))
(comment
  (run-app
    ["R 4"
     "U 4"
     "L 3"
     "D 1"
     "R 4"
     "D 1"
     "L 5"
     "R 2"]))

; (defn -main []
;   (-> *in*
;       (java.io.BufferedReader.)
;       (line-seq)
;       (run-app)
;       (println)))

(defn follow-head [initial-tail head-coords]
  (->> head-coords
       (reduce
         (fn [tail-coords head-coord]
           (let [tail-coord (move-tail head-coord (peek tail-coords))]
             (conj tail-coords tail-coord)))
         [initial-tail])
       (rest)))

(rest [1 2 3 4 5])

; the head is always independent of their tail
; calculate the tails movement and use that for the new head movement of the next piece
; repeat 9 times
(defn run-app2 [input]
  (->> input
       (parse-input)
       (reduce (fn [movements line] (into movements (generate-coords (peek movements) line))) [[0 0]])
       ((apply comp (repeat 9 (partial follow-head [0 0]))))
       (set)
       (count)))

(comment
  (run-app2
    ["R 4"
     "U 4"
     "L 3"
     "D 1"
     "R 4"
     "D 1"
     "L 5"
     "R 2"]))

(comment
  (run-app2
    ["R 5"
     "U 8"
     "L 8"
     "D 3"
     "R 17"
     "D 10"
     "L 25"
     "U 20"]))

(defn -main []
  (-> *in*
      (java.io.BufferedReader.)
      (line-seq)
      (run-app2)
      (println)))
