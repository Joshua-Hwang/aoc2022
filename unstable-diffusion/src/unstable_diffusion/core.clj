(ns unstable-diffusion.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn parse-line
  "Returns a list of indices where # is present"
  [line]
  (->> line
       (map-indexed vector)
       (filter #(= \# (second %)))
       (map first)))

(defn parse-input [input]
  (->> input
       (map parse-line)
       (mapcat (fn [row ls] (map #(vector row %) ls)) (iterate inc 0))
       (set)))

(comment
  (parse-input 
    (list
      "....."
      "..##."
      "..#.."
      "....."
      "..##."
      ".....")))

(defn propose-move
  "cardinal-priority should contain :north :south :west :east"
  [system cardinal-priority [row col :as location]]
  ; stay where you are
  (if (empty? cardinal-priority) location
    (if (empty? (filter system (for [row-op [identity dec inc] col-op [identity dec inc]
                                     :when (not= identity row-op col-op)]
                                 [(row-op row) (col-op col)])))
      location
      (let [cardinal (first cardinal-priority)
            to-check (apply map #(vector (%1 row) (%2 col))
                            (case cardinal
                              :north [(repeat dec) [identity dec inc]]
                              :south [(repeat inc) [identity dec inc]]
                              :west [[identity dec inc] (repeat dec)]
                              :east [[identity dec inc] (repeat inc)]))
            blockers (filter system to-check)]
        (if (seq blockers)
          (recur system (rest cardinal-priority) location)
          (first to-check))))))

(comment
  (propose-move #{[2 5]} [:north :east :south] [3 4]))

(defn cycle-queue [q]
  (conj (pop q) (peek q)))

(defn diffuse'
  "cardinal-priority needs to be queue"
  [system cardinal-priority]
  ; store proposals in map. Key is proposed location and value is location of proposee
  ; When conflict occurs all elves proposing get reassigned to existing locations
  ; Will this cause further bumping? No because elves only propose places that don't already have elves
  (let [proposals (group-by (partial propose-move system cardinal-priority) system)
        conflicting (->> proposals (filter #(< 1 (count (second %)))) (mapcat second))
        good-proposals (->> proposals (filter #(= 1 (count (second %)))) (map first))]
    (cons system (lazy-seq (diffuse'
                             (set (concat conflicting good-proposals))
                             (cycle-queue cardinal-priority))))))
(map first (filter #(= 1 (count (second %))) {[3 3] [[4 3]], [3 2] [[2 2] [4 2]], [0 3] [[1 3]], [0 2] [[1 2]]}))
(defn diffuse [system]
  (diffuse' system (conj clojure.lang.PersistentQueue/EMPTY :north :south :west :east)))

(comment (take 4 (diffuse #{[1 2] [1 3] [2 2] [4 2] [4 3]})))

(defn get-dimensions
  "Returns [top bottom left right]"
  [system]
  (let [left (apply min (map second system))
        right (inc (apply max (map second system)))
        top (apply min (map first system))
        bottom (inc (apply max (map first system)))]
    [top bottom left right]))

(defn render-system [system]
  (let [[top bottom left right] (get-dimensions system)]
    (str/join \newline
              (map
                (fn [row]
                  (str/join
                    (map
                      (fn [col]
                        (if (system [row col]) \# \.))
                      (range left right))))
                (range top bottom)))))

(comment (println (render-system #{[1 2] [1 3] [2 2] [4 2] [4 3]})))

(defn run-app [input]
  (let [system (parse-input input)]
    (-> system
         (diffuse)
         (nth 10)
         (get-dimensions)
         ((fn [[top bottom left right]] (- (* (- bottom top) (- right left)) (count system)))))))

; (defn -main []
;   (->> *in*
;        (java.io.BufferedReader.)
;        (line-seq)
;        (run-app)
;        (println)))

(defn take-till-duplicate' [prev coll]
  (if (= prev (first coll)) (list prev)
    (cons prev (lazy-seq (take-till-duplicate' (first coll) (rest coll))))))

(defn take-till-duplicate [coll]
  (take-till-duplicate' (first coll) (rest coll)))

(defn run-app2 [input]
  (let [system (parse-input input)]
    (-> system
         (diffuse)
         (take-till-duplicate)
         (count))))

(defn -main []
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (run-app2)
       (println)))
