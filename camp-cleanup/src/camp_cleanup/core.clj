(ns camp-cleanup.core
  (:require
    [clojure.set :as set]
    [clojure.string :as string])
  (:gen-class))

(defn int-or-nil [val] (try (Integer/parseInt val) (catch NumberFormatException _)))

(defn parse-line [line]
  (as-> line l
    (string/split l #",")
    (map #(string/split % #"-") l)
    (map (partial map #(Integer/parseInt %)) l)
    ))
(comment (parse-line "2-4,6-8"))

;; checks if l1-r1 encompasses l2-r2
(defn within? [[l1 r1] [l2 r2]]
  (and (<= l1 l2) (>= r1 r2)))
(comment (within? [2 4] [6 8]))
(comment (within? [4 6] [6 6]))

(defn run-app [input]
  (->> input
       (map parse-line)
       (map (fn [[interval1 interval2]]
              (if (or
                    (within? interval1 interval2)
                    (within? interval2 interval1))
                1 0)))
       (apply +)))
(comment (run-app ["2-4,6-8" "2-3,4-5" "6-6,4-6"]))

; (defn -main [] (println (run-app (line-seq (java.io.BufferedReader. *in*)))))

;; ranges are inclusive
;; no adjustments are made because the adjustments will cancel out
(defn overlap? [[l1 r1] [l2 r2]]
  (let [l (min l1 l2) r (max r1 r2)]
    (<= (- r l) (+ (- r1 l1) (- r2 l2)))))
(comment (overlap? [2 4] [6 8]))
(comment (overlap? [4 6] [6 6]))

(defn run-app2 [input]
  (->> input
       (map parse-line)
       (map (fn [[interval1 interval2]]
              (if (overlap? interval1 interval2) 1 0)))
       (apply +)))
(comment (run-app2 ["2-4,6-8" "2-3,4-5" "5-7,7-9" "2-8,3-7" "6-6,4-6" "2-6,4-8"]))

(defn -main [] (println (run-app2 (line-seq (java.io.BufferedReader. *in*)))))
