(ns full-of-hot-air.core
  (:require [clojure.string :as str])
  (:gen-class))

(def snafu->digit
  {\2 2
   \1 1
   \0 0
   \- -1
   \= -2})

(def digit->snafu
  {2 \2
   1 \1
   0 \0
   -1 \-
   -2 \=})

(defn snafu->int [s]
  (->> s
       (reverse)
       (map snafu->digit)
       (map * (iterate (partial * 5) 1))
       (apply +)))

(comment
  (snafu->int "1=-0-2"))
(comment
  (snafu->int "1="))


(defn int->snafu' [carry-one? ls]
  ; ls is already reversed
  (if (empty? ls)
    (if carry-one? [1] [])
    (let [d (first ls)
          d (if carry-one? (inc d) d)]
      ; if 3 or 4 subtract 5 and carry one
      (if (< 2 d)
        (cons (- d 5) (int->snafu' true (rest ls)))
        (cons d (int->snafu' false (rest ls)))))))

(defn int->snafu [i]
  (->> (Long/toString i 5)
       (map #(Character/digit % 5))
       (reverse)
       (int->snafu' false)
       (reverse)
       (map digit->snafu)
       (str/join)))

(comment (int->snafu 314159265))

(defn run-app [input]
  (->> input
       ((fn [arg] (println arg) arg))
       (map snafu->int)
       ((fn [arg] (println arg) arg))
       (apply +)
       ((fn [arg] (println arg) arg))
       (int->snafu)))

(defn -main []
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (run-app)
       (println)))
