(ns rock-paper-scissor.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn parse-first-col [sym]
  (case sym
    "A" :rock
    "B" :paper
    "C" :scissors))

(defn parse-second-col [sym]
  (case sym
    "X" :rock
    "Y" :paper
    "Z" :scissors))

;; in - vector with entries "A X"
;; Where first column is {"A" "B" "C"} Rock Paper Scissors
;; second column is {"X" "Y" "Z"} Rock Paper Scissors
(defn parse-input [lines]
  (->> lines
      (map #(str/split % #" "))
      (map #(hash-map :theirs (parse-first-col (first %)) :mine (parse-second-col (peek %))))))
(comment (parse-input ["A Y" "B X" "C Z"]))

(defn shape-score [shape]
  (case shape
    :rock 1
    :paper 2
    :scissors 3))

;; returns :mine, :theirs or :tie for draw
(defn winner [match]
  (case (:mine match)
    :rock (case (:theirs match)
            :rock nil
            :paper :theirs
            :scissors :mine)
    :paper (case (:theirs match)
             :rock :mine
             :paper nil
             :scissors :theirs)
    :scissors (case (:theirs match)
                :rock :theirs
                :paper :mine
                :scissors nil)))
(comment (winner {:mine :rock :theirs :scissors}))

(defn calc-score [match]
  (+ (shape-score (:mine match))
     (case (winner match)
       :theirs 0
       nil 3
       :mine 6)))
(comment (calc-score {:theirs :rock :mine :paper}))

(defn run-app [input]
  (->> input
       parse-input
       (map calc-score)
       (apply +)))
(comment (run-app ["A Y" "B X" "C Z"]))

; (defn -main [] (println (run-app (line-seq (java.io.BufferedReader. *in*)))))

(defn parse-second-col2 [sym]
  (case sym
    "X" :lose
    "Y" :tie
    "Z" :win))

(defn parse-input2 [lines]
  (->> lines
      (map #(str/split % #" "))
      (map #(hash-map :theirs (parse-first-col (first %)) :outcome (parse-second-col2 (peek %))))))
(comment (parse-input2 ["A Y" "B X" "C Z"]))

(defn choose-shape [outcome theirs]
  (case outcome
    :win (case theirs
           :rock :paper
           :paper :scissors
           :scissors :rock)
    :lose (case theirs
            :rock :scissors
            :paper :rock
            :scissors :paper)
    :tie theirs))

(defn run-app2 [input]
  (->> input
       parse-input2
       (map #(assoc % :mine (choose-shape (:outcome %) (:theirs %))))
       (map calc-score)
       (apply +)))
(comment (run-app2 ["A Y" "B X" "C Z"]))

(defn -main [] (println (run-app2 (line-seq (java.io.BufferedReader. *in*)))))
