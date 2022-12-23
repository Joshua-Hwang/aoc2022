(ns monkey-math.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn string->function [operation]
  (case operation
    "+" +
    "-" -
    "*" *
    "/" /))

(defn parse-line [line]
  (let [[monkey expression] (str/split line #":")
        expression (str/trim expression)]
    (if-let [value (parse-long expression)]
      {:monkey monkey :value value}
      (let [[dep1 operation dep2] (str/split expression #" ")]
        {:monkey monkey :operation (string->function operation) :deps (vector dep1 dep2)}))))

(comment (parse-line "root: pppw + sjmn"))
(comment (parse-line "root: 5"))

(defn build-graph [input]
  (->> input
       (map parse-line)
       (reduce (fn [graph {monkey :monkey :as expression}]
                 (assoc graph monkey expression))
               {})))

(comment
  (build-graph
    (list "root: pppw + sjmn"
          "dbpl: 5"
          "cczh: sllz + lgvd"
          "zczc: 2"
          "ptdq: humn - dvpt"
          "dvpt: 3"
          "lfqf: 4"
          "humn: 5"
          "ljgn: 2"
          "sjmn: drzm * dbpl"
          "sllz: 4"
          "pppw: cczh / lfqf"
          "lgvd: ljgn * ptdq"
          "drzm: hmdt - zczc"
          "hmdt: 32")))

(def evaluate-monkey
  (memoize
    (fn [graph monkey]
      (println "Evaluating" monkey)
      (if-let [value (get-in graph [monkey :value])]
        value
        (let [{operation :operation [dep1 dep2] :deps} (graph monkey)]
               (operation (evaluate-monkey graph dep1) (evaluate-monkey graph dep2)))))))

(defn run-app [input]
  ; create a dependency graph
  (let [graph (build-graph input)]
    (evaluate-monkey graph "root")))

(comment
  (run-app
    (list "root: pppw + sjmn"
          "dbpl: 5"
          "cczh: sllz + lgvd"
          "zczc: 2"
          "ptdq: humn - dvpt"
          "dvpt: 3"
          "lfqf: 4"
          "humn: 5"
          "ljgn: 2"
          "sjmn: drzm * dbpl"
          "sllz: 4"
          "pppw: cczh / lfqf"
          "lgvd: ljgn * ptdq"
          "drzm: hmdt - zczc"
          "hmdt: 32")))

; (defn -main []
;   (->> *in*
;        (java.io.BufferedReader.)
;        (line-seq)
;        (run-app)
;        (println)))

(defn parse-line2 [line]
  (let [[monkey expression] (str/split line #":")
        expression (str/trim expression)]
    (case monkey
      "root" (let [[dep1 _ dep2] (str/split expression #" ")]
               {:monkey monkey :operation := :deps (vector dep1 dep2)})
      "humn" {:monkey monkey :value :x}
      (if-let [value (parse-long expression)]
        {:monkey monkey :value value}
        (let [[dep1 operation dep2] (str/split expression #" ")]
          {:monkey monkey :operation (string->function operation) :deps (vector dep1 dep2)})))))

(comment (parse-line2 "root: pppw + sjmn"))
(comment (parse-line2 "pppw: cczh / lfqf"))
(comment (parse-line2 "humn: 5"))

(defn build-graph2 [input]
  (->> input
       (map parse-line2)
       (reduce (fn [graph {monkey :monkey :as expression}]
                 (assoc graph monkey expression))
               {})))

(comment
  (build-graph2
    (list "root: pppw + sjmn"
          "dbpl: 5"
          "cczh: sllz + lgvd"
          "zczc: 2"
          "ptdq: humn - dvpt"
          "dvpt: 3"
          "lfqf: 4"
          "humn: 5"
          "ljgn: 2"
          "sjmn: drzm * dbpl"
          "sllz: 4"
          "pppw: cczh / lfqf"
          "lgvd: ljgn * ptdq"
          "drzm: hmdt - zczc"
          "hmdt: 32")))

(def evaluate-monkey2
  (memoize
    (fn [graph monkey]
      (println "Evaluating" monkey)
      (if-let [value (get-in graph [monkey :value])]
        value
        (let [{operation :operation [dep1 dep2] :deps} (graph monkey)
              dep1-value (evaluate-monkey2 graph dep1)
              dep2-value (evaluate-monkey2 graph dep2)]
          ((if (and (not= :equal operation) (number? dep1-value) (number? dep2-value)) eval identity)
           (list operation dep1-value dep2-value)))))))

(def inverse-operation
  {+ -
   - +
   * /
   / *})

(def non-commutative #{/ -})

(comment (inverse-operation +))

(defn algebra
  "Handles very basic algebra with unknowns on a single side"
  [[_ lhs rhs :as equation]]
  (if (or (= :x lhs) (= :x rhs)) equation
    (let [[known [op l r]] (if (number? lhs) [lhs rhs] [rhs lhs])
          inv-op (inverse-operation op)
          [new-known expression] (if (number? r)
                                   [(inv-op known r) l]
                                   ; fance if statement because 4/x = 150
                                   [(if (non-commutative op) (op l known) (inv-op known l)) r])]
      (recur (list := new-known expression)))))

; (4 + (2 * (x - 3))) / 4 = 150
(comment (algebra (list := (list / (list + 4 (list * 2 (list - :x 3))) 4) 150)))
(comment (algebra (list := (list / 4 :x) 150))) ; 4 / x = 150 => x = 4 / 150
(comment (algebra (list := (list / :x 4) 150))) ; x / 4 = 150 => x = 150 * 4
(comment (algebra (list := (list - 4 :x) 150))) ; 4 - x = 150 => x = 4 - 150
(comment (algebra (list := (list - :x 4) 150))) ; x - 4 = 150 => x = 150 + 4

(defn run-app2 [input]
  ; create a dependency graph
  (let [graph (build-graph2 input)
        expression (evaluate-monkey2 graph "root")]
    (algebra expression)))

(comment
  (run-app2
    (list "root: pppw + sjmn"
          "dbpl: 5"
          "cczh: sllz + lgvd"
          "zczc: 2"
          "ptdq: humn - dvpt"
          "dvpt: 3"
          "lfqf: 4"
          "humn: 5"
          "ljgn: 2"
          "sjmn: drzm * dbpl"
          "sllz: 4"
          "pppw: cczh / lfqf"
          "lgvd: ljgn * ptdq"
          "drzm: hmdt - zczc"
          "hmdt: 32")))

(defn -main []
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (run-app2)
       (println)))
