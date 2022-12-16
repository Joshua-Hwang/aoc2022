(ns distress-signal.core
  (:require [clojure.string :as str]
            [clojure.pprint :as pp])
  (:gen-class))

(defn condense
  "Condense the stack n times"
  [n stack]
  (nth
    (iterate #(conj (nnext %) (conj (second %) (first %))) stack)
    n))

; split by comma
; collection with bracket symbols and numbers
(defn parse-line [line]
  (ffirst
    (reduce
      (fn [stack c]
        (let [head-is-int (int? (peek stack))]
          (case c
            \[ (conj stack []) ; \] for vim editor
            ; condense head of stack lower
            ; if int is head then condense that then 
            ; indicates end of list element
            ; if head is number then needs to condensed into list
            ; list then needs to be condensed into lower
            ; note the root list will attempt to condense down which results in a nested list
            \] (condense (if head-is-int 2 1) stack)
            ; indicates end of element and another element follows
            ; if the head is a number condense it
            ; if the head is a list then it's already condensed
            \, (condense (if head-is-int 1 0) stack)
            (if head-is-int
              (conj (rest stack) (+ (* 10 (peek stack)) (- (int c) (int \0))))
              (conj stack (- (int c) (int \0)))))))
      '() line)))

(comment (parse-line "[12,[2,[3,[4,[5,6,7]]]],8,9]"))
(comment (parse-line "[[[],[]],[]]"))
(comment (parse-line "[[[],[],[[]]],[[[[[]]]]]]"))

(first (filter (partial not= 0) '(0 0 0 0)))

; left should have smaller values and less items in each list
; essentially left - right
(defn compare-pair [left right]
  (let [left (if (int? left) [left] left)
        right (if (int? right) [right] right)]
    (->> (map (fn [l r]
                (if (and (int? l) (int? r))
                  (compare l r)
                  (compare-pair l r)))
              left right)
         ; list is now a single number
         ; first first non-zero
         (filter (partial not= 0))
         (first)
         (#(or % (- (count left) (count right)))))))

(comment
  (compare-pair 
    [1,[2,[3,[4,[5,6,7]]]],8,9]
    [1,[2,[3,[4,[5,6,0]]]],8,9]))
(comment
  (compare-pair
    [[4,4],4,4]
    [[4,4],4,4,4]))

(defn run-app [input]
  (->> input
       (partition-by (partial = ""))
       (filter (comp not str/blank? first))
       (map (partial mapv parse-line))
       (map (partial apply compare-pair))
       (map-indexed (fn [idx res] (if (neg? res) (inc idx) 0)))
       (apply +)))

(comment
  (run-app
    '("[1,1,3,1,1]"
       "[1,1,5,1,1]"
       ""
       "[[1],[2,3,4]]"
       "[[1],4]"
       ""
       "[9]"
       "[[8,7,6]]"
       ""
       "[[4,4],4,4]"
       "[[4,4],4,4,4]"
       ""
       "[7,7,7,7]"
       "[7,7,7]"
       ""
       "[]"
       "[3]"
       ""
       "[[[]]]"
       "[[]]"
       ""
       "[1,[2,[3,[4,[5,6,7]]]],8,9]"
       "[1,[2,[3,[4,[5,6,0]]]],8,9]")))

; (defn -main []
;   (->> *in*
;        (java.io.BufferedReader.)
;        (line-seq)
;        (run-app)
;        (println)))

(defn run-app2 [input]
  (let [sorted (->> input
                    (filter (complement str/blank?))
                    (map parse-line)
                    (cons [[2]])
                    (cons [[6]])
                    (sort compare-pair))]
    (* (inc (.indexOf sorted [[2]]))
       (inc (.indexOf sorted [[6]])))))

(comment
  (run-app2
    '("[1,1,3,1,1]"
       "[1,1,5,1,1]"
       ""
       "[[1],[2,3,4]]"
       "[[1],4]"
       ""
       "[9]"
       "[[8,7,6]]"
       ""
       "[[4,4],4,4]"
       "[[4,4],4,4,4]"
       ""
       "[7,7,7,7]"
       "[7,7,7]"
       ""
       "[]"
       "[3]"
       ""
       "[[[]]]"
       "[[]]"
       ""
       "[1,[2,[3,[4,[5,6,7]]]],8,9]"
       "[1,[2,[3,[4,[5,6,0]]]],8,9]")))

(defn -main []
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (run-app2)
       (println)))
