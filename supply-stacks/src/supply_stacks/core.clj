(ns supply-stacks.core
  (:require [clojure.string :as string])
  (:gen-class))

(def spy #(do (println "DEBUG:" %) %))

;; this is an input
;;[D]        
;;[N] [C]    
;;[Z] [M] [P]
;; 1   2   3 
;; important characters are seen indexes (0-based)
;; 1 5 9 13 (every 4 starting from 1)
(comment (count "[Z] [M]")) ; 7
(comment (count "[Z] [M] [P]")) ; 11
(comment (count "[Z] [M] [P] [X]")) ; 15
;; this is because there are spaces between each crate
(defn parse-crate-level [line]
  (->> (range 1 (count line) 4)
       (map (partial get line))
       (map #(if (= % \space) nil %))))
(comment (parse-crate-level "[Z] [M] [P] [X]"))
(comment (parse-crate-level "    [M] [P] [X]"))

;; uses a single line of input to determine number of crates
(defn parse-num-crates [line] (/ (inc (count line)) 4))

;; crate-lines must be a seq of vectors containing characters
(defn stack-crates [crate-lines]
  (let [num-crates (count (first crate-lines))]
    (reduce
      (fn [coll crate-line] ; val is a seq of characters or nils
        (reduce (fn [coll idx]
                  (update coll idx conj (get crate-line idx)))
                coll
                (->> (range num-crates) (filter (partial get crate-line)))))
      (vec (repeat num-crates '())) (reverse crate-lines))))
(comment (stack-crates '([nil \m \p \x] [\z \a \b \c])))

(comment (map peek ['(\z) '(\m \a) '(\p \b) '(\x \c)]))
(comment (map peek ['[\z] '[\m \a] '[\p \b] '[\x \c]]))

(comment (->> (range 5) (filter (partial get [nil \a \b nil \c]))))
(comment (vec (repeat 5 '())))

(defn parse-instruction [line]
  (let [[_ move _ from _ to] (string/split line #" ")]
    [(Integer/parseInt move) (dec (Integer/parseInt from)) (dec (Integer/parseInt to))]))
(comment (parse-instruction "move 1 from 2 to 1"))

(defn handle-instruction [move from to stacks]
  (let
    [crates-to-move (take move (get stacks from))]
    (-> stacks
        (update to #(apply conj % crates-to-move))
        (update from (partial drop move)))))
(comment (handle-instruction 1 1 0 ['(\n \z) '(\d \c \m) '(\p)]))

;; Proof lists are LIFO
(comment (apply conj '(99 98 97 96) (take 3 '(1 2 3 4 5))))

(defn run-app [input]
  (let [stacks (->> input
                    (take-while #(not (string/includes? % "1")))
                    (map parse-crate-level)
                    (map vec)
                    (vec)
                    (stack-crates))
        instructions (->> input
                          (drop-while #(not= 0 (count %)))
                          (rest)
                          (map parse-instruction))]
    (as-> stacks s
      (reduce (fn [stacks [move from to]]
                (handle-instruction move from to stacks)) s instructions)
      (map first s))))

(run-app
  ["    [D]    "
   "[N] [C]    "
   "[Z] [M] [P]"
   " 1   2   3 "
   ""
   "move 1 from 2 to 1"
   "move 3 from 1 to 3"
   "move 2 from 2 to 1"
   "move 1 from 1 to 2"])

; (defn -main [] (println (run-app (line-seq (java.io.BufferedReader. *in*)))))

(defn handle-instruction2 [move from to stacks]
  (let
    [crates-to-move (reverse (take move (get stacks from)))]
    (-> stacks
        (update to #(apply conj % crates-to-move))
        (update from (partial drop move)))))

(defn run-app2 [input]
  (let [stacks (->> input
                    (take-while #(not (string/includes? % "1")))
                    (map parse-crate-level)
                    (map vec)
                    (vec)
                    (stack-crates))
        instructions (->> input
                          (drop-while #(not= 0 (count %)))
                          (rest)
                          (map parse-instruction))]
    (as-> stacks s
      (reduce (fn [stacks [move from to]]
                (handle-instruction2 move from to stacks)) s instructions)
      (map first s))))

(defn -main [] (println (run-app2 (line-seq (java.io.BufferedReader. *in*)))))
