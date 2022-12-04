(ns rucksack-reorg.core
  (:require [clojure.set :as set])
  (:gen-class))

(def spy #(do (println "DEBUG:" %) %))

(defn split-in-half [s]
  (if (odd? (count s))
    (throw (AssertionError. "s should be even"))
    (let [m (/ (count s) 2)]
      [(subs s 0 m) (subs s m)])
    ))
(comment (split-in-half "012345"))
(comment (set "123")) ; #{\1 \2 \3}
(comment (hash-set "123")) ; #{"123"}

(defn priority [c]
  (let [c (int c)]
    (if (< c (int \a))
      (+ 27 (- c (int \A))) ; upper case
      (inc (- c (int \a))))))
(comment (priority \p))
(comment (priority \L))
(comment (priority \a))
(comment (priority \z))
(comment (priority \A))
(comment (priority \Z))

(defn run-app [input]
  (->> input
       (map split-in-half)
       (map #(set/intersection (set (first %)) (set (peek %))))
       (map #(if (not= 1 (count %))
               (throw (AssertionError. (str "expect exactly 1 shared character got: " (count %))))
               (first %)))
       (map priority)
       (apply +)))
(comment (run-app
           ["vJrwpWtwJgWrhcsFMMfFFhFp"
            "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
            "PmmdzqPrVvPwwTWBwg"
            "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
            "ttgJtRGJQctTZtZT"
            "CrZsJsPPZsGzwwsLwLmpwMDw"]))

; (defn -main [] (println (run-app (line-seq (java.io.BufferedReader. *in*)))))

(defn group-elves [elves]
  (if (not= 0 (mod (count elves) 3))
    (throw (AssertionError. (str "expect count to be divisible by 3 got: " (count elves))))
    (reduce (fn [coll val]
              (if (= 3 (count (peek coll)))
                (conj coll [val])
                (conj (pop coll) (conj (peek coll) val))))
            [[]] elves)))
(comment (group-elves ["asdf" "abcde" "yooo" "a" "b" "c"]))


(defn run-app2 [input]
  (->> input
       group-elves
       (map #(apply set/intersection (map set %)))
       (map #(if (not= 1 (count %))
               (throw (AssertionError. (str "expect exactly 1 shared character got: " (count %))))
               (first %)))
       (map priority)
       (apply +)))
(comment (run-app2
           ["vJrwpWtwJgWrhcsFMMfFFhFp"
            "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
            "PmmdzqPrVvPwwTWBwg"
            "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
            "ttgJtRGJQctTZtZT"
            "CrZsJsPPZsGzwwsLwLmpwMDw"]))

(defn -main [] (println (run-app2 (line-seq (java.io.BufferedReader. *in*)))))
