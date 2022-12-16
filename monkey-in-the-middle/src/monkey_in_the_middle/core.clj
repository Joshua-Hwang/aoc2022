(ns monkey-in-the-middle.core
  (:require [clojure.string :as str]
            [clojure.set :as set])
  (:gen-class))

(defn parse-id [line]
  (as-> line l
    (str/split l #" " 2)
    (second l)
    (.substring l 0 (dec (count l)))))

(comment (parse-id "Monkey 0:"))

(defn parse-items [line]
  (as-> line l
    (-> l
        (str/split #":")
        (second)
        (str/split #","))
    (->> l
         (map str/trim)
         (mapv parse-long))))

(comment (parse-items "  Starting items: 79, 98"))
(comment (parse-items "  Starting items: 74"))
(comment (parse-items "  Starting items: 54, 65, 75, 74"))

(defn parse-operation [line]
  (let [tokens (str/split line #" ")
        [operator term] (take-last 2 tokens)]
    {:operator (case operator "*" :multiply "+" :addition)
     :term (if (= term "old") :self (parse-long term))}))

(comment (parse-operation "  Operation: new = old * 19"))
(comment (parse-operation "  Operation: new = old + 6"))
(comment (parse-operation "  Operation: new = old * old"))

; all tests are divisibility
(defn parse-test [line] (-> line (str/split #" ") (peek) (parse-long)))

(defn parse-if-branch [line] (-> line (str/split #" ") (peek)))

(defn parse-monkey [input]
  (->> input
       ; it's up to the higher level to determine when to pass
       (map str/trim)
       ; map each line to a function
       (map vector [(comp (partial hash-map :id) parse-id)
                    (comp (partial hash-map :items) parse-items)
                    (comp (partial hash-map :operation) parse-operation)
                    (comp (partial hash-map :test) parse-test)
                    (comp (partial hash-map :true) parse-if-branch)
                    (comp (partial hash-map :false) parse-if-branch)])
       (reduce
         (fn [monkey [f line]]
           (into monkey (f line))) {:count 0})))

(comment
  (parse-monkey
    '("Monkey 0:"
              "  Starting items: 79, 98"
              "  Operation: new = old * 19"
              "  Test: divisible by 23"
              "    If true: throw to monkey 2"
              "    If false: throw to monkey 3")))

(comment
  (map parse-monkey
       '(("Monkey 0:"
                  "  Starting items: 79, 98"
                  "  Operation: new = old * 19"
                  "  Test: divisible by 23"
                  "    If true: throw to monkey 2"
                  "    If false: throw to monkey 3")
         ("Monkey 1:"
                  "  Starting items: 54, 65, 75, 74"
                  "  Operation: new = old + 6"
                  "  Test: divisible by 19"
                  "    If true: throw to monkey 2"
                  "    If false: throw to monkey 0"
                  ))))

; perform operation
; divide by 3
; check divisibility
(defn compute-new-worry [monkey item]
  (let [{{:keys [operator term]} :operation} monkey
        term (if (= term :self) item term)]
    (-> item
        ((case operator :multiply * :addition +) term)
        (/ 3)
        (int))))

(defn compute-destination [monkey item]
  (if (= 0 (mod item (monkey :test))) (monkey :true) (monkey :false)))

(comment (update {"hello" "world"} "hello" println "what what" "no way"))
(comment (update-in {"hello" "world"} ["hello"] println "what what" "no way"))

(defn send-items [monkeys idx]
  (let [monkey (get monkeys idx)
        items (get monkey :items)
        ; clear items pre-emptively
        monkeys (-> monkeys (update-in [idx :items] empty) (update-in [idx :count] (partial + (count items))))]
    (->> items
         (map (partial compute-new-worry monkey))
         (group-by (partial compute-destination monkey))
         (reduce (fn [monkeys [dest item]]
                   (-> monkeys
                       ; in case you forget update-in is like ->
                       (update-in [dest :items] (partial apply conj) item)))
                 monkeys))))

(comment (send-items {"0"
                      {:id "0",
                       :count 0,
                       :items '[79 98],
                       :operation {:operator :multiply, :term 19},
                       :test 23,
                       :true "2",
                       :false "3"},
                      "1"
                      {:id "1",
                       :count 0,
                       :items '[54 65 75 74],
                       :operation {:operator :addition, :term 6},
                       :test 19,
                       :true "2",
                       :false "0"},
                      "2"
                      {:id "2",
                       :count 0,
                       :items '[79 60 97],
                       :operation {:operator :multiply, :term :self},
                       :test 13,
                       :true "1",
                       :false "3"},
                      "3"
                      {:id "3",
                       :count 0,
                       :items '[74],
                       :operation {:operator :addition, :term 3},
                       :test 17,
                       :true "0",
                       :false "1"}}
                     "2"))

(defn compute-round [monkeys]
  ((->> (keys monkeys)
        ; sequence of functions curried on index. Waiting for monkeys
        (map (fn [idx] (fn [monkeys] (send-items monkeys idx))))
        ; composition requires this be the other way around
        (reverse)
        (apply comp))
   monkeys))

(defn run-app [input]
  (->> input 
       (partition-by empty?)
       (filter (comp not empty? first))
       (map parse-monkey)
       ; hacky
       (zipmap (map str (iterate inc 0)))
       (iterate compute-round)
       (drop 20)
       (first)
       (map (fn [[_ monkey]] (monkey :count)))
       (sort)
       (take-last 2)
       (apply *)))

(comment
  (run-app
           (list "Monkey 0:"
                 "  Starting items: 79, 98"
                 "  Operation: new = old * 19"
                 "  Test: divisible by 23"
                 "    If true: throw to monkey 2"
                 "    If false: throw to monkey 3"
                 ""
                 "Monkey 1:"
                 "  Starting items: 54, 65, 75, 74"
                 "  Operation: new = old + 6"
                 "  Test: divisible by 19"
                 "    If true: throw to monkey 2"
                 "    If false: throw to monkey 0"
                 ""
                 "Monkey 2:"
                 "  Starting items: 79, 60, 97"
                 "  Operation: new = old * old"
                 "  Test: divisible by 13"
                 "    If true: throw to monkey 1"
                 "    If false: throw to monkey 3"
                 ""
                 "Monkey 3:"
                 "  Starting items: 74"
                 "  Operation: new = old + 3"
                 "  Test: divisible by 17"
                 "    If true: throw to monkey 0"
                 "    If false: throw to monkey 1" )))

; (defn -main []
;   (->> *in*
;        (java.io.BufferedReader.)
;        (line-seq)
;        (run-app)
;        (println)))

(defn compute-new-worry2 [monkey limit item]
  (let [{{:keys [operator term]} :operation} monkey
        term (if (= term :self) item term)]
    (-> item
        ((case operator :multiply *' :addition +') term)
        (mod limit))))

(defn send-items2 [monkeys idx]
  (let [monkey (get monkeys idx)
        items (get monkey :items)
        ; clear items pre-emptively
        monkeys (-> monkeys (update-in [idx :items] empty) (update-in [idx :count] (partial + (count items))))
        limit (->> monkeys (map (fn [[_ monkey]] (monkey :test))) (apply *))]
    (->> items
         (map (partial compute-new-worry2 monkey limit))
         (group-by (partial compute-destination monkey))
         (reduce (fn [monkeys [dest item]]
                   (-> monkeys
                       ; in case you forget update-in is like ->
                       (update-in [dest :items] (partial apply conj) item)))
                 monkeys))))

(defn compute-round2 [monkeys]
  ((->> (keys monkeys)
        ; sequence of functions curried on index. Waiting for monkeys
        (map (fn [idx] (fn [monkeys] (send-items2 monkeys idx))))
        ; composition requires this be the other way around
        (reverse)
        (apply comp))
   monkeys))

(defn run-app2 [input]
  (->> input 
       (partition-by empty?)
       (filter (comp not empty? first))
       (map parse-monkey)
       ; hacky
       (zipmap (map str (iterate inc 0)))
       (iterate compute-round2)
       (drop 10000)
       (first)
       (map (fn [[_ monkey]] (monkey :count)))
       (sort)
       (take-last 2)
       (apply *')))

; (defn -main []
;   (println
;     (run-app2
;       (list "Monkey 0:"
;             "  Starting items: 79, 98"
;             "  Operation: new = old * 19"
;             "  Test: divisible by 23"
;             "    If true: throw to monkey 2"
;             "    If false: throw to monkey 3"
;             ""
;             "Monkey 1:"
;             "  Starting items: 54, 65, 75, 74"
;             "  Operation: new = old + 6"
;             "  Test: divisible by 19"
;             "    If true: throw to monkey 2"
;             "    If false: throw to monkey 0"
;             ""
;             "Monkey 2:"
;             "  Starting items: 79, 60, 97"
;             "  Operation: new = old * old"
;             "  Test: divisible by 13"
;             "    If true: throw to monkey 1"
;             "    If false: throw to monkey 3"
;             ""
;             "Monkey 3:"
;             "  Starting items: 74"
;             "  Operation: new = old + 3"
;             "  Test: divisible by 17"
;             "    If true: throw to monkey 0"
;             "    If false: throw to monkey 1" ))))

(defn -main []
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (run-app2)
       (println)))
