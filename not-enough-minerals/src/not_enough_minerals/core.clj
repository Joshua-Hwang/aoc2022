(ns not-enough-minerals.core
  (:gen-class))

(defn parse-line [line]
  (let [[idx ore-bot-ore clay-bot-ore obsidian-bot-ore obsidian-bot-clay geode-bot-ore geode-bot-obsidian] (map parse-long (re-seq #"\d+" line))]
    {:idx idx :costs [[ore-bot-ore 0 0 0] [clay-bot-ore 0 0 0] [obsidian-bot-ore obsidian-bot-clay 0 0] [geode-bot-ore 0 geode-bot-obsidian 0]]}))

(comment (parse-line "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian."))

(defn parse-all [input]
  (->> input
       (map parse-line)
       (reduce (fn [blueprints {blueprint :idx costs :costs}]
                 (assoc blueprints blueprint costs))
               (sorted-map))))

(comment (parse-all
           (list
             "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian."
             "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")))

(defn vector-add [a b] (mapv + a b))
(defn vector-subtract [a b] (mapv - a b))

(def dp'
  (memoize
    (fn [costs minute bots
         [_ _ _ geode :as resources]]
      (println minute bots resources)
      ; if minute > 24 can't do anything and return geode
      ; check if buying anything is possible. 
      ; take max of each purchase or just waiting another minute
      ; if we intended to bottom-up we have to fill a 7D space with lengths 24
      ; if we intend to top-down we have a change of hitting recursion limits
      ; greedy algorithm states you should buy when possible or not at all. Don't know how to optimise for this though
      (if (< 24 minute)
        geode
        ; potential new bots don't contribute to resource acquisition this turn
        (let [new-resources (vector-add resources bots)]
          (apply max
                 (dp' costs (inc minute) bots new-resources)
                 (map-indexed
                   (fn [idx cost]
                     (if (not-every? (partial <= 0) (vector-subtract resources cost))
                       ; can't make that robot. Not enough resources
                       0
                       ; end of this minute new resources are collected AFTER buliding with existing resources
                       (dp' costs (inc minute) (update bots idx inc) (vector-subtract new-resources cost))))
                   costs)))))))

(defn dp
  "costs is of the form [[1 0 0 0] [3 0 0 0]]
  where the resources (and bots) are ordered by ore, clay, obsidian, geode"
  [costs]
  (dp' costs 1 [1 0 0 0] [0 0 0 0]))

(comment (dp [[4 0 0 0] [2 0 0 0] [3 14 0 0] [2 0 7 0]]))

(defn ore [v] (nth v 0))
(defn clay [v] (nth v 1))
(defn obsidian [v] (nth v 2))
(defn geode [v] (nth v 3))

(defn next-states [costs {bots :bots resources :resources :as state}]
  (let [new-resources (vector-add resources bots)]
    (cons (assoc state :resources new-resources)
           (remove nil?
                   (map-indexed
                     (fn [idx cost]
                       ; check if we can build a robot with CURRENT resources
                       (if (not-every? (partial <= 0) (vector-subtract resources cost))
                         ; can't make that robot. Not enough resources
                         nil
                         ; end of this minute new resources are collected AFTER buliding with existing resources
                         {:bots (update bots idx inc) :resources (vector-subtract new-resources cost)}))
                     costs)))))

(comment (next-states [[4 0 0 0] [2 0 0 0] [3 14 0 0] [2 0 7 0]] {:bots [1 1 1 1] :resources [10 10 10 10]}))
(comment (next-states [[4 0 0 0] [2 0 0 0] [3 14 0 0] [2 0 7 0]] {:bots [1 1 0 0] :resources [2 1 0 0]}))

(defn bot-score
  "heuristic scores which weights geode bots more"
  [bots]
  (->> bots
       (map * [0 1 2 3])
       (apply +)))

(comment (bot-score [2 0 0 0]))
(comment (bot-score [1 2 0 0]))

(defn heuristic-sort [s1 s2]
  ; comparator returns effectively s1 - s2
  ; -1 means s1 < s2
  ; +1 means s1 > s2
  ; sort by geodes
  ; sort by bots weighted differently for each bot
  ; sort by other resources (reverse). Good resource usage
  (let [geode-order
        (compare
          (-> s1 (:resources) (geode))
          (-> s2 (:resources) (geode)))]
    (if (not= 0 geode-order) geode-order
      (let [bot-order
            (compare
              (bot-score (s1 :bots))
              (bot-score (s2 :bots)))]
        (if (not= 0 bot-order) bot-order
          (let [obsidian-order
                (compare
                  (-> s1 (:resources) (obsidian))
                  (-> s2 (:resources) (obsidian)))]
            (if (not= 0 obsidian-order) obsidian-order
              (let [clay-order
                    (compare
                      (-> s1 (:resources) (clay))
                      (-> s2 (:resources) (clay)))]
                clay-order))))))))

(comment (heuristic-sort {:bots [100 100 100 100] :resources [100 100 100 0]} {:bots [0 0 0 0] :resources [0 0 0 1]}))
(comment (heuristic-sort {:bots [100 100 100 100] :resources [100 100 100 1]} {:bots [0 0 0 0] :resources [0 0 0 1]}))

(defn dp2'
  "states is of the form (list {:bots [1 0 0 0] :resources [0 0 0 0]})"
  [costs minute states]
  (println minute (count states))
  ; (println "Start of minute" minute states)
  (if (< 24 minute)
    ;(->> states (map :resources) (map geode) (apply max))
    ; less efficient but easier for debugging
    ; you can pop the hood and edit things easier this way
    (first (reverse (sort heuristic-sort states)))
    (->> states
         (mapcat (partial next-states costs))
         ; heuristically cull states that are "worse". I think the order is
         (sort heuristic-sort)
         (reverse)
         (take 1000)
         ; ((fn [states] (println "End of minute" minute states) states))
         (recur costs (inc minute)))))

(defn dp2 [costs]
  (->> (dp2' costs 1 '({:bots [1 0 0 0] :resources [0 0 0 0]}))
       (:resources)
       (geode)))

(defn run-app [input]
  (->> input
       (parse-all)
       ; mapv purely for printing aesthetics
       ; would be a lazy sequence otherwise
       (mapv (fn [[blueprint-id costs]] (* blueprint-id (dp2 costs))))
       (apply +)))

; (defn -main []
;   (->> *in*
;        (java.io.BufferedReader.)
;        (line-seq)
;        (run-app)
;        (println)))

(defn dp3'
  "states is of the form (list {:bots [1 0 0 0] :resources [0 0 0 0]})"
  [costs minute states]
  (println minute (count states))
  ; (println "Start of minute" minute states)
  (if (< 32 minute)
    ;(->> states (map :resources) (map geode) (apply max))
    ; less efficient but easier for debugging
    ; you can pop the hood and edit things easier this way
    (first (reverse (sort heuristic-sort states)))
    (->> states
         (mapcat (partial next-states costs))
         ; heuristically cull states that are "worse". I think the order is
         (sort heuristic-sort)
         (reverse)
         (take 1000)
         ; ((fn [states] (println "End of minute" minute states) states))
         (recur costs (inc minute)))))

(defn dp3 [costs]
  (->> (dp3' costs 1 '({:bots [1 0 0 0] :resources [0 0 0 0]}))
       (:resources)
       (geode)))

(defn run-app2 [input]
  (->> input
       (parse-all)
       (vals)
       (take 3)
       ; mapv purely for printing aesthetics
       ; would be a lazy sequence otherwise
       (mapv dp3)
       (apply *)))

(defn -main []
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (run-app2)
       (println)))
