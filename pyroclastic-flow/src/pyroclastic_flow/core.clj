(ns pyroclastic-flow.core
  (:require [clojure.set :as set]
            [clojure.string :as str])
  (:gen-class))

(defn parse-input [input]
  (map-indexed (fn [index c] (if (= \< c) [:left index] [:right index])) input))

(comment
  (parse-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"))

(defn create-rocks []
  ; coords are based on bottom left
  ; [x y]
  (map-indexed
    (fn [index rock] [rock index])
    [#{[0 0] [1 0] [2 0] [3 0]}
     #{[1 0] [0 1] [1 1] [2 1] [1 2]}
     #{[0 0] [1 0] [2 0] [2 1] [2 2]}
     #{[0 0] [0 1] [0 2] [0 3]}
     #{[0 0] [1 0] [0 1] [1 1]}]))

(defn create-rock-grid []
  (sorted-set-by (fn [[x1 y1] [x2 y2]]
                   (let [c (compare y1 y2)]
                     (if (not= c 0) c (compare x1 x2))))))

(defn rock-collides? [grid rock] (seq (filter grid rock)))

(defn rock-left [rock] (apply min (map first rock)))
(defn rock-right [rock] (apply max (map first rock)))
(defn rock-bottom [rock] (apply min (map second rock)))
(defn rock-top [rock] (apply max (map second rock)))

(defn simulate-rock [{grid :grid, winds :winds height :height offset :offset} rock]
  ; first thing is to blow the rock
  ; positive x is right
  ; positive y is up
  ; afterwards fall down one
  ; if falling down collides then move up and freeze the rock
  ; (println (first (first winds)))
  ; (println rock)
  (let [wind (first winds)
        new-rock (->> rock
                      ; blow rock
                      (map #(update % 0 (case (first wind) :right inc' :left dec' identity)))
                      ; push back if collides
                      (#(if (or (> 0 (rock-left %)) (<= 7 (rock-right %))
                                (rock-collides? grid %))
                          rock
                          %))
                      ; drop rock
                      (map #(update % 1 dec')))]
    ; dropping has collided with bottom or rock. Logical choice is to raise it
    (if (or (> 0 (rock-bottom new-rock)) (rock-collides? grid new-rock))
      (let [new-rock (map (fn [[x y]] [x (inc' y)]) new-rock)]
        (list {
               :grid (apply conj grid rock)
               :height height}
              {
               :grid (apply conj grid new-rock)
               :wind-index (second wind)
               :winds (rest winds)
               :height (max height (inc' (rock-top new-rock)))
               :offset offset
               }))
      (cons {
             :grid (apply conj grid rock)
             :height height}
            (lazy-seq (simulate-rock {:grid grid :winds (rest winds) :height height :offset offset} new-rock))))))

(comment
  (simulate-rock
    {:grid (create-rock-grid)
     :winds '(:right :right :right :left :left :right :left :right :right :left)
     :height 0}
    (map (fn [[x y]] [(+ 2 x) (+ 3 y)]) #{[0 0] [1 0] [2 0] [3 0]})))

(defn simulate-rocks [initial-state rocks]
  (let [rock (map (fn [[x y]] [(+' 2 x) (+' 3 (initial-state :height) y)]) (ffirst rocks))
        simulation (simulate-rock initial-state rock)
        next-state (assoc (last simulation) :rock-index (second (first rocks)))
        ; next-state (if (> 100000 (next-state :height))
        ;              next-state
        ;              (->> (subseq (next-state :grid) >= [0 (- (next-state :height) 100)])
        ;                   (map (fn [[x y]] [x (- y 99900)])) ; forumla is 100000 - 100 which is the lowest value after the culling
        ;                   (into (create-rock-grid))
        ;                   (assoc next-state :offset (+' 99900 (next-state :offset)) :height (- (next-state :height) 99900) :grid)))
        ]
    (if (empty? (initial-state :winds))
      '()
      ; (concat simulation (lazy-seq (simulate-rocks (last simulation) (rest rocks))))
      (cons next-state (lazy-seq (simulate-rocks next-state (rest rocks))))
      )))

(defn render-grid [grid bottom height]
  (str
    height \newline
    (str/join
      \newline
      (map
        (fn [row]
          (str/join (map
                      (fn [column] (if (grid [column row]) \# \.))
                      (range 7))))
        (reverse (range bottom height))))))

(defn run-app [input]
  (let [winds (parse-input input)]
    (-> (simulate-rocks {:grid (sorted-set-by
                                 (fn [[x1 y1] [x2 y2]]
                                   (let [c (compare x1 x2)]
                                     (if (not= c 0) c (compare y1 y2))))),
                         :winds (cycle winds),
                         :height 0
                         :real-height 0
                         :offset 0}
                        (cycle (create-rocks)))
        ; (drop 100)
        ; (take 300)
        ; (map (fn [{grid :grid height :height}] (render-grid grid 0 (+ 4 height))))
        ; (str/join "\n-------\n")
        (nth (dec 2022))
        ; (nth (dec 11))
        (:height)
        )))

(comment
  (run-app ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"))

; (defn -main []
;   (->> *in*
;        (java.io.BufferedReader.)
;        (line-seq)
;        (first)
;        (run-app)
;        (println)))

(defn take-nths [nths coll]
  (if-let [d (first nths)]
    (let [tail (drop d coll)
          taken (first tail)]
      (cons taken (lazy-seq (take-nths (drop 1 nths) tail))))
    nil))

(comment (take-nths [0 1] [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16]))
(comment (take-nths (cycle [1 2 3 4]) (cycle [0 2 4 9 15])))

; okay so the deal with part 2 is you have to find some period
; to do this I start printing out the rock-index and wind-index once a rock has landed
; eventually you'll be able to see a pattern
; (starting junk... pattern... pattern again)
; using that pattern and take-nths you should be able to calculate the total height
; something like
; height of starting junk + height from pattern * periods + remaining height that isn't in the pattern
; I used the sample input to fine tune it just right
(defn run-app2 [input]
  (let [winds (parse-input input)
        ;period (* 5 (count winds))
        period 1705
        ] ; the 7 is period after skipping first
    (println "period:" period)
    (let [simulation (->> (simulate-rocks {:grid (sorted-set-by
                                                   (fn [[x1 y1] [x2 y2]]
                                                     (let [c (compare x1 x2)]
                                                       (if (not= c 0) c (compare y1 y2))))),
                                           :winds (cycle winds),
                                           :height 0
                                           :offset 0N}
                                          (cycle (create-rocks)))
                          ; (drop 100)
                          ; (take 300)
                          ; (map (fn [{grid :grid height :height}] (render-grid grid 0 (+ 4 height))))
                          ; (str/join "\n-------\n")
                          ; (drop (dec' 1000000000000N))
                          ;(drop (dec' 2022))
                          ;(drop (dec' 2)) ; second rock (index 1)
                          ;(first)
                          ;(drop (dec period))
                          ;(drop 1)
                          ; (take-nth period)
                          ; (map #(+ (% :height) (% :offset)))
                          ; (partition 2 1)
                          ; (map (fn [[a b]] (- b a)))
                          ;(nth (dec' 1000000000000N))
                          ; (nth (dec 2022))
                          ; (#(+ (% :height) (% :offset)))
                          )]
      ;(println (select-keys (first (drop period simulation))))
      ;(map #(select-keys % [:wind-index :rock-index]) (take-nths [1700 1705 1705 1705 1705] simulation))
      ;(map #(select-keys % [:wind-index :rock-index]) simulation)
      ; (println (:wind-index (first (drop period simulation))))
      ; (println (:height (first (drop (+ period 400) simulation))))
      ;(println (:height (first (drop ))))
      (->> simulation
           ; (drop 1700)
           ; (drop 1705)
           ; (take-nth period)
           ;(take-nth 1700)
           ; (take-nth 1590)
           (take-nths [1700 1705 1705 1705 1705 1590])
           (map :height)
           (cons 0) ; start at height 0
           (partition 2 1)
           (map (fn [[a b]] (- b a)))
           )
      )))

(comment
  (run-app2 ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"))

(defn -main []
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (first)
       (run-app2)
       (run! println)))
