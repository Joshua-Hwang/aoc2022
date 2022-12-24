(ns blizzard-basin.core
  (:require [clojure.string :as str])
  (:gen-class))

; Create lazy-seq containing the board
; the board should be a map with location as key, value as a list of blizzard directions
; Create function that identifies loose spaces
; run BFS on the loose spaces

(defn parse-line
  "Not designed for the hash border. Please crop it"
  [line]
  (->> line
       (map #(case % \^ :up \> :right \v :down \< :left nil))
       (map-indexed vector)
       (remove #(nil? (second %)))))

(comment (parse-line ">>.<^<"))

(defn parse-input [input]
  ; ignore border. Starting position and end positions are known.
  (let [cropped-input (->> input
                           (drop 1)
                           ; drop-last is lazy
                           (drop-last)
                           (map (fn [line] (subs line 1 (dec (count line))))))
        height (count cropped-input)
        width (count (first cropped-input))]
    {:height height :width width
     :system (->> cropped-input
                  (map parse-line)
                  (mapcat (fn [row blizzards] (mapcat (fn [[col direction]] [[row col] [direction]]) blizzards)) (iterate inc 0))
                  (apply array-map))}))

(comment
  (parse-input 
    (list "#.######"
          "#>>.<^<#"
          "#.<..<<#"
          "#>v.><>#"
          "#<^v^^>#"
          "######.#")))

(defn render-system [{height :height width :width system :system}]
  (str/join \newline
            (map
              (fn [row]
                (str/join
                  (map
                    (fn [col]
                      (str (count (system [row col]))))
                    (range width))))
              (range height))))

(defn move [height width [row col] direction]
  (case direction
    :up [(mod (dec row) height) col]
    :right [row (mod (inc col) width)]
    :down [(mod (inc row) height) col]
    :left [row (mod (dec col) width)]))

(comment (move 5 5 [4 3] :down))

(defn simulate-blizzard
  "First step will output a step of the simulation"
  [{height :height width :width system :system :as state}]
  (let [new-system (reduce (fn [new-system [position blizzards]]
                             (reduce (fn [new-system blizzard]
                                       (update new-system (move height width position blizzard) (partial cons blizzard)))
                                     new-system blizzards))
                           {} system)
        new-state (assoc state :system new-system)]
    (cons new-state (lazy-seq (simulate-blizzard new-state)))))

(comment
  (->> {:height 5,
        :width 5,
        :system
        {[1 0] [:right],
         [3 3] [:down]}}
       (simulate-blizzard)
       (take 5)
       (map render-system)
       (str/join "\n---------------\n")
       (println)))
(comment
  (->> {:height 4,
        :width 6,
        :system
        {[0 0] [:right],
         [0 1] [:right],
         [0 3] [:left],
         [0 4] [:up],
         [0 5] [:left],
         [1 1] [:left],
         [1 4] [:left],
         [1 5] [:left],
         [2 0] [:right],
         [2 1] [:down],
         [2 3] [:right],
         [2 4] [:left],
         [2 5] [:right],
         [3 0] [:left],
         [3 1] [:up],
         [3 2] [:down],
         [3 3] [:up],
         [3 4] [:up],
         [3 5] [:right]}}
       (simulate-blizzard)
       (take 5)
       (map render-system)
       (str/join "\n---------------\n")
       (println)))

(defn bfs' [blizzard-states states]
  ; Also check valid moves are within height and width (and not negative)
  (let [{height :height width :width system-next-frame :system} (first blizzard-states)
        new-states (->> states
                        (mapcat
                          (fn [[row col]]
                            ; 5 choices
                            ; up right down left
                            ; stay still
                            [[(dec row) col]
                             [row (inc col)]
                             [(inc row) col]
                             [row (dec col)]
                             [row col]]))
                        ; remove outside of boundaries
                        (filter (fn [[row col :as position]] (or (= [-1 0] position)
                                                                 (= [height (dec width)] position)
                                                                 (and (<= 0 row (dec height))
                                                                      (<= 0 col (dec width))))))
                        ; remove non-valid
                        (remove system-next-frame)
                        ; remove duplicates
                        (set))]
    (cons new-states (lazy-seq (bfs' (rest blizzard-states) new-states)))))

; will need to check these states until someone hits [row - 1, col - 1]
; Add 1 to minutes (or don't because of off by one errors)
(defn bfs [blizzard-states]
  (bfs' blizzard-states #{[-1 0]}))

(comment
  (->> {:height 4,
        :width 6,
        :system
        {[0 0] [:right],
         [0 1] [:right],
         [0 3] [:left],
         [0 4] [:up],
         [0 5] [:left],
         [1 1] [:left],
         [1 4] [:left],
         [1 5] [:left],
         [2 0] [:right],
         [2 1] [:down],
         [2 3] [:right],
         [2 4] [:left],
         [2 5] [:right],
         [3 0] [:left],
         [3 1] [:up],
         [3 2] [:down],
         [3 3] [:up],
         [3 4] [:up],
         [3 5] [:right]}}
       (simulate-blizzard)
       (bfs)
       (take 18)))

(defn run-app [input]
  (let [blizzard-states (->> input (parse-input) (simulate-blizzard))
        {height :height width :width} (first blizzard-states)
        states (take-while seq (bfs blizzard-states))]
    ; off by one error
    (+ 1 (count (take-while (fn [states] (nil? (states [height (dec width)]))) states)))))

(comment
  (run-app 
    (list "#.######"
          "#>>.<^<#"
          "#.<..<<#"
          "#>v.><>#"
          "#<^v^^>#"
          "######.#")))

; (defn -main []
;   (->> *in*
;        (java.io.BufferedReader.)
;        (line-seq)
;        (run-app)
;        (println)))

; will need to check these states until someone hits [row - 1, col - 1]
; Add 1 to minutes (or don't because of off by one errors)
(defn bfs2 [blizzard-states start]
  (bfs' blizzard-states #{start}))

(defn run-app2 [input]
  (let [blizzard-states (->> input (parse-input) (simulate-blizzard))
        {height :height width :width} (first blizzard-states)
        states (take-while seq (bfs2 blizzard-states [-1 0]))
        ; go goal
        first-part (count (take-while (fn [states] (nil? (states [height (dec width)]))) states))
        ; go back
        blizzard-states (drop first-part blizzard-states)
        states (take-while seq (bfs2 blizzard-states [height (dec width)]))
        second-part (count (take-while (fn [states] (nil? (states [-1 0]))) states))
        ; go goal again
        blizzard-states (drop second-part blizzard-states)
        states (take-while seq (bfs2 blizzard-states [-1 0]))
        third-part (count (take-while (fn [states] (nil? (states [height (dec width)]))) states))]
    ; off by two error
    (+ 1 first-part second-part third-part)))

(comment
  (run-app2
    (list "#.######"
          "#>>.<^<#"
          "#.<..<<#"
          "#>v.><>#"
          "#<^v^^>#"
          "######.#")))

(defn -main []
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (run-app2)
       (println)))

