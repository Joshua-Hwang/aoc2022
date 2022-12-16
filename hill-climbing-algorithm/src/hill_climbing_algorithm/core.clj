(ns hill-climbing-algorithm.core
  (:require [clojure.string :as str]
            [clojure.set :as set])
  (:gen-class))

(defn find-coords [hmap c]
  (->> hmap
       (map (fn [row line]
              [row (str/index-of line c)])
            (iterate inc 0))
       (filter second)
       (first)))

(comment
  (find-coords
    ["Sabqponm"
     "abcryxxl"
     "accszExk"
     "acctuvwj"
     "abdefghi"]
    \E))

(defn valid-moves [hmap coord]
  (let [h (get-in hmap coord)
        nesw [[-1 0] [0 1] [1 0] [0 -1]]]
    (->> nesw
         (map (partial mapv + coord))
         ; make sure the coord actually exists
         (filter (partial get-in hmap))
         (filter #(<= (get-in hmap %) (inc h))))))

(comment
  (valid-moves
    [[83 97 98 113 112 111 110 109]
     [97 98 99 114 121 120 120 108]
     [97 99 99 115 122 69 120 107]
     [97 99 99 116 117 118 119 106]
     [97 98 100 101 102 103 104 105]]
    [2 3]))

(defn bfs [hmap visited visiting]
  ; take visiting
  ; map valid-moves visiting
  ; flatten
  ; filter (partial contains? visited)
  ; bfs hmap (into visited visiting)
  (->> visiting
       (map (partial valid-moves hmap))
       (apply concat)
       (filter (partial (complement contains?) visited))
       (set)
       (#(cons % (lazy-seq (bfs hmap (into visited visiting) %))))))

(comment
  (bfs
    [[83 97 98 113 112 111 110 109]
     [97 98 99 114 121 120 120 108]
     [97 99 99 115 122 69 120 107]
     [97 99 99 116 117 118 119 106]
     [97 98 100 101 102 103 104 105]]
    #{[3 2]} #{[2 3] [3 3]}))

(comment
  (take 5
        (bfs
          [[83 97 98 113 112 111 110 109]
           [97 98 99 114 121 120 120 108]
           [97 99 99 115 122 69 120 107]
           [97 99 99 116 117 118 119 106]
           [97 98 100 101 102 103 104 105]]
          #{[3 2]} #{[2 3] [3 3]})))


; input is expected to be vector of strings
(defn run-app [input]
  (let [start (find-coords input \S)
        end (find-coords input \E)
        hmap (->> input
                  (map #(str/replace % #"S" "a"))
                  (map #(str/replace % #"E" "z"))
                  (mapv (partial mapv int)))]
    ; we'll carry a "visited" set and a "visiting" set
    ; classic BFS
    ; we generate this as a lazy sequence
    ; count all steps except the one that arrives at E
    ; visiting start is step 1
    ; visiting doesn't count as a step
    ; filter till end is in "visiting"
    ; return the number of iterations taken
    (->> (bfs hmap #{} #{start})
         (map vector (iterate inc 1))
         (filter (fn [[_ visiting]] (contains? visiting end)))
         (first)
         (first))))

(comment
  (run-app
    '("Sabqponm"
       "abcryxxl"
       "accszExk"
       "acctuvwj"
       "abdefghi")))

; (defn -main []
;   (->> *in*
;        (java.io.BufferedReader.)
;        (line-seq)
;        (vec)
;        (run-app)
;        (println)))

(defn find-coords2 [hmap c]
  (->> hmap
       (map (fn [row line]
              [row (str/index-of line c)])
            (iterate inc 0))
       (filter second)))

; input is expected to be vector of strings
(defn run-app2' [hmap start end]
  (->> (bfs hmap #{} #{start})
       (map vector (iterate inc 1))
       (filter (fn [[_ visiting]] (contains? visiting end)))
       (first)
       (first)))

(defn run-app2 [input]
  (let [starts (concat (find-coords2 input \S) (find-coords2 input \a))
        end (first (find-coords2 input \E))
        hmap (->> input
                  (map #(str/replace % #"S" "a"))
                  (map #(str/replace % #"E" "z"))
                  (mapv (partial mapv int)))]
    (->> starts
         (map #(run-app2' hmap % end))
         (apply min))))

(defn -main []
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (vec)
       (run-app2)
       (println)))
