(ns boiling-boulders.core
  (:require [clojure.string :as str]
            [clojure.set :as set])
  (:gen-class))

(defn parse-line [line]
  (->> (str/split line #",")
       (mapv parse-long)))

(comment
  (parse-line "2,3,5"))

(defn parse-all [input]
  (->> input (map parse-line) (set)))

(comment
  (parse-all
    (list
      "2,2,2"
      "1,2,2"
      "3,2,2"
      "2,1,2"
      "2,3,2"
      "2,2,1"
      "2,2,3"
      "2,2,4"
      "2,2,6"
      "1,2,5"
      "3,2,5"
      "2,1,5"
      "2,3,5")))

(defn neighbour-cubes [cube]
  (for [side [inc dec] xyz [0 1 2]] (update cube xyz side)))

(comment (neighbour-cubes [0 0 0]))
(comment (neighbour-cubes [1 2 2]))

(defn calculate-exposed-faces [cubes]
  ((reduce
     (fn [{faces :faces cubes :cubes} new-cube]
       ; add 6 faces
       ; remove 2 for every touching cube
       (let [faces-hidden (* 2 (count (filter cubes (neighbour-cubes new-cube))))]
         {:faces (+ faces (- 6 faces-hidden))
          :cubes (conj cubes new-cube)}))
     {:faces 0 :cubes #{}}
     cubes)
   :faces))

(defn run-app [input]
  (->> input
       (map parse-line)
       (calculate-exposed-faces)))

(comment
  (run-app
    (list
      "2,2,2"
      "1,2,2"
      "3,2,2"
      "2,1,2"
      "2,3,2"
      "2,2,1"
      "2,2,3"
      "2,2,4"
      "2,2,6"
      "1,2,5"
      "3,2,5"
      "2,1,5"
      "2,3,5")))

; (defn -main []
;   (->> *in*
;        (java.io.BufferedReader.)
;        (line-seq)
;        (run-app)
;        (println)))

(defn x-coord [[x _ _]] x)
(defn y-coord [[_ y _]] y)
(defn z-coord [[_ _ z]] z)

; was used for checking how much space to investigate
; findings reveal dfs is possible
(defn check-bounds [cubes]
  (->> cubes
       (apply mapv vector)
       ((juxt
          (comp (partial apply min) x-coord)
          (comp (partial apply max) x-coord)
          (comp (partial apply min) y-coord)
          (comp (partial apply max) y-coord)
          (comp (partial apply min) z-coord)
          (comp (partial apply max) z-coord)))))

(comment
  (check-bounds
    #{[2 1 5]
      [2 2 4]
      [2 3 5]
      [2 2 6]
      [2 3 2]
      [2 2 2]
      [3 2 2]
      [2 2 1]
      [3 2 5]
      [2 2 3]
      [1 2 5]
      [2 1 2]
      [1 2 2]}))

; simplified to a cube for no good reason
; this exceeded the stack
(defn dfs [cubes bounds visited pos]
  (println "depth?")
  (if (or (not-every? #(<= (bounds 0) % (bounds 1)) pos) (cubes pos))
    ; collides or is out of bounds
    visited
    ; does not collide. Move on
    (let [new-visited (conj visited pos)]
      (->> (neighbour-cubes pos)
           (reduce (fn [visited new-pos] (conj visited (dfs cubes bounds visited new-pos))) new-visited)
           ; (map (partial dfs cubes bounds new-visited))
           ; (apply set/union)
           ))))

(defn bfs
  "make visited and positions sets"
  [cubes bounds visited positions]
  (if (empty? positions)
    visited
    ; assume all positions are valid
    ; thus positions can be added to visited
    (let [new-visited (apply conj visited positions)]
      (->> positions
           (mapcat neighbour-cubes)
           (set)
           ; remove out of bounds
           (filter (partial every? #(<= (bounds 0) % (bounds 1))))
           ; remove collisions
           (remove cubes)
           ; remove visited
           (remove new-visited)
           (recur cubes bounds new-visited)))))

(comment
  (bfs
    #{[2 1 5] [2 2 4] [2 3 5] [2 2 6] [2 3 2] [2 2 2] [3 2 2] [2 2 1] [3 2 5] [2 2 3] [1 2 5] [2 1 2] [1 2 2]}
    [1 6] #{} #{[1 1 1]}))

(comment (bfs #{[2 2 2]} [1 6] #{} #{[1 1 1]}))

(defn run-app2 [input]
  (let [cubes (parse-all input)
        ; these bounds will create an "external" shape
        ; see external-shape for more context
        bounds ((juxt
                  (comp dec (partial apply min))
                  (comp inc (partial apply max)))
                (check-bounds cubes))
        ; all sides on outside make a cube. Delete those faces
        side-length (inc (- (bounds 1) (bounds 0)))
        cube-faces (* 6 (* side-length side-length))
        start-pos (vector (bounds 0) (bounds 0) (bounds 0))
        external-shape (bfs cubes bounds #{} #{start-pos})
        external-shape-faces (calculate-exposed-faces external-shape)]
    (- external-shape-faces cube-faces)))

(comment
  (run-app2
    (list
      "2,2,2"
      "1,2,2"
      "3,2,2"
      "2,1,2"
      "2,3,2"
      "2,2,1"
      "2,2,3"
      "2,2,4"
      "2,2,6"
      "1,2,5"
      "3,2,5"
      "2,1,5"
      "2,3,5")))

(defn -main []
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (run-app2)
       (println)))
