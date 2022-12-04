(ns calorie-counting.core
  (:gen-class))

(defn collect-inventories [lines]
  (
   reduce (fn [coll val]
            (if val
              (conj (pop coll) (conj (peek coll) val))
              (conj coll [])
              ))
   [[]] lines
   ))

(defn int-or-nil [val] (try (Integer/parseInt val) (catch NumberFormatException _)))

; Advent of Code has a part 1 and part 2
(defn run-app1 [in]
  (let
    [input ((comp collect-inventories (partial map int-or-nil)) in)]
    (apply max (map (partial apply +) input))))

(comment (run-app1 ["1000" "2000" "3000" "" "4000" "5000" "" "6000"]))

; (defn -main [] (println (run-app1 (line-seq (java.io.BufferedReader. *in*)))))

(defn run-app2 [in]
  (let
    [input (->> in (map int-or-nil) collect-inventories)]
    (->> input
         (map (partial apply +))
         sort
         reverse
         (take 3)
         (apply +))))

(defn -main [] (println (run-app2 (line-seq (java.io.BufferedReader. *in*)))))
