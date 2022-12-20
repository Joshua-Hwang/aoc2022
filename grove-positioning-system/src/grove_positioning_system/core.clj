(ns grove-positioning-system.core
  (:gen-class))

(defn move
  "Removed from old-idx.
  What exists at new-idx will be pushed to a lesser index"
  [ls old-idx new-idx]
  (let [v (nth ls old-idx)
        v-removed (concat (take old-idx ls) (drop (inc old-idx) ls))]
    (concat (take new-idx v-removed) (cons v (drop new-idx v-removed)))))

(comment (move [0 11 22 33 44 55 66] 2 4))
(comment (move [0 11 22 33 44 55 66] 4 0))

(defn parse-input [input]
  (map parse-long input))

(comment (parse-input (list "1" "2" "-3" "3" "-2" "0" "4")))

(defn sort-cipher
  "cipher is of the form [[idx value] [idx value]...]"
  [cipher]
  (->> cipher
       (sort-by first)
       (map second)))

(comment (sort-cipher [[1 1] [2 2] [3 -3] [6 3] [0 -2] [5 0] [4 4]]))

; if old-idx < new-idx (move right)
; things in between old-idx and new-idx have to have decremented indices
; if old-idx > new-idx (move left)
; things in between have to have incremented indices

(defn mix' [idx cipher]
  (println idx)
  (if (= idx (count cipher))
    ; cipher done
    cipher
    (let [[old-pos value] (cipher idx)
          ; mod is always positive
          new-pos (mod (+ old-pos value) (dec (count cipher)))
          min-pos (min old-pos new-pos)
          max-pos (max old-pos new-pos)
          op (if (< old-pos new-pos) dec inc)
          new-cipher (assoc-in
                       (mapv (fn [[pos value :as original]]
                               (if (not (<= min-pos pos max-pos))
                                 original
                                 ; you'll notice I'm not doing modulo here
                                 ; left as an exercise to the reader
                                 [(op pos) value]))
                             cipher)
                       [idx 0] new-pos)]
      (recur (inc idx) new-cipher))))

(defn mix [cipher]
  (vec (sort-cipher (mix' 0 (vec (map-indexed vector cipher))))))

(defn run-app [input]
  (let [plain (mix (parse-input input))
        zero-idx (.indexOf plain 0)]
    (println plain)
    (->> [1000 2000 3000]
         (map #(mod (+ zero-idx %) (count plain)))
         (map plain)
         (apply +))))

(comment (run-app (list "1" "2" "-3" "3" "-2" "0" "4")))

; (defn -main []
;   (->> *in*
;        (java.io.BufferedReader.)
;        (line-seq)
;        (run-app)
;        (println)))

(defn mix2 [cipher]
  (->> cipher
       (map-indexed vector)
       (vec)
       (iterate (partial mix' 0))
       (#(nth % 10))
       (sort-cipher)
       (vec)))

(defn run-app2 [input]
  (let [plain (vec (mix2 (map (partial * 811589153) (parse-input input))))
        zero-idx (.indexOf plain 0)]
    (->> [1000 2000 3000]
         (map #(mod (+ zero-idx %) (count plain)))
         (map plain)
         (apply +))))

(comment (run-app2 (list "1" "2" "-3" "3" "-2" "0" "4")))

(defn -main []
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (run-app2)
       (println)))
