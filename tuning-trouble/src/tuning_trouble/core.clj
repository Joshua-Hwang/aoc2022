(ns tuning-trouble.core
  (:gen-class))

; (def xf
;   (comp
;     (filter odd?)
;     (map inc)
;     (take 5)))
; (transduce xf println [1 2 3 4 5 6 7])
; (sequence)

; NOTE clojure.lang.PersistentQueue/EMPTY is a thing for FIFO

(defn run-app [input]
  (reduce (fn [[c window] val]
            (if (apply distinct? window)
              (reduced [c window])
              [(inc c) (-> window (pop) (conj val))]))
          [4 (into clojure.lang.PersistentQueue/EMPTY (take 4 input))] (drop 4 input)))
(comment (run-app (map char "mjqjpqmgbljsphdztnvjfqwrcgsmlb"))) ; [7 <-(\j \p \q \m)-<]

(apply distinct? [1 2 3 1])


; (defn -main []
;   (println
;     (run-app (->> #(.read *in*)
;                   (repeatedly)
;                   (take-while (partial not= -1))
;                   (map char)))))

(defn run-app2 [input]
  (let [window-size 14]
    (reduce (fn [[c window] val]
              (if (apply distinct? window)
                (reduced [c window])
                [(inc c) (-> window (pop) (conj val))]))
            [window-size (into clojure.lang.PersistentQueue/EMPTY (take window-size input))]
            (drop window-size input))))

(defn -main []
  (println
    (run-app2 (->> #(.read *in*)
                   (repeatedly)
                   (take-while (partial not= -1))
                   (map char)))))
