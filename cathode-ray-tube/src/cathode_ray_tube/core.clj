(ns cathode-ray-tube.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn spy [arg] (println arg) arg)

(defn parse-instruction [line]
  (condp #(str/starts-with? %2 %1) line
    "noop" {:op :noop}
    "addx" {:op :addx :val (-> line (str/split #" " 2) (second) (Integer/parseInt))}))

(defn addx-seq [instructions]
  ((fn raddx [register-x cycle-num [instruction & instructions]]
     (case (get instruction :op)
       :noop (cons register-x
                   (lazy-seq (raddx register-x (inc cycle-num) instructions)))
       :addx (cons register-x
                   (cons register-x
                         (lazy-seq (raddx (+ (:val instruction) register-x) (+ 2 cycle-num) instructions))))
       (list register-x)))
   1 1 instructions))

(comment (take 100 (addx-seq '({:op :noop} {:op :addx, :val 3} {:op :addx, :val -5}))))

(defn run-app [input]
  (->> input
       (map parse-instruction)
       (addx-seq)
       (drop 19)
       (take-nth 40)
       (map * (iterate (partial + 40) 20))
       (apply +)))

(comment (iterate (partial + 40) 20))

(comment (run-app ["noop" "addx 3" "addx -5"]))
(comment ({:op :addx, :val 3} :op))

; (defn -main []
;   (->> *in*
;        (java.io.BufferedReader.)
;        (line-seq)
;        (run-app)
;        (println)))

(defn crt-print [sprite-locations]
  (map (fn [idx sprite-centre]
         (let [write-head (mod idx 40)
               dist (abs (- write-head sprite-centre))]
           (if (>= 1 dist) \# \.)))
       (range) sprite-locations))

(defn run-app2 [input]
  (->> input
       (map parse-instruction)
       (addx-seq)
       (crt-print)
       (partition 40)
       (map (partial apply str))
       (run! println)))

(comment (run-app2
           ["addx 15"
            "addx -11"
            "addx 6"
            "addx -3"
            "addx 5"
            "addx -1"
            "addx -8"
            "addx 13"
            "addx 4"
            "noop"
            "addx -1"
            "addx 5"
            "addx -1"
            "addx 5"
            "addx -1"
            "addx 5"
            "addx -1"
            "addx 5"
            "addx -1"
            "addx -35"
            "addx 1"
            "addx 24"
            "addx -19"
            "addx 1"
            "addx 16"
            "addx -11"
            "noop"
            "noop"
            "addx 21"
            "addx -15"
            "noop"
            "noop"
            "addx -3"
            "addx 9"
            "addx 1"
            "addx -3"
            "addx 8"
            "addx 1"
            "addx 5"
            "noop"
            "noop"
            "noop"
            "noop"
            "noop"
            "addx -36"
            "noop"
            "addx 1"
            "addx 7"
            "noop"
            "noop"
            "noop"
            "addx 2"
            "addx 6"
            "noop"
            "noop"
            "noop"
            "noop"
            "noop"
            "addx 1"
            "noop"
            "noop"
            "addx 7"
            "addx 1"
            "noop"
            "addx -13"
            "addx 13"
            "addx 7"
            "noop"
            "addx 1"
            "addx -33"
            "noop"
            "noop"
            "noop"
            "addx 2"
            "noop"
            "noop"
            "noop"
            "addx 8"
            "noop"
            "addx -1"
            "addx 2"
            "addx 1"
            "noop"
            "addx 17"
            "addx -9"
            "addx 1"
            "addx 1"
            "addx -3"
            "addx 11"
            "noop"
            "noop"
            "addx 1"
            "noop"
            "addx 1"
            "noop"
            "noop"
            "addx -13"
            "addx -19"
            "addx 1"
            "addx 3"
            "addx 26"
            "addx -30"
            "addx 12"
            "addx -1"
            "addx 3"
            "addx 1"
            "noop"
            "noop"
            "noop"
            "addx -9"
            "addx 18"
            "addx 1"
            "addx 2"
            "noop"
            "noop"
            "addx 9"
            "noop"
            "noop"
            "noop"
            "addx -1"
            "addx 2"
            "addx -37"
            "addx 1"
            "addx 3"
            "noop"
            "addx 15"
            "addx -21"
            "addx 22"
            "addx -6"
            "addx 1"
            "noop"
            "addx 2"
            "addx 1"
            "noop"
            "addx -10"
            "noop"
            "noop"
            "addx 20"
            "addx 1"
            "addx 2"
            "addx 2"
            "addx -6"
            "addx -11"
            "noop"
            "noop"
            "noop"]))

(defn -main []
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (run-app2)
       (println)))
