(ns proboscidea-volcanium.core
  (:gen-class))

; many valves have 0 flow rate. Shorten the graph by condensing these and making movement time their aggregation.

(defn parse-line [line]
  (->> line
       (re-seq #"[A-Z][A-Z]|\d+")
       ((fn [[valve rate & leads-to]]
          {:valve-name valve
           :rate (parse-long rate)
           :leads-to (zipmap leads-to (repeat 1))}))))

(comment (parse-line "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"))

(defn build-system [input]
  (->> input
       (map parse-line)
       (#(zipmap (map :valve-name %) %))))

(comment
  (build-system
    (list
      "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
      "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
      "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
      "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
      "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
      "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
      "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
      "Valve HH has flow rate=22; tunnel leads to valve GG"
      "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
      "Valve JJ has flow rate=21; tunnel leads to valve II")))

(defn compress-valve 
  "Assumes only two :leads-to"
  [graph valve-to-delete-name]
  (as-> graph g
    (let [valve-to-delete (g valve-to-delete-name)
          connected-valves (:leads-to valve-to-delete)]
      (reduce
        (fn [g [valve-name time-to-valve-to-delete]]
          (update-in g [valve-name :leads-to]
                     ; remove valve from leads-to
                     ; remember time-taken
                     ; add new-connection with time + above
                     ; if path to other-valve exists choose minimum time-taken
                     (fn [valve-leads]
                       (reduce
                         (fn [valve-leads [new-connection-name new-connection-time]]
                           (update valve-leads new-connection-name
                                   (fn [existing-connection-time]
                                     (min
                                       (+ time-to-valve-to-delete new-connection-time)
                                       (if (nil? existing-connection-time) ##Inf existing-connection-time)))))
                         (dissoc valve-leads (:valve-name valve-to-delete))
                         (filter (fn [[v-name _]] (not= v-name valve-name)) connected-valves)))))
        g connected-valves))
    (dissoc g valve-to-delete-name)))

(comment
  (compress-valve
    {"FF" {:valve-name "FF", :rate 0, :leads-to {"EE" 2, "GG" 3}},
     "GG" {:valve-name "GG", :rate 0, :leads-to {"FF" 3, "HH" 1}},
     "EE" {:valve-name "EE", :rate 3, :leads-to {"FF" 2, "DD" 1}}}
    "FF"))

(comment
  (compress-valve
    {"FF" {:valve-name "FF", :rate 0, :leads-to {"EE" 2, "GG" 3}},
     "GG" {:valve-name "GG", :rate 0, :leads-to {"FF" 3, "HH" 1, "EE" 2}},
     "EE" {:valve-name "EE", :rate 3, :leads-to {"FF" 2, "DD" 1, "GG" 2}}}
    "FF"))

(defn compress-graph
  "reduce all nodes with rate 0 except AA.
  All with rate=0 connect two nodes"
  [graph]
  (let [damaged-valves (->> graph
                            (vals)
                            (filter #(= 0 (:rate %)))
                            (map :valve-name)
                            (filter (partial not= "AA")))]
    (reduce compress-valve graph damaged-valves)))

(comment
  (compress-graph
    {"JJ" {:valve-name "JJ", :rate 21, :leads-to {"II" 1}},
     "HH" {:valve-name "HH", :rate 22, :leads-to {"GG" 1}},
     "FF" {:valve-name "FF", :rate 0, :leads-to {"EE" 1, "GG" 1}},
     "GG" {:valve-name "GG", :rate 0, :leads-to {"FF" 1, "HH" 1}},
     "DD" {:valve-name "DD", :rate 20, :leads-to {"CC" 1, "AA" 1, "EE" 1}},
     "CC" {:valve-name "CC", :rate 2, :leads-to {"DD" 1, "BB" 1}},
     "II" {:valve-name "II", :rate 0, :leads-to {"AA" 1, "JJ" 1}},
     "BB" {:valve-name "BB", :rate 13, :leads-to {"CC" 1, "AA" 1}},
     "AA" {:valve-name "AA", :rate 0, :leads-to {"DD" 1, "II" 1, "BB" 1}},
     "EE" {:valve-name "EE", :rate 3, :leads-to {"FF" 1, "DD" 1}}}))

(defn fw
  "https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm"
  [graph]
  (let [nodes (keys graph)]
    (reduce
      (fn [graph [k i j]]
        (let [dist-ij (get-in graph [i :leads-to j] ##Inf)
              dist-ik (get-in graph [i :leads-to k] ##Inf)
              dist-kj (get-in graph [k :leads-to j] ##Inf)]
          (if (> dist-ij (+ dist-ik dist-kj))
            (-> graph
                (assoc-in [i :leads-to j] (+ dist-ik dist-kj))
                (assoc-in [j :leads-to i] (+ dist-ik dist-kj)))
            graph)))
      graph
      (for [k nodes
            i nodes
            j nodes
            :when (and (not= k i) (not= k j) (not= i j))]
        [k i j]))))

(defn bfs
  "Graph must have passed through fw
  Set states to location AA and opened to contain AA. This prevents exploring opening the valve and wasting time.
  states is a collection of {:location \"AA\" :pressure 123 :opened #{\"AA\" \"GG\" \"HH\"} :sleep-till 3}
  concept of sleep has to be put in because we compressed the graph"
  [graph states minute]
  ; pressure can be calculated by (* minutes-left flow-rate)
  ; when all valves are open set sleep-till 30
  ; minutes start at 1 end at 30 (not before 30)
  ; if opened in minute 2 only start releasing at minute 3 thus minutes-left = 30 - minutes
  ; staying still is never a good option
  ; assume graph passed through fw so never return to any :opened valves
  (if (< 30 minute)
    '()
    (let [minutes-left (- 30 minute)
          active-states (filter #(>= minute (:sleep-till %)) states)
          future-states (filter #(< minute (:sleep-till %)) states)]
      (->> active-states
           ; generate possible moves from current state
           (map (fn [{location :location
                      pressure :pressure
                      opened :opened}]
                  (if (not (contains? opened location))
                    ; open valve at current location
                    (let [new-pressure (+ pressure (* minutes-left (get-in graph [location :rate])))
                          new-opened (conj opened location)]
                      (list
                        {:location location
                         :pressure new-pressure
                         :opened new-opened
                         :sleep-till (if (= (count graph) (count new-opened)) 31 (inc minute))}))
                    ; travel (as long as not visited)
                    (->> (get-in graph [location :leads-to])
                         (filter (fn [[new-location _]] (not (contains? opened new-location))))
                         (map (fn [[new-location time-to-new-location]]
                                {:location new-location
                                 :pressure pressure
                                 :opened opened
                                 :sleep-till (+ minute time-to-new-location)}))))))
           ; flatten out states
           (apply concat)
           (concat future-states)
           (#(cons % (lazy-seq (bfs graph % (inc minute)))))))))

(defn run-app [input]
  (let [graph (->> input (build-system) (compress-graph) (fw))]
    graph
    (->>(bfs graph
             [{:location "AA"
               :pressure 0
               :opened #{"AA"}
               :sleep-till 1}]
             1)
             (last)
             (map :pressure)
             (apply max))))

(comment
  (run-app
    (list
      "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
      "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
      "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
      "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
      "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
      "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
      "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
      "Valve HH has flow rate=22; tunnel leads to valve GG"
      "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
      "Valve JJ has flow rate=21; tunnel leads to valve II")))

; (defn -main []
;   (->> *in*
;        (java.io.BufferedReader.)
;        (line-seq)
;        (run-app)
;        (println)))

(defn bfs2
  "Graph must have passed through fw
  Set states to location AA and opened to contain AA. This prevents exploring opening the valve and wasting time.
  states is a collection of {:location \"AA\" :elephant-location \"AA\" :pressure 123 :opened #{\"AA\" \"GG\" \"HH\"} :sleep-till 3 :elephant-sleep-till 4}
  concept of sleep has to be put in because we compressed the graph"
  [graph states minute]
  (if (< 30 minute)
    '()
    (let [minutes-left (- 30 minute)
          best-pressure (apply max 0 (map :pressure states))
          ; best-case is the state turns on all unopened valves right now
          ; states (filter (fn [{opened :opened, pressure :pressure}]
          ;                  (let [unopened (map val (remove #(opened (key %)) graph))
          ;                        optimistic-pressure (+ pressure (apply + (map * (reverse (sort (map :rate unopened))) (#(interleave % %) (range minutes-left 0 -1)))))]
          ;                    (<= best-pressure optimistic-pressure)))
          ;                states)
          ; states (take 15 (sort (fn [{a :pressure} {b :pressure}] (compare a b)) states))
          ; active-states (filter #(or (>= minute (:elephant-sleep-till %)) (>= minute (:sleep-till %))) states)
          ; read later people were culling ridiculous amounts. Fair enough.
          active-states (take 300 (sort (fn [{a :pressure} {b :pressure}] (compare b a)) (filter #(or (>= minute (:elephant-sleep-till %)) (>= minute (:sleep-till %))) states)))
          future-states (filter #(and (< minute (:elephant-sleep-till %)) (< minute (:sleep-till %))) states)]
      (println minute (count states))
      (->> active-states
           ; generate human moves
           (map (fn [{location :location
                      elephant-location :elephant-location
                      pressure :pressure
                      opened :opened
                      sleep-till :sleep-till
                      elephant-sleep-till :elephant-sleep-till
                      :as state}]
                  (if (< minute sleep-till)
                    ; active-states that only involve the elephant need to be present
                    (list state)
                    (if (not (contains? opened location))
                      ; open valve at current location
                      (let [new-pressure (+ pressure (* minutes-left (get-in graph [location :rate])))
                            new-opened (conj opened location)]
                        (list
                          {:location location
                           :elephant-location elephant-location
                           :pressure new-pressure
                           :opened new-opened
                           :sleep-till (if (= (count graph) (count new-opened)) 31 (inc minute))
                           :elephant-sleep-till (if (= (count graph) (count new-opened)) 31 elephant-sleep-till)}))
                      ; travel (as long as not visited)
                      (->> (get-in graph [location :leads-to])
                           (filter (fn [[new-location _]] (not (contains? opened new-location))))
                           (map (fn [[new-location time-to-new-location]]
                                  {:location new-location
                                   :elephant-location elephant-location
                                   :pressure pressure
                                   :opened opened
                                   :sleep-till (+ minute time-to-new-location)
                                   :elephant-sleep-till elephant-sleep-till})))))))
           ; flatten out states
           (apply concat)
           ; generate elephant moves
           (map (fn [{location :location
                      elephant-location :elephant-location
                      pressure :pressure
                      opened :opened
                      sleep-till :sleep-till
                      elephant-sleep-till :elephant-sleep-till
                      :as state}]
                  (if (< minute elephant-sleep-till)
                    ; active-states that only involve the human need to be present
                    (list state)
                    (if (not (contains? opened elephant-location))
                      ; open valve at current elephant location
                      (let [new-pressure (+ pressure (* minutes-left (get-in graph [elephant-location :rate])))
                            new-opened (conj opened elephant-location)]
                        (list
                          {:location location
                           :elephant-location elephant-location
                           :pressure new-pressure
                           :opened new-opened
                           :sleep-till (if (= (count graph) (count new-opened)) 31 sleep-till)
                           :elephant-sleep-till (if (= (count graph) (count new-opened)) 31 (inc minute))}))
                      ; travel (as long as not visited)
                      (->> (get-in graph [elephant-location :leads-to])
                           (filter (fn [[new-location _]] (not (contains? opened new-location))))
                           ; don't go to where the human is going
                           ; "what if the elephant get there faster?"
                           ; in the other situaton where the elephant and human positions are swapped will be equivalent
                           (filter (fn [[new-location _]] (not= location new-location)))
                           (map (fn [[new-location time-to-new-location]]
                                  {:location location
                                   :elephant-location new-location
                                   :pressure pressure
                                   :opened opened
                                   :sleep-till sleep-till
                                   :elephant-sleep-till (+ minute time-to-new-location)})))))))
           ; flatten out states
           (apply concat)
           (concat future-states)
           (#(cons % (lazy-seq (bfs2 graph % (inc minute)))))))))

(defn run-app2 [input]
  (let [graph (->> input (build-system) (compress-graph) (fw))]
    graph
    (->>(bfs2 graph
              [{:location "AA"
                :elephant-location "AA"
                :pressure 0
                :opened #{"AA"}
                :sleep-till 5
                :elephant-sleep-till 5}]
              5)
              ; (take 3)
              (last)
              (map :pressure)
              (apply max)
              )))

(comment
  (run-app2
    (list
      "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
      "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
      "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
      "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
      "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
      "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
      "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
      "Valve HH has flow rate=22; tunnel leads to valve GG"
      "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
      "Valve JJ has flow rate=21; tunnel leads to valve II")))

(defn -main []
  (->> *in*
       (java.io.BufferedReader.)
       (line-seq)
       (run-app2)
       (println)))
