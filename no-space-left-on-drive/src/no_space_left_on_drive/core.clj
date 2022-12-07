(ns no-space-left-on-drive.core
  (:require [clojure.string :as str])
  (:gen-class))

(def spy #(do (println "DEBUG:" %) %))

(defn change-directory [path dir]
  (case dir
    "/" []
    ".." (-> path pop pop)
    (conj path :dirs dir)))

; could be solved using (assoc path (count path) dirname)
(defn create-dir [fs path dirname]
  (assoc-in fs (conj path :dirs dirname) {:dirs {}, :files {}, :size 0}))

(defn add-size [fs path filesize]
  (if (empty? path)
    (update-in fs (conj path :size) + filesize)
    (recur (update-in fs (conj path :size) + filesize) (-> path pop pop) filesize)))

(defn create-file [fs path filename filesize]
  (-> fs
      (assoc-in (conj path :files filename) filesize)
      (add-size path filesize)))

(defn parse-fs [input]
  (first
    (reduce
      (fn [[fs path] line]
        (condp #(str/starts-with? %2 %1) line
          ; change path
          "$ cd" (let [[_ _ dir] (str/split line #" " 3)] [fs (change-directory path dir)])
          "$ ls" [fs path]
          ; it's a directory
          "dir " (let [[_ dir] (str/split line #" " 2)] [(create-dir fs path dir) path])
          ; it's a file
          (let [[filesize filename] (str/split line #" " 2) filesize (Integer/parseInt filesize)]
            [(create-file fs path filename filesize) path])))
      ;; using :dirs and :files because I don't know how to do introspection
      [{:dirs {}, :files {}, :size 0} []] input)))

; not a good idea
(defn collect-sizes [fs]
  (cons
    (:size fs)
    (flatten (map collect-sizes (-> fs
                                    (get :dirs)
                                    (vals))))))

(defn run-app [input]
  (as-> input it
    (-> it
        (parse-fs)
        (collect-sizes))
    (->> it
         (filter (partial >= 100000))
         (apply +))))

(run-app
  ["$ cd /"
   "$ ls"
   "dir a"
   "14848514 b.txt"
   "8504156 c.dat"
   "dir d"
   "$ cd a"
   "$ ls"
   "dir e"
   "29116 f"
   "2557 g"
   "62596 h.lst"
   "$ cd e"
   "$ ls"
   "584 i"
   "$ cd .."
   "$ cd .."
   "$ cd d"
   "$ ls"
   "4060174 j"
   "8033020 d.log"
   "5626152 d.ext"
   "7214296 k"])

; (defn -main [] (println (run-app (line-seq (java.io.BufferedReader. *in*)))))

(defn run-app2 [input]
  (let [directories (-> input
                        (parse-fs)
                        (collect-sizes))
        ; calculation works out this way
        ; available = 70000000 - used
        ; to-delete = 30000000 - available
        to-delete (- (first directories) 40000000)]
    (->> directories
         (sort)
         (drop-while (partial >= to-delete))
         (first))))

(run-app2
  ["$ cd /"
   "$ ls"
   "dir a"
   "14848514 b.txt"
   "8504156 c.dat"
   "dir d"
   "$ cd a"
   "$ ls"
   "dir e"
   "29116 f"
   "2557 g"
   "62596 h.lst"
   "$ cd e"
   "$ ls"
   "584 i"
   "$ cd .."
   "$ cd .."
   "$ cd d"
   "$ ls"
   "4060174 j"
   "8033020 d.log"
   "5626152 d.ext"
   "7214296 k"])

(defn -main [] (println (run-app2 (line-seq (java.io.BufferedReader. *in*)))))
