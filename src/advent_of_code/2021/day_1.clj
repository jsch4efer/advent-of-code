(ns advent-of-code.2021.day-1
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn compute-depth-two [measurements]
  (->> measurements
       (partition 2 1)
       (map (partial apply <=))
       (filter identity)
       (count)
       ))

(defn compute-depth-three [measurements]
  (->> measurements
       (partition 3 1)
       (map (partial reduce +))
       (partition 2 1)
       (map (partial apply <))
       (filter identity)
       (count)
       ))

(comment

  (def example-data [199 200 208 210 200 207 240 269 260 263])

  (def problem-data (->> (slurp (io/resource "2021/day-1"))
               (string/split-lines)
               (map read-string)))

  ;; Part 1

  (compute-depth-two example-data)
;; => 7

  (compute-depth-two problem-data)
;; => 1709

  ;; Part 2

  (compute-depth-three example-data)
;; => 5

  (compute-depth-three problem-data)
;; => 1761

  )
