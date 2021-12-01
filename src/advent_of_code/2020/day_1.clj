(ns advent-of-code-2020.day-1
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

;; Part 1

(defn find-pair [elements]
  (->> elements
       (map-indexed (fn [i n]
                      (->> elements
                           (take-last (- (count elements) i))
                           (map (fn [x] [n x]))
                           (filter (fn [[a b]] (not= a b))))))
       (apply concat)
       (filter (fn [[a b]] (= (+ a b) 2020)))
       (first)))

(defn solve-part-1 [data]
  (apply * (find-pair data)))

(comment
  (solve-part-1 [1721
                 979
                 366
                 299
                 675
                 1456])
  ;; => 514579

  (solve-part-1 (->> (slurp (io/resource "day-1"))
                     (s/split-lines)
                     (map (partial read-string))))
  ;; => 1019571
)


;; Part 2


(defn find-triple [elements]
  (->> (for [a elements
             b elements
             c elements] [a b c])
       (filter (fn [v] (= (count (set v)) 3)))
       (filter (fn [[a b c]] (= (+ a b c) 2020)))
       (first)))

(defn solve-part-2 [data]
  (apply * (find-triple data)))

(comment
  (solve-part-2 [1721
                 979
                 366
                 299
                 675
                 1456])
  ;; => 241861950

  (solve-part-2 (->> (slurp (io/resource "day-1"))
                     (s/split-lines)
                     (map (partial read-string))))
  ;; => 100655544
)
