(ns advent-of-code.2021.day-7
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(defn parse-crab-positions [input]
  (->>  (string/split input #",") (map read-string)))

(defn find-minimum-steps [crabs cost-fn]
    (apply min (for [p (range 0 (apply max crabs))]
                 (->> crabs
                      (map (partial cost-fn p ))
                      (reduce +))
                 ))
      )

(comment
  ;; Part 1

  (def example-input   "16,1,2,0,4,2,7,1,2,14")

  (def input (slurp (io/resource "2021/day-7")))

  (def cost-fn-part1 (fn [pos crab] (Math/abs (- pos crab))))

  (find-minimum-steps (parse-crab-positions example-input) cost-fn-part1)
;; => 37

  (find-minimum-steps (parse-crab-positions input) cost-fn-part1)
;; => 331067

  ;; Part 2

  (def cost-fn-part2 (fn [pos crab] (let [n (Math/abs (- crab pos))]
                                        (/ (* (+ n 1) n) 2)) ))

  (find-minimum-steps (parse-crab-positions example-input) cost-fn-part2)
;; => 168

  (find-minimum-steps (parse-crab-positions input) cost-fn-part2)
;; => 92881128

)
