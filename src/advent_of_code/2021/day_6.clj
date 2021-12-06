(ns advent-of-code.2021.day-6
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.io :as io]))


(defn parse-fish [input]
  (->> (string/split input #",")
       (map read-string) (into [])
       frequencies) )

(defn simulate-fish [days fish]
  (->> (iterate inc 0)
       (take days)
       (reduce (fn [fs _]
                 (->> fs
                      (mapcat (fn [f] (if (= f 0) [6 8] [(dec f)] ) ) )
                      (into [])

                      ) )
               fish)
       )
  )

  (defn count-fish [days fish]
    (->> (iterate (fn [fish]
                    (-> fish
                        (assoc 0 (get fish 1 0))
                        (assoc 1 (get fish 2 0))
                        (assoc 2 (get fish 3 0))
                        (assoc 3 (get fish 4 0))
                        (assoc 4 (get fish 5 0))
                        (assoc 5 (get fish 6 0))
                        (assoc 6 (+ (get fish 7 0) (get fish 0 0)))
                        (assoc 7 (+ (get fish 8 0)))
                        (assoc 8 (get fish 0 0))))
                  fish)
         (take (inc days))
         (last)
         (vals)
         (reduce + 0)
         ))

(comment
  ;; Part 1


  (->>  "3,4,3,1,2" parse-fish (count-fish 18))
  ;; => 26

  (->>  "3,4,3,1,2" parse-fish (count-fish 80))
  ;; => 5934

  (->> (slurp (io/resource "2021/day-6")) parse-fish (count-fish 80))
  ;; => 372300


  ;; Part 2

  (->> "3,4,3,1,2" parse-fish (count-fish 256))
;; => 26984457539

  (->> (slurp (io/resource "2021/day-6")) parse-fish (count-fish 256))
;; => 1675781200288
  ;; => nil
  )
