(ns advent-of-code.2021.day-7
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.io :as io]))


(comment
  ;; Part 1

  (let [crabs (->>
               #_(string/split  "16,1,2,0,4,2,7,1,2,14" #",")
               (string/split (slurp (io/resource "2021/day-7")) #"," )

               (map read-string)
               )]
    (apply min (for [p (range 0 (apply max crabs))]
                 (->> crabs
                      (map (fn [crab] (Math/abs (- crab p))))
                      (reduce +))
                 ))
    )

  (->> (slurp (io/resource "2021/day-7"))
       )


  ;; Part 2


  (->> (slurp (io/resource "2021/day-6")) parse-fish (count-fish 256))




)
