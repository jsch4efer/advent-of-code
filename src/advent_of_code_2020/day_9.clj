(ns advent-of-code-2020.day-9
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer :all]))



;; Preamble of 25 numbers
;; After that: Each number shoulbe be the sum of any tow of the 25 immediately previous numbers (window?)
;; Two numbers have a different values and solution is not unique
;; Used numbers fall out of the number set
;; Find first number in the list that does not have this property!


(def example-input [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])

(def input (->> (slurp (io/resource "day-9"))
                (string/split-lines)
                (map read-string)
                (into [])))

(defn- all-sum-pairs [numbers]
  (->> (for [i (range 0 (count numbers))
             j (range 0 (count numbers))]
         [j i])
       (filter #(not= (first %) (second %)))
       (map (fn [[i j]]
              [(+ (nth numbers i) (nth numbers j)) #{i j}]))
       (reduce (fn [result [sum indices]] (update result sum #(conj (or % #{}) indices))) {})))

(defn- sum-contained? [sum-set sum indicies]
  (some
   #(<= (first (sort indicies)) (first (sort %)) (last (sort %)) (last (sort indicies)))
   (get sum-set sum)))

(defn find-first-violation [numbers preamble-length]
  (let [sum-pair-set (all-sum-pairs numbers)]
    (loop [[start end] [0 (dec preamble-length)]
           candidate (inc end)]
      (if (sum-contained? sum-pair-set (nth numbers candidate) #{start end})
        (if (< (inc candidate) (count numbers))
          (recur [(inc start) candidate] (inc candidate))
          :no-violation-found)
        {:violation (nth numbers candidate)
         :position candidate}))))

(defn find-fix [numbers {:keys [position violation]}]
  (loop [fix-size 2]
    (when (< fix-size position)
      (if-let [fix (->> (partition fix-size 1 numbers)
                        (map (fn [possible-fix] [possible-fix (reduce + possible-fix)]))
                        (filter #(= (second %) violation))
                        (map first)
                        (first))]
        (+ (apply min fix) (apply max fix))
        (recur (inc fix-size))))))

(comment

  ;; Part 1

  (find-first-violation example-input 5)
 ;; => {:violation 127, :position 14}

  (find-first-violation input 25)
 ;; => {:violation 257342611, :position 575}



 ;; Part 2


  (find-fix example-input (find-first-violation example-input 5))
 ;; => 62

  (find-fix input (find-first-violation input 25))
 ;; => 35602097
  )
