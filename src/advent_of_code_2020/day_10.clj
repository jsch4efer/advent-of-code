(ns advent-of-code-2020.day-10
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer :all])
  (:import java.lang.Math))


(defn build-adapter-chain [adapters]
  (let [sorted-adapters (sort adapters)
        device-joltage (+ (last sorted-adapters) 3)]
    (loop [adapter-chain [0]]
      (let [input (last adapter-chain)]
        (if-let [candidate-output  (->> sorted-adapters
                                        (filter (fn [output](<= 1 (- output input) 3)))
                                        (first))]
          (recur (conj adapter-chain candidate-output))
          (conj adapter-chain device-joltage)
          )))))

(defn joltage-differences [adapter-chain]
  (->> adapter-chain
   (partition 2 1)
   (map (fn [[left right]] (- right left)))
   ))

(defn joltage-difference-distribution [adapter-chain]
  (frequencies (joltage-differences adapter-chain)))

(def example-input-1 [16 10 15 5 1 11 7 19 6 12 4])

(def example-input-2 [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])

(def input (->> "day-10"
                io/resource
                slurp
                string/split-lines
                (map read-string)))

(comment
  ;; Part 1

  (joltage-difference-distribution (build-adapter-chain example-input-1))
  ;; => {1 7, 3 5}

  (joltage-difference-distribution (build-adapter-chain example-input-2))
  ;; => {1 22, 3 10}

  (->> (joltage-difference-distribution (build-adapter-chain input))
       (vals)
       (reduce *))
  ;; => 2475


  ;; Part 2

  (defn find-optional-adapters [chain length]
    (->> (range 0 (dec (count chain)))
         (partition (+ length 2) 1)
         (filter (fn [subchain] (<= 1 (- (nth chain (last subchain)) (nth chain (first subchain))) 3)))
         (map #(disj (set (rest %)) (last %)))
         ))

  (defn- build-chains-by-size [chain]
    (loop [length 1
           opt-seq-by-size {}]
      (let [optionals (find-optional-adapters chain length)]
        (if (not-empty optionals)
          (recur (inc length) (assoc opt-seq-by-size length (->> chain
                                                                 (map-indexed (fn [i _] (if (some #(contains? % i) optionals) 1 0)) )
                                                                 )))
          opt-seq-by-size
          )
        ))
    )


  (def opts-by-size-example-1 (build-chains-by-size (build-adapter-chain example-input-1)))

  (get opts-by-size-example-1 3)
  ;; => (0 0 0 1 1 0 0 0 0 0 0 0 0)
  ;; => (0 0 0 1 1 0 0 1 0 0 0 0 0)

  (*           4       2)
  ;; => 8


  (def opts-by-size-example-2 (build-chains-by-size (build-adapter-chain example-input-2)))

  (get opts-by-size-example-2 3)
  ;; => (0 1 1 1 0 0 1 1 1 0 0 0 1 1 0 0 0 0 0 0 1 1 1 0 0 0 0 0 1 1 1 0 0)
  ;; => (0 1 1 1 0 0 1 1 1 0 0 0 1 1 0 0 1 0 0 0 1 1 1 0 0 0 0 0 1 1 1 0 0)

  (*         7        7           4      2         7              7 )
  ;; => 19208


  (def opts-by-size (build-chains-by-size (build-adapter-chain input)))


  (get opts-by-size 3)

  ;; => (0 1 1 0 0 0 1 1 0 0 0 0 0 0 0 1 1 0 0 1 1 1 0 0 0 0 0 1 1 0 0 1 1 1 0 0 1 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 1 1 0 0 0 0 0 1 1 1 0 0 1 1 0 0 0 1 1 1 0 0 1 1 0 0 1 1 1 0 0)
  ;; => (0 1 1 0 0 0 1 1 0 0 1 0 0 0 0 1 1 0 0 1 1 1 0 0 1 0 0 1 1 0 0 1 1 1 0 0 1 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 0 0 1 0 0 1 0 0 0 0 0 0 1 1 1 0 0 0 1 1 0 0 1 0 0 1 1 1 0 0 1 1 0 0 0 1 1 1 0 0 1 1 0 0 1 1 1 0 0)

  (*        4        4       2          4       7        2      4      7          7        4       4       4       4       4        2     2                7        4       2       7        4           7      4         7    )
  ;; => 442136281481216
  )
