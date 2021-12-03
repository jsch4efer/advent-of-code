(ns advent-of-code.2021.day-3
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))


(def example-diagnostics "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(defn parse-diagnostic-report [input]
  (->> (string/split-lines input)))

(defn most-common-bits [diagonistic-report pos]
  (let [freqs (->> diagonistic-report
                   (map #(get % pos))
                   (frequencies))]
    (cond
      (< (get freqs \1 0) (get freqs \0 0)) \0
      (>= (get freqs \1 0) (get freqs \0 0)) \1
      )
    ))

(defn least-common-bits [diagnostic-report pos]
  (case (most-common-bits diagnostic-report pos)
    \1 \0
    \0 \1)
  )

(defn gamma-rate [diagnostic-report]
  (Integer/parseInt (->>
                    (range 0 (-> diagnostic-report first count))
                    (map (partial most-common-bits diagnostic-report))
                    (string/join "")
                    )
                    2)
  )

(defn epsilon-rate [diagnostic-report]
  (Integer/parseInt (->>
                    (range 0 (-> diagnostic-report first count))
                    (map (partial least-common-bits diagnostic-report))
                    (string/join "")
                    )
                    2)
  )

(defn power-of-consumption [diag-report]
  (* (gamma-rate diag-report)
       (epsilon-rate diag-report))
  )


(->> example-diagnostics
     parse-diagnostic-report
     power-of-consumption
     )
;; => 198


(def input (slurp (io/resource "2021/day-3")))

(->> input
     parse-diagnostic-report
     power-of-consumption)
;; => 3847100





(defn filter-pos [diag-report pos bit-criteria]
  (->> diag-report
       (filter #(-> (get % pos) bit-criteria))
       )
  )

(defn oxygen-gen-rating [diag-report]
  (Integer/parseInt
   (->> (range 0 (-> diag-report first count))
        (reduce (fn [remaining pos]
                  (if (= 1 (count remaining))
                    remaining
                    (let [criteria (most-common-bits remaining pos)]
                      (filter-pos remaining pos (partial = criteria))
                      ))
                  )
                diag-report)
        first
        )
   2)
    )

(defn c02-scrubber-rating [diag-report]
  (Integer/parseInt
   (->> (range 0 (-> diag-report first count))
        (reduce (fn [remaining pos]
                  (if (= 1 (count remaining))
                    remaining
                    (let [criteria (least-common-bits remaining pos)]
                      (filter-pos remaining pos (partial = criteria))
                      ))
                  )
                diag-report)
        first
        )
   2)
    )




(defn life-support-rating [diag-report]
  (* (oxygen-gen-rating diag-report) (c02-scrubber-rating diag-report)))

(->> (parse-diagnostic-report example-diagnostics)
     (life-support-rating))
;; => 230


(->> (parse-diagnostic-report input)
     (life-support-rating))
;; => 4105235
