(ns advent-of-code.2020.day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-answers [input]
  (->> input
       string/split-lines
       (partition-by #(when (= % "") :split))
       (filter #(not= % '("")))))

(defn count-yes-for-anyone [answers]
  (->> answers
       (map #(->> % (mapcat seq) (into #{})))
       (map count)
       (reduce +)))

(defn count-yes-for-everyone [answers]
  (->> answers
       (map (fn [group]
              (->> group
                   (map #(->> % seq (into #{})))
                   (reduce clojure.set/intersection))))
       (map count)
       (reduce +)))

(comment

  ;; Part 1

  (def example-data "abc

a
b
c

ab
ac

a
a
a
a

b")

  (->> example-data
       parse-answers
       count-yes-for-anyone)
  ;; => 11


  (->> (slurp (io/resource "2020/day-6"))
       parse-answers
       count-yes-for-anyone)
  ;; => 6878




  ;; Part 2


  (->> example-data
       parse-answers
       count-yes-for-everyone)
  ;; => 6

  (->> (slurp (io/resource "2020/day-6"))
       parse-answers


       count-yes-for-everyone))
