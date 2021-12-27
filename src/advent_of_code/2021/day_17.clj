(ns advent-of-code.2021.day-17
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(defn step [[[x y] [dx dy] max-y]]
  (let [next-x (+ x dx)
        next-y (+ y dy)]
    [[next-x next-y]
     [(cond (< dx 0) (inc dx)
            (> dx 0) (dec dx)
            :else 0)
      (dec dy)]
     (max next-y max-y)]))

(defn target->on-track? [target]
  (let [max-x (apply max (:x target))
        min-y (apply min (:y target))]
    (fn [[x y]]
      (and (<= x max-x) (>= y min-y)))))

(defn target->target-hit? [target]
  (let [max-x (apply max (:x target))
        min-x (apply min (:x target))
        max-y (apply max (:y target))
        min-y (apply min (:y target))]
    (fn [[x y]]
      (and (<= min-x x max-x) (<= min-y y max-y)))))

(defn compute-trajectory [d target-hit? on-track?]
  (->> (iterate step [[0 0] d 0])
       (take-while (fn [[pos _ _]] (or (target-hit? pos)
                                       (on-track? pos))))))

(defn find-trajectory [target max-y]
  (let [target-hit? (target->target-hit? target)
        on-track? (target->on-track? target)]
    (->> (for [dx (range 1 (inc (apply max (:x target))))
               dy (range (apply min (:y target)) (inc  max-y))]
           [[dx dy] (last (compute-trajectory [dx dy] target-hit? on-track?))])
         (filter (fn [[_ l]] (target-hit? (first l)))))))

(defn find-max-trajectory [target max-y]
  (let [target-hit? (target->target-hit? target)
        on-track? (target->on-track? target)]
    (->> (for [dx (range 1 (inc (apply max (:x target))))
               dy (range (apply min (:y target)) (inc max-y))]
           [[dx dy] (last (compute-trajectory [dx dy] target-hit? on-track?))])
         (filter (fn [[_ l]] (target-hit? (first l))))
         (sort-by (comp last second) (comparator >))
         first)))

(defn parse-target [input]
  (let [matcher (re-matcher #"target area: x=(?<xlow>[-]?\d+)\.\.(?<xhigh>[-]?\d+), y=(?<ylow>[-]?\d+)\.\.(?<yhigh>[-]?\d+)" (string/replace input "\n" ""))]
    (when (.matches matcher)
      {:x [(Long/parseLong (.group matcher "xlow")) (Long/parseLong (.group matcher "xhigh"))]
       :y [(Long/parseLong (.group matcher "ylow")) (Long/parseLong (.group matcher "yhigh"))]})))

(comment

  ;; Part 1

  (let [target (parse-target "target area: x=20..30, y=-10..-5")]
    (find-max-trajectory target 20))
;; => [[6 9] [[21 -10] [0 -11] 45]]

  (let [target (parse-target (slurp (io/resource "2021/day-17")))]
    (find-max-trajectory target 500))
;; => [[15 139] [[120 -140] [0 -141] 9730]]

  ;; Part 2

  (let [target (parse-target "target area: x=20..30, y=-10..-5")]
    (count (find-trajectory target 100)))
;; => 112

  (let [target (parse-target (slurp (io/resource "2021/day-17")))]
    (count (find-trajectory target 500)))
;; => 4110
  )
