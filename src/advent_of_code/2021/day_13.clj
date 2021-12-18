(ns advent-of-code.2021.day-13
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(defn parse-page [input]
  (let [lines  (string/split-lines input)
        [top bottom] (->> lines
                          (split-with not-empty))
        points (->> top
                    (map #(->> (string/split % #",")
                               (map read-string)
                               (into [])))
                    (into #{}))
        instructions (->> bottom
                          (drop 1)
                          (map #(last (string/split % #" ")))
                          (map (fn [inst]
                                 (let [[left right] (string/split inst  #"=")]
                                   [(case left "x" :left "y" :up) (read-string right)]))))]

    [points instructions]))

(defn fold-page [page [direction pos]]
  (->> page
       (map (fn [[x y]]
              (let [dx (- pos x)
                    dy (- pos y)]
                (case direction
                  :up     (cond
                            (> dy 0) [x y]
                            (< dy 0) [x (+ pos dy)])
                  :left  (cond
                           (> dx 0) [x y]
                           (< dx 0) [(+ pos dx) y])))))
       (filter identity)
       (into #{})))

(defn read-chars [paper]
  (->> (for [i (range 0 9)]
         (->> (for [x (range 0 5)
                    y (range 0 7)]
                (let [xi (+ (* 5 i) x)]
                  (if (paper [xi y]) [[y x] "#"] [[y x] "."])))
              (reduce (fn [res [p c]]
                        (assoc-in res p c))
                      (->> (vec (repeat 7 (vec (repeat 5 "."))))))))
       (into [])))

(comment
  (def example "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
")

  (let [[points instructions] (parse-page example)]
    (println (sort points))
    (-> points
        (fold-page  (first instructions))
        count))
;; => 17


  (let [[points instructions] (parse-page (slurp (io/resource "2021/day-13")))]
    (count (fold-page points (first instructions))))
;; => 923

  (let [[points instructions] (parse-page (slurp (io/resource "2021/day-13")))
        folded (reduce fold-page points instructions)
        chars (read-chars folded)]
    (->> (apply mapv vector chars)
         (run! println)))
; [[# # # . .] [# # # # .] [# # # . .] [. # # . .] [. # # . .] [. . # # .] [# # # . .] [# # # . .] [. . . . .]]
; [[# . . # .] [# . . . .] [# . . # .] [# . . # .] [# . . # .] [. . . # .] [# . . # .] [# . . # .] [. . . . .]]
; [[# . . # .] [# # # . .] [# . . # .] [# . . . .] [# . . . .] [. . . # .] [# . . # .] [# # # . .] [. . . . .]]
; [[# # # . .] [# . . . .] [# # # . .] [# . . . .] [# . # # .] [. . . # .] [# # # . .] [# . . # .] [. . . . .]]
; [[# . . . .] [# . . . .] [# . # . .] [# . . # .] [# . . # .] [# . . # .] [# . . . .] [# . . # .] [. . . . .]]
; [[# . . . .] [# # # # .] [# . . # .] [. # # . .] [. # # # .] [. # # . .] [# . . . .] [# # # . .] [. . . . .]]
; [[. . . . .] [. . . . .] [. . . . .] [. . . . .] [. . . . .] [. . . . .] [. . . . .] [. . . . .] [. . . . .]]
  )
