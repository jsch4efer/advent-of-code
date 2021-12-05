(ns advent-of-code.2021.day-5
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(defn parse-vent-coords [input]
                    (->> input
                         (string/split-lines)
                         (map #(string/split % #" -> "))
                         (map (fn [[start end]]
                                [(->> (string/split start #",") (map read-string) (into []))
                                 (->> (string/split end #",") (map read-string) (into []))] ))))

(defn compute-vent-map [vent-coords point-filter]
  (let [to-range (fn [a b]
                   (cond
                     (< a b) (range a (inc b))
                     (> a b) (range a (dec b) -1)
                     (= a b) (repeat a))
                   )
        vent-map (->> vent-coords
                      (filter point-filter)
                      (mapcat (fn [[[x1 y1] [x2 y2]]]
                                (cond
                                  (= (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))
                                  (zipmap
                                   (for [x (to-range x1 x2)] x)
                                   (for [y (to-range y1 y2)] y))
                                  (= x1 x2) (for [y (to-range y1 y2)] [x1 y])
                                  (= y1 y2) (for [x (to-range x1 x2)] [x y1])
                                  )
                                )
                              )
                      )
        ]
    vent-map
    ))

(defn danger-areas [vents]
  (->> vents
       frequencies
       (filter (fn [[_ freq]] (<= 2 freq)))
       count)
  )

(comment

  (def example-vents "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
")

  ;; Part 1

    (def point-filter-part-1 (fn [[[x1 y1] [x2 y2]]] (or (= x1 x2) (= y1 y2))))

  (-> example-vents
      parse-vent-coords
      (compute-vent-map point-filter-part-1)
      danger-areas)
;; => 5

  (-> (slurp (io/resource "2021/day-5"))
      parse-vent-coords
      (compute-vent-map point-filter-part-1)
      danger-areas)
;; => 8111

  ;; Part 2


  (def point-filter-part-2 (fn [[[x1 y1] [x2 y2]]] (or (= x1 x2) (= y1 y2)
                                                       (= (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))
                                                       )))

  (-> example-vents
      parse-vent-coords
      (compute-vent-map point-filter-part-2)
      danger-areas
      )
;; => 12

  (-> (slurp (io/resource "2021/day-5"))
      parse-vent-coords
      (compute-vent-map point-filter-part-2)
      danger-areas)
;; => 22088


  )
