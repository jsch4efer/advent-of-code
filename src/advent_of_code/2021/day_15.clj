(ns advent-of-code.2021.day-15
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(def example "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
")

(defn parse-cave [input]
  (->> (string/split-lines input)
       (map #(->> %
                  (map (comp read-string str))
                  (into [])))
       (into [])))

(defn neighbors [cave dim-x dim-y]
  (->> [[0 1] [1 0] [-1 0] [0 -1]]
       (map (partial mapv + cave))
       (filter (fn [[ny nx]]
                 (and (<= 0 ny (dec dim-y)) (<= 0 nx (dec dim-x)))))
       (into [])))

(defn find-low-risk-path [cave-map start]
  (let [dim-x (count (first cave-map))
        dim-y (count cave-map)
        end [(dec dim-y) (dec dim-x)]]
    (loop [cave->risk (assoc-in
                       (->> (repeat (->> (repeat Long/MAX_VALUE)
                                         (take dim-x)
                                         (into [])))
                            (take dim-y)
                            (into []))
                       start
                       0)
           cave->predes {}
           caves-to-visit (into #{} (for [y (range 0 dim-y)
                                          x (range 0 dim-x)]
                                      [y x]))]
      (if-let [candidate (->> caves-to-visit
                              (sort-by #(get-in cave->risk %))
                              (first))]
        (let [[next-risks next-predes] (reduce
                                        (fn [[risks predes] neighbor]
                                          (if (contains? caves-to-visit neighbor)
                                            (let [alternative (+ (get-in cave->risk candidate) (get-in cave-map neighbor))]
                                              (if (< alternative (get-in cave->risk neighbor))
                                                [(assoc-in risks neighbor alternative)
                                                 (assoc-in predes neighbor candidate)]
                                                [risks predes]))
                                            [risks predes]))

                                        [cave->risk cave->predes]
                                        (neighbors candidate dim-x dim-y))]

          (recur next-risks next-predes (disj caves-to-visit candidate)))
        (get-in cave->risk end)))))

(comment

  (find-low-risk-path (parse-cave example) [0 0])
  ;; => 40

  (find-low-risk-path (parse-cave (slurp (io/resource "2021/day-15"))) [0 0])
  ;; => 602
  )
