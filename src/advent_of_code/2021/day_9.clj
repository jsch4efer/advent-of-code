(ns advent-of-code.2021.day-9
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(defn parse-height-map [input]
  (->> (string/split-lines input)
       (map #(->> (seq %)
                  (map str)
                  (map read-string)
                  (into [])))
       (into [])))

(defn point->neighbors [x y]
  [[y (inc x)]
   [(inc y) x]
   [y (dec x)]
   [(dec y) x]])

(defn to-direction [[y1 x1] [y2 x2]]
  (let [dx (- x1 x2)
        dy (- y1 y2)]
    (cond
      (and (= 0 dx) (> 0 dy)) :D
      (and (= 0 dx) (< 0 dy)) :U
      (and (< 0 dx) (= 0 dy)) :L
      (and (> 0 dx) (= 0 dy)) :R
      :else :S)))

(defn find-low-points [height-map]
  (->> (for [x (range 0 (-> height-map first count))
             y (range 0 (-> height-map count))]
         (let [h (get-in height-map [y x])]
           (when (->> (point->neighbors x y)
                      (map #(get-in height-map % Integer/MAX_VALUE))
                      (map (partial < h))
                      (reduce (fn [res next] (and res next)) true))
             [y x])))
       (filter identity)))

(defn solve-part1 [input]
  (let [height-map (parse-height-map input)]
    (->> (find-low-points height-map)
         (map (partial get-in height-map))
         (map inc)
         (reduce +))))

(defn calculate-directions [height-map width height]
  (->> (for [x (range 0 width)
             y (range 0 height)]
         (let [p [y x]
               v (get-in height-map p)
               nmin (->> (point->neighbors x y)
                         (filter (fn [[ny nx]] (and (<= 0 nx (dec width)) (<= 0 ny (dec height)))))
                         (map (fn [n] [n (- (get-in height-map n) v)]))
                         (reduce (fn [[p vmin] [n v]]
                                   (if (< vmin v)
                                     [p vmin]
                                     [n v]))
                                 [p 0])
                         first)]
           [p nmin]))
       (map (fn [[p n]] [p (to-direction p n)]))
       (reduce (fn [res [p n]] (assoc-in res p n))
               height-map)))

(defn combine-heights-and-directions [heights directions width height]
  (->> (for [x (range 0 width)
             y (range 0 height)]
         [y x])
       (reduce (fn [res next]
                 (update-in res next (fn [height] [height (get-in directions next)])))
               heights)))

(defn collect-basins [heights-and-directions width height]
  (let [step (fn [[y x]]
               (let [[_ d] (get-in heights-and-directions [y x])
                     [ny nx] (case d
                               :L [y (dec x)]
                               :R [y (inc x)]
                               :D [(inc y) x]
                               :U [(dec y) x]
                               :S [y x])]
                 (if (and (<= 0 nx (dec width)) (<= 0 ny (dec height)))
                   [ny nx]
                   [y x])))]
    (->> (for [x (range 0 width)
               y (range 0 height)]
           [y x])
         (filter (fn [p] (not= 9 (first (get-in heights-and-directions p)))))
         (reduce (fn [res p]
                   (let [path (->> (iterate step p)
                                   (partition 2 1)
                                   (take-while (partial apply not=))
                                   (reduce concat))
                         last (last path)]
                     (update res last
                             (fn [basin]
                               (set/union (or basin #{}) (set path))))))
                 {})
         (filter first))))

(defn get-largest-n-basins [basins n]
  (->> basins
       (sort-by #(-> % second count))
       (take-last n)))

(defn solve-part2 [input]
  (let [heights (parse-height-map input)
        width (-> heights first count)
        height (-> heights count)
        directions  (calculate-directions heights width height)
        heights-and-directions (combine-heights-and-directions heights directions width height)
        basins (collect-basins heights-and-directions width height)]
    (->> (get-largest-n-basins basins 3)
         (map (comp count second))
         (reduce *))))

(comment

  (def example-input "2199943210
3987894921
9856789892
8767896789
9899965678
")

  (def input (slurp (io/resource "2021/day-9")))

  ;; Part 1


  (solve-part1 example-input)
;; => 15

  (solve-part1 input)
;; => 591

  ;; Part 2

  (solve-part2 example-input)
;; => 1134

  (solve-part2 (slurp (io/resource "2021/day-9")))
;; => 1113424
  )
