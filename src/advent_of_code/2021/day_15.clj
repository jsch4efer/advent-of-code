(ns advent-of-code.2021.day-15
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.set :as set]))

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

(defn find-low-risk-path [cave start]
  (let [dim-y (count cave)
        dim-x (count (first cave))
        end [(dec dim-y) (dec dim-x)]]
    (loop [path->risk {start 0}
           cave->predes {}
           caves-to-visit (apply sorted-map-by (comparator <) (->> (for [y (range 0 dim-y)
                                                                         x (range 0 dim-x)]
                                                                     [(get path->risk [y x] Long/MAX_VALUE) [y x]])
                                                                   (reduce (fn [res [risk cave]]
                                                                             (update res risk (fn [old]
                                                                                                (conj (or old #{}) cave))))
                                                                           {})
                                                                   (mapcat identity)))
           visited #{}
                                                 ]
      (if-let [[risk candidates] (first caves-to-visit)]
        (let [candidate (first candidates)
              new-caves-to-visit (if (= 1 (count candidates))
                                   (dissoc caves-to-visit risk)
                                   (assoc caves-to-visit risk (rest candidates)))
              [next-risks next-predes next-visit] (reduce
                                                   (fn [[risks predes visit] neighbor]
                                                     (if (not (contains? visited neighbor))
                                                       (let [alternative (+ risk (get-in cave neighbor))
                                                             existing (get path->risk neighbor Long/MAX_VALUE)]
                                                         (if (< alternative existing)
                                                           [(assoc risks neighbor alternative)
                                                            (assoc predes neighbor candidate)
                                                            (let [new-relatives (disj (get visit existing) neighbor)]
                                                              (if (empty? new-relatives)
                                                                (dissoc visit existing)
                                                                (-> visit
                                                                    (update existing #(disj % neighbor))
                                                                    (update alternative (partial set/union #{neighbor})))
                                                                ))
                                                            ]
                                                           [risks predes visit]))
                                                       [risks predes visit]))

                                                   [path->risk cave->predes new-caves-to-visit]
                                                   (neighbors candidate dim-x dim-y))]

          (recur next-risks next-predes next-visit (conj visited candidate)))
        (get path->risk end Long/MAX_VALUE)
        ))))

(defn cave-risk-2 [cave-map]
  (let [x-dim (count (first cave-map))
        y-dim (count cave-map)]
    (fn [[y x]]
      (let [base-risk (get-in cave-map [(mod y y-dim) (mod x x-dim)])
            tile-dist (+ (int (/ x x-dim)) (int (/ y y-dim)))
            risk (+ base-risk tile-dist)]
        (inc (mod (dec risk) 9))
        ))) )

(let [r (cave-risk-2 [[8]])]
  (for [y (range 10)]
    (for [x (range 10)]
        (r [(* 10 y) (* 10 x)])
        )
    ))

(defn generate-large-cave [cave]
  (let [risks (cave-risk-2 cave)]
    (into []
          (for [y (range (* 5 (count cave)))]
            (into [] (for [x (range (* 5 (count (first cave))))]
                       (risks [y x])))
            )))
  )

(comment

  ;; Part 1

  (find-low-risk-path (parse-cave example) [0 0])
  ;; => 40

  (find-low-risk-path (parse-cave (slurp (io/resource "2021/day-15"))) [0 0])
  ;; => 602

  ;; Part 2

  (find-low-risk-path (generate-large-cave (parse-cave example)) [0 0])
;; => 315

  (find-low-risk-path (generate-large-cave (parse-cave (slurp (io/resource "2021/day-15")))) [0 0])
;; => 2935
  )
