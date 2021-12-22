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

(defn find-low-risk-path [{:keys [dim-x dim-y]} cave->risk start]
  (let [end [(dec dim-y) (dec dim-x)]]
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
        (do (println "First candidate with risk " risk )
           (let [candidate (first candidates)
                 new-caves-to-visit (if (= 1 (count candidates))
                                      (dissoc caves-to-visit risk)
                                      (assoc caves-to-visit risk (rest candidates)))
                 [next-risks next-predes next-visit] (reduce
                                           (fn [[risks predes visit] neighbor]
                                             (if (not (contains? visited neighbor))
                                               (let [alternative (+ risk (cave->risk neighbor))
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

             (println "Candidate checked:" candidate)
             (recur next-risks next-predes next-visit (conj visited candidate))))
        (get path->risk end Long/MAX_VALUE)
        ))))

(defn cave-risk-2 [cave-map]
    (fn [[y x]]
      (let [base-risk (get-in cave-map [(mod y 10) (mod x 10)])
            risk (+ base-risk (int (/ x 10)) (int (/ y 10)))]
        (if (< 9 risk)
          (mod (inc risk) 10)
          risk))
      ))

(comment

  ;; Part 1

  (find-low-risk-path {:dim-x 10 :dim-y 10} (partial get-in (parse-cave example)) [0 0])
  ;; => 40

  (let [cave (parse-cave (slurp (io/resource "2021/day-15")))
        dim {:dim-x (count (first cave)) :dim-y (count cave)}]
    (find-low-risk-path dim (partial get-in cave) [0 0]))
  ;; => 602

  ;; Part 2



  (find-low-risk-path {:dim-x 50 :dim-y 50} (cave-risk-2 (parse-cave example)) [0 0])
;; => 315

  (let [cave (parse-cave (slurp (io/resource "2021/day-15")))
        dim {:dim-x (* 5 (count (first cave))) :dim-y (* 5 (count cave))}]
    (find-low-risk-path dim (cave-risk-2 cave) [0 0]))

  )


(let [s (apply sorted-map  (mapcat identity (into [] {10 :b 2 :c 5 :d})))]
  (first (assoc (dissoc s 2) 3 :e)))
