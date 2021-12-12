(ns advent-of-code.2021.day-11
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(defn parse-octopus [input]
  (->> (string/split-lines input)
       (map (fn [line] (->> line (map (comp read-string str)) (into []))))
       (into [])))

(defn neighborhood [[y x]]
  (->> (for [dy [-1 0 1]
             dx [-1 0 1]]
         [(+ y dy) (+ x dx)])
       (filter (fn [[ny nx]]
                 (and (<= 0 ny 9)
                      (<= 0 nx 9))))))

(defn flashed-neighbors [octopus flashed? x-dim y-dim]
  (let [flashing (->> (for [x (range 0 x-dim) y (range 0 y-dim)] [y x])
                      (filter #(not (flashed? %)))
                      (filter #(< 9 (get-in octopus %)))
                      (into #{}))]
    [(->> flashing
          (mapcat neighborhood)
          (reduce (fn [res n] (update-in res n inc)) octopus))
     (set/union flashed? flashing)]))

(defn increase-energy [octopus x-dim y-dim]
  (->> (for [x (range 0 x-dim)
             y (range 0 y-dim)]
         [y x])
       (reduce (fn [os o]
                 (update-in os o inc))
               octopus)))

(defn print-octopus [octopus]
  (println "--------")
  (->> octopus (map identity) (run! println))
  (println "--------")
  octopus)

(defn step [[octopus flash-count]]
  (let [y-dim (-> octopus count)
        x-dim (-> octopus first count)]
    (loop [octopus (increase-energy octopus x-dim y-dim)
           flashed? #{}
           iter 0]
      (let [[flashed-octopus new-flashed?] (flashed-neighbors octopus flashed? x-dim y-dim)]
        (if (= flashed? new-flashed?)
          [(reduce #(assoc-in %1 %2 0) flashed-octopus new-flashed?) (+ flash-count (count new-flashed?))]
          (recur flashed-octopus new-flashed? (inc iter)))))))

(defn count-flashed-octopus [octopus steps]
  (->> (iterate step [octopus 0])
       (take (inc steps))
       (last)
       (second)
       ))

(comment

  (def example-input "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
")

  (def input "1326253315
3427728113
5751612542
6543868322
4422526221
2234325647
1773174887
7281321674
6562513118
4824541522
")

  ;; Part 1

  (count-flashed-octopus (parse-octopus example-input) 100)
  ;; => 1656

  (count-flashed-octopus (parse-octopus input) 100))
;; => 1749
