(ns advent-of-code-2020.day-3
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(defn collect-trees [{:keys [right down]} geology]
  (let [rows (s/split-lines geology)]
    (loop [x 0
           y 0
           visited []]
      (let [next-y (+ y down)
            next-x (+ x right)]
        (if (>= next-y (count rows))
          (->> visited (filter #(= % \#)) count)
          (let [row (nth rows next-y)]
            (recur next-x next-y (conj visited (->>
                                                (cycle row)
                                                (drop next-x)
                                                (first))))))))))

(defn test-multiple-slopes [slopes geology]
  (->> slopes
       (map #(collect-trees % geology))
       (reduce *)))

(comment

  ;; Part 1

  (def example "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

  (collect-trees {:right 3 :down 1} example)
  ;; => 7

  (def data (slurp (io/resource "day-3")))

  (collect-trees {:right 3 :down 1} data)
  ;; => 209



  ;; Part 2


  (def slopes [{:right 1 :down 1}
               {:right 3 :down 1}
               {:right 5 :down 1}
               {:right 7 :down 1}
               {:right 1 :down 2}])

  (test-multiple-slopes slopes example)
  ;; => 336

  (test-multiple-slopes slopes data)
  ;; => 1574890240
)
