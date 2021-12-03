(ns advent-of-code.2020.day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


(defn parse-seats [input]
  (->> input
       string/split-lines
       (map (fn [line]
              (->> line
                   (map #(case % \L 0 \. nil \# 1) )
                   (into []))))
       (into [])))

(defn count-occupied-seats [seats]
  (->> seats
       (mapcat identity)
       (filter identity)
       (reduce +))
  )

(defn seat-round [seats count-occupied-neighbours seat-rules]
  (->>
   (for [x (-> seats first count range)
         y (-> seats count range)]
     [[y x] (count-occupied-neighbours seats [y x])])
   (reduce (fn [last [pos n]]
             (assoc-in last pos (seat-rules (get-in seats pos) n)))
            seats)))

(defn simulate-seats [input count-occupied-neighbours seat-rules]
  (loop [seats (parse-seats input)]
    (let [next-seats (seat-round seats count-occupied-neighbours seat-rules)]
      (if (= seats next-seats)
        next-seats
        (recur next-seats))
      )
    ))

(comment

  ;; Part 1

  (def example "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

  (def seat-range-part-1 (fn [seats [y x]]
                           (->> (for [y-n (range (dec y) (+ y 2))
                                      x-n (range (dec x) (+ x 2))]
                                  [y-n x-n])
                                (filter (partial not= [y x]))
                                (map (partial get-in seats))
                                (filter identity)
                                (reduce +))))

  (def seat-rule-part-1 (fn [seat neighbor-count]
                          (case seat
                            0 (if (= neighbor-count 0) 1 0)
                            1 (if (<= 4 neighbor-count) 0 1)
                            nil nil
                            )))

  (count-occupied-seats (simulate-seats example seat-range-part-1 seat-rule-part-1))
  ;; => 37

  (def input (-> "2020/day-11" io/resource slurp))

  (count-occupied-seats (simulate-seats input seat-range-part-1 seat-rule-part-1))
  ;; => 2204

  ;; Part 2

  (def example-2 ".......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#.....")

  (def seat-range-part-2 (fn [seats [y x]]
                           (let [max-x-pos (-> seats first count dec)
                                 max-y-pos (-> seats count dec)
                                 valid-pos? (fn [[y x]]
                                              (and (<= 0 y max-y-pos)
                                                   (<= 0 x max-x-pos)))
                                 gen-pos (fn [pos steps]
                                           (->> (iterate
                                                    (fn [last-pos] [(+ (first last-pos) (first steps))
                                                                    (+ (second last-pos) (second steps))])
                                                    pos)
                                                (drop 1)
                                                (take-while valid-pos?)
                                                ))
                                 occupied-neighbor (fn [seats direction]
                                                     (or (->> direction
                                                              (map (partial get-in seats))
                                                              (filter #(or (= 1 %) (= 0 %)))
                                                              (first))
                                                         0))
                                 ]
                           (->> [[0 1] [0 -1] [1 0] [-1 0] [1 1] [1 -1] [-1 1] [-1 -1]]
                                (map #(gen-pos [y x] %))
                                (map #(occupied-neighbor seats %))
                                (reduce +)
                                )

                           )))

  (def seat-rule-part-2 (fn [seat neighbor-count]
                          (case seat
                            0 (if (= neighbor-count 0) 1 0)
                            1 (if (<= 5 neighbor-count) 0 1)
                            nil nil
                            )))

 (def example-empty ".##.##.
#.#.#.#
##...##
...L...
##...##
#.#.#.#
.##.##.")

  (count-occupied-seats (simulate-seats example-2 seat-range-part-2 seat-rule-part-2))
;; => 8

  (count-occupied-seats (simulate-seats example seat-range-part-2 seat-rule-part-2))
;; => 26

  (count-occupied-seats (simulate-seats input seat-range-part-2 seat-rule-part-2))
;; => 1986

  )
